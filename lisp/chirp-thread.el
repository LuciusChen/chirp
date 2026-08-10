;;; chirp-thread.el --- Thread view for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch, enrich, order, and render a focused tweet conversation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)
(require 'chirp-spam-rules)

(defcustom chirp-thread-spam-keywords
  (copy-tree chirp-spam-rules-default)
  "Keywords used to hide replies in thread views.

Each nonempty string is matched literally and case-insensitively against reply
text and expanded URLs.  A nested list matches only when all of its strings are
nonempty and occur, which lets specific split templates avoid broad
single-keyword matches.  The conservative defaults come from repeated spam in
real public replies, with Chinese patterns prioritized over English ones.  The
thread's focus tweet is never filtered.  Set this option to nil to disable
keyword filtering, or replace and extend the list with local patterns."
  :type '(repeat (choice string (repeat string)))
  :group 'chirp)

(defun chirp-thread--key (tweet)
  "Return a stable key for TWEET."
  (or (plist-get tweet :id)
      (plist-get tweet :url)))

(defun chirp-thread--reorder (tweets focus-id)
  "Move the tweet matching FOCUS-ID to the front of TWEETS."
  (if (not focus-id)
      tweets
    (let* ((focus (cl-find focus-id tweets
                           :key #'chirp-thread--key
                           :test #'equal))
           (rest (cl-remove focus-id tweets
                            :key #'chirp-thread--key
                            :test #'equal)))
      (if focus
          (cons focus rest)
        tweets))))

(defun chirp-thread--spam-reply-p (tweet)
  "Return non-nil when reply TWEET matches a configured spam keyword."
  (and (not (eq (plist-get tweet :timeline-context) 'related))
       (let ((case-fold-search t)
             (content
              (string-join
               (cl-remove-if-not
                #'stringp
                (cons (plist-get tweet :text)
                      (plist-get tweet :urls)))
               "\n")))
         (cl-labels
             ((matches
               (keyword)
               (when (stringp keyword)
                 (let ((trimmed (string-trim keyword)))
                   (and (not (string-empty-p trimmed))
                        (string-match-p (regexp-quote trimmed) content))))))
           (cl-some
            (lambda (rule)
              (if (listp rule)
                  (and rule (cl-every #'matches rule))
                (matches rule)))
            chirp-thread-spam-keywords)))))

(defun chirp-thread--filter-spam-replies (tweets)
  "Hide keyword-matching replies from TWEETS while preserving the focus."
  (if (or (null tweets)
          (null chirp-thread-spam-keywords))
      tweets
    (cons (car tweets)
          (cl-remove-if #'chirp-thread--spam-reply-p (cdr tweets)))))

(defun chirp-thread--title (tweet-or-url)
  "Return a display title for TWEET-OR-URL."
  (if (and (stringp tweet-or-url)
           (string-match "/status/\\([0-9]+\\)" tweet-or-url))
      (format "Thread: %s" (match-string 1 tweet-or-url))
    (format "Thread: %s"
            (if (stringp tweet-or-url)
                tweet-or-url
              (or (plist-get tweet-or-url :id) "tweet")))))

(defun chirp-thread--seed-tweets (tweet-or-url focus-id)
  "Return a renderable list for TWEET-OR-URL matching FOCUS-ID, or nil."
  (when (and (listp tweet-or-url)
             (eq (plist-get tweet-or-url :kind) 'tweet)
             (or (null focus-id)
                 (equal (plist-get tweet-or-url :id) focus-id)))
    (list tweet-or-url)))

(defun chirp-thread--article-fetch-needed-p (tweet)
  "Return non-nil when TWEET should be enriched via `twitter article'."
  (and (plist-get tweet :id)
       (not (chirp-first-nonblank (plist-get tweet :article-text)))
       (or (chirp-first-nonblank (plist-get tweet :article-title))
           (and (string-empty-p (or (plist-get tweet :text) ""))
                (plist-get tweet :urls)))))

(defun chirp-thread--maybe-apply-article (tweets article-tweet)
  "Return TWEETS with ARTICLE-TWEET replacing the current focus when ids match."
  (if (and tweets
           article-tweet
           (equal (plist-get (car tweets) :id)
                  (plist-get article-tweet :id)))
      (cons article-tweet (cdr tweets))
    tweets))

(defun chirp-thread--render-view
    (buffer title refresh ordered &optional anchor-id display-p)
  "Render ORDERED thread tweets into BUFFER.

TITLE and REFRESH are the usual buffer metadata.  When ANCHOR-ID is non-nil,
restore point to that entry after rendering."
  (let ((focus (car ordered))
        (replies (cdr ordered)))
    (chirp-render-into-buffer
     buffer title refresh
     (lambda ()
       (if focus
           (progn
             (chirp-render-insert-thread-focus-tweet focus)
             (when replies
               (chirp-render-insert-thread-divider)
               (dolist (tweet replies)
                 (chirp-render-insert-thread-reply tweet))))
         (chirp-render-insert-empty "No thread data returned."))))
    (with-current-buffer buffer
      (or (and anchor-id
               (chirp-restore-point-anchor anchor-id))
          (chirp-move-point-to-first-entry)))
    (when display-p
      (chirp-display-buffer buffer))))

(defun chirp-thread-open (tweet-or-url &optional focus-id buffer)
  "Open a thread for TWEET-OR-URL focused on FOCUS-ID in BUFFER."
  (interactive "sTweet ID or URL: ")
  (let* ((request-target (cond
                          ((stringp tweet-or-url) tweet-or-url)
                          ((plist-get tweet-or-url :url))
                          ((plist-get tweet-or-url :id))
                          (t (user-error "Need a tweet id or URL"))))
         (title (chirp-thread--title request-target))
         (buffer (or buffer (chirp-buffer)))
         (refresh (lambda ()
                    (chirp-backend-invalidate-thread request-target)
                    (when focus-id
                      (chirp-backend-invalidate-article focus-id))
                    (chirp-thread-open request-target focus-id buffer)))
         (saved-ordered nil)
         (prefetched-article nil)
         (article-requested-p nil)
         (token nil))
    (cl-labels
        ((render-current (&optional anchor-id display-p)
           (chirp-thread--render-view
            buffer title refresh saved-ordered anchor-id display-p)
           (with-current-buffer buffer
             (setq-local chirp--rerender-function
                         (lambda ()
                           (render-current
                            (with-current-buffer buffer
                              (chirp-capture-point-anchor)))))))
         (prefetch-current ()
           (chirp-media-prefetch-tweets saved-ordered buffer)
           (chirp-enrich-quoted-tweets saved-ordered buffer))
         (present-current (&optional anchor-id display-p)
           (render-current anchor-id display-p)
           (prefetch-current))
         (apply-prefetched-article ()
           (setq saved-ordered
                 (chirp-thread--maybe-apply-article
                  saved-ordered
                  prefetched-article)))
         (handle-article-success (article-tweet _envelope)
           (when (chirp-request-current-p buffer token)
             (setq prefetched-article article-tweet)
             (when saved-ordered
               (let ((anchor-id (with-current-buffer buffer
                                  (chirp-capture-point-anchor))))
                 (apply-prefetched-article)
                 (present-current anchor-id)
                 (chirp-clear-status buffer)))))
         (maybe-request-article (tweet)
           (when (and (not article-requested-p)
                      (chirp-thread--article-fetch-needed-p tweet))
             (setq article-requested-p t)
             (chirp-set-status buffer "Thread ready · loading article...")
             (chirp-backend-article
              (plist-get tweet :id)
              #'handle-article-success
              (lambda (_message)
                (when (chirp-request-current-p buffer token)
                  (chirp-clear-status buffer)))))))
    (setq token (chirp-begin-background-request buffer title))
    (chirp-set-status buffer "Loading thread...")
    (when-let* ((seed (chirp-thread--seed-tweets tweet-or-url focus-id)))
      (setq saved-ordered seed)
      (present-current nil t))
    (when (and (listp tweet-or-url)
               (plist-get tweet-or-url :id))
      (maybe-request-article tweet-or-url))
    (chirp-backend-thread
     request-target
     (lambda (tweets _envelope)
       (when (chirp-request-current-p buffer token)
         (setq saved-ordered
               (chirp-thread--filter-spam-replies
                (chirp-thread--reorder tweets focus-id)))
         (apply-prefetched-article)
         (present-current nil t)
         (if-let* ((focus (car saved-ordered)))
             (progn
               (maybe-request-article focus)
               (unless article-requested-p
                 (chirp-clear-status buffer)))
           (chirp-clear-status buffer))))
     (lambda (message)
       (when (chirp-request-current-p buffer token)
         (chirp-show-error buffer title refresh message)))))))

(provide 'chirp-thread)

;;; chirp-thread.el ends here
