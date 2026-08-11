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
text, expanded URLs, the author's display name, and the author's handle.  A
nested list matches only when all of its strings are nonempty and occur, which
lets specific split templates avoid broad single-keyword matches.  The
conservative defaults come from repeated spam in real public replies, with
Chinese patterns prioritized over English ones.  The thread's focus tweet is
never filtered.  Set this option to nil to disable keyword filtering, or
replace and extend the list with local patterns."
  :type '(repeat (choice string (repeat string)))
  :group 'chirp)

(defcustom chirp-thread-spam-rules-file
  (locate-user-emacs-file "chirp/spam-rules.txt")
  "File containing persistent user spam phrases and keywords.

Store one literal rule per line.  Empty lines and lines beginning with `#' are
ignored.  These rules share the same case-insensitive match scope as
`chirp-thread-spam-keywords': reply text, expanded URLs, author display names,
and author handles."
  :type 'file
  :group 'chirp)

(defun chirp-thread--normalize-spam-rule (rule)
  "Return RULE as one trimmed line, or nil when it is empty."
  (when (stringp rule)
    (let ((normalized
           (string-trim
            (replace-regexp-in-string "[[:space:]]+" " " rule))))
      (unless (string-empty-p normalized)
        normalized))))

(defun chirp-thread--literal-rule-present-p (rule rules)
  "Return non-nil when literal RULE already occurs in RULES ignoring case."
  (when-let* ((key (chirp-thread--normalize-spam-rule rule)))
    (setq key (downcase key))
    (cl-some
     (lambda (candidate)
       (and (stringp candidate)
            (equal key
                   (downcase
                    (or (chirp-thread--normalize-spam-rule candidate) "")))))
     rules)))

(defun chirp-thread--read-user-spam-rules ()
  "Return literal spam rules read from `chirp-thread-spam-rules-file'."
  (when (and (stringp chirp-thread-spam-rules-file)
             (file-readable-p chirp-thread-spam-rules-file)
             (not (file-directory-p chirp-thread-spam-rules-file)))
    (with-temp-buffer
      (insert-file-contents chirp-thread-spam-rules-file)
      (let ((seen (make-hash-table :test #'equal))
            rules)
        (dolist (line (split-string (buffer-string) "\n"))
          (when-let* ((rule (chirp-thread--normalize-spam-rule line))
                      ((not (string-prefix-p "#" rule)))
                      (key (downcase rule))
                      ((not (gethash key seen))))
            (puthash key t seen)
            (push rule rules)))
        (nreverse rules)))))

(defun chirp-thread--effective-spam-rules ()
  "Return built-in, customized, and persistent literal spam rules."
  (let ((rules (copy-tree chirp-thread-spam-keywords)))
    (dolist (rule (chirp-thread--read-user-spam-rules) rules)
      (unless (chirp-thread--literal-rule-present-p rule rules)
        (setq rules (append rules (list rule)))))))

(defun chirp-thread--append-user-spam-rule (rule)
  "Append literal RULE to `chirp-thread-spam-rules-file'."
  (let* ((file (expand-file-name chirp-thread-spam-rules-file))
         (directory (file-name-directory file))
         (needs-newline
          (and (file-readable-p file)
               (> (file-attribute-size (file-attributes file)) 0)
               (with-temp-buffer
                 (insert-file-contents file)
                 (not (eq (char-before (point-max)) ?\n))))))
    (make-directory directory t)
    (with-temp-buffer
      (set-buffer-file-coding-system 'utf-8-unix)
      (when needs-newline
        (insert "\n"))
      (insert rule "\n")
      (write-region (point-min) (point-max) file t 'silent))))

(defun chirp-thread--spam-rule-suggestion (authorp)
  "Return a spam-rule suggestion from point.

When AUTHORP is non-nil, prefer the current author's display name or handle.
Otherwise prefer the active region and then the current reply text."
  (let ((entry (chirp-entry-at-point)))
    (chirp-thread--normalize-spam-rule
     (cond
      (authorp
       (or (plist-get entry :author-name)
           (plist-get entry :author-handle)))
      ((use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end)))
      ((eq (plist-get entry :kind) 'tweet)
       (plist-get entry :text))))))

(defun chirp-thread-add-spam-rule (&optional authorp)
  "Persist one literal spam phrase or keyword and refresh the current view.

Use the active region as the initial input, or the current reply text when no
region is active.  With prefix argument AUTHORP, use the current author's
display name or handle instead."
  (interactive "P")
  (let* ((suggestion (chirp-thread--spam-rule-suggestion authorp))
         (rule (chirp-thread--normalize-spam-rule
                (read-string "Spam phrase or keyword: " suggestion))))
    (unless rule
      (user-error "Spam rule cannot be empty"))
    (when (string-prefix-p "#" rule)
      (user-error "Spam rule cannot begin with #"))
    (if (chirp-thread--literal-rule-present-p
         rule (chirp-thread--effective-spam-rules))
        (message "Spam rule already exists: %s" rule)
      (chirp-thread--append-user-spam-rule rule)
      (when (functionp chirp--refresh-function)
        (chirp-refresh))
      (message "Added spam rule: %s" rule))))

(defun chirp-thread-edit-spam-rules ()
  "Open `chirp-thread-spam-rules-file' for manual editing."
  (interactive)
  (unless (and (stringp chirp-thread-spam-rules-file)
               (not (string-empty-p chirp-thread-spam-rules-file)))
    (user-error "No user spam rules file is configured"))
  (let ((file (expand-file-name chirp-thread-spam-rules-file)))
    (make-directory (file-name-directory file) t)
    (find-file file)))

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

(defun chirp-thread--spam-reply-p (tweet &optional rules)
  "Return non-nil when reply TWEET or its author matches a spam keyword.

Use RULES instead of `chirp-thread-spam-keywords' when it is non-nil."
  (and (not (eq (plist-get tweet :timeline-context) 'related))
       (let ((case-fold-search t)
             (content
              (string-join
               (cl-remove-if-not
                #'stringp
                (append (list (plist-get tweet :text)
                              (plist-get tweet :author-name)
                              (plist-get tweet :author-handle))
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
            (or rules chirp-thread-spam-keywords))))))

(defun chirp-thread--filter-spam-replies (tweets)
  "Hide keyword-matching replies from TWEETS while preserving the focus."
  (if (or (null tweets)
          (null chirp-thread-spam-keywords))
      tweets
    (let ((rules (chirp-thread--effective-spam-rules)))
      (cons (car tweets)
            (cl-remove-if
             (lambda (tweet)
               (chirp-thread--spam-reply-p tweet rules))
             (cdr tweets))))))

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
