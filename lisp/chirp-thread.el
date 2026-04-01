;;; chirp-thread.el --- Thread view for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)

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

(defun chirp-thread--title (tweet-or-url)
  "Return a display title for TWEET-OR-URL."
  (if (and (stringp tweet-or-url)
           (string-match "/status/\\([0-9]+\\)" tweet-or-url))
      (format "Thread: %s" (match-string 1 tweet-or-url))
    (format "Thread: %s"
            (if (stringp tweet-or-url)
                tweet-or-url
              (or (plist-get tweet-or-url :id) "tweet")))))

(defun chirp-thread--render-view (buffer title refresh ordered &optional anchor-id)
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
               (chirp-goto-entry-id anchor-id))
          (chirp-move-point-to-first-entry)))))

(defun chirp-thread-open (tweet-or-url &optional focus-id)
  "Open a thread for TWEET-OR-URL."
  (interactive "sTweet ID or URL: ")
  (let* ((request-target (cond
                          ((stringp tweet-or-url) tweet-or-url)
                          ((plist-get tweet-or-url :url))
                          ((plist-get tweet-or-url :id))
                          (t (user-error "Need a tweet id or URL"))))
         (title (chirp-thread--title request-target))
         (buffer (chirp-buffer))
         (refresh (lambda () (chirp-thread-open request-target focus-id))))
    (chirp--maybe-push-history)
    (let ((token (chirp-show-loading buffer title refresh)))
      (chirp-backend-thread
       request-target
       (lambda (tweets _envelope)
         (when (chirp-request-current-p buffer token)
           (let ((ordered (chirp-thread--reorder tweets focus-id)))
             (chirp-thread--render-view buffer title refresh ordered)
             (with-current-buffer buffer
               (setq-local chirp--rerender-function
                           (let ((saved-ordered ordered)
                                 (saved-title title)
                                 (saved-refresh refresh))
                             (lambda ()
                               (chirp-thread--render-view
                                buffer
                                saved-title
                                saved-refresh
                                saved-ordered
                                (with-current-buffer buffer
                                  (chirp-entry-id-at-point)))))))
             (chirp-media-prefetch-tweets ordered buffer))))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-show-error buffer title refresh message)))))))

(provide 'chirp-thread)

;;; chirp-thread.el ends here
