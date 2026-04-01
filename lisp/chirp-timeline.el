;;; chirp-timeline.el --- Timeline views for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)

(defun chirp-timeline--set-kind (buffer kind)
  "Record timeline KIND in BUFFER."
  (with-current-buffer buffer
    (setq-local chirp--timeline-kind kind)))

(defun chirp-timeline--title (kind)
  "Return the buffer title for timeline KIND."
  (pcase kind
    ('home "For You")
    ('following "Following")
    (_ "Timeline")))

(defun chirp-timeline--following-p (kind)
  "Return non-nil when KIND is the Following timeline."
  (eq kind 'following))

(defun chirp-timeline--refresh-function (kind)
  "Return a refresh function for timeline KIND."
  (lambda ()
    (chirp-timeline--open kind (or chirp--timeline-limit
                                   chirp-default-max-results))))

(defun chirp-timeline--current-count (buffer)
  "Return the current timeline post count for BUFFER."
  (with-current-buffer buffer
    (or chirp--timeline-count
        (let ((count 0)
              (pos (chirp--entry-position-forward (point-min))))
          (while pos
            (setq count (1+ count)
                  pos (chirp--entry-position-forward
                       (min (point-max) (1+ pos)))))
          count))))

(defun chirp-timeline--render (buffer title refresh tweets &optional kind limit anchor-id)
  "Render TWEETS into BUFFER."
  (let ((tweet-count (length tweets)))
    (chirp-render-into-buffer
     buffer title refresh
     (lambda ()
       (if tweets
           (dolist (tweet tweets)
             (chirp-render-insert-tweet tweet))
         (chirp-render-insert-empty "No posts returned."))))
  (with-current-buffer buffer
    (setq-local chirp--timeline-kind kind)
    (setq-local chirp--timeline-limit (and kind limit))
    (setq-local chirp--timeline-count (and kind tweet-count))
    (setq-local chirp--timeline-load-more-function
                (and (memq kind '(home following))
                     #'chirp-load-more))
    (setq-local chirp--timeline-exhausted-p
                (and (memq kind '(home following))
                     (< tweet-count limit)))
    (setq-local chirp--rerender-function
                (let ((saved-tweets tweets)
                      (saved-title title)
                      (saved-refresh refresh)
                      (saved-kind kind)
                      (saved-limit limit))
                  (lambda ()
                    (chirp-timeline--render
                     buffer
                     saved-title
                     saved-refresh
                     saved-tweets
                     saved-kind
                     saved-limit
                     (chirp-entry-id-at-point)))))
    (setq-local chirp--timeline-loading-more nil)
    (or (and anchor-id
             (chirp-goto-entry-id anchor-id))
        (chirp-move-point-to-first-entry)))
    (chirp-media-prefetch-tweets tweets buffer)))

(defun chirp-timeline--handle-feed-success
    (buffer title refresh tweets kind limit anchor-id loading-more previous-count)
  "Handle a successful feed response for BUFFER."
  (let ((tweet-count (length tweets)))
    (if (and loading-more
             (numberp previous-count)
             (<= tweet-count previous-count))
        (with-current-buffer buffer
          (setq-local chirp--timeline-loading-more nil)
          (setq-local chirp--request-token nil)
          (setq-local chirp--timeline-exhausted-p t))
      (chirp-timeline--render buffer title refresh tweets kind limit anchor-id)
      (when (and loading-more
                 (< tweet-count limit))
        (message "No older posts.")))
    (when (and loading-more
               (numberp previous-count)
               (<= tweet-count previous-count))
      (message "No older posts."))))

(defun chirp-timeline--open (kind &optional limit anchor-id loading-more)
  "Open timeline KIND with LIMIT posts.

When ANCHOR-ID is non-nil, restore point to that entry after rendering.
When LOADING-MORE is non-nil, keep the current buffer visible while fetching."
  (let* ((buffer (chirp-buffer))
         (title (chirp-timeline--title kind))
         (limit (or limit chirp-default-max-results))
         (refresh (chirp-timeline--refresh-function kind))
         (previous-count (and loading-more
                              (chirp-timeline--current-count buffer)))
         (token (if loading-more
                    (progn
                      (with-current-buffer buffer
                        (setq-local chirp--timeline-loading-more t))
                      (chirp-begin-request buffer))
                  (progn
                    (chirp--maybe-push-history)
                    (chirp-show-loading buffer title refresh)))))
    (when loading-more
      (message "Loading older posts..."))
    (chirp-timeline--set-kind buffer kind)
    (chirp-backend-feed
     (lambda (tweets _envelope)
       (when (chirp-request-current-p buffer token)
         (chirp-timeline--handle-feed-success
          buffer title refresh tweets kind limit anchor-id loading-more previous-count)))
     (chirp-timeline--following-p kind)
     (lambda (message)
       (when (chirp-request-current-p buffer token)
         (with-current-buffer buffer
           (setq-local chirp--timeline-loading-more nil)
           (setq-local chirp--request-token nil))
         (chirp-timeline--set-kind buffer kind)
         (if loading-more
             (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))
           (chirp-show-error buffer title refresh message))))
     limit)))

(defun chirp-timeline-open-home ()
  "Open the home timeline."
  (interactive)
  (chirp-timeline--open 'home chirp-default-max-results))

(defun chirp-timeline-open-following ()
  "Open the following timeline."
  (interactive)
  (chirp-timeline--open 'following chirp-default-max-results))

(defun chirp-load-more (&optional anchor-id)
  "Load older posts for the current Home or Following timeline."
  (interactive)
  (unless (memq chirp--timeline-kind '(home following))
    (user-error "Current view does not support loading more posts"))
  (cond
   (chirp--timeline-loading-more
    (message "Already loading older posts..."))
   (chirp--timeline-exhausted-p
    (message "No older posts."))
   (t
    (chirp-timeline--open
     chirp--timeline-kind
     (+ (or chirp--timeline-limit chirp-default-max-results)
        (max 1 chirp-timeline-load-more-step))
     (or anchor-id (chirp-entry-id-at-point))
     t))))

(defun chirp-timeline-open-bookmarks ()
  "Open bookmarks."
  (interactive)
  (let* ((buffer (chirp-buffer))
         (refresh #'chirp-timeline-open-bookmarks))
    (chirp--maybe-push-history)
    (let ((token (chirp-show-loading buffer "Bookmarks" refresh)))
      (chirp-timeline--set-kind buffer nil)
      (chirp-backend-bookmarks
       (lambda (tweets _envelope)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--render buffer "Bookmarks" refresh tweets nil)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--set-kind buffer nil)
           (chirp-show-error buffer "Bookmarks" refresh message)))))))

(defun chirp-timeline-open-search (query)
  "Open search results for QUERY."
  (interactive "sSearch X: ")
  (let* ((title (format "Search: %s" query))
         (buffer (chirp-buffer))
         (refresh (lambda () (chirp-timeline-open-search query))))
    (chirp--maybe-push-history)
    (let ((token (chirp-show-loading buffer title refresh)))
      (chirp-timeline--set-kind buffer nil)
      (chirp-backend-search
       query
       (lambda (tweets _envelope)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--render buffer title refresh tweets nil)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--set-kind buffer nil)
           (chirp-show-error buffer title refresh message)))))))

(defun chirp-toggle-home-following ()
  "Toggle between the home and following timelines."
  (interactive)
  (pcase chirp--timeline-kind
    ('home
     (chirp-timeline-open-following))
    ('following
     (chirp-timeline-open-home))
    (_
     (user-error "Current view is not a home timeline"))))

(provide 'chirp-timeline)

;;; chirp-timeline.el ends here
