;;; chirp-profile.el --- Profile view for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)

(declare-function chirp-backend-invalidate-user "chirp-backend" (handle))
(defvar chirp-backend--bypass-read-cache)

(defun chirp-profile-open (handle &optional buffer)
  "Open HANDLE's profile."
  (interactive "sProfile handle: ")
  (let* ((clean-handle (string-remove-prefix "@" handle))
         (title (format "@%s" clean-handle))
         (buffer (or buffer (chirp-buffer)))
         (refresh (lambda ()
                    (chirp-backend-invalidate-user clean-handle)
                    (let ((chirp-backend--bypass-read-cache t))
                      (chirp-profile-open clean-handle buffer))))
         (token nil)
         (saved-user nil)
         (saved-tweets nil)
         (user-ready nil)
         (tweets-ready nil)
         (finished nil))
    (cl-labels
        ((render-current (&optional anchor-id)
           (chirp-render-into-buffer
            buffer title refresh
            (lambda ()
              (chirp-render-insert-user-summary saved-user)
              (chirp-render-insert-section "Recent Posts")
              (if saved-tweets
                  (chirp-render-insert-tweet-list saved-tweets)
                (chirp-render-insert-empty "No recent posts returned."))))
           (with-current-buffer buffer
             (setq-local chirp--rerender-function
                         (lambda ()
                           (render-current
                            (with-current-buffer buffer
                              (chirp-capture-point-anchor)))))
             (or (and anchor-id
                      (chirp-restore-point-anchor anchor-id))
                 (chirp-move-point-to-first-entry))))
         (finish-success ()
           (when (and (not finished)
                      user-ready
                      tweets-ready
                      (chirp-request-current-p buffer token))
             (setq finished t)
             (render-current)
             (chirp-display-buffer buffer)
             (chirp-media-prefetch-user saved-user buffer)
             (chirp-media-prefetch-tweets saved-tweets buffer)
             (chirp-enrich-quoted-tweets saved-tweets buffer)))
         (finish-error (message &optional posts-failed-p)
           (when (and (not finished)
                      (chirp-request-current-p buffer token))
             (setq finished t)
             (chirp-show-error
              buffer title refresh
              (if posts-failed-p
                  (format "Profile loaded, but recent posts failed.\n\n%s"
                          message)
                message)))))
      (setq token (chirp-begin-background-request buffer title))
      (chirp-backend-user
       clean-handle
       (lambda (user _envelope)
         (when (chirp-request-current-p buffer token)
           (setq saved-user user
                 user-ready t)
           (finish-success)))
       (lambda (message)
         (finish-error message)))
      (chirp-backend-user-posts
       clean-handle
       (lambda (tweets _posts-envelope)
         (when (chirp-request-current-p buffer token)
           (setq saved-tweets tweets
                 tweets-ready t)
           (finish-success)))
       (lambda (message)
         (finish-error message t))))))

(provide 'chirp-profile)

;;; chirp-profile.el ends here
