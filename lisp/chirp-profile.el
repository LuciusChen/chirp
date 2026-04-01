;;; chirp-profile.el --- Profile view for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)

(defun chirp-profile-open (handle)
  "Open HANDLE's profile."
  (interactive "sProfile handle: ")
  (let* ((clean-handle (string-remove-prefix "@" handle))
         (title (format "@%s" clean-handle))
         (buffer (chirp-buffer))
         (refresh (lambda () (chirp-profile-open clean-handle))))
    (chirp--maybe-push-history)
    (let ((token (chirp-show-loading buffer title refresh)))
      (chirp-backend-user
       clean-handle
       (lambda (user _envelope)
         (when (chirp-request-current-p buffer token)
           (chirp-backend-user-posts
            clean-handle
            (lambda (tweets _posts-envelope)
              (when (chirp-request-current-p buffer token)
                (chirp-render-into-buffer
                 buffer title refresh
                 (lambda ()
                   (chirp-render-insert-user-summary user)
                   (chirp-render-insert-section "Recent Posts")
                   (if tweets
                       (dolist (tweet tweets)
                         (chirp-render-insert-tweet tweet))
                     (chirp-render-insert-empty "No recent posts returned."))))
                (with-current-buffer buffer
                  (setq-local chirp--rerender-function
                              (let ((saved-user user)
                                    (saved-tweets tweets)
                                    (saved-title title)
                                    (saved-refresh refresh))
                                (lambda ()
                                  (let ((anchor-id (with-current-buffer buffer
                                                     (chirp-entry-id-at-point))))
                                    (chirp-render-into-buffer
                                     buffer saved-title saved-refresh
                                     (lambda ()
                                       (chirp-render-insert-user-summary saved-user)
                                       (chirp-render-insert-section "Recent Posts")
                                       (if saved-tweets
                                           (dolist (tweet saved-tweets)
                                             (chirp-render-insert-tweet tweet))
                                         (chirp-render-insert-empty "No recent posts returned."))))
                                    (with-current-buffer buffer
                                      (or (and anchor-id
                                               (chirp-goto-entry-id anchor-id))
                                          (chirp-move-point-to-first-entry)))))))
                  (chirp-move-point-to-first-entry))
                (chirp-media-prefetch-user user buffer)
                (chirp-media-prefetch-tweets tweets buffer)))
            (lambda (message)
              (when (chirp-request-current-p buffer token)
                (chirp-show-error
                 buffer title refresh
                 (format "Profile loaded, but recent posts failed.\n\n%s"
                         message)))))))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-show-error buffer title refresh message)))))))

(provide 'chirp-profile)

;;; chirp-profile.el ends here
