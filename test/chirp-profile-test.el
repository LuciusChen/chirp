;;; chirp-profile-test.el --- Tests for Chirp profile loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-profile)

(ert-deftest chirp-profile-open-starts-user-and-post-requests-in-parallel ()
  "Profile loading should request user metadata and posts concurrently."
  (let ((buffer (generate-new-buffer " *chirp-profile-test*"))
        user-callback
        posts-called)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle _callback &optional _errback)
                     (setq posts-called t)))
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (chirp-profile-open "alice" buffer)
          (should (functionp user-callback))
          (should posts-called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'chirp-profile-test)

;;; chirp-profile-test.el ends here
