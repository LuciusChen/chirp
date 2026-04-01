;;; chirp-timeline-test.el --- Tests for timeline loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-timeline)

(ert-deftest chirp-load-more-stops-when-timeline-is-exhausted ()
  "Loading more should short-circuit once the timeline is exhausted."
  (with-temp-buffer
    (chirp-view-mode)
    (setq-local chirp--timeline-kind 'home)
    (setq-local chirp--timeline-limit 20)
    (setq-local chirp--timeline-exhausted-p t)
    (let ((open-called nil)
          (last-message nil))
      (cl-letf (((symbol-function 'chirp-timeline--open)
                 (lambda (&rest _args)
                   (setq open-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq last-message (apply #'format format-string args)))))
        (chirp-load-more))
      (should-not open-called)
      (should (equal last-message "No older posts.")))))

(ert-deftest chirp-timeline-handle-feed-success-keeps-view-when-no-growth ()
  "Loading more should keep the current view when the response adds nothing."
  (let ((buffer (generate-new-buffer " *chirp-timeline-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (setq-local chirp--timeline-loading-more t)
          (setq-local chirp--request-token 'token)
          (let ((render-called nil)
                (last-message nil))
            (cl-letf (((symbol-function 'chirp-timeline--render)
                       (lambda (&rest _args)
                         (setq render-called t)))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq last-message (apply #'format format-string args)))))
              (chirp-timeline--handle-feed-success
               buffer
               "For You"
               #'ignore
               (list (list :id "1") (list :id "2"))
               'home
               40
               "2"
               t
               2))
            (should-not render-called)
            (should chirp--timeline-exhausted-p)
            (should-not chirp--timeline-loading-more)
            (should-not chirp--request-token)
            (should (equal last-message "No older posts."))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'chirp-timeline-test)

;;; chirp-timeline-test.el ends here
