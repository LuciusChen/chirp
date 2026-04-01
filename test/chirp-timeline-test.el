;;; chirp-timeline-test.el --- Tests for timeline loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-timeline)

(ert-deftest chirp-shared-buffer-tracks-renamed-buffer-object ()
  "Renaming a Chirp buffer should keep `chirp-buffer' pointed at the same object."
  (let ((chirp--shared-buffer nil))
    (unwind-protect
        (let ((buffer (chirp-buffer)))
          (should (buffer-live-p buffer))
          (chirp--apply-buffer-name buffer "For You")
          (should (eq chirp--shared-buffer buffer))
          (should (eq (chirp-buffer) buffer))
          (should (string= (buffer-name buffer) "*chirp: For You*")))
      (when (buffer-live-p chirp--shared-buffer)
        (kill-buffer chirp--shared-buffer))
      (setq chirp--shared-buffer nil))))

(ert-deftest chirp-collect-top-level-tweets-hides-promoted-posts ()
  "Promoted tweets should be dropped when filtering is enabled."
  (let ((chirp-hide-promoted-posts t))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (chirp-collect-top-level-tweets
                            (list '(("id" . "1")
                                    ("text" . "normal")
                                    ("author" . (("screenName" . "alice")
                                                 ("name" . "Alice"))))
                                  '(("id" . "2")
                                    ("text" . "ad")
                                    ("isPromoted" . t)
                                    ("author" . (("screenName" . "brand")
                                                 ("name" . "Brand")))))))
                   '("1")))))

(ert-deftest chirp-collect-top-level-tweets-can-keep-promoted-posts ()
  "Promoted tweets should remain visible when filtering is disabled."
  (let ((chirp-hide-promoted-posts nil))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (chirp-collect-top-level-tweets
                            (list '(("id" . "1")
                                    ("text" . "normal")
                                    ("author" . (("screenName" . "alice")
                                                 ("name" . "Alice"))))
                                  '(("id" . "2")
                                    ("text" . "ad")
                                    ("isPromoted" . t)
                                    ("author" . (("screenName" . "brand")
                                                 ("name" . "Brand")))))))
                   '("1" "2")))))

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
               nil
               2
               nil
               nil))
            (should-not render-called)
            (should chirp--timeline-exhausted-p)
            (should-not chirp--timeline-loading-more)
            (should-not chirp--request-token)
            (should (equal last-message "No older posts."))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-refresh-merges-newer-posts-at-top ()
  "Refreshing should prepend newer tweets and keep older visible tweets."
  (let ((render-args nil)
        (last-message nil))
    (cl-letf (((symbol-function 'chirp-timeline--render)
               (lambda (&rest args)
                 (setq render-args args)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq last-message (apply #'format format-string args)))))
      (chirp-timeline--handle-feed-success
       (current-buffer)
       "For You"
       #'ignore
       (list (list :id "3") (list :id "2"))
       'home
       20
       "2"
       nil
       t
       nil
       (list (list :id "2") (list :id "1"))
       t))
    (should render-args)
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (nth 3 render-args))
                   '("3" "2" "1")))
    (should-not (nth 6 render-args))
    (should (eq (nth 7 render-args) t))
    (should (equal last-message "1 new post."))))

(ert-deftest chirp-timeline-refresh-reports-no-new-posts ()
  "Refreshing should say when nothing new was added."
  (let ((render-args nil)
        (last-message nil))
    (cl-letf (((symbol-function 'chirp-timeline--render)
               (lambda (&rest args)
                 (setq render-args args)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq last-message (apply #'format format-string args)))))
      (chirp-timeline--handle-feed-success
       (current-buffer)
       "For You"
       #'ignore
       (list (list :id "2") (list :id "1"))
       'home
       20
       "2"
       nil
       t
       nil
               (list (list :id "2") (list :id "1"))
               nil))
    (should render-args)
    (should (equal (nth 6 render-args) "2"))
    (should (equal last-message "No new posts."))))

(ert-deftest chirp-timeline-refresh-only-counts-new-prefix-posts ()
  "Refreshing should only count tweets inserted ahead of the current top item."
  (let ((render-args nil)
        (last-message nil))
    (cl-letf (((symbol-function 'chirp-timeline--render)
               (lambda (&rest args)
                 (setq render-args args)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq last-message (apply #'format format-string args)))))
      (chirp-timeline--handle-feed-success
       (current-buffer)
       "For You"
       #'ignore
       (list (list :id "2") (list :id "3") (list :id "1"))
       'home
       20
       "2"
       nil
       t
       nil
       (list (list :id "2") (list :id "1"))
       nil))
    (should render-args)
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (nth 3 render-args))
                   '("2" "3" "1")))
    (should (equal (nth 6 render-args) "2"))
    (should (equal last-message "No new posts."))))


(ert-deftest chirp-clean-text-decodes-html-entities ()
  "Tweet text should decode common HTML entities."
  (should (equal (chirp-clean-text "a &gt; b &amp; c &lt; d")
                 "a > b & c < d"))
  (should (equal (chirp-clean-text "say &quot;hi&quot; &#39;now&#39;")
                 "say \"hi\" 'now'"))
  (should (equal (chirp-clean-text "A&#10;B&#x21;")
                 "A\nB!")))

(provide 'chirp-timeline-test)

;;; chirp-timeline-test.el ends here
