;;; chirp-notifications-test.el --- Tests for Chirp notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-notifications)

(ert-deftest chirp-backend-notifications-requests-count ()
  "Notification reads should forward the requested result limit."
  (let (captured-args)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation args _callback &rest _options)
                 (setq captured-args args))))
      (chirp-backend-notifications #'ignore nil 12 "previous"))
    (should (equal (cdr (assoc "cursor" captured-args)) "previous"))
    (should (equal (cdr (assoc "count" captured-args)) 12))))

(ert-deftest chirp-notifications-first-check-is-baseline-and-new-items-notify-once ()
  "The first response seeds ids; later unseen activities notify once."
  (let ((chirp-notifications-mode t)
        (chirp-notifications--initialized nil)
        (chirp-notifications--seen-ids nil)
        (chirp-notifications--checking t)
        notified)
    (cl-letf (((symbol-function 'chirp-notifications--notify)
               (lambda (notification)
                 (push (chirp-get notification "id") notified))))
      (chirp-notifications--handle-success
       '((("id" . "n2")) (("id" . "n1"))) nil)
      (should-not notified)
      (should chirp-notifications--initialized)
      (chirp-notifications--handle-success
       '((("id" . "n3")) (("id" . "n2"))) nil)
      (should (equal notified '("n3")))
      (chirp-notifications--handle-success
       '((("id" . "n3")) (("id" . "n2"))) nil)
      (should (equal notified '("n3"))))))

(ert-deftest chirp-notifications-platform-boundary-strips-text-properties ()
  "Native backends should receive plain strings."
  (let ((system-type 'darwin)
        captured)
    (cl-letf (((symbol-function 'chirp-notifications--notify-macos)
               (lambda (title body)
                 (setq captured (list title body)))))
      (chirp-notifications--notify
       `(("type" . "reply")
         ("message" . ,(propertize "hello" 'face 'bold)))))
    (should (equal captured '("Chirp · Reply" "hello")))
    (should-not (text-properties-at 0 (cadr captured)))))

(ert-deftest chirp-notifications-macos-passes-content-as-arguments ()
  "AppleScript source should not embed notification content."
  (let (command)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq command (plist-get args :command)))))
      (chirp-notifications--notify-macos "A \"title\"" "line 1\nline 2"))
    (should (equal (last command 2) '("A \"title\"" "line 1\nline 2")))
    (should-not (string-match-p "A \"title\"" (nth 2 command)))
    (should-not (string-match-p "line 1" (nth 4 command)))))

(ert-deftest chirp-activity-navigates-and-appends-older-events ()
  (save-window-excursion
    (with-temp-buffer
      (let ((view (current-buffer)) requested callback opened)
        (cl-letf (((symbol-function 'chirp-backend-notifications)
                   (lambda (ok _fail _limit cursor)
                     (push cursor requested)
                     (setq callback ok)))
                  ((symbol-function 'chirp-thread-open) (lambda (id &rest _) (setq opened id))))
          (chirp-notifications-open view)
          (funcall callback '(((id . "a") (message . "First") (tweetId . "42")
                               (timestampMs . "1795833600000"))
                              ((id . "b") (message . "Second")))
                   '((nextCursor . "older")))
          (goto-char (point-min))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal opened "42"))
          (search-forward "First  ")
          (should (get-text-property (point) 'help-echo))
          (setq opened nil)
          (call-interactively (key-binding (kbd "RET")))
          (should (equal opened "42"))
          (call-interactively (key-binding (kbd "n")))
          (should (equal (chirp-entry-id-at-point) "b"))
          (let (notice)
            (cl-letf (((symbol-function 'message) (lambda (text &rest _) (setq notice text))))
              (call-interactively (key-binding (kbd "RET")))
              (should (equal notice "This activity has no linked post"))))
          (call-interactively (key-binding (kbd "N")))
          (should (eq chirp--status-kind 'loading))
          (funcall callback '(((id . "b") (message . "Second"))) '((nextCursor . "next")))
          (should (string-match-p "N continues" chirp--status-text))
          (call-interactively (key-binding (kbd "N")))
          (funcall callback '(((id . "b") (message . "Second")) ((id . "c") (message . "Third"))) nil)
          (should (equal requested '("next" "older" nil)))
          (should (equal chirp--status-text "End of list"))
          (call-interactively (key-binding (kbd "n")))
          (should (equal (chirp-entry-id-at-point) "c"))
          (call-interactively (key-binding (kbd "p")))
          (should (equal (chirp-entry-id-at-point) "b"))
          (should chirp--timeline-exhausted-p))))))

(provide 'chirp-notifications-test)

;;; chirp-notifications-test.el ends here
