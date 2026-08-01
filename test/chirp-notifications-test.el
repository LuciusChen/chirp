;;; chirp-notifications-test.el --- Tests for Chirp notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-notifications)

(ert-deftest chirp-backend-notifications-builds-minimal-command ()
  "Notification requests should only pass the command and result limit."
  (let (captured-args)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args _callback &optional _errback)
                 (setq captured-args args))))
      (chirp-backend-notifications #'ignore nil 12))
    (should (equal captured-args '("notifications" "--max" "12")))))

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

(provide 'chirp-notifications-test)

;;; chirp-notifications-test.el ends here
