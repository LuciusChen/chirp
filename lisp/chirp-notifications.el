;;; chirp-notifications.el --- Desktop notifications for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Poll account activity and present new events as desktop notifications.

;;; Code:

(require 'seq)
(require 'chirp-core)
(require 'chirp-backend)

(declare-function notifications-notify "notifications" (&rest params))

(defcustom chirp-notifications-interval 300
  "Seconds between account activity checks."
  :type 'number
  :group 'chirp)

(defcustom chirp-notifications-max-results 20
  "Maximum number of recent activities checked each time."
  :type 'integer
  :group 'chirp)

(defconst chirp-notifications--seen-limit 200
  "Maximum number of notification ids retained for deduplication.")

(defvar chirp-notifications--timer nil
  "Timer used by `chirp-notifications-mode'.")

(defvar chirp-notifications--checking nil
  "Non-nil while a notification request is running.")

(defvar chirp-notifications--initialized nil
  "Non-nil after the first notification response establishes a baseline.")

(defvar chirp-notifications--seen-ids nil
  "Recently seen notification ids, newest first.")

(defun chirp-notifications--plain-string (value)
  "Return VALUE as a string without text properties."
  (substring-no-properties (format "%s" (or value ""))))

(defun chirp-notifications--kind-label (notification)
  "Return a display label for NOTIFICATION's activity type."
  (pcase (chirp-get notification "type")
    ("like" "Like")
    ("follow" "Follow")
    ("retweet" "Repost")
    ("mention" "Mention")
    ("reply" "Reply")
    ("quote" "Quote")
    (_ "Notification")))

(defun chirp-notifications--body (notification)
  "Return the best desktop notification body for NOTIFICATION."
  (or (chirp-first-nonblank (chirp-get notification "message"))
      "New X activity."))

(defun chirp-notifications--notify-linux (title body)
  "Display TITLE and BODY through freedesktop notifications."
  (if (require 'notifications nil t)
      (notifications-notify
       :app-name "Chirp"
       :title title
       :body body
       :timeout 5000)
    (message "%s: %s" title body)))

(defun chirp-notifications--notify-macos (title body)
  "Display TITLE and BODY through AppleScript without source interpolation."
  (make-process
   :name "chirp-notification"
   :buffer nil
   :command
   (list "/usr/bin/osascript"
         "-e" "on run argv"
         "-e" "display notification (item 2 of argv) with title (item 1 of argv)"
         "-e" "end run"
         "--"
         title
         body)
   :noquery t))

(defun chirp-notifications--notify (notification)
  "Display NOTIFICATION using the native platform backend."
  (let ((title (chirp-notifications--plain-string
                (format "Chirp · %s"
                        (chirp-notifications--kind-label notification))))
        (body (chirp-notifications--plain-string
               (chirp-notifications--body notification))))
    (condition-case err
        (pcase system-type
          ('darwin (chirp-notifications--notify-macos title body))
          ('gnu/linux (chirp-notifications--notify-linux title body))
          (_ (message "%s: %s" title body)))
      (error
       (message "Chirp notification failed: %s" (error-message-string err))))))

(defun chirp-notifications--remember (ids)
  "Remember IDS while keeping the deduplication list bounded."
  (setq chirp-notifications--seen-ids
        (seq-take (delete-dups (append ids chirp-notifications--seen-ids))
                  chirp-notifications--seen-limit)))

(defun chirp-notifications--handle-success (notifications _envelope)
  "Process a successful NOTIFICATIONS response."
  (setq chirp-notifications--checking nil)
  (when chirp-notifications-mode
    (let* ((with-ids (seq-filter
                      (lambda (notification)
                        (chirp-get notification "id"))
                      notifications))
           (new (and chirp-notifications--initialized
                     (seq-filter
                      (lambda (notification)
                        (not (member (chirp-get notification "id")
                                     chirp-notifications--seen-ids)))
                      with-ids)))
           (ids (mapcar (lambda (notification)
                          (chirp-get notification "id"))
                        with-ids)))
      (chirp-notifications--remember ids)
      (setq chirp-notifications--initialized t)
      (dolist (notification (reverse new))
        (chirp-notifications--notify notification)))))

(defun chirp-notifications--handle-error (message)
  "Finish a failed notification check with MESSAGE."
  (setq chirp-notifications--checking nil)
  (when chirp-notifications-mode
    (message "Chirp notification check failed: %s" message)))

(defun chirp-notifications-check ()
  "Check once for new account activity."
  (interactive)
  (when (and (not chirp-notifications-mode)
             (called-interactively-p 'interactive))
    (user-error "Enable chirp-notifications-mode first"))
  (when (and chirp-notifications-mode
             (not chirp-notifications--checking))
    (setq chirp-notifications--checking t)
    (chirp-backend-notifications
     #'chirp-notifications--handle-success
     #'chirp-notifications--handle-error
     (max 1 chirp-notifications-max-results))))

;;;###autoload
(define-minor-mode chirp-notifications-mode
  "Poll X account activity and display native desktop notifications."
  :global t
  :group 'chirp
  (if chirp-notifications-mode
      (progn
        (when (timerp chirp-notifications--timer)
          (cancel-timer chirp-notifications--timer))
        (setq chirp-notifications--initialized nil
              chirp-notifications--seen-ids nil
              chirp-notifications--checking nil)
        (chirp-notifications-check)
        (let ((interval (max 1 chirp-notifications-interval)))
          (setq chirp-notifications--timer
                (run-at-time interval
                             interval
                             #'chirp-notifications-check))))
    (when (timerp chirp-notifications--timer)
      (cancel-timer chirp-notifications--timer))
    (setq chirp-notifications--timer nil
          chirp-notifications--checking nil)))

(provide 'chirp-notifications)

;;; chirp-notifications.el ends here
