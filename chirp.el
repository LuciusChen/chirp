;;; chirp.el --- Browse X timelines -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;; Author: lucius
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.4.3"))
;; Keywords: convenience, comm
;; URL: https://github.com/LuciusChen/chirp

;;; Commentary:

;; chirp.el is a lightweight X/Twitter browser for Emacs.  It retrieves data directly from X and renders timelines,
;; tweet threads, and profiles inside special-mode buffers.

;;; Code:

(eval-and-compile
  (let* ((file (or load-file-name (buffer-file-name) default-directory))
         (dir (file-name-directory (file-truename file))))
    (add-to-list 'load-path (expand-file-name "lisp" dir))))

(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-x)
(require 'chirp-x-cookies)
(require 'chirp-notifications)
(require 'chirp-media)
(require 'chirp-render)
(require 'chirp-actions)
(require 'chirp-thread)
(require 'chirp-profile)
(require 'chirp-timeline)

(defun chirp--clear-session-state ()
  "Clear Chirp's in-memory caches and optimistic state."
  (chirp-backend-clear-cache)
  (clrhash chirp-quoted-tweet-cache)
  (clrhash chirp-tweet-state-overrides)
  (setq chirp-pending-tweet-states (make-hash-table :test #'equal)))

(defun chirp--clipboard-cookies ()
  "Return `(AUTH-TOKEN . CT0)' parsed from clipboard cURL or Cookie text, or nil."
  (let ((text (or (and (fboundp 'gui-get-selection)
                       (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING)))
                  (and (fboundp 'current-kill)
                       (ignore-errors (current-kill 0 t))))))
    (and (stringp text)
         (chirp-x-parse-cookie-input text))))

(defun chirp--read-login-cookies (&optional prompt)
  "Prompt for an X Cookie header or `auth_token' and `ct0' pair.

Optional PROMPT overrides the first prompt string."
  (let* ((first (string-trim
                 (read-passwd (or prompt "X Cookie header or auth_token: "))))
         (parsed (chirp-x-parse-cookie-input first)))
    (if parsed
        parsed
      (let* ((auth (string-trim
                    (replace-regexp-in-string "\\`auth_token=" "" first)))
             (ct0-raw (string-trim (read-passwd "X ct0 cookie: ")))
             (ct0 (string-trim
                   (replace-regexp-in-string "\\`ct0=" "" ct0-raw))))
        (cons auth ct0)))))

(defun chirp--acquire-fallback-cookies (&optional diagnostic)
  "Acquire X login cookies from the clipboard or masked manual entry.

Optional DIAGNOSTIC explains why automatic browser import did not succeed."
  (let ((clip (chirp--clipboard-cookies)))
    (if (and clip
             (y-or-n-p (format "%sFound X cookies in clipboard (cURL/Cookie). Use them? "
                               (if diagnostic (concat diagnostic ". ") ""))))
        (list :auth-token (car clip) :ct0 (cdr clip) :browser "clipboard")
      (let ((prompt (if diagnostic
                        (format "%s. Paste Cookie header or auth_token: " diagnostic)
                      "No local browser X session found. Paste Cookie header or auth_token: ")))
        (pcase-let ((`(,auth . ,ct0) (chirp--read-login-cookies prompt)))
          (list :auth-token auth :ct0 ct0))))))

(defun chirp--select-chosen-login-cookies ()
  "Prompt the user to pick a browser profile, clipboard import, or manual paste."
  (let* ((sources (chirp-x-cookies-available-sources))
         (diagnostic (chirp-x-cookies-last-diagnostic))
         (choices (append (mapcar (lambda (src)
                                    (cons (format "Import from %s" (plist-get src :label))
                                          src))
                                  sources)
                          '(("Import from clipboard (Copy as cURL / Cookie)" . clipboard)
                            ("Paste Cookie or token manually" . manual))))
         (prompt (if (and (null sources) diagnostic)
                     (format "Sign in to X (%s): " diagnostic)
                   "Sign in to X: "))
         (selection (cdr (assoc (completing-read prompt choices nil t) choices))))
    (pcase selection
      ('clipboard
       (if-let* ((clip (chirp--clipboard-cookies)))
           (list :auth-token (car clip) :ct0 (cdr clip) :browser "clipboard")
         (user-error
          "No X cookies found in clipboard; copy a request via Copy as cURL or copy the Cookie header")))
      ('manual
       (pcase-let ((`(,auth . ,ct0) (chirp--read-login-cookies)))
         (list :auth-token auth :ct0 ct0)))
      (_
       (or (chirp-x-cookies-import (plist-get selection :browser)
                                   (plist-get selection :profile))
           (user-error "Could not read X cookies from %s%s"
                       (plist-get selection :label)
                       (if-let* ((diag (chirp-x-cookies-last-diagnostic)))
                           (format " (%s)" diag)
                         "")))))))

(defun chirp--verify-and-open (acquired command args &optional on-failure)
  "Verify ACQUIRED X session, save it, and invoke COMMAND with ARGS.

Optional ON-FAILURE receives the failure reason string."
  (let* ((auth-token (plist-get acquired :auth-token))
         (ct0 (plist-get acquired :ct0))
         (browser (plist-get acquired :browser))
         (credentials (chirp-x-make-credentials auth-token ct0)))
    (message "%s"
             (if browser
                 (format "Verifying X session imported from %s..." browser)
               "Verifying X credentials..."))
    (chirp-backend-verify-credentials
     credentials
     (lambda (user _envelope)
       (chirp-x-save-credentials auth-token ct0)
       (chirp--clear-session-state)
       (message "Signed in%s%s."
                (if-let* ((handle (plist-get user :handle)))
                    (format " as @%s" handle)
                  " to X")
                (if browser (format " (imported from %s)" browser) ""))
       (apply command args))
     (lambda (reason)
       (if on-failure
           (funcall on-failure reason)
         (message "%s"
                  (if browser
                      (format "Imported %s session failed (%s). Run C-u M-x chirp-login to choose another source."
                              browser reason)
                    (format "X sign-in failed: %s" reason))))))))

(defun chirp--sign-in (command args &optional choose-source-p)
  "Acquire X session cookies, verify and save them, then run COMMAND with ARGS.

When CHOOSE-SOURCE-P is non-nil, prompt to pick a browser profile, clipboard
import, or manual entry.  Otherwise, automatically try local browser profiles
in priority order and fall back to clipboard or manual entry when none exist."
  (if choose-source-p
      (chirp--verify-and-open (chirp--select-chosen-login-cookies) command args)
    (let ((next-candidate (chirp-x-cookies-make-iterator))
          attempts)
      (cl-labels
          ((try-next ()
             (if-let* ((cand (funcall next-candidate)))
                 (chirp--verify-and-open
                  cand command args
                  (lambda (reason)
                    (push (cons (plist-get cand :browser) reason) attempts)
                    (try-next)))
               (if attempts
                   (let ((summary (mapconcat
                                   (lambda (entry)
                                     (format "%s: %s" (car entry) (cdr entry)))
                                   (nreverse attempts)
                                   "; ")))
                     (message "Local browser X session failed (%s). Run C-u M-x chirp-login to choose another source or import from clipboard."
                              summary))
                 (chirp--verify-and-open
                  (chirp--acquire-fallback-cookies (chirp-x-cookies-last-diagnostic))
                  command args)))))
        (try-next)))))

(defun chirp--open (command &rest args)
  "Call COMMAND with ARGS after obtaining an X session."
  (if (chirp-x-credentials)
      (apply command args)
    (chirp--sign-in command args)))

;;;###autoload
(defun chirp-login (&optional choose-source)
  "Import or enter X session cookies, save to `chirp-x-auth-file', and open Home.

With prefix argument CHOOSE-SOURCE, prompt to select a browser or paste
cookies manually."
  (interactive "P")
  (chirp--sign-in #'chirp-timeline-open-home nil choose-source))

;;;###autoload
(defun chirp-logout ()
  "Remove Chirp's saved X session file and clear in-memory caches."
  (interactive)
  (chirp-x-clear-credentials)
  (chirp--clear-session-state)
  (if (chirp-x-credentials)
      (message "Removed saved Chirp auth file (credentials still provided by environment or auth-source).")
    (message "Signed out of Chirp.")))

;;;###autoload
(defun chirp ()
  "Open the home timeline, signing in to X when needed."
  (interactive)
  (chirp--open #'chirp-timeline-open-home))

;;;###autoload
(defun chirp-following ()
  "Open the following timeline."
  (interactive)
  (chirp--open #'chirp-timeline-open-following))

;;;###autoload
(defun chirp-notifications ()
  "Show recent account activity."
  (interactive)
  (chirp--open #'chirp-notifications-open))

;;;###autoload
(defun chirp-bookmarks ()
  "Open bookmarks."
  (interactive)
  (chirp--open #'chirp-timeline-open-bookmarks))

;;;###autoload
(defun chirp-likes ()
  "Open liked tweets for the current account."
  (interactive)
  (chirp--open #'chirp-timeline-open-likes))

;;;###autoload
(defun chirp-me ()
  "Open the authenticated account's profile."
  (interactive)
  (chirp--open
   #'chirp-backend-whoami
   (lambda (user _envelope)
     (if-let* ((handle (plist-get user :handle)))
         (chirp-profile-open handle)
       (message "X returned a whoami payload Chirp could not parse.")))
   (lambda (message)
     (message "%s" message))))

;;;###autoload
(defun chirp-list (&optional list-id-or-url)
  "Open a list timeline.

When LIST-ID-OR-URL is nil, prompt from the authenticated account's lists."
  (interactive)
  (chirp--open #'chirp-timeline-open-list list-id-or-url))

;;;###autoload
(defun chirp-search (query)
  "Search X for QUERY."
  (interactive (list (read-string "Search X: " nil 'chirp-search-history)))
  (chirp--open #'chirp-timeline-open-search query))

;;;###autoload
(defun chirp-thread (tweet-or-url)
  "Open a thread for TWEET-OR-URL."
  (interactive "sTweet ID or URL: ")
  (chirp--open #'chirp-thread-open tweet-or-url))

;;;###autoload
(defun chirp-profile (handle)
  "Open HANDLE's profile."
  (interactive "sProfile handle: ")
  (chirp--open #'chirp-profile-open handle))

;;;###autoload
(defun chirp-profile-followers (handle)
  "Open followers for HANDLE."
  (interactive "sProfile handle: ")
  (chirp--open #'chirp-profile-open-followers handle))

;;;###autoload
(defun chirp-profile-following-users (handle)
  "Open followed accounts for HANDLE."
  (interactive "sProfile handle: ")
  (chirp--open #'chirp-profile-open-following-users handle))

(provide 'chirp)

;;; chirp.el ends here
