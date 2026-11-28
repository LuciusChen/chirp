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
(require 'chirp-web)
(require 'chirp-notifications)
(require 'chirp-media)
(require 'chirp-render)
(require 'chirp-actions)
(require 'chirp-thread)
(require 'chirp-profile)
(require 'chirp-timeline)

(defun chirp--sign-in (command &rest args)
  "Start a new X session and continue with COMMAND and ARGS."
  (chirp-web-login (lambda ()
                     (chirp-backend-clear-cache)
                     (clrhash chirp-quoted-tweet-cache)
                     (clrhash chirp-tweet-state-overrides)
                     (apply command args))))

(defun chirp--open (command &rest args)
  "Call COMMAND with ARGS after obtaining an X session."
  (if (chirp-x-session)
      (apply command args)
    (apply #'chirp--sign-in command args)))

;;;###autoload
(defun chirp-login ()
  "Open X sign-in and return to the home timeline."
  (interactive)
  (chirp--sign-in #'chirp-timeline-open-home))

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
  (interactive "sSearch X: ")
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
