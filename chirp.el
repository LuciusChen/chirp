;;; chirp.el --- Browse X timelines from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: lucius
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.4.3"))
;; Keywords: convenience, comm
;; URL: https://github.com/LuciusChen/chirp

;;; Commentary:

;; chirp.el is a lightweight X/Twitter browser for Emacs.  It delegates
;; network access and authentication to twitter-cli, then renders timelines,
;; tweet threads, and profiles inside special-mode buffers.

;;; Code:

(eval-and-compile
  (let ((dir (file-name-directory
              (or load-file-name
                  (buffer-file-name)
                  default-directory))))
    (add-to-list 'load-path (expand-file-name "lisp" dir))))

(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)
(require 'chirp-actions)
(require 'chirp-thread)
(require 'chirp-profile)
(require 'chirp-timeline)

(declare-function chirp-profile-open-followers "chirp-profile" (handle &optional buffer))
(declare-function chirp-profile-open-following-users "chirp-profile" (handle &optional buffer))

;;;###autoload
(defun chirp-home ()
  "Open the home timeline."
  (interactive)
  (chirp-timeline-open-home))

;;;###autoload
(defun chirp-following ()
  "Open the following timeline."
  (interactive)
  (chirp-timeline-open-following))

;;;###autoload
(defun chirp-bookmarks ()
  "Open bookmarks."
  (interactive)
  (chirp-timeline-open-bookmarks))

;;;###autoload
(defun chirp-likes ()
  "Open liked tweets for the current account."
  (interactive)
  (chirp-timeline-open-likes))

;;;###autoload
(defun chirp-me ()
  "Open the authenticated account's profile."
  (interactive)
  (chirp-backend-whoami
   (lambda (user _envelope)
     (if-let* ((handle (plist-get user :handle)))
         (chirp-profile-open handle)
       (message "twitter-cli returned a whoami payload Chirp could not parse.")))
   (lambda (message)
     (message "%s" message))))

;;;###autoload
(defun chirp-list (&optional list-id-or-url)
  "Open a list timeline.

When LIST-ID-OR-URL is nil, prompt from the authenticated account's lists."
  (interactive)
  (chirp-timeline-open-list list-id-or-url))

;;;###autoload
(defun chirp-search (query)
  "Search X for QUERY."
  (interactive "sSearch X: ")
  (chirp-timeline-open-search query))

;;;###autoload
(defun chirp-thread (tweet-or-url)
  "Open a thread for TWEET-OR-URL."
  (interactive "sTweet ID or URL: ")
  (chirp-thread-open tweet-or-url))

;;;###autoload
(defun chirp-profile (handle)
  "Open HANDLE's profile."
  (interactive "sProfile handle: ")
  (chirp-profile-open handle))

;;;###autoload
(defun chirp-profile-followers (handle)
  "Open followers for HANDLE."
  (interactive "sProfile handle: ")
  (chirp-profile-open-followers handle))

;;;###autoload
(defun chirp-profile-following-users (handle)
  "Open followed accounts for HANDLE."
  (interactive "sProfile handle: ")
  (chirp-profile-open-following-users handle))

(provide 'chirp)

;;; chirp.el ends here
