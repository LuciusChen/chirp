;;; chirp-actions.el --- Transient write actions for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provide Chirp's compose buffer and interactive read/write actions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'chirp-core)
(require 'chirp-backend)

(require 'chirp-x)
(require 'chirp-render)
(declare-function chirp "chirp" ())
(declare-function chirp-me "chirp" ())

(defcustom chirp-compose-temporary-directory
  (expand-file-name "compose/" (locate-user-emacs-file "chirp/"))
  "Directory used for temporary clipboard image attachments.

Files created here are owned by the compose buffer and removed when the draft is
cancelled, the attachment is removed, or the send completes."
  :type 'directory
  :group 'chirp)

(defcustom chirp-translation-language "zh"
  "Language code used by `chirp-translate-at-point'."
  :type 'string
  :group 'chirp)

(defvar-local chirp-compose-kind nil
  "Compose action kind for the current Chirp compose buffer.")

(defvar-local chirp-compose-target-id nil
  "Reply or quote target id for the current Chirp compose buffer.")

(defvar-local chirp-compose-target-handle nil
  "Target handle shown in the current Chirp compose buffer.")

(defvar-local chirp-compose-target-url nil
  "Target URL shown in the current Chirp compose buffer.")

(defvar-local chirp-compose--target-name nil
  "Author display name of the reply or quote target.")

(defvar-local chirp-compose--target-preview nil
  "Short excerpt of the reply or quote target.")

(defvar-local chirp-compose--target-media-summary nil
  "Short summary of media attached to the reply or quote target.")

(defvar-local chirp-compose--notice nil
  "Transient warning or status notice shown in the compose header line.")

(defvar-local chirp-compose-source-buffer nil
  "Source Chirp view buffer that opened the current compose buffer.")

(defvar-local chirp-compose-attachments nil
  "Attached image paths for the current compose buffer.")

(defvar-local chirp-compose-temp-attachments nil
  "Temporary attachment paths owned by the current compose buffer.")

(defvar-local chirp-compose-body-start-marker nil
  "Marker at the start of the editable compose body.")

(defvar-local chirp-compose-body-end-marker nil
  "Marker at the end of the editable compose body.")

(defvar-local chirp-compose-sending nil
  "Non-nil while the current compose buffer is sending a draft.")

(defvar-local chirp-compose--count-cache nil
  "Modification tick and formatted character count of the current draft.")

(defvar-local chirp-compose--mention-cache nil
  "User completion results keyed by query in the current draft.")

(defvar chirp-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'chirp-compose-send)
    (define-key map (kbd "C-c C-o") #'chirp-me)
    (define-key map (kbd "C-c C-k") #'chirp-compose-cancel)
    (define-key map (kbd "C-c C-a") #'chirp-compose-attach-image)
    (define-key map (kbd "C-c C-v") #'chirp-compose-paste-image)
    (define-key map (kbd "C-c C-d") #'chirp-compose-remove-image)
    (define-key map (kbd "M-TAB") #'completion-at-point)
    map)
  "Keymap for `chirp-compose-mode'.")

(defvar chirp-compose--target-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'chirp-compose-open-target)
    map)
  "Keymap attached to clickable target tweet headers in compose buffers.")

(define-derived-mode chirp-compose-mode text-mode "Chirp-Compose"
  "Major mode for composing Chirp posts."
  (setq-local header-line-format '(:eval (chirp-compose--header-line)))
  (add-hook 'kill-buffer-hook #'chirp-compose--cleanup-temp-attachments nil t)
  (setq-local require-final-newline nil)
  (setq-local mode-line-process
              '(:eval (chirp-compose--count-label)))
  (setq-local completion-ignore-case t)
  (setq-local chirp-compose--mention-cache nil)
  (add-hook 'completion-at-point-functions
            #'chirp-compose-mention-completion-at-point nil t)
  (visual-line-mode 1))

(defun chirp-compose--count-label ()
  "Return the weighted character count for the mode line."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eq tick (car chirp-compose--count-cache))
      (let* ((n (chirp-backend-tweet-weighted-length (string-trim (chirp-compose--current-body))))
             (label (format " %d/280%s" n (if (> n 280) " · Note" ""))))
        (setq chirp-compose--count-cache
              (cons tick (cond ((> n 25000) (propertize label 'face 'error))
                               ((> n 280) (propertize label 'face 'mode-line-emphasis))
                               ((> n 260) (propertize label 'face 'warning))
                               (t label))))))
    (cdr chirp-compose--count-cache)))

(defun chirp-compose--mention-bounds ()
  "Return handle bounds at point when composing an @mention."
  (let ((body-start (and (markerp chirp-compose-body-start-marker)
                         (marker-position chirp-compose-body-start-marker)))
        (body-end (and (markerp chirp-compose-body-end-marker)
                       (marker-position chirp-compose-body-end-marker))))
    (when (and body-start body-end
               (<= body-start (point) body-end))
      (save-restriction
        (narrow-to-region body-start body-end)
        (save-match-data
          (when (looking-back
                 "\\(?:\\`\\|[^A-Za-z0-9_@]\\)@\\([A-Za-z0-9_]+\\)"
                 (point-min))
            (cons (match-beginning 1) (point))))))))

(defun chirp-compose--mention-query ()
  "Return the handle prefix at point when long enough for an X lookup."
  (when-let* ((bounds (chirp-compose--mention-bounds))
              ((>= (- (cdr bounds) (car bounds)) 2)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun chirp-compose--lookup-mention (buffer entry)
  "Fetch handle suggestions for pending ENTRY if BUFFER still needs them."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((query (car entry)))
        (if (not (equal query (chirp-compose--mention-query)))
            (setq chirp-compose--mention-cache (delq entry chirp-compose--mention-cache))
          (chirp-backend-search-users
           query
           (lambda (users _) (setcdr entry users))
           (lambda (reason)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq chirp-compose--mention-cache (delq entry chirp-compose--mention-cache))))
             (message "Mention lookup: %s" reason))))))))

(defun chirp-compose-mention-completion-at-point ()
  "Offer handle candidates as they arrive, using the active completion UI."
  (when-let* ((query (chirp-compose--mention-query)))
    (let ((entry (assoc-string query chirp-compose--mention-cache t)))
      (unless entry
        (let ((prefix (seq-find (lambda (cached)
                                  (and (cdr cached) (string-prefix-p (car cached) query t)))
                                chirp-compose--mention-cache)))
          (setq entry (cons query (cdr prefix))))
        (push entry chirp-compose--mention-cache)
        (run-at-time 0.12 nil #'chirp-compose--lookup-mention (current-buffer) entry))
      (list (- (point) (length query)) (point)
            (completion-table-dynamic
             (lambda (_) (delq nil (mapcar (lambda (user) (plist-get user :handle)) (cdr entry)))))
            :annotation-function
            (lambda (handle)
              (when-let* ((user (seq-find (lambda (u) (equal handle (plist-get u :handle))) (cdr entry)))
                          (name (plist-get user :name)))
                (concat "  " name)))
            :exclusive 'no))))

(defun chirp-compose--temp-directory ()
  "Return the directory used for temporary compose attachments."
  (let ((dir (file-name-as-directory
              (expand-file-name chirp-compose-temporary-directory))))
    (make-directory dir t)
    dir))

(defun chirp-actions--tweet-at-point ()
  "Return the tweet entry at point, or signal a user error."
  (let ((entry (chirp-entry-at-point)))
    (if (eq (plist-get entry :kind) 'tweet)
        entry
      (user-error "Current entry is not a tweet"))))

(defun chirp-actions--tweet-id-at-point ()
  "Return the current tweet id, or signal a user error."
  (or (plist-get (chirp-actions--tweet-at-point) :id)
      (user-error "Current tweet has no id")))

(defun chirp-actions--user-at-point ()
  "Return the current user entry, or signal a user error."
  (let* ((entry (chirp-entry-at-point))
         (handle (or (plist-get entry :handle)
                     (plist-get entry :author-handle)
                     (chirp-author-handle-at-point))))
    (if handle
        (list :kind 'user
              :handle (string-remove-prefix "@" handle)
              :profile-url (format "https://x.com/%s"
                                   (string-remove-prefix "@" handle)))
      (user-error "No profile available at point"))))

(defun chirp-actions--show-error (message)
  "Show MESSAGE as a condensed action failure."
  (message "Chirp action failed: %s"
           (replace-regexp-in-string "[\r\n]+" "  " message)))

(defun chirp-actions--refresh-buffer (buffer)
  "Refresh BUFFER when it is a live Chirp view."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when chirp--refresh-function
        (funcall chirp--refresh-function)))))

(defun chirp-actions--refresh-user-buffer-if-needed (buffer)
  "Refresh BUFFER after a user action when it currently shows a user entry."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (eq (plist-get (chirp-entry-at-point) :kind) 'user)))
    (chirp-actions--refresh-buffer buffer)))

(defun chirp-actions--format-error (args reason)
  "Format error REASON for backend action ARGS."
  (if (and (equal (car args) "bookmark")
           (string-match-p "(HTTP 404)" reason))
      (format "%s. Open https://x.com/i/status/%s to bookmark on X."
              reason (cadr args))
    reason))

(defun chirp-actions--perform (args on-success &optional on-error)
  "Run action ARGS and call ON-SUCCESS with the decoded payload.

When ON-ERROR is non-nil, call it with the human-readable error message."
  (chirp-backend-request
   args
   (lambda (data envelope)
     (chirp-backend-clear-cache)
     (funcall on-success data envelope))
   (or on-error
       (lambda (reason)
         (chirp-actions--show-error (chirp-actions--format-error args reason))))))

(defun chirp-actions--sync-metric (id state-key count-key target confirmed)
  "Redraw the metric for ID in open views.
When CONFIRMED, commit TARGET to STATE-KEY and adjust COUNT-KEY."
  (when confirmed (chirp-set-tweet-state-override id state-key target))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'chirp-view-mode)
        (chirp-update-tweet-by-id
         buffer id
         (lambda (tweet)
           (when (and confirmed
                      (not (eq target (chirp-boolean-value (plist-get tweet state-key)))))
             (plist-put tweet state-key target)
             (plist-put tweet count-key
                        (chirp-adjust-count (plist-get tweet count-key) (if target 1 -1)))))
         t)))))

(defun chirp-actions--set-state (state-key command count-key state-value success-message)
  "Set STATE-KEY for the current tweet using COMMAND.

Preview the change in open views until X confirms or rejects it.
COUNT-KEY is adjusted locally to reflect STATE-VALUE.  Display SUCCESS-MESSAGE
when the request succeeds."
  (let* ((id (chirp-actions--tweet-id-at-point))
         (key (cons id state-key))
         (pending chirp-pending-tweet-states)
         (buffer (current-buffer))
         (args (list command id)))
    (if (gethash key pending)
        (message "%s already in progress…" (capitalize command))
      (cl-labels ((finish (reason)
                    (remhash key pending)
                    (when (eq pending chirp-pending-tweet-states)
                      (chirp-actions--sync-metric id state-key count-key state-value (null reason))
                      (cond
                       (reason (chirp-actions--show-error (chirp-actions--format-error args reason)))
                       (t
                        (when (and (eq state-key :bookmarked-p) (not state-value)
                                   (buffer-live-p buffer)
                                   (string= (buffer-local-value 'chirp--view-title buffer) "Bookmarks"))
                          (chirp-actions--refresh-buffer buffer))
                        (message "%s" success-message))))))
        (puthash key (if state-value 'on 'off) pending)
        (chirp-actions--sync-metric id state-key count-key state-value nil)
        (message "%s…" (capitalize command))
        (condition-case err
            (chirp-actions--perform args (lambda (_data _envelope) (finish nil)) #'finish)
          (error (finish (error-message-string err))))))))

(defun chirp-actions--toggle-state (state-key command-on command-off count-key
                                              success-on success-off)
  "Toggle the current tweet STATE-KEY using COMMAND-ON and COMMAND-OFF.

Adjust COUNT-KEY and display SUCCESS-ON or SUCCESS-OFF for the resulting state."
  (let* ((tweet (chirp-actions--tweet-at-point))
         (current-state (chirp-boolean-value (plist-get tweet state-key)))
         (next-state (not current-state))
         (command (if next-state command-on command-off))
         (success-message (if next-state success-on success-off)))
    (chirp-actions--set-state state-key command count-key next-state success-message)))

(defun chirp-toggle-like-at-point ()
  "Toggle like state for the tweet at point."
  (interactive)
  (chirp-actions--toggle-state
   :liked-p
   "like"
   "unlike"
   :like-count
   "Liked."
   "Like removed."))

(defun chirp-toggle-bookmark-at-point ()
  "Toggle bookmark state for the tweet at point."
  (interactive)
  (chirp-actions--toggle-state
   :bookmarked-p
   "bookmark"
   "unbookmark"
   :bookmark-count
   "Bookmarked."
   "Bookmark removed."))

(defun chirp-toggle-retweet-at-point ()
  "Toggle retweet state for the tweet at point."
  (interactive)
  (chirp-actions--toggle-state
   :retweeted-p
   "retweet"
   "unretweet"
   :retweet-count
   "Retweeted."
   "Retweet removed."))

(defun chirp-follow-user-at-point ()
  "Follow the user at point."
  (interactive)
  (let* ((user (chirp-actions--user-at-point))
         (handle (plist-get user :handle))
         (buffer (current-buffer)))
    (chirp-actions--perform
     (list "follow" handle)
     (lambda (_data _envelope)
       (chirp-actions--refresh-user-buffer-if-needed buffer)
       (message "Now following @%s." handle)))))

(defun chirp-unfollow-user-at-point ()
  "Unfollow the user at point."
  (interactive)
  (let* ((user (chirp-actions--user-at-point))
         (handle (plist-get user :handle))
         (buffer (current-buffer)))
    (chirp-actions--perform
     (list "unfollow" handle)
     (lambda (_data _envelope)
       (chirp-actions--refresh-user-buffer-if-needed buffer)
       (message "Unfollowed @%s." handle)))))

(defun chirp-toggle-follow-user-at-point ()
  "Toggle follow state for the user at point."
  (interactive)
  (let* ((entry (chirp-entry-at-point))
         (following (chirp-boolean-value (plist-get entry :viewer-following-p))))
    (if following
        (chirp-unfollow-user-at-point)
      (chirp-follow-user-at-point))))

(defun chirp-delete-at-point ()
  "Delete the tweet at point after confirmation."
  (interactive)
  (let ((id (chirp-actions--tweet-id-at-point))
        (buffer (current-buffer)))
    (when (y-or-n-p (format "Delete tweet %s? " id))
      (chirp-actions--perform
       (list "delete" id)
       (lambda (_data _envelope)
         (chirp-clear-tweet-state-overrides id)
         (chirp-actions--refresh-buffer buffer)
         (message "Tweet deleted."))))))

(defun chirp-compose--buffer-name ()
  "Return a compose buffer name for the current draft."
  (pcase chirp-compose-kind
    ('reply
     (format "*chirp compose: Reply @%s*"
             (or chirp-compose-target-handle "?")))
    ('quote
     (format "*chirp compose: Quote @%s*"
             (or chirp-compose-target-handle "?")))
    (_ "*chirp compose: Post*")))

(defun chirp-compose-open-target ()
  "Open the reply or quote target URL in a browser."
  (interactive)
  (if chirp-compose-target-url
      (browse-url chirp-compose-target-url)
    (user-error "No target URL for this draft")))

(defun chirp-compose--header-line ()
  "Return the header-line string for the current compose buffer."
  (cond
   (chirp-compose-sending
    (propertize " Sending draft…" 'face 'mode-line-emphasis))
   (chirp-compose--notice
    (concat " " (propertize chirp-compose--notice 'face 'warning)))
   (t
    (concat
     " "
     (mapconcat
      (lambda (pair)
        (concat (propertize (car pair) 'face 'help-key-binding)
                " "
                (propertize (cdr pair) 'face 'shadow)))
      '(("C-c C-c" . "send")
        ("C-c C-k" . "cancel")
        ("C-c C-a" . "attach")
        ("C-c C-v" . "paste")
        ("M-TAB" . "mention"))
      "  ")))))

(defun chirp-compose--format-target-preview (text)
  "Normalize and bound target tweet TEXT for a compose context card."
  (let* ((lines (cl-loop for line in (split-string (string-trim text) "\n")
                         for cleaned = (string-trim line)
                         unless (string-empty-p cleaned)
                         collect cleaned))
         (joined (string-join (seq-take lines 4) "\n"))
         (clipped (truncate-string-to-width joined 260 nil nil "…")))
    (if (and (> (length lines) 4)
             (not (string-suffix-p "…" clipped)))
        (concat clipped "…")
      clipped)))

(defun chirp-compose--target-media-summary (tweet)
  "Return a compact media summary for TWEET, or nil."
  (when-let* ((media (plist-get tweet :media)))
    (let ((photos 0) (videos 0) (gifs 0))
      (dolist (item media)
        (pcase (plist-get item :type)
          ("video" (setq videos (1+ videos)))
          ("animated_gif" (setq gifs (1+ gifs)))
          (_ (setq photos (1+ photos)))))
      (let ((parts
             (delq nil
                   (list (when (> photos 0)
                           (format "%d %s" photos (if (= photos 1) "photo" "photos")))
                         (when (> videos 0)
                           (format "%d %s" videos (if (= videos 1) "video" "videos")))
                         (when (> gifs 0)
                           (if (= gifs 1) "GIF" (format "%d GIFs" gifs)))))))
        (when parts
          (string-join parts ", "))))))

(defun chirp-compose--target-card-string ()
  "Return a styled read-only card for the reply or quote target, or nil."
  (when (memq chirp-compose-kind '(reply quote))
    (let* ((bar (propertize "│ " 'face 'chirp-quoted-tweet-block-face))
           (name (chirp-first-nonblank chirp-compose--target-name))
           (handle (chirp-first-nonblank chirp-compose-target-handle))
           (heading
            (concat
             (when name
               (propertize name 'face 'chirp-author-face))
             (when (and name handle) " ")
             (if handle
                 (propertize (concat "@" handle) 'face 'chirp-handle-face)
               (unless name
                 (propertize (or chirp-compose-target-id "post")
                             'face 'chirp-handle-face)))
             (when chirp-compose--target-media-summary
               (propertize (format " · %s" chirp-compose--target-media-summary)
                           'face 'chirp-meta-face))))
           (header-line
            (if chirp-compose-target-url
                (propertize heading
                            'mouse-face 'highlight
                            'pointer 'hand
                            'keymap chirp-compose--target-link-map
                            'help-echo (format "Click to open %s" chirp-compose-target-url))
              heading))
           (body-lines
            (when chirp-compose--target-preview
              (mapcar (lambda (line)
                        (propertize (concat bar line) 'wrap-prefix bar))
                      (split-string chirp-compose--target-preview "\n"))))
           (card (concat (propertize (concat bar header-line) 'wrap-prefix bar)
                         (when body-lines
                           (concat "\n" (string-join body-lines "\n")))
                         "\n")))
      (add-face-text-property 0 (length card)
                              'chirp-quoted-tweet-block-face 'append card)
      card)))

(defun chirp-compose--header-string ()
  "Return the read-only header shown above the compose body."
  (if (eq chirp-compose-kind 'reply)
      (let ((card (chirp-compose--target-card-string))
            (target (if chirp-compose-target-handle
                        (propertize (concat "@" chirp-compose-target-handle)
                                    'face 'chirp-handle-face)
                      (propertize (or chirp-compose-target-id "post")
                                  'face 'chirp-meta-face))))
        (concat (or card "")
                "\n"
                (propertize "↳ Replying to " 'face 'chirp-thread-reply-context-face)
                target
                "\n\n"))
    ""))

(defun chirp-compose--attachments-string ()
  "Return the read-only attachment section, or nil when none are attached."
  (when chirp-compose-attachments
    (concat
     (propertize (format "Images (%d/4)" (length chirp-compose-attachments))
                 'face 'bold)
     (propertize "  ·  C-c C-d remove" 'face 'shadow)
     "\n"
     (mapconcat
      #'identity
      (cl-loop for path in chirp-compose-attachments
               for index from 1
               for temp-p = (member path chirp-compose-temp-attachments)
               for label = (if temp-p
                               (format "Clipboard image (%s)"
                                       (upcase (or (file-name-extension path) "img")))
                             (file-name-nondirectory path))
               collect
               (concat (when (display-images-p)
                         (when-let* ((image (ignore-errors
                                              (create-image path nil nil :max-width 80 :max-height 48))))
                           (propertize " " 'display image)))
                       (propertize (format "  [%d] %s" index label)
                                   'help-echo (abbreviate-file-name path))))
      "\n")
     "\n")))

(defun chirp-compose--footer-string ()
  "Return the read-only footer shown after the compose body."
  (let ((sections
         (delq nil
               (list (chirp-compose--attachments-string)
                     (when (eq chirp-compose-kind 'quote)
                       (chirp-compose--target-card-string))))))
    (if sections
        (concat "\n" (string-join sections "\n"))
      "")))

(defun chirp-compose--locked-string (text)
  "Return a copy of TEXT marked as read-only compose chrome."
  (let ((locked (copy-sequence text)))
    (add-text-properties 0 (length locked)
                         '(read-only t
                           rear-nonsticky t
                           field chirp-compose-info)
                         locked)
    locked))

(defun chirp-compose--current-body ()
  "Return the current editable compose body."
  (if (and (markerp chirp-compose-body-start-marker)
           (markerp chirp-compose-body-end-marker))
      (buffer-substring-no-properties
       (marker-position chirp-compose-body-start-marker)
       (marker-position chirp-compose-body-end-marker))
    ""))

(defun chirp-compose--refresh-display ()
  "Refresh compose chrome around the editable draft body."
  (let* ((body (chirp-compose--current-body))
         (modified (buffer-modified-p))
         (body-end nil)
         (point-offset (if (and (markerp chirp-compose-body-start-marker)
                                (>= (point) (marker-position chirp-compose-body-start-marker)))
                           (- (point) (marker-position chirp-compose-body-start-marker))
                         0))
         (header (chirp-compose--header-string))
         (footer (chirp-compose--footer-string))
         (inhibit-read-only t))
    (when (and (not (string-empty-p footer))
               (not (string-suffix-p "\n" body)))
      (setq body (concat body "\n")))
    (erase-buffer)
    (unless (string-empty-p header)
      (insert (chirp-compose--locked-string header)))
    (setq-local chirp-compose-body-start-marker (copy-marker (point)))
    (insert body)
    (setq body-end (point))
    (unless (string-empty-p footer)
      (insert (chirp-compose--locked-string footer)))
    (setq-local chirp-compose-body-end-marker (copy-marker body-end t))
    (goto-char (+ (marker-position chirp-compose-body-start-marker)
                  (min point-offset (length body))))
    (set-buffer-modified-p modified)))

(defun chirp-compose--ensure-attachment-room ()
  "Signal a user error when the current draft already has four images."
  (when (>= (length chirp-compose-attachments) 4)
    (user-error "Up to 4 attached images are supported")))

(defun chirp-compose--mime-extension (mime-type)
  "Return a file extension for MIME-TYPE."
  (pcase (downcase (or mime-type ""))
    ("image/png" ".png")
    ((or "image/jpeg" "image/jpg") ".jpg")
    ("image/gif" ".gif")
    ("image/webp" ".webp")
    (_ nil)))

(defun chirp-compose--process-lines (program &rest args)
  "Return PROGRAM output lines for ARGS, or nil on failure."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil (current-buffer) nil args)))
      (when (and (numberp status)
                 (zerop status))
        (split-string (buffer-string) "\n" t "[ \t\r]+")))))

(defun chirp-compose--first-image-type (types)
  "Return the first image MIME type in TYPES."
  (cl-find-if #'chirp-compose--mime-extension
              types))

(defconst chirp-compose--macos-paste-image-script
  "ObjC.import('AppKit');
function run(argv) {
  var pb = $.NSPasteboard.generalPasteboard;
  var img = $.NSImage.alloc.initWithPasteboard(pb);
  if (!img || img.isNil()) { throw new Error('No clipboard image'); }
  var tiff = img.TIFFRepresentation;
  if (!tiff || tiff.isNil()) { throw new Error('No image data'); }
  var rep = $.NSBitmapImageRep.imageRepWithData(tiff);
  if (!rep || rep.isNil()) { throw new Error('Could not decode image'); }
  var png = rep.representationUsingTypeProperties($.NSBitmapImageFileTypePNG, $());
  if (!png || png.isNil() || !png.writeToFileAtomically(argv[0], true)) {
    throw new Error('Could not write image');
  }
}"
  "JXA script used on macOS to save a clipboard image as PNG.")

(defun chirp-compose--clipboard-image-backend ()
  "Return a plist describing how to paste an image from the clipboard."
  (let (program)
    (cond
     ((eq system-type 'darwin)
      (if (setq program (executable-find "pngpaste"))
          (list :kind 'filearg
                :program program
                :args nil
                :extension ".png")
        (list :kind 'filearg
              :program "/usr/bin/osascript"
              :args (list "-l" "JavaScript"
                          "-e" chirp-compose--macos-paste-image-script
                          "--")
              :extension ".png")))
     ((setq program (and (getenv "WAYLAND_DISPLAY")
                         (executable-find "wl-paste")))
      (when-let* ((mime-type (chirp-compose--first-image-type
                              (chirp-compose--process-lines program "--list-types"))))
        (list :kind 'stdout
              :program program
              :args (list "--no-newline" "--type" mime-type)
              :extension (chirp-compose--mime-extension mime-type))))
     ((setq program (and (getenv "DISPLAY")
                         (executable-find "xclip")))
      (when-let* ((mime-type (chirp-compose--first-image-type
                              (chirp-compose--process-lines
                               program "-selection" "clipboard" "-t" "TARGETS" "-o"))))
        (list :kind 'stdout
              :program program
              :args (list "-selection" "clipboard" "-t" mime-type "-o")
              :extension (chirp-compose--mime-extension mime-type)))))))

(defun chirp-compose--write-command-output-to-file (program args file)
  "Write PROGRAM ARGS output to FILE, returning non-nil on success."
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((status (apply #'process-file program nil (current-buffer) nil args)))
        (when (and (numberp status)
                   (zerop status)
                   (> (buffer-size) 0))
          (write-region nil nil file nil 'silent)
          t)))))

(defun chirp-compose--paste-image-to-file (backend file)
  "Paste clipboard image using BACKEND into FILE."
  (pcase (plist-get backend :kind)
    ('stdout
     (chirp-compose--write-command-output-to-file
      (plist-get backend :program)
      (plist-get backend :args)
      file))
    ('filearg
     (let ((status (apply #'process-file
                          (plist-get backend :program)
                          nil nil nil
                          (append (plist-get backend :args)
                                  (list file)))))
       (and (numberp status)
            (zerop status)
            (file-exists-p file)
            (> (file-attribute-size (file-attributes file)) 0))))
    (_ nil)))

(defun chirp-compose--drop-temp-attachment (file)
  "Delete FILE when it is a temporary compose attachment."
  (when (member file chirp-compose-temp-attachments)
    (setq-local chirp-compose-temp-attachments
                (delete file chirp-compose-temp-attachments))
    (ignore-errors
      (when (file-exists-p file)
        (delete-file file)))))

(defun chirp-compose--cleanup-temp-attachments ()
  "Delete all temporary attachments owned by the current compose buffer."
  (dolist (file chirp-compose-temp-attachments)
    (ignore-errors
      (when (file-exists-p file)
        (delete-file file))))
  (setq chirp-compose-temp-attachments nil))

(defun chirp-compose--add-attachment (path &optional temporary)
  "Attach PATH to the current draft.

When TEMPORARY is non-nil, PATH is owned by the current compose buffer."
  (let ((file (expand-file-name path)))
    (chirp-x-image-type file)
    (when (member file chirp-compose-attachments)
      (user-error "Image already attached"))
    (setq-local chirp-compose-attachments
                (append chirp-compose-attachments (list file)))
    (when temporary
      (setq-local chirp-compose-temp-attachments
                  (append chirp-compose-temp-attachments (list file))))
    (chirp-compose--refresh-display)
    (set-buffer-modified-p t)
    file))

(defun chirp-compose-attach-image (path)
  "Attach image PATH to the current draft."
  (interactive (list (read-file-name "Attach image: " nil nil t)))
  (chirp-compose--ensure-attachment-room)
  (message "Attached %s"
           (file-name-nondirectory
            (chirp-compose--add-attachment path))))

(defun chirp-compose-paste-image ()
  "Paste one image from the clipboard into the current draft."
  (interactive)
  (chirp-compose--ensure-attachment-room)
  (let* ((backend (chirp-compose--clipboard-image-backend))
         (file nil)
         (attached nil))
    (unless backend
      (user-error "No clipboard image available or no supported paste backend"))
    (setq file (make-temp-file
                (expand-file-name "chirp-compose-"
                                  (chirp-compose--temp-directory))
                nil
                               (plist-get backend :extension)))
    (unwind-protect
        (progn
          (unless (chirp-compose--paste-image-to-file backend file)
            (user-error "Clipboard does not currently contain an image"))
          (chirp-compose--add-attachment file t)
          (setq attached t)
          (message "Pasted %s" (file-name-nondirectory file)))
      (unless attached
        (ignore-errors
          (when (and file (file-exists-p file))
            (delete-file file)))))))

(defun chirp-compose-remove-image ()
  "Remove one image attachment from the current draft."
  (interactive)
  (unless chirp-compose-attachments
    (user-error "No attached images"))
  (let* ((choices (cl-loop for path in chirp-compose-attachments
                           for index from 1
                           collect (cons (format "[%d] %s" index
                                                 (file-name-nondirectory path))
                                         path)))
         (choice (if (= (length choices) 1)
                     (cdar choices)
                   (cdr (assoc (completing-read "Remove image: "
                                                choices
                                                nil
                                                t
                                                nil
                                                nil
                                                (caar choices))
                               choices))))
         (removed (expand-file-name choice)))
    (setq-local chirp-compose-attachments
                (delete removed chirp-compose-attachments))
    (chirp-compose--drop-temp-attachment removed)
    (chirp-compose--refresh-display)
    (set-buffer-modified-p t)
    (message "Removed %s" (file-name-nondirectory removed))))

(defun chirp-compose--body-text ()
  "Return the current compose body."
  (let ((text (string-trim (chirp-compose--current-body))))
    (if (string-empty-p text)
        (user-error "Text cannot be empty")
      text)))

(defun chirp-compose-send ()
  "Send this draft and release its attachments after confirmed success."
  (interactive)
  (when chirp-compose-sending (user-error "Draft is already sending"))
  (let ((draft (current-buffer))
        (source chirp-compose-source-buffer)
        (kind chirp-compose-kind)
        (target chirp-compose-target-id)
        (target-handle chirp-compose-target-handle)
        (attachments chirp-compose-attachments)
        (text (chirp-compose--body-text)))
    (setq chirp-compose-sending t
          chirp-compose--notice nil
          buffer-read-only t)
    (chirp-compose--hide draft source)
    (message "Sending draft...")
    (cl-labels ((restore (reason)
                  (when (buffer-live-p draft)
                    (with-current-buffer draft
                      (setq chirp-compose-sending nil
                            buffer-read-only nil
                            chirp-compose--notice
                            (if (string-match-p "outcome is unknown" reason)
                                "Check your posts with C-c C-o before retrying"
                              "Draft retained · C-c C-c retry · C-c C-k cancel")))
                    (display-buffer draft))
                  (chirp-actions--show-error reason)))
      (condition-case err
          (chirp-backend-compose
           :kind kind :text text :target-id target :target-handle target-handle
           :attachments attachments
           :callback (lambda (_data _envelope)
                       (chirp-backend-clear-cache)
                       (when (buffer-live-p draft)
                         (with-current-buffer draft (set-buffer-modified-p nil))
                         (kill-buffer draft))
                       (when (buffer-live-p source) (chirp-actions--refresh-buffer source))
                       (message "Sent."))
           :errback #'restore)
        (error (restore (error-message-string err)))))))

(defun chirp-compose--other-window-showing-buffer (buffer &optional except-window)
  "Return a live window showing BUFFER other than EXCEPT-WINDOW, or nil."
  (cl-find-if
   (lambda (window)
     (and (not (eq window except-window))
          (eq (window-buffer window) buffer)))
   (window-list nil 'no-minibuf)))

(defun chirp-compose--hide (buffer &optional restore-buffer)
  "Hide compose BUFFER and restore RESTORE-BUFFER when possible."
  (when (buffer-live-p buffer)
    (let ((window (get-buffer-window buffer t))
          (restore (and (buffer-live-p restore-buffer)
                        (not (eq buffer restore-buffer))
                        restore-buffer)))
      (when window
        (let ((other-restore-window
               (and restore
                    (chirp-compose--other-window-showing-buffer
                     restore
                     window))))
          (cond
           ((and other-restore-window
                 (not (one-window-p t)))
            (condition-case nil
                (delete-window window)
              (error
               (set-window-buffer window restore))))
           (restore
            (set-window-buffer window restore))
           (t
            (switch-to-prev-buffer window)))))
      (bury-buffer buffer))))

(defun chirp-compose-cancel ()
  "Cancel the current draft."
  (interactive)
  (when chirp-compose-sending (user-error "Draft is still sending"))
  (when (or (and (string-blank-p (chirp-compose--current-body))
                 (null chirp-compose-attachments))
            (yes-or-no-p "Discard this draft? "))
    (let ((draft (current-buffer)))
      (chirp-compose--hide draft chirp-compose-source-buffer)
      (with-current-buffer draft (set-buffer-modified-p nil))
      (kill-buffer draft))))

(defun chirp-compose--view-buffer-p (buffer)
  "Return non-nil when BUFFER is a live Chirp view buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (derived-mode-p 'chirp-view-mode))))

(defun chirp-compose--source-buffer ()
  "Return the view buffer that should own a newly opened compose buffer."
  (let* ((current (current-buffer))
         (selected (window-buffer (selected-window)))
         (minibuffer-source
          (and (active-minibuffer-window)
               (window-live-p (minibuffer-selected-window))
               (window-buffer (minibuffer-selected-window)))))
    (or (and (chirp-compose--view-buffer-p current) current)
        (and (chirp-compose--view-buffer-p minibuffer-source) minibuffer-source)
        (and (chirp-compose--view-buffer-p selected) selected)
        selected
        current)))

(defun chirp-compose-open (kind &optional tweet)
  "Open a compose buffer for KIND.

When TWEET is non-nil, use it as the reply or quote target."
  (let* ((source (chirp-compose--source-buffer))
         (buffer (generate-new-buffer "*chirp compose*")))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (chirp-compose-mode)
      (setq-local chirp-compose-kind kind)
      (setq-local chirp-compose-target-id (plist-get tweet :id))
      (setq-local chirp-compose-target-handle (plist-get tweet :author-handle))
      (setq-local chirp-compose-target-url (plist-get tweet :url))
      (setq-local chirp-compose--target-name (plist-get tweet :author-name))
      (setq-local chirp-compose--target-preview
                  (when-let* ((body (plist-get tweet :text)))
                    (chirp-compose--format-target-preview body)))
      (setq-local chirp-compose--target-media-summary
                  (chirp-compose--target-media-summary tweet))
      (setq-local chirp-compose--notice nil)
      (setq-local chirp-compose-source-buffer source)
      (setq-local chirp-compose-attachments nil)
      (setq-local chirp-compose-temp-attachments nil)
      (setq-local chirp-compose-sending nil)
      (rename-buffer (chirp-compose--buffer-name) t)
      (chirp-compose--refresh-display)
      (set-buffer-modified-p nil)
      (goto-char (marker-position chirp-compose-body-start-marker)))))

(defun chirp-compose-post ()
  "Open a compose buffer for a new post."
  (interactive)
  (chirp-compose-open 'post))

(defun chirp-reply-at-point ()
  "Open a compose buffer to reply to the tweet at point."
  (interactive)
  (chirp-compose-open 'reply (chirp-actions--tweet-at-point)))

(defun chirp--dispatch-mouse-action (event)
  "Place point at mouse EVENT and activate a clicked control."
  (interactive "e")
  (mouse-set-point event)
  (pcase (get-text-property (point) 'chirp-tweet-action)
    ('quote (chirp-quote-at-point))
    ('reply (chirp-reply-at-point))
    ('retweet (chirp-toggle-retweet-at-point))
    ('like (chirp-toggle-like-at-point))
    ('bookmark (chirp-toggle-bookmark-at-point))
    (_ (when (eq (get-text-property (point) 'pointer) 'hand)
         (chirp-open-at-point)))))

(defun chirp-quote-at-point ()
  "Open a compose buffer to quote the tweet at point."
  (interactive)
  (chirp-compose-open 'quote (chirp-actions--tweet-at-point)))

(defun chirp-copy-url-at-point ()
  "Copy the canonical URL of the current tweet."
  (interactive)
  (if-let* ((url (car (chirp-tweet-candidate-urls (chirp-actions--tweet-at-point)))))
      (progn
        (kill-new url)
        (message "Copied %s" url))
    (user-error "Current tweet has no canonical URL")))

(defun chirp-translate-at-point ()
  "Translate the tweet at point and show the result below its text."
  (interactive)
  (let* ((tweet-id (chirp-actions--tweet-id-at-point))
         (buffer (current-buffer))
         (language (string-trim chirp-translation-language)))
    (when (string-empty-p language)
      (user-error "Translation language must not be empty"))
    (message "Translating to %s..." language)
    (chirp-backend-translate
     tweet-id
     language
     (lambda (data _envelope)
       (if-let* ((translation (chirp-first-nonblank
                               (chirp-get data "translation"))))
           (let ((destination
                  (or (chirp-first-nonblank
                       (chirp-get data "destinationLanguage"))
                      language)))
             (chirp-set-tweet-state-override
              tweet-id :translation translation)
             (chirp-set-tweet-state-override
              tweet-id :translation-language destination)
             (chirp-update-tweet-by-id
              buffer
              tweet-id
              (lambda (tweet)
                (plist-put tweet :translation translation)
                (plist-put tweet :translation-language destination))
              t)
             (message "Translated to %s." destination))
         (chirp-actions--show-error
          "No translated text returned by X")))
     #'chirp-actions--show-error)))

(transient-define-prefix chirp-dispatch ()
  "Show Chirp write actions."
  [["Timeline"
    ("h" "For You" chirp)
    ("f" "Following" chirp-following)
    ("u" "Me" chirp-me)
    ("b" "Bookmarks" chirp-bookmarks)
    ("L" "Liked" chirp-likes)
    ("s" "List" chirp-list)
    ("/" "Search" chirp-search)
    ("i" "Activity" chirp-notifications)
    ("I" "Sign in" chirp-login)]
   ["Compose"
    ("c" "Post" chirp-compose-post)
    ("r" "Reply" chirp-reply-at-point)
    ("Q" "Quote" chirp-quote-at-point)]
   ["Actions"
    ("R" chirp-toggle-retweet-at-point
     :description (lambda () (if (plist-get (chirp-entry-at-point) :retweeted-p)
                                 "Undo retweet" "Retweet")))
    ("+" chirp-follow-user-at-point
     :description (lambda () (if (plist-get (chirp-entry-at-point) :viewer-following-p)
                                 "Follow (already following)" "Follow")))
    ("-" "Unfollow" chirp-unfollow-user-at-point)
    ("l" chirp-toggle-like-at-point
     :description (lambda () (if (plist-get (chirp-entry-at-point) :liked-p)
                                 "Unlike" "Like")))
    ("B" chirp-toggle-bookmark-at-point
     :description (lambda () (if (plist-get (chirp-entry-at-point) :bookmarked-p)
                                 "Remove bookmark" "Bookmark")))]
   ["Other"
    ("d" "Delete" chirp-delete-at-point)
    ("T" "Translate" chirp-translate-at-point)
    ("y" "Copy URL" chirp-copy-url-at-point)
    ("o" "Browser" chirp-browse-at-point)]])

(provide 'chirp-actions)

;;; chirp-actions.el ends here
