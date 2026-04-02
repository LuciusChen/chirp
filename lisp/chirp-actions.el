;;; chirp-actions.el --- Transient write actions for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'chirp-core)
(require 'chirp-backend)

(declare-function chirp-backend-clear-cache "chirp-backend" ())
(declare-function chirp-timeline-open-home "chirp-timeline" ())
(declare-function chirp-timeline-open-following "chirp-timeline" ())

(defvar-local chirp-compose-kind nil
  "Compose action kind for the current Chirp compose buffer.")

(defvar-local chirp-compose-target-id nil
  "Reply or quote target id for the current Chirp compose buffer.")

(defvar-local chirp-compose-target-handle nil
  "Target handle shown in the current Chirp compose buffer.")

(defvar-local chirp-compose-target-url nil
  "Target URL shown in the current Chirp compose buffer.")

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

(defvar chirp-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'chirp-compose-send)
    (define-key map (kbd "C-c C-k") #'chirp-compose-cancel)
    (define-key map (kbd "C-c C-a") #'chirp-compose-attach-image)
    (define-key map (kbd "C-c C-v") #'chirp-compose-paste-image)
    (define-key map (kbd "C-c C-d") #'chirp-compose-remove-image)
    (define-key map (kbd "q") #'chirp-compose-cancel)
    map)
  "Keymap for `chirp-compose-mode'.")

(define-derived-mode chirp-compose-mode text-mode "Chirp-Compose"
  "Major mode for composing Chirp posts."
  (setq-local header-line-format nil)
  (setq-local require-final-newline nil)
  (visual-line-mode 1))

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

(defun chirp-actions--show-error (message)
  "Show MESSAGE as a condensed action failure."
  (message "Chirp action failed: %s"
           (replace-regexp-in-string "[\r\n]+" "  " message)))

(defun chirp-actions--refresh-current-view ()
  "Refresh the current Chirp view."
  (when chirp--refresh-function
    (funcall chirp--refresh-function)))

(defun chirp-actions--refresh-buffer (buffer)
  "Refresh BUFFER when it is a live Chirp view."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (chirp-actions--refresh-current-view))))

(defun chirp-actions--perform (args on-success &optional on-error)
  "Run twitter-cli ARGS and call ON-SUCCESS with the decoded payload.

When ON-ERROR is non-nil, call it with the human-readable error message."
  (chirp-backend-request
   args
   (lambda (data envelope)
     (chirp-backend-clear-cache)
     (funcall on-success data envelope))
   (or on-error
       #'chirp-actions--show-error)))

(defun chirp-actions--apply-state (buffer tweet-id state-key state-value count-key)
  "Update tweet state in BUFFER for TWEET-ID.

STATE-KEY is set to STATE-VALUE and COUNT-KEY is adjusted when needed."
  (chirp-set-tweet-state-override tweet-id state-key state-value)
  (chirp-update-tweet-by-id
   buffer
   tweet-id
   (lambda (tweet)
     (let ((old-state (chirp-boolean-value (plist-get tweet state-key))))
       (plist-put tweet state-key state-value)
       (unless (eq old-state state-value)
         (plist-put tweet
                    count-key
                    (chirp-adjust-count
                     (plist-get tweet count-key)
                     (if state-value 1 -1))))))
   t))

(defun chirp-actions--bookmarks-view-p (buffer)
  "Return non-nil when BUFFER is currently showing bookmarks."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (string= chirp--view-title "Bookmarks"))))

(defun chirp-actions--set-state (state-key command count-key state-value success-message)
  "Set STATE-KEY for the current tweet using COMMAND.

COUNT-KEY is adjusted locally to reflect STATE-VALUE."
  (let* ((tweet-id (chirp-actions--tweet-id-at-point))
         (buffer (current-buffer)))
    (chirp-actions--perform
     (list command tweet-id)
     (lambda (_data _envelope)
       (chirp-actions--apply-state buffer tweet-id state-key state-value count-key)
       (when (and (eq state-key :bookmarked-p)
                  (not state-value)
                  (chirp-actions--bookmarks-view-p buffer))
         (chirp-actions--refresh-buffer buffer))
       (message "%s" success-message)))))

(defun chirp-actions--toggle-state (state-key command-on command-off count-key
                                              success-on success-off)
  "Toggle the current tweet STATE-KEY using COMMAND-ON and COMMAND-OFF."
  (let* ((tweet (chirp-actions--tweet-at-point))
         (current-state (chirp-boolean-value (plist-get tweet state-key)))
         (next-state (not current-state))
         (command (if next-state command-on command-off))
         (success-message (if next-state success-on success-off)))
    (chirp-actions--set-state state-key command count-key next-state success-message)))

(defun chirp-like-at-point ()
  "Like the tweet at point."
  (interactive)
  (chirp-actions--set-state :liked-p "like" :like-count t "Liked."))

(defun chirp-unlike-at-point ()
  "Unlike the tweet at point."
  (interactive)
  (chirp-actions--set-state :liked-p "unlike" :like-count nil "Like removed."))

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

(defun chirp-bookmark-at-point ()
  "Bookmark the tweet at point."
  (interactive)
  (chirp-actions--set-state :bookmarked-p "bookmark" :bookmark-count t "Bookmarked."))

(defun chirp-unbookmark-at-point ()
  "Remove the current tweet from bookmarks."
  (interactive)
  (chirp-actions--set-state :bookmarked-p "unbookmark" :bookmark-count nil "Bookmark removed."))

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

(defun chirp-retweet-at-point ()
  "Retweet the tweet at point."
  (interactive)
  (chirp-actions--set-state :retweeted-p "retweet" :retweet-count t "Retweeted."))

(defun chirp-unretweet-at-point ()
  "Undo the retweet at point."
  (interactive)
  (chirp-actions--set-state :retweeted-p "unretweet" :retweet-count nil "Retweet removed."))

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

(defun chirp-delete-at-point ()
  "Delete the tweet at point after confirmation."
  (interactive)
  (let ((id (chirp-actions--tweet-id-at-point))
        (buffer (current-buffer)))
    (when (y-or-n-p (format "Delete tweet %s? " id))
      (chirp-actions--perform
       (list "delete" "--yes" id)
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

(defun chirp-compose--header-string ()
  "Return the read-only header shown above the compose body."
  (let ((title (pcase chirp-compose-kind
                 ('reply "Reply")
                 ('quote "Quote")
                 (_ "Post")))
        (context
         (pcase chirp-compose-kind
           ('reply
            (if chirp-compose-target-handle
                (format "Replying to @%s" chirp-compose-target-handle)
              (format "Replying to %s" chirp-compose-target-id)))
           ('quote
            (if chirp-compose-target-handle
                (format "Quoting @%s" chirp-compose-target-handle)
              (format "Quoting %s" chirp-compose-target-id)))
           (_ "Compose a new post."))))
    (concat
     (propertize title 'face 'bold)
     "\n"
     (propertize context 'face 'shadow)
     (when chirp-compose-target-url
       (concat "\n"
               (propertize chirp-compose-target-url 'face 'link)))
     "\n\n")))

(defun chirp-compose--footer-string ()
  "Return the read-only footer shown after the compose body."
  (let ((attachments
         (if chirp-compose-attachments
             (mapconcat
              (lambda (path)
                (format "  %s" (abbreviate-file-name path)))
              chirp-compose-attachments
              "\n")
           (propertize "  No images attached." 'face 'shadow))))
    (concat
     "\n\n"
     (propertize "Images" 'face 'bold)
     "\n"
     attachments
     "\n\n"
     (propertize
      "C-c C-a attach   C-c C-v paste   C-c C-d remove   C-c C-c send   C-c C-k cancel"
      'face 'shadow))))

(defun chirp-compose--locked-string (text)
  "Return TEXT propertized as read-only compose chrome."
  (propertize text
              'read-only t
              'rear-nonsticky '(read-only field)
              'field 'chirp-compose-info))

(defun chirp-compose--current-body ()
  "Return the current editable compose body."
  (if (and (markerp chirp-compose-body-start-marker)
           (markerp chirp-compose-body-end-marker))
      (buffer-substring-no-properties
       (marker-position chirp-compose-body-start-marker)
       (marker-position chirp-compose-body-end-marker))
    ""))

(defun chirp-compose--refresh-display ()
  "Refresh compose overlays for the current buffer."
  (let* ((body (chirp-compose--current-body))
         (modified (buffer-modified-p))
         (body-end nil)
         (point-offset (if (and (markerp chirp-compose-body-start-marker)
                                (>= (point) (marker-position chirp-compose-body-start-marker)))
                           (- (point) (marker-position chirp-compose-body-start-marker))
                         0))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (chirp-compose--locked-string (chirp-compose--header-string)))
    (setq-local chirp-compose-body-start-marker (copy-marker (point)))
    (insert body)
    (setq body-end (point))
    (insert (chirp-compose--locked-string (chirp-compose--footer-string)))
    (setq-local chirp-compose-body-end-marker (copy-marker body-end t))
    (goto-char (+ (marker-position chirp-compose-body-start-marker)
                  (min point-offset (length body))))
    (set-buffer-modified-p modified)))

(defun chirp-compose--ensure-attachment-room ()
  "Signal a user error when the current draft already has four images."
  (when (>= (length chirp-compose-attachments) 4)
    (user-error "twitter-cli supports up to 4 attached images")))

(defun chirp-compose--mime-extension (mime-type)
  "Return a file extension for MIME-TYPE."
  (pcase (downcase (or mime-type ""))
    ("image/png" ".png")
    ((or "image/jpeg" "image/jpg") ".jpg")
    ("image/gif" ".gif")
    ("image/webp" ".webp")
    ("image/bmp" ".bmp")
    (_ ".img")))

(defun chirp-compose--process-lines (program &rest args)
  "Return PROGRAM output lines for ARGS, or nil on failure."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil (current-buffer) nil args)))
      (when (and (numberp status)
                 (zerop status))
        (split-string (buffer-string) "\n" t "[ \t\r]+")))))

(defun chirp-compose--first-image-type (types)
  "Return the first image MIME type in TYPES."
  (cl-find-if (lambda (type)
                (string-prefix-p "image/" type))
              types))

(defun chirp-compose--clipboard-image-backend ()
  "Return a plist describing how to paste an image from the clipboard."
  (let ((wl-paste (executable-find "wl-paste"))
        (pngpaste (executable-find "pngpaste"))
        (xclip (executable-find "xclip")))
    (or
     (when wl-paste
       (when-let* ((mime-type (chirp-compose--first-image-type
                               (chirp-compose--process-lines wl-paste "--list-types"))))
         (list :kind 'stdout
               :program wl-paste
               :args (list "--no-newline" "--type" mime-type)
               :extension (chirp-compose--mime-extension mime-type))))
     (when pngpaste
       (list :kind 'filearg
             :program pngpaste
             :args nil
             :extension ".png"))
     (when xclip
       (when-let* ((mime-type (chirp-compose--first-image-type
                               (chirp-compose--process-lines
                                xclip "-selection" "clipboard" "-t" "TARGETS" "-o"))))
         (list :kind 'stdout
               :program xclip
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

(defun chirp-compose--take-temp-attachments ()
  "Return temporary attachments and detach them from the current buffer."
  (prog1 (copy-sequence chirp-compose-temp-attachments)
    (setq-local chirp-compose-temp-attachments nil)))

(defun chirp-compose--cleanup-files (files)
  "Delete every path in FILES, ignoring missing files."
  (dolist (file files)
    (ignore-errors
      (when (file-exists-p file)
        (delete-file file)))))

(defun chirp-compose--add-attachment (path &optional temporary)
  "Attach PATH to the current draft.

When TEMPORARY is non-nil, PATH is owned by the current compose buffer."
  (let ((file (expand-file-name path)))
    (unless (file-regular-p file)
      (user-error "Attachment is not a regular file"))
    (unless (file-readable-p file)
      (user-error "Attachment is not readable"))
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
    (setq file (make-temp-file "chirp-compose-" nil
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
  (let* ((choice (if (= (length chirp-compose-attachments) 1)
                     (car chirp-compose-attachments)
                   (completing-read "Remove image: "
                                    chirp-compose-attachments
                                    nil
                                    t
                                    nil
                                    nil
                                    (car chirp-compose-attachments))))
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

(defun chirp-compose--image-args ()
  "Return command arguments for current draft attachments."
  (let (args)
    (dolist (path chirp-compose-attachments)
      (setq args (append args (list "-i" path))))
    args))

(defun chirp-compose--command-args ()
  "Return twitter-cli arguments for the current draft."
  (let ((text (chirp-compose--body-text))
        (image-args (chirp-compose--image-args)))
    (pcase chirp-compose-kind
      ('reply
       (append (list "reply" chirp-compose-target-id text) image-args))
      ('quote
       (append (list "quote" chirp-compose-target-id text) image-args))
      (_
       (append (list "post" text) image-args)))))

(defun chirp-compose-send ()
  "Send the current draft."
  (interactive)
  (when chirp-compose-sending
    (user-error "Draft is already sending"))
  (let* ((compose-buffer (current-buffer))
         (source-buffer chirp-compose-source-buffer)
         (args (chirp-compose--command-args))
         (temp-attachments (chirp-compose--take-temp-attachments))
         (success-message
          (pcase chirp-compose-kind
            ('reply "Reply sent.")
            ('quote "Quote tweet sent.")
            (_ "Post sent.")))
         (sending-message
          (pcase chirp-compose-kind
            ('reply "Sending reply...")
            ('quote "Sending quote tweet...")
            (_ "Sending post..."))))
    (setq-local chirp-compose-sending t)
    (condition-case err
        (progn
          (chirp-actions--perform
           args
           (lambda (_data _envelope)
             (chirp-compose--cleanup-files temp-attachments)
             (when (buffer-live-p source-buffer)
               (chirp-actions--refresh-buffer source-buffer))
             (message "%s" success-message))
           (lambda (message)
             (chirp-compose--cleanup-files temp-attachments)
             (chirp-actions--show-error message)))
          (chirp-compose--close compose-buffer source-buffer)
          (message "%s" sending-message))
      (error
       (setq-local chirp-compose-sending nil)
       (setq-local chirp-compose-temp-attachments temp-attachments)
       (signal (car err) (cdr err))))))

(defun chirp-compose--other-window-showing-buffer (buffer &optional except-window)
  "Return a live window showing BUFFER other than EXCEPT-WINDOW, or nil."
  (cl-find-if
   (lambda (window)
     (and (window-live-p window)
          (not (eq window except-window))
          (eq (window-buffer window) buffer)))
   (window-list nil 'no-minibuf)))

(defun chirp-compose--close (buffer &optional restore-buffer)
  "Close compose BUFFER and restore RESTORE-BUFFER when possible."
  (when (buffer-live-p buffer)
    (let ((window (get-buffer-window buffer t))
          (restore (and (buffer-live-p restore-buffer)
                        (not (eq buffer restore-buffer))
                        restore-buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (chirp-compose--cleanup-temp-attachments))
      (when (window-live-p window)
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
      (kill-buffer buffer))))

(defun chirp-compose-cancel ()
  "Cancel the current draft."
  (interactive)
  (chirp-compose--close (current-buffer) chirp-compose-source-buffer))

(defun chirp-compose--source-buffer ()
  "Return the view buffer that should own a newly opened compose buffer."
  (or (window-buffer (selected-window))
      (current-buffer)))

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
      (setq-local chirp-compose-source-buffer source)
      (setq-local chirp-compose-attachments nil)
      (setq-local chirp-compose-temp-attachments nil)
      (setq-local chirp-compose-sending nil)
      (rename-buffer (chirp-compose--buffer-name) t)
      (add-hook 'kill-buffer-hook #'chirp-compose--cleanup-temp-attachments nil t)
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

(defun chirp-quote-at-point ()
  "Open a compose buffer to quote the tweet at point."
  (interactive)
  (chirp-compose-open 'quote (chirp-actions--tweet-at-point)))

(transient-define-prefix chirp-dispatch ()
  "Show Chirp write actions."
  [["Timeline"
    ("h" "For You" chirp-timeline-open-home)
    ("f" "Following" chirp-timeline-open-following)]
   ["Compose"
    ("c" "Post" chirp-compose-post)
    ("r" "Reply" chirp-reply-at-point)
    ("Q" "Quote" chirp-quote-at-point)]
   ["Tweet"
    ("R" "Retweet" chirp-toggle-retweet-at-point)]
   ["Engage"
    ("l" "Like" chirp-toggle-like-at-point)
    ("B" "Bookmark" chirp-toggle-bookmark-at-point)]
   ["Other"
    ("d" "Delete" chirp-delete-at-point)
    ("o" "Browser" chirp-browse-at-point)]])

(provide 'chirp-actions)

;;; chirp-actions.el ends here
