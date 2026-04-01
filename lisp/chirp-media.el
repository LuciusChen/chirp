;;; chirp-media.el --- Media fetching and display for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'cl-lib)
(require 'image-mode)
(require 'subr-x)
(require 'url)
(require 'url-parse)
(require 'chirp-core)

(defcustom chirp-cache-directory
  (locate-user-emacs-file "chirp/")
  "Directory used for cached Chirp media files."
  :type 'directory
  :group 'chirp)

(defcustom chirp-avatar-size 28
  "Maximum pixel size for avatars."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-thumbnail-size 96
  "Maximum pixel size for timeline and thread thumbnails."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-view-max-width 1200
  "Maximum image width in the media viewer."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-view-max-height 900
  "Maximum image height in the media viewer."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-render-from-cache-only t
  "When non-nil, list views render avatars and thumbnails only from cache.

Missing files are prefetched in the background instead of blocking first paint."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-media-prefetch-images t
  "When non-nil, Chirp prefetches missing list-view images in the background."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-media-prefetch-concurrency 4
  "Maximum number of concurrent background media downloads."
  :type 'integer
  :group 'chirp)

(defcustom chirp-media-prefetch-command
  (executable-find "curl")
  "Command used for background media prefetching.

When nil, Chirp skips background prefetch and only uses cached list-view images."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'chirp)

(defcustom chirp-video-player-command
  (executable-find "mpv")
  "External video player command used for tweet videos.
When nil, Chirp opens video URLs in a browser."
  :type '(choice (const :tag "Browser" nil) string)
  :group 'chirp)

(defcustom chirp-video-thumbnail-command
  (executable-find "ffmpeg")
  "Command used to extract video and animated GIF thumbnails.
When nil, Chirp falls back to a text placeholder for video-like media."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'chirp)

(defcustom chirp-video-thumbnail-offset 0.0
  "Seconds into a video-like media item used for thumbnail extraction."
  :type 'number
  :group 'chirp)

(defvar-local chirp--media-list nil
  "Media list displayed by the current Chirp media buffer.")

(defvar-local chirp--media-index 0
  "Currently selected media index in the current media buffer.")

(defvar-local chirp--media-title nil
  "Base title used by the current media buffer.")

(defvar chirp-media--prefetch-queue nil
  "Queued background media download jobs.")

(defvar chirp-media--prefetch-active 0
  "Number of active background media download jobs.")

(defvar chirp-media--prefetch-pending (make-hash-table :test #'equal)
  "Map cache file paths to pending prefetch callbacks.")

(defvar chirp-media--thumbnail-pending (make-hash-table :test #'equal)
  "Map video thumbnail paths to pending extraction callbacks.")

(defun chirp-media-quit ()
  "Close the current media view.
When possible, return to the previous Chirp view."
  (interactive)
  (if chirp--history
      (chirp-back)
    (quit-window)))

(defvar chirp-media-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "b") #'chirp-back)
    (define-key map (kbd "n") #'chirp-media-next)
    (define-key map (kbd "p") #'chirp-media-previous)
    (define-key map (kbd "o") #'chirp-media-browse)
    (define-key map (kbd "q") #'chirp-media-quit)
    map)
  "Keymap for `chirp-media-view-mode'.")

(define-derived-mode chirp-media-view-mode special-mode "Chirp-Media"
  "Major mode for large media in Chirp.")

(defvar chirp-media-image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-mode-map)
    (define-key map (kbd "b") #'chirp-back)
    (define-key map (kbd "n") #'chirp-media-next)
    (define-key map (kbd "p") #'chirp-media-previous)
    (define-key map (kbd "o") #'chirp-media-browse)
    (define-key map (kbd "q") #'chirp-media-quit)
    map)
  "Keymap for `chirp-media-image-mode'.")

(define-derived-mode chirp-media-image-mode image-mode "Chirp-Image"
  "Image mode used for Chirp photo viewing."
  (setq-local header-line-format nil))

(defun chirp-media-video-like-p (media)
  "Return non-nil when MEDIA is video-like."
  (member (plist-get media :type) '("video" "animated_gif")))

(defun chirp-media-at-point ()
  "Return the media item stored at point, or nil."
  (or (get-text-property (point) 'chirp-media-item)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-media-item))))

(defun chirp-media-index-at-point ()
  "Return the media index stored at point, or nil."
  (or (get-text-property (point) 'chirp-media-index)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-media-index))))

(defun chirp-media-list-at-point ()
  "Return the media list stored at point, or nil."
  (or (get-text-property (point) 'chirp-media-list)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-media-list))))

(defun chirp-media--cache-subdir (kind)
  "Return the cache subdirectory for KIND."
  (let ((dir (expand-file-name kind chirp-cache-directory)))
    (make-directory dir t)
    dir))

(defun chirp-media--url-extension (url fallback)
  "Return a file extension for URL, or FALLBACK."
  (let* ((parsed (ignore-errors (url-generic-parse-url url)))
         (path (and parsed (url-filename parsed)))
         (base (and path
                    (car (split-string (file-name-nondirectory path) "\\?"))))
         (ext (and base (file-name-extension base))))
    (downcase (or ext fallback))))

(defun chirp-media--cache-file (url kind fallback-ext)
  "Return a cache file path for URL of KIND."
  (expand-file-name
   (format "%s.%s"
           (secure-hash 'sha1 url)
           (chirp-media--url-extension url fallback-ext))
   (chirp-media--cache-subdir kind)))

(defun chirp-media-cached-file (url kind &optional fallback-ext)
  "Return the cached local file for URL of KIND, or nil when absent."
  (when (and (stringp url)
             (not (string-empty-p url)))
    (let ((path (chirp-media--cache-file url kind (or fallback-ext "bin"))))
      (when (file-exists-p path)
        path))))

(defun chirp-media--download-file (url path)
  "Download URL to PATH if needed."
  (unless (file-exists-p path)
    (condition-case nil
        (let ((inhibit-message t))
          (url-copy-file url path t))
      (error nil)))
  (when (file-exists-p path)
    path))

(defun chirp-media-local-file (url kind &optional fallback-ext)
  "Return a local cached file for URL of KIND."
  (when (and (stringp url)
             (not (string-empty-p url)))
    (chirp-media--download-file
     url
     (chirp-media--cache-file url kind (or fallback-ext "bin")))))

(defun chirp-media--prefetch-enabled-p ()
  "Return non-nil when background prefetching can run."
  (and chirp-media-prefetch-images
       (display-images-p)
       chirp-media-prefetch-command
       (> chirp-media-prefetch-concurrency 0)))

(defun chirp-media--prefetch-finish (path success)
  "Finish a background prefetch for PATH with SUCCESS."
  (let ((callbacks (prog1 (gethash path chirp-media--prefetch-pending)
                     (remhash path chirp-media--prefetch-pending))))
    (setq chirp-media--prefetch-active (max 0 (1- chirp-media--prefetch-active)))
    (dolist (callback callbacks)
      (when callback
        (ignore-errors (funcall callback success path))))
    (chirp-media--start-next-prefetch)))

(defun chirp-media--thumbnail-finish (path success)
  "Finish a background thumbnail extraction for PATH with SUCCESS."
  (let ((callbacks (prog1 (gethash path chirp-media--thumbnail-pending)
                     (remhash path chirp-media--thumbnail-pending))))
    (dolist (callback callbacks)
      (when callback
        (ignore-errors (funcall callback success path))))))

(defun chirp-media--start-next-prefetch ()
  "Start queued prefetch jobs while capacity is available."
  (while (and chirp-media--prefetch-queue
              (< chirp-media--prefetch-active chirp-media-prefetch-concurrency))
    (let* ((job (pop chirp-media--prefetch-queue))
           (url (plist-get job :url))
           (path (plist-get job :path))
           (buffer (generate-new-buffer " *chirp-prefetch*")))
      (setq chirp-media--prefetch-active (1+ chirp-media--prefetch-active))
      (make-process
       :name "chirp-prefetch"
       :buffer buffer
       :command (list chirp-media-prefetch-command
                      "-L" "-f" "-sS"
                      "-o" path
                      url)
       :noquery t
       :sentinel
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (let ((ok (and (zerop (process-exit-status process))
                          (file-exists-p path))))
             (unless ok
               (ignore-errors
                 (when (file-exists-p path)
                   (delete-file path))))
             (when (buffer-live-p (process-buffer process))
               (kill-buffer (process-buffer process)))
             (chirp-media--prefetch-finish path ok))))))))

(defun chirp-media-prefetch-file (url kind &optional fallback-ext callback)
  "Prefetch URL of KIND into cache and run CALLBACK when newly available."
  (when (chirp-media--prefetch-enabled-p)
    (let ((path (chirp-media--cache-file url kind (or fallback-ext "bin"))))
      (cond
       ((file-exists-p path)
        path)
       ((gethash path chirp-media--prefetch-pending)
        (puthash path
                 (cons callback (gethash path chirp-media--prefetch-pending))
                 chirp-media--prefetch-pending)
        path)
       (t
        (puthash path (list callback) chirp-media--prefetch-pending)
        (setq chirp-media--prefetch-queue
              (nconc chirp-media--prefetch-queue
                     (list (list :url url :path path))))
        (chirp-media--start-next-prefetch)
        path)))))

(defun chirp-media--prefetch-callback (buffer)
  "Return a callback that requests a rerender of BUFFER after media arrives."
  (when (buffer-live-p buffer)
    (lambda (success _path)
      (when (and success
                 (buffer-live-p buffer))
        (chirp-request-rerender buffer)))))

(defun chirp-media-prefetch-avatar (url buffer)
  "Prefetch avatar URL for BUFFER."
  (when (and (stringp url)
             (not (string-empty-p url)))
    (chirp-media-prefetch-file url "avatars" "jpg"
                               (chirp-media--prefetch-callback buffer))))

(defun chirp-media--thumbnail-sentinel (thumbnail-file)
  "Return a sentinel that finalizes THUMBNAIL-FILE extraction."
  (lambda (process _event)
    (when (memq (process-status process) '(exit signal))
      (let ((ok (and (zerop (process-exit-status process))
                     (file-exists-p thumbnail-file))))
        (unless ok
          (ignore-errors
            (when (file-exists-p thumbnail-file)
              (delete-file thumbnail-file))))
        (when (buffer-live-p (process-buffer process))
          (kill-buffer (process-buffer process)))
        (chirp-media--thumbnail-finish thumbnail-file ok)))))

(defun chirp-media--prefetch-video-thumbnail-from-file (media video-file buffer)
  "Extract a thumbnail for MEDIA from VIDEO-FILE and rerender BUFFER on success."
  (let* ((thumbnail-file (chirp-media--video-thumbnail-file media))
         (callback (chirp-media--prefetch-callback buffer)))
    (cond
     ((file-exists-p thumbnail-file)
      (when callback
        (funcall callback t thumbnail-file)))
     ((or (not chirp-video-thumbnail-command)
          (not (file-exists-p video-file)))
      nil)
     ((gethash thumbnail-file chirp-media--thumbnail-pending)
      (puthash thumbnail-file
               (cons callback
                     (gethash thumbnail-file chirp-media--thumbnail-pending))
               chirp-media--thumbnail-pending))
     (t
      (puthash thumbnail-file (list callback) chirp-media--thumbnail-pending)
      (let ((process-buffer (generate-new-buffer " *chirp-thumb*")))
        (make-process
         :name "chirp-thumb"
         :buffer process-buffer
         :command (list chirp-video-thumbnail-command
                        "-y"
                        "-loglevel" "error"
                         "-ss" (number-to-string chirp-video-thumbnail-offset)
                         "-i" video-file
                         "-frames:v" "1"
                         thumbnail-file)
         :noquery t
         :sentinel (chirp-media--thumbnail-sentinel thumbnail-file)))))))

(defun chirp-media--prefetch-video-thumbnail-via-download (media buffer)
  "Download MEDIA in the background and extract a thumbnail for BUFFER."
  (chirp-media-prefetch-file
   (plist-get media :url)
   "media"
   "mp4"
   (lambda (success video-file)
     (when success
       (chirp-media--prefetch-video-thumbnail-from-file media video-file buffer)))))

(defun chirp-media-prefetch-video-thumbnail (media buffer)
  "Prefetch a list-view thumbnail for video-like MEDIA in BUFFER."
  (let ((preview-url (plist-get media :preview-url)))
    (if (and (stringp preview-url)
             (not (string-empty-p preview-url)))
        (chirp-media-prefetch-file
         preview-url
         "video-thumbnails"
         "jpg"
         (lambda (success _path)
           (if success
               (when-let* ((callback (chirp-media--prefetch-callback buffer)))
                 (funcall callback t nil))
             (chirp-media--prefetch-video-thumbnail-via-download media buffer))))
      (chirp-media--prefetch-video-thumbnail-via-download media buffer))))

(defun chirp-media-prefetch-media (media buffer)
  "Prefetch list-view MEDIA for BUFFER."
  (when (and (stringp (plist-get media :url))
             (not (string-empty-p (plist-get media :url))))
    (cond
     ((string= (plist-get media :type) "photo")
      (chirp-media-prefetch-file (plist-get media :url) "media" "jpg"
                                 (chirp-media--prefetch-callback buffer)))
     ((chirp-media-video-like-p media)
      (chirp-media-prefetch-video-thumbnail media buffer)))))

(defun chirp-media-prefetch-tweet (tweet buffer)
  "Prefetch list-view assets for TWEET in BUFFER."
  (chirp-media-prefetch-avatar (plist-get tweet :author-avatar-url) buffer)
  (dolist (media (plist-get tweet :media))
    (chirp-media-prefetch-media media buffer)))

(defun chirp-media-prefetch-tweets (tweets buffer)
  "Prefetch list-view assets for TWEETS in BUFFER."
  (when (chirp-media--prefetch-enabled-p)
    (dolist (tweet tweets)
      (chirp-media-prefetch-tweet tweet buffer))))

(defun chirp-media-prefetch-user (user buffer)
  "Prefetch list-view assets for USER in BUFFER."
  (when (chirp-media--prefetch-enabled-p)
    (chirp-media-prefetch-avatar (plist-get user :avatar-url) buffer)))

(defun chirp-media--scaled-image (file max-width max-height)
  "Create a scaled image descriptor for FILE."
  (when (and file
             (display-images-p))
    (condition-case nil
        (let ((image (create-image file)))
          (when image
            (pcase-let* ((`(,width . ,height) (image-size image t))
                         (scale (min 1.0
                                     (/ (float max-width) (max 1.0 width))
                                     (/ (float max-height) (max 1.0 height)))))
              (when (< scale 1.0)
                (plist-put (cdr image) :scale scale))
              image)))
      (error nil))))

(defun chirp-media-avatar-image (url)
  "Return a small avatar image descriptor for URL."
  (when-let* ((file (if chirp-media-render-from-cache-only
                        (chirp-media-cached-file url "avatars" "jpg")
                      (chirp-media-local-file url "avatars" "jpg"))))
    (chirp-media--scaled-image file chirp-avatar-size chirp-avatar-size)))

(defun chirp-media-thumbnail-image (media)
  "Return a thumbnail descriptor for MEDIA."
  (cond
   ((string= (plist-get media :type) "photo")
    (when-let* ((file (if chirp-media-render-from-cache-only
                          (chirp-media-cached-file (plist-get media :url)
                                                   "media"
                                                   "jpg")
                        (chirp-media-local-file (plist-get media :url)
                                                "media"
                                                "jpg"))))
      (chirp-media--scaled-image file
                                 chirp-media-thumbnail-size
                                 chirp-media-thumbnail-size)))
   ((chirp-media-video-like-p media)
    (when-let* ((file (if chirp-media-render-from-cache-only
                          (or (and-let* ((preview-url (plist-get media :preview-url)))
                                (chirp-media-cached-file preview-url
                                                         "video-thumbnails"
                                                         "jpg"))
                              (let ((thumbnail-file (chirp-media--video-thumbnail-file media)))
                                (and (file-exists-p thumbnail-file)
                                     thumbnail-file)))
                        (chirp-media-video-thumbnail-file media))))
      (chirp-media--scaled-image file
                                 chirp-media-thumbnail-size
                                 chirp-media-thumbnail-size)))))

(defun chirp-media-view-image (media)
  "Return a large image descriptor for MEDIA."
  (when-let* ((file (chirp-media-local-file (plist-get media :url)
                                            "media"
                                            "jpg")))
    (chirp-media--scaled-image file
                               chirp-media-view-max-width
                               chirp-media-view-max-height)))

(defun chirp-media-browse ()
  "Browse the current media URL."
  (interactive)
  (if-let* ((media (or (chirp-media-at-point)
                       (nth chirp--media-index chirp--media-list)))
            (url (plist-get media :url)))
      (browse-url url)
    (user-error "No media URL available")))

(defun chirp-media--photo-file (media)
  "Return a local file path for photo MEDIA."
  (chirp-media-local-file (plist-get media :url) "media" "jpg"))

(defun chirp-media--video-file (media)
  "Return a local file path for video-like MEDIA."
  (chirp-media-local-file (plist-get media :url) "media" "mp4"))

(defun chirp-media--video-thumbnail-file (media)
  "Return the cached thumbnail path for video-like MEDIA."
  (expand-file-name
   (format "%s.jpg"
           (secure-hash
            'sha1
            (format "%s@%s"
                    (or (plist-get media :url) "")
                    chirp-video-thumbnail-offset)))
   (chirp-media--cache-subdir "video-thumbnails")))

(defun chirp-media--extract-video-thumbnail (video-file thumbnail-file)
  "Extract a thumbnail from VIDEO-FILE into THUMBNAIL-FILE."
  (when (and chirp-video-thumbnail-command
             (file-exists-p video-file))
    (let ((status (call-process chirp-video-thumbnail-command
                                nil nil nil
                                "-y"
                                "-loglevel" "error"
                                "-ss" (number-to-string chirp-video-thumbnail-offset)
                                "-i" video-file
                                "-frames:v" "1"
                                thumbnail-file)))
      (and (zerop status)
           (file-exists-p thumbnail-file)
           thumbnail-file))))

(defun chirp-media-video-thumbnail-file (media)
  "Return a thumbnail file path for video-like MEDIA, or nil."
  (when (chirp-media-video-like-p media)
    (or (and-let* ((preview-url (plist-get media :preview-url)))
          (chirp-media-local-file preview-url "video-thumbnails" "jpg"))
        (when-let* ((video-file (chirp-media--video-file media)))
          (let ((thumbnail-file (chirp-media--video-thumbnail-file media)))
            (or (and (file-exists-p thumbnail-file)
                     thumbnail-file)
                (chirp-media--extract-video-thumbnail video-file thumbnail-file)))))))

(defun chirp-media--render-image-buffer (buffer media-list index title)
  "Render photo MEDIA-LIST at INDEX into BUFFER using `image-mode'."
  (let* ((media (nth index media-list))
         (file (chirp-media--photo-file media)))
    (unless file
      (user-error "Image preview unavailable"))
    (if (not (display-images-p))
        (chirp-media--render-buffer buffer media-list index title)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents-literally file))
        (chirp-media-image-mode)
        (use-local-map chirp-media-image-mode-map)
        (setq-local chirp--media-list media-list)
        (setq-local chirp--media-index index)
        (setq-local chirp--media-title title)
        (setq-local chirp--view-title title)
        (setq-local chirp--timeline-kind nil)
        (setq-local chirp--refresh-function nil)
        (setq-local header-line-format nil)
        (goto-char (point-min)))
      (chirp-display-buffer buffer)
      (message "%s (%d/%d)" title (1+ index) (length media-list)))))

(defun chirp-media--render-buffer (buffer media-list index title)
  "Render MEDIA-LIST at INDEX into BUFFER."
  (let* ((media (nth index media-list))
         (total (length media-list)))
    (with-current-buffer buffer
      (chirp-media-view-mode)
      (setq-local chirp--media-list media-list)
      (setq-local chirp--media-index index)
      (setq-local chirp--media-title title)
      (setq-local chirp--view-title title)
      (setq-local chirp--timeline-kind nil)
      (setq-local header-line-format nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s (%d/%d)\n\n" title (1+ index) total))
        (cond
         ((string= (plist-get media :type) "photo")
          (if-let* ((image (chirp-media-view-image media)))
              (insert-image image (format "[image %d]" (1+ index)))
            (insert "Image preview unavailable.\n"))
          (insert "\n\n"))
         ((chirp-media-video-like-p media)
          (insert (if (string= (plist-get media :type) "animated_gif")
                      "Animated GIF media.\n\n"
                    "Video media.\n\n"))
          (insert "Use `o` to open the source URL, or configure `chirp-video-player-command`.\n\n"))
         (t
          (insert "Unsupported media type.\n\n")))
        (insert (format "Type: %s\n" (or (plist-get media :type) "unknown")))
        (when-let* ((width (plist-get media :width))
                    (height (plist-get media :height)))
          (insert (format "Size: %sx%s\n" width height)))
        (when-let* ((url (plist-get media :url)))
          (insert (format "URL: %s\n" url)))
        (goto-char (point-min))))
    (chirp-display-buffer buffer)))

(defun chirp-media-open (media-list index &optional title push-history)
  "Open MEDIA-LIST at INDEX."
  (let* ((safe-index (max 0 (min index (1- (length media-list)))))
         (media (nth safe-index media-list))
         (base-title (or title "Chirp Media")))
    (if (null media)
        (user-error "No media available")
      (if (chirp-media-video-like-p media)
          (if chirp-video-player-command
              (progn
                (start-process "chirp-video" nil chirp-video-player-command
                               (plist-get media :url))
                (message "Opening video with %s" chirp-video-player-command))
            (browse-url (plist-get media :url)))
        (when push-history
          (chirp--maybe-push-history))
        (if (string= (plist-get media :type) "photo")
            (chirp-media--render-image-buffer
             (chirp-buffer)
             media-list
             safe-index
             base-title)
          (chirp-media--render-buffer
           (chirp-buffer)
           media-list
           safe-index
           base-title))))))

(defun chirp-media-open-at-point ()
  "Open the media item at point."
  (interactive)
  (let ((media-list (chirp-media-list-at-point))
        (index (or (chirp-media-index-at-point) 0)))
    (if media-list
        (chirp-media-open media-list
                          index
                          (or chirp--view-title "Chirp Media")
                          t)
      (user-error "No media at point"))))

(defun chirp-media-next ()
  "Open the next media item in the current viewer."
  (interactive)
  (if (<= (length chirp--media-list) 1)
      (user-error "No next media item")
    (chirp-media-open chirp--media-list
                      (mod (1+ chirp--media-index) (length chirp--media-list))
                      chirp--media-title)))

(defun chirp-media-previous ()
  "Open the previous media item in the current viewer."
  (interactive)
  (if (<= (length chirp--media-list) 1)
      (user-error "No previous media item")
    (chirp-media-open chirp--media-list
                      (mod (1- chirp--media-index) (length chirp--media-list))
                      chirp--media-title)))

(provide 'chirp-media)

;;; chirp-media.el ends here
