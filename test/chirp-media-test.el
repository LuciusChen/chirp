;;; chirp-media-test.el --- Tests for Chirp media prefetch helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-media)

(ert-deftest chirp-media-prefetch-video-thumbnail-tries-remote-extraction-without-preview ()
  "Video/GIF thumbnail prefetch should try remote extraction before full download."
  (let ((chirp-media-prefetch-video-fallback-download nil)
        (chirp-media-prefetch-video-remote-thumbnail t)
        remote-called
        download-called
        prefetch-called)
    (cl-letf (((symbol-function 'chirp-media--prefetch-video-thumbnail-from-url)
               (lambda (&rest _args)
                 (setq remote-called t)
                 t))
              ((symbol-function 'chirp-media--prefetch-video-thumbnail-via-download)
               (lambda (&rest _args)
                 (setq download-called t)))
              ((symbol-function 'chirp-media-prefetch-file)
               (lambda (&rest _args)
                 (setq prefetch-called t))))
      (chirp-media-prefetch-video-thumbnail
       '(:type "animated_gif" :url "https://example.com/anim.mp4")
       (current-buffer)))
    (should remote-called)
    (should-not download-called)
    (should-not prefetch-called)))

(ert-deftest chirp-media-prefetch-video-thumbnail-falls-back-to-download-after-remote-failure ()
  "Full download fallback should run only after remote extraction fails."
  (let ((chirp-media-prefetch-video-fallback-download t)
        (chirp-media-prefetch-video-remote-thumbnail t)
        download-called)
    (cl-letf (((symbol-function 'chirp-media--prefetch-video-thumbnail-from-url)
               (lambda (_media _buffer fallback)
                 (funcall fallback)
                 t))
              ((symbol-function 'chirp-media--prefetch-video-thumbnail-via-download)
               (lambda (&rest _args)
                 (setq download-called t))))
      (chirp-media-prefetch-video-thumbnail
       '(:type "video" :url "https://example.com/video.mp4")
       (current-buffer)))
    (should download-called)))

(ert-deftest chirp-media-prefetch-video-thumbnail-prefers-preview-url ()
  "Video thumbnail prefetch should use preview images when available."
  (let ((chirp-media-prefetch-video-fallback-download nil)
        captured)
    (cl-letf (((symbol-function 'chirp-media-prefetch-file)
               (lambda (url kind ext callback)
                 (setq captured (list url kind ext (functionp callback)))))
              ((symbol-function 'chirp-media--prefetch-video-thumbnail-via-download)
               (lambda (&rest _args)
                 (ert-fail "unexpected full download fallback"))))
      (chirp-media-prefetch-video-thumbnail
       '(:type "video"
         :url "https://example.com/video.mp4"
         :preview-url "https://example.com/preview.jpg")
       (current-buffer)))
    (should (equal captured
                   '("https://example.com/preview.jpg" "video-thumbnails" "jpg" t)))))

(ert-deftest chirp-media-prefetch-tweet-recurses-into-quoted-tweet ()
  "Quoted tweet media should also be prefetched."
  (let (avatars media-urls)
    (cl-letf (((symbol-function 'chirp-media-prefetch-avatar)
               (lambda (url _buffer)
                 (push url avatars)))
              ((symbol-function 'chirp-media-prefetch-media)
               (lambda (media _buffer)
                 (push (plist-get media :url) media-urls))))
      (chirp-media-prefetch-tweet
       '(:author-avatar-url "https://example.com/main-avatar.jpg"
         :media ((:type "photo" :url "https://example.com/main.jpg"))
         :quoted-tweet (:author-avatar-url "https://example.com/quoted-avatar.jpg"
                        :media ((:type "photo" :url "https://example.com/quoted.jpg"))))
       (current-buffer)))
    (should (equal avatars
                   '("https://example.com/quoted-avatar.jpg"
                     "https://example.com/main-avatar.jpg")))
    (should (equal media-urls
                   '("https://example.com/quoted.jpg"
                     "https://example.com/main.jpg")))))

(ert-deftest chirp-media-prefetch-tweet-prefetches-article-images-and-link-cards ()
  "Article images and external link cards should join normal media prefetch."
  (let (media-urls card-urls)
    (cl-letf (((symbol-function 'chirp-media-prefetch-avatar)
               (lambda (&rest _args)))
              ((symbol-function 'chirp-media-prefetch-media)
               (lambda (media _buffer)
                 (push (plist-get media :url) media-urls)))
              ((symbol-function 'chirp-media-prefetch-link-card)
               (lambda (url _buffer)
                 (push url card-urls))))
      (chirp-media-prefetch-tweet
       '(:author-avatar-url "https://example.com/avatar.jpg"
         :urls ("https://github.com/example/project")
         :article-text "Intro.\n\n![Cover](https://example.com/cover.jpg)")
       (current-buffer)))
    (should (equal media-urls
                   '("https://example.com/cover.jpg")))
    (should (equal card-urls
                   '("https://github.com/example/project")))))

(ert-deftest chirp-media-parse-link-card-html-extracts-opengraph-fields ()
  "Link-card HTML parsing should extract title, description, and image."
  (let ((card
         (chirp-media--parse-link-card-html
          "<html><head><meta property=\"og:title\" content=\"microsoft/RD-Agent\"><meta property=\"og:description\" content=\"Research &amp; development\"><meta property=\"og:image\" content=\"/preview.png\"></head></html>"
          "https://github.com/microsoft/RD-Agent")))
    (should (equal (plist-get card :url)
                   "https://github.com/microsoft/RD-Agent"))
    (should (equal (plist-get card :title)
                   "microsoft/RD-Agent"))
    (should (equal (plist-get card :description)
                   "Research & development"))
    (should (equal (plist-get card :image-url)
                   "https://github.com/preview.png"))))

(ert-deftest chirp-media-scaled-dimensions-preserve-aspect-ratio ()
  "Scaled dimensions should fit the target box without distorting aspect ratio."
  (should (equal (chirp-media--scaled-dimensions 921 1008 128 128)
                 '(117 . 128)))
  (should (equal (chirp-media--scaled-dimensions 1651 1079 128 128)
                 '(128 . 84))))

(ert-deftest chirp-media-thumbnail-image-uses-photo-thumbnail-wrapper ()
  "Photo thumbnails should prefer the fixed-size SVG wrapper renderer."
  (let ((chirp-media-render-from-cache-only t)
        rendered-file)
    (cl-letf (((symbol-function 'chirp-media-cached-file)
               (lambda (&rest _args)
                 "/tmp/chirp-photo-thumb.jpg"))
              ((symbol-function 'chirp-media--photo-thumbnail-image)
               (lambda (file width height)
                 (setq rendered-file (list file width height))
                 'photo-image)))
        (should (eq (chirp-media-thumbnail-image
                     '(:type "photo"
                       :url "https://example.com/photo.jpg"))
                    'photo-image))
      (should (equal rendered-file
                     '("/tmp/chirp-photo-thumb.jpg" 128 128))))))

(ert-deftest chirp-media-sliced-thumbnail-image-aligns-height-to-char-grid ()
  "Quote slicing should create a file-backed image sized to full text rows."
  (let ((chirp-media-render-from-cache-only t)
        created-args)
    (cl-letf (((symbol-function 'chirp-media-cached-file)
               (lambda (&rest _args)
                 "/tmp/chirp-photo-thumb.jpg"))
              ((symbol-function 'chirp-media--chars-xheight)
               (lambda (n &optional _frame)
                 (* n 24)))
              ((symbol-function 'create-image)
               (lambda (file &optional _type _data-p &rest props)
                 (setq created-args (cons file props))
                 (cons 'image props))))
      (let ((image (chirp-media-sliced-thumbnail-image
                    '(:type "photo"
                      :url "https://example.com/photo.jpg"
                      :width 921
                      :height 1008))))
        (should image)
        (should (= (plist-get (cdr image) :chirp-nslices) 6))
        (should (equal created-args
                       '("/tmp/chirp-photo-thumb.jpg"
                         :height (6 . ch)
                         :scale 1.0
                         :ascent center
                         :chirp-nslices 6)))))))

(ert-deftest chirp-media-thumbnail-image-badges-video-like-media ()
  "Video-like thumbnails should use the play-badge renderer."
  (let ((chirp-media-render-from-cache-only t)
        rendered-file)
    (cl-letf (((symbol-function 'chirp-media-cached-file)
               (lambda (&rest _args)
                 "/tmp/chirp-video-thumb.jpg"))
              ((symbol-function 'chirp-media--video-badged-thumbnail-image)
               (lambda (file size)
                 (setq rendered-file (list file size))
                 'badge-image)))
      (should (eq (chirp-media-thumbnail-image
                   '(:type "video"
                     :preview-url "https://example.com/preview.jpg"))
                  'badge-image))
      (should (equal rendered-file
                     '("/tmp/chirp-video-thumb.jpg" 128))))))

(ert-deftest chirp-media-thumbnail-placeholder-image-exists-for-video-like-media ()
  "Video-like media should reserve thumbnail space before the real preview arrives."
  (cl-letf (((symbol-function 'display-images-p)
             (lambda () t)))
    (should (chirp-media-thumbnail-placeholder-image
             '(:type "video" :url "https://example.com/video.mp4")))
    (should (chirp-media-thumbnail-placeholder-image
             '(:type "animated_gif" :url "https://example.com/anim.mp4")))
    (should-not (chirp-media-thumbnail-placeholder-image
                 '(:type "photo" :url "https://example.com/photo.jpg")))))

(ert-deftest chirp-normalize-media-item-preserves-preview-and-variants ()
  "Structured media payloads should keep preview URLs and variant lists."
  (let* ((media
          (chirp-normalize-media-item
           '(("type" . "video")
             ("url" . "https://high.mp4")
             ("previewUrl" . "https://preview.jpg")
             ("variants"
              . ((("url" . "https://high.mp4")
                  ("bitrate" . 2176000))
                 (("url" . "https://low.mp4")
                  ("bitrate" . 832000))))))))
    (should (equal (plist-get media :preview-url) "https://preview.jpg"))
    (should (equal (mapcar (lambda (variant) (plist-get variant :url))
                           (plist-get media :variants))
                   '("https://high.mp4" "https://low.mp4")))))

(ert-deftest chirp-media-quit-restores-source-buffer-point ()
  "Closing media should restore point and scroll state in the source buffer."
  (let ((source (generate-new-buffer " *chirp-media-source*"))
        (viewer (generate-new-buffer " *chirp-media-viewer*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (with-current-buffer source
            (chirp-view-mode)
            (let ((inhibit-read-only t))
              (dotimes (index 80)
                (insert (format "line %02d\n" index))))
            (goto-char (point-min))
            (forward-line 40)
            (set-window-start (selected-window)
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line 34)
                                (point)))
            (recenter 0))
          (let ((source-point (with-current-buffer source (point)))
                (source-window-state (chirp-capture-window-state source)))
            (cl-letf (((symbol-function 'chirp-media--render-image-buffer)
                       (lambda (buffer media-list index title)
                         (with-current-buffer buffer
                           (chirp-media-view-mode)
                           (setq-local chirp--media-list media-list)
                           (setq-local chirp--media-index index)
                           (setq-local chirp--media-title title)
                           (setq-local chirp--view-title title))
                         (chirp-display-buffer buffer))))
              (chirp-media-open
               '((:type "photo" :url "https://example.com/photo.jpg"))
               0
               "Media"
               viewer))
            (with-current-buffer viewer
              (chirp-media-quit))
            (should (eq (window-buffer (selected-window)) source))
            (with-current-buffer source
              (should (= (point) source-point)))
            (should (= (window-point (selected-window)) source-point))
            (should
             (= (window-start (selected-window))
                (with-current-buffer source
                  (chirp-point-position-from-anchor
                   (plist-get source-window-state :start-anchor)))))))
      (dolist (buffer (list source viewer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-media-open-video-launches-player-with-pipe-connection ()
  "Opening video media should launch the external player without a PTY."
  (let ((chirp-video-player-command "/usr/bin/mpv")
        (chirp-video-playback-max-bitrate 2176000)
        (source (generate-new-buffer " *chirp-video-source*"))
        captured-command
        captured-connection-type
        captured-query-flag)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (with-current-buffer source
            (chirp-view-mode))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured-command (plist-get args :command))
                       (setq captured-connection-type (plist-get args :connection-type))
                       'fake-process))
                    ((symbol-function 'set-process-query-on-exit-flag)
                     (lambda (_process flag)
                       (setq captured-query-flag flag))))
            (chirp-media-open
             '((:type "video"
                :url "https://example.com/high.mp4"
                :variants ((:url "https://example.com/high.mp4" :bitrate 4096000)
                           (:url "https://example.com/mid.mp4" :bitrate 2176000)
                           (:url "https://example.com/low.mp4" :bitrate 832000))))
             0
             "Media"))
          (should (equal captured-command
                         '("/usr/bin/mpv" "https://example.com/mid.mp4")))
          (should (eq captured-connection-type 'pipe))
          (should (eq captured-query-flag nil)))
      (dolist (buffer (list source))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-media-play-launches-configured-player ()
  "Media viewer playback should launch the configured external player on demand."
  (let ((chirp-video-player-command "/usr/bin/mpv")
        (chirp-video-playback-max-bitrate 2176000)
        captured-command
        captured-query-flag)
    (with-temp-buffer
      (chirp-media-view-mode)
      (setq-local chirp--media-list '((:type "animated_gif"
                                       :url "https://example.com/anim-high.mp4"
                                       :variants ((:url "https://example.com/anim-high.mp4" :bitrate 4096000)
                                                  (:url "https://example.com/anim-low.mp4" :bitrate 832000)))))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq captured-command (plist-get args :command))
                   'fake-process))
                ((symbol-function 'set-process-query-on-exit-flag)
                 (lambda (_process flag)
                   (setq captured-query-flag flag))))
        (chirp-media-play)))
    (should (equal captured-command
                   '("/usr/bin/mpv" "https://example.com/anim-low.mp4")))
    (should (eq captured-query-flag nil))))

(ert-deftest chirp-media-play-launches-mpv-with-configured-window-size ()
  "mpv playback should honor `chirp-video-player-window-size'."
  (let ((chirp-video-player-command "/usr/bin/mpv")
        (chirp-video-player-window-size '(1280 . 720))
        captured-command)
    (with-temp-buffer
      (chirp-media-view-mode)
      (setq-local chirp--media-list '((:type "video"
                                       :url "https://example.com/video.mp4")))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq captured-command (plist-get args :command))
                   'fake-process))
                ((symbol-function 'set-process-query-on-exit-flag)
                 (lambda (&rest _args)
                   nil)))
        (chirp-media-play)))
    (should (equal captured-command
                   '("/usr/bin/mpv" "--geometry=1280x720" "https://example.com/video.mp4")))))

(ert-deftest chirp-media-play-falls-back-to-browser-when-player-is-disabled ()
  "When no external player is configured, Chirp should browse the media URL."
  (let ((chirp-video-player-command nil)
        browsed-url)
    (with-temp-buffer
      (chirp-media-view-mode)
      (setq-local chirp--media-list '((:type "animated_gif"
                                       :url "https://example.com/anim.mp4")))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _args)
                   (setq browsed-url url))))
        (chirp-media-play)))
    (should (equal browsed-url "https://example.com/anim.mp4"))))

(ert-deftest chirp-media-download-url-prefers-original-photo-and-highest-video-variant ()
  "Downloads should prefer original photos and the highest bitrate video URL."
  (should
   (equal
    (chirp-media-download-url
     '(:type "photo"
       :url "https://pbs.twimg.com/media/abc123.jpg?name=small"))
    "https://pbs.twimg.com/media/abc123.jpg?name=orig"))
  (should
   (equal
    (chirp-media-download-url
     '(:type "video"
       :url "https://example.com/mid.mp4"
       :variants ((:url "https://example.com/low.mp4" :bitrate 832000)
                  (:url "https://example.com/high.mp4" :bitrate 4096000)
                  (:url "https://example.com/mid.mp4" :bitrate 2176000))))
    "https://example.com/high.mp4")))

(ert-deftest chirp-media-download-at-point-starts-async-download ()
  "Downloading media should prompt for a target path and spawn curl asynchronously."
  (let ((chirp-media-prefetch-command "/usr/bin/curl")
        (chirp-media-download-directory "~/Downloads/")
        captured-command
        start-message)
    (with-temp-buffer
      (chirp-media-view-mode)
      (setq-local chirp--media-list '((:type "video"
                                       :url "https://example.com/mid.mp4"
                                       :variants ((:url "https://example.com/high.mp4" :bitrate 4096000)))))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _args)
                   "/tmp/chirp-download.mp4"))
                ((symbol-function 'file-exists-p)
                 (lambda (_path)
                   nil))
                ((symbol-function 'make-directory) #'ignore)
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq captured-command (plist-get args :command))
                   'fake-process))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq start-message (apply #'format format-string args)))))
        (chirp-media-download-at-point)))
    (should (equal captured-command
                   '("/usr/bin/curl"
                     "-L" "-f" "-sS"
                     "-o" "/tmp/chirp-download.mp4"
                     "https://example.com/high.mp4")))
    (should (equal start-message "Downloading chirp-download.mp4..."))))

(ert-deftest chirp-media-download-at-point-falls-back-to-browserless-copy-when-no-curl ()
  "Downloading media without curl should use `url-copy-file'."
  (let ((chirp-media-prefetch-command nil)
        copied-url
        copied-target
        final-message)
    (with-temp-buffer
      (chirp-media-view-mode)
      (setq-local chirp--media-list '((:type "photo"
                                       :url "https://pbs.twimg.com/media/abc123.jpg")))
      (setq-local chirp--media-index 0)
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _args)
                   "/tmp/chirp-photo.jpg"))
                ((symbol-function 'file-exists-p)
                 (lambda (_path)
                   nil))
                ((symbol-function 'make-directory) #'ignore)
                ((symbol-function 'url-copy-file)
                 (lambda (url target &optional _ok-if-exists)
                   (setq copied-url url
                         copied-target target)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq final-message (apply #'format format-string args)))))
        (chirp-media-download-at-point)))
    (should (equal copied-url
                   "https://pbs.twimg.com/media/abc123.jpg?name=orig"))
    (should (equal copied-target "/tmp/chirp-photo.jpg"))
    (should (equal final-message "Downloaded /tmp/chirp-photo.jpg"))))

(provide 'chirp-media-test)

;;; chirp-media-test.el ends here
