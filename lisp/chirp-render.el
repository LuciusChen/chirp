;;; chirp-render.el --- Rendering helpers for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chirp-core)
(require 'chirp-media)

(defface chirp-section-face
  '((t :inherit bold :height 1.2))
  "Face used for section titles."
  :group 'chirp)

(defface chirp-author-face
  '((t :inherit (bold font-lock-keyword-face)))
  "Face used for author names."
  :group 'chirp)

(defface chirp-handle-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for handles."
  :group 'chirp)

(defface chirp-meta-face
  '((t :inherit shadow))
  "Face used for metadata."
  :group 'chirp)

(defface chirp-media-placeholder-face
  '((t :inherit shadow :box t))
  "Face used for text media placeholders."
  :group 'chirp)

(defface chirp-thread-reply-context-face
  '((t :inherit shadow :slant italic))
  "Face used for reply context lines in thread views."
  :group 'chirp)

(defface chirp-thread-divider-face
  '((t :inherit shadow))
  "Face used for separators inside thread views."
  :group 'chirp)

(defface chirp-active-metric-face
  '((t :inherit (bold success)))
  "Face used for active tweet state metrics."
  :group 'chirp)

(defun chirp-render--mark-entry (start end entry)
  "Mark the region from START to END as ENTRY."
  (when (< start end)
    (add-text-properties
     start end
     `(chirp-entry-item ,entry
                        chirp-entry-url
                        ,(or (plist-get entry :url)
                             (plist-get entry :profile-url))
                        pointer hand
                        help-echo "RET: open  m: media  A: author  o: browser"))
    (put-text-property start (1+ start) 'chirp-entry-start t)))

(defun chirp-render-insert-section (title)
  "Insert section heading TITLE."
  (insert (propertize title 'face 'chirp-section-face))
  (insert "\n\n"))

(defun chirp-render-insert-empty (message)
  "Insert MESSAGE for an empty state."
  (insert message)
  (insert "\n"))

(defun chirp-render--insert-prefix (prefix &optional face)
  "Insert PREFIX using FACE."
  (when prefix
    (insert (if face
                (propertize prefix 'face face)
              prefix))))

(defun chirp-render--prefix-string (prefix face)
  "Return PREFIX propertized with FACE, or nil."
  (when prefix
    (if face
        (propertize prefix 'face face)
      prefix)))

(defun chirp-render--metric-string (label value &optional active)
  "Return a metric string for LABEL and VALUE.

When ACTIVE is non-nil, emphasize the metric."
  (propertize (format "%s %s" label (chirp-format-count value))
              'face (if active
                        'chirp-active-metric-face
                      'chirp-meta-face)))

(defun chirp-render--insert-filled-text (text &optional prefix prefix-face)
  "Insert TEXT and fill it to the current window width."
  (let ((start (point))
        (fill-prefix (chirp-render--prefix-string prefix prefix-face)))
    (chirp-render--insert-prefix prefix prefix-face)
    (insert (chirp-clean-text text))
    (insert "\n")
    (fill-region start (1- (point)))))

(defun chirp-render--insert-avatar (url)
  "Insert an avatar for URL when possible."
  (if-let* ((image (chirp-media-avatar-image url)))
      (progn
        (insert-image image " ")
        (insert " "))
    (insert "  ")))

(defun chirp-render--insert-media-cell (media media-list index)
  "Insert one media cell for MEDIA."
  (let ((start (point))
        (placeholder
         (pcase (plist-get media :type)
           ("video"
            (format "[video %s]"
                    (if-let* ((width (plist-get media :width))
                              (height (plist-get media :height)))
                        (format "%sx%s" width height)
                      "open")))
           ("animated_gif" "[gif]")
           (_ "[image]"))))
    (if-let* ((thumb (chirp-media-thumbnail-image media)))
        (insert-image thumb placeholder)
      (insert (propertize placeholder 'face 'chirp-media-placeholder-face)))
    (add-text-properties
     start (point)
     `(chirp-media-item ,media
                        chirp-media-index ,index
                        chirp-media-list ,media-list
                        pointer hand
                        help-echo "RET: open media  o: browser"))))

(defun chirp-render-insert-media-strip (media-list &optional prefix prefix-face)
  "Insert a grid of thumbnails for MEDIA-LIST."
  (when media-list
    (chirp-render--insert-prefix prefix prefix-face)
    (cl-loop for media in media-list
             for index from 0
             do (chirp-render--insert-media-cell media media-list index))
    (insert "\n\n")))

(defun chirp-render--insert-tweet (tweet &optional prefix prefix-face show-reply-context)
  "Insert TWEET at point, optionally prefixed for thread rendering."
  (let* ((start (point))
         (author (or (plist-get tweet :author-name) "Unknown"))
         (handle (plist-get tweet :author-handle))
         (created-at (plist-get tweet :created-at))
         (meta-start nil))
    (chirp-render--insert-prefix prefix prefix-face)
    (chirp-render--insert-avatar (plist-get tweet :author-avatar-url))
    (insert (propertize author 'face 'chirp-author-face))
    (when handle
      (insert " ")
      (insert (propertize (format "@%s" handle) 'face 'chirp-handle-face)))
    (when created-at
      (insert "  ")
      (insert (propertize created-at 'face 'chirp-meta-face)))
    (insert "\n")
    (when (and show-reply-context
               (plist-get tweet :reply-to-handle))
      (chirp-render--insert-prefix prefix prefix-face)
      (insert (propertize (format "replying to @%s"
                                  (plist-get tweet :reply-to-handle))
                          'face 'chirp-thread-reply-context-face))
      (insert "\n"))
    (chirp-render--insert-filled-text (or (plist-get tweet :text) "")
                                      prefix
                                      prefix-face)
    (chirp-render-insert-media-strip (plist-get tweet :media)
                                     prefix
                                     prefix-face)
    (setq meta-start (point))
    (chirp-render--insert-prefix prefix prefix-face)
    (insert (chirp-render--metric-string "Replies" (plist-get tweet :reply-count)))
    (insert "   ")
    (insert (chirp-render--metric-string
             (if (plist-get tweet :retweeted-p) "RTed" "RT")
             (plist-get tweet :retweet-count)
             (plist-get tweet :retweeted-p)))
    (insert "   ")
    (insert (chirp-render--metric-string
             (if (plist-get tweet :liked-p) "Liked" "Likes")
             (plist-get tweet :like-count)
             (plist-get tweet :liked-p)))
    (insert "   ")
    (insert (chirp-render--metric-string "Quotes" (plist-get tweet :quote-count)))
    (insert "   ")
    (insert (chirp-render--metric-string
             (if (plist-get tweet :bookmarked-p) "Saved" "Bookmarks")
             (plist-get tweet :bookmark-count)
             (plist-get tweet :bookmarked-p)))
    (insert "   ")
    (insert (chirp-render--metric-string "Views" (plist-get tweet :view-count)))
    (insert "\n")
    (insert "\n")
    (chirp-render--mark-entry start (point) tweet)
    (put-text-property meta-start (point) 'rear-nonsticky t)))

(defun chirp-render-insert-tweet (tweet)
  "Insert TWEET at point."
  (chirp-render--insert-tweet tweet))

(defun chirp-render-insert-thread-focus-tweet (tweet)
  "Insert the focus TWEET in a thread view."
  (chirp-render--insert-tweet tweet))

(defun chirp-render-insert-thread-reply (tweet)
  "Insert a reply TWEET in a thread view."
  (chirp-render--insert-tweet tweet nil nil t))

(defun chirp-render-insert-thread-divider ()
  "Insert a subtle divider between the focus tweet and replies."
  (insert (propertize (make-string 36 ?-) 'face 'chirp-thread-divider-face))
  (insert "\n\n"))

(defun chirp-render-insert-user-summary (user)
  "Insert USER summary."
  (let ((start (point)))
    (chirp-render--insert-avatar (plist-get user :avatar-url))
    (insert (propertize (or (plist-get user :name) "Unknown")
                        'face 'chirp-author-face))
    (when-let* ((handle (plist-get user :handle)))
      (insert " ")
      (insert (propertize (format "@%s" handle) 'face 'chirp-handle-face)))
    (insert "\n")
    (when-let* ((bio (plist-get user :bio)))
      (unless (string-empty-p bio)
        (chirp-render--insert-filled-text bio)))
    (insert
     (propertize
      (format "Posts %s   Following %s   Followers %s"
              (chirp-format-count (plist-get user :posts))
              (chirp-format-count (plist-get user :following))
              (chirp-format-count (plist-get user :followers)))
      'face 'chirp-meta-face))
    (insert "\n")
    (when-let* ((joined (plist-get user :joined)))
      (insert (propertize (format "Joined %s" joined) 'face 'chirp-meta-face))
      (insert "\n"))
    (when-let* ((url (plist-get user :profile-url)))
      (insert (propertize url 'face 'link))
      (insert "\n"))
    (insert "\n")
    (chirp-render--mark-entry start (point) user)))

(provide 'chirp-render)

;;; chirp-render.el ends here
