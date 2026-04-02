;;; chirp-render.el --- Rendering helpers for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(declare-function nerd-icons-faicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))

(require 'cl-lib)
(require 'subr-x)
(require 'chirp-core)
(require 'chirp-media)
(require 'nerd-icons nil t)

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

(defface chirp-link-face
  '((t :inherit link))
  "Face used for expanded links."
  :group 'chirp)

(defface chirp-article-title-face
  '((t :inherit (bold font-lock-doc-face)))
  "Face used for article titles."
  :group 'chirp)

(defface chirp-article-summary-face
  '((t :inherit font-lock-doc-face))
  "Face used for short article previews."
  :group 'chirp)

(defface chirp-link-card-title-face
  '((t :inherit (bold font-lock-doc-face)))
  "Face used for external link-card titles."
  :group 'chirp)

(defface chirp-link-card-description-face
  '((t :inherit shadow))
  "Face used for external link-card descriptions."
  :group 'chirp)

(defface chirp-quoted-tweet-block-face
  '((((class color) (background light))
     :background "#f3f5f7"
     :extend t)
    (((class color) (background dark))
     :background "#1f242b"
     :extend t)
    (t :inherit shadow))
  "Face layered beneath quoted-tweet blocks."
  :group 'chirp)

(defface chirp-quoted-tweet-face
  '((t :inherit (bold font-lock-doc-face)))
  "Face used for quoted-tweet headers."
  :group 'chirp)

(defface chirp-media-placeholder-face
  '((t :inherit shadow :box t))
  "Face used for text media placeholders."
  :group 'chirp)

(defface chirp-thread-reply-context-face
  '((t :inherit shadow :slant italic))
  "Face used for reply context lines in thread views."
  :group 'chirp)

(defface chirp-social-context-face
  '((t :inherit shadow))
  "Face used for home/following social context lines."
  :group 'chirp)

(defface chirp-thread-divider-face
  '((t :inherit shadow))
  "Face used for separators inside thread views."
  :group 'chirp)

(defface chirp-active-metric-face
  '((t :inherit (bold success)))
  "Face used for active tweet state metrics."
  :group 'chirp)

(defface chirp-liked-metric-face
  '((((class color) (background light))
     :inherit bold
     :foreground "#d73a49")
    (((class color) (background dark))
     :inherit bold
     :foreground "#ff7b8b")
    (t :inherit chirp-active-metric-face))
  "Face used for liked tweet metrics."
  :group 'chirp)

(defface chirp-retweeted-metric-face
  '((((class color) (background light))
     :inherit bold
     :foreground "#1f9d55")
    (((class color) (background dark))
     :inherit bold
     :foreground "#4ddf83")
    (t :inherit chirp-active-metric-face))
  "Face used for retweeted tweet metrics."
  :group 'chirp)

(defface chirp-bookmarked-metric-face
  '((((class color) (background light))
     :inherit bold
     :foreground "#2563eb")
    (((class color) (background dark))
     :inherit bold
     :foreground "#6ea8ff")
    (t :inherit chirp-active-metric-face))
  "Face used for bookmarked tweet metrics."
  :group 'chirp)

(defun chirp-render--metric-face (label active)
  "Return the face used for metric LABEL.

When ACTIVE is non-nil, prefer the action-specific face for LABEL."
  (if active
      (pcase label
        ('like 'chirp-liked-metric-face)
        ('retweet 'chirp-retweeted-metric-face)
        ('bookmark 'chirp-bookmarked-metric-face)
        (_ 'chirp-active-metric-face))
    'chirp-meta-face))

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

(defun chirp-render--mark-subentry (start end entry)
  "Mark the region from START to END as nested ENTRY."
  (when (< start end)
    (add-text-properties
     start end
     `(chirp-subentry-item ,entry
                           chirp-subentry-url ,(plist-get entry :url)
                           pointer hand
                           help-echo "RET: open quoted tweet  o: browser"))))

(defun chirp-render--mark-url-region (start end url)
  "Mark the region from START to END as opening URL in a browser."
  (when (and (< start end)
             (stringp url)
             (not (string-empty-p url)))
    (add-text-properties
     start end
     `(chirp-subentry-url ,url
                          pointer hand
                          help-echo "o: browser"))))

(defun chirp-render--mark-author-region (start end handle)
  "Mark the region from START to END as the avatar region for HANDLE."
  (when (and handle
             (< start end))
    (add-text-properties
     start end
     `(chirp-author-handle ,handle
                           chirp-author-profile-url
                           ,(format "https://x.com/%s" handle)
                           pointer hand
                           help-echo "RET: open author profile  o: browser"))))

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

(defun chirp-render--apply-wrap-prefix (start end prefix face)
  "Apply PREFIX as the visual wrap prefix on text between START and END."
  (when (and prefix
             (< start end))
    (put-text-property start end
                       'wrap-prefix
                       (chirp-render--prefix-string prefix face))))

(defun chirp-render--metric-string (label value &optional active)
  "Return a metric string for LABEL and VALUE.

When ACTIVE is non-nil, emphasize the metric."
  (let* ((face (chirp-render--metric-face label active))
         (prefix
          (pcase label
            ('reply
             (if (fboundp 'nerd-icons-faicon)
                 (nerd-icons-faicon "nf-fa-reply" :face face)
               "Replies"))
            ('retweet
             (if (fboundp 'nerd-icons-faicon)
                 (nerd-icons-faicon "nf-fa-retweet" :face face)
               "RT"))
            ('like
             (if (fboundp 'nerd-icons-faicon)
                 (nerd-icons-faicon "nf-fa-heart" :face face)
               "Likes"))
            ('quote
             (if (fboundp 'nerd-icons-mdicon)
                 (nerd-icons-mdicon "nf-md-format_quote_open" :face face)
               "Quotes"))
            ('bookmark
             (if (fboundp 'nerd-icons-mdicon)
                 (nerd-icons-mdicon "nf-md-bookmark" :face face)
               "Bookmarks"))
            ('view
             (if (fboundp 'nerd-icons-mdicon)
                 (nerd-icons-mdicon "nf-md-eye" :face face)
               "Views"))
            (_
             (format "%s" label)))))
    (propertize (format "%s %s" prefix (chirp-format-count value))
                'face face)))

(defun chirp-render--insert-filled-text (text &optional prefix prefix-face)
  "Insert TEXT and let Emacs wrap it visually in the current window."
  (dolist (line (split-string (chirp-clean-text text) "\n" nil))
    (chirp-render--insert-prefix prefix prefix-face)
    (let ((start (point)))
      (insert line)
      (insert "\n")
      (chirp-render--apply-wrap-prefix start (point) prefix prefix-face))))

(defun chirp-render--insert-face-text (text face &optional prefix prefix-face)
  "Insert TEXT using FACE, optionally preceded by PREFIX."
  (dolist (line (split-string (chirp-clean-text text) "\n" nil))
    (chirp-render--insert-prefix prefix prefix-face)
    (let ((start (point)))
      (if (string-empty-p line)
          (insert "")
        (insert (propertize line 'face face)))
      (insert "\n")
      (chirp-render--apply-wrap-prefix start (point) prefix prefix-face))))

(defun chirp-render--insert-expanded-urls (urls &optional prefix prefix-face)
  "Insert URLS as separate readable lines."
  (when urls
    (dolist (url urls)
      (chirp-render--insert-prefix prefix prefix-face)
      (let ((start (point)))
        (insert (propertize url 'face 'chirp-link-face))
        (insert "\n")
        (chirp-render--apply-wrap-prefix start (point) prefix prefix-face)))))

(defun chirp-render--insert-article-preview (tweet &optional detailp prefix prefix-face)
  "Insert article metadata for TWEET.

When DETAILP is non-nil, use a longer preview."
  (let* ((title (plist-get tweet :article-title))
         (preview (chirp-tweet-article-preview tweet (if detailp 420 220)))
         (text (or (plist-get tweet :text) "")))
    (when (and title
               (not (string-empty-p title))
               (not (string= title text)))
      (chirp-render--insert-face-text title 'chirp-article-title-face prefix prefix-face))
    (when (and preview
               (not (string-empty-p preview))
               (not (string= preview text))
               (not (string= preview title)))
      (chirp-render--insert-face-text preview 'chirp-article-summary-face prefix prefix-face))))

(defun chirp-render--insert-article-body (tweet &optional prefix prefix-face)
  "Insert the full article content for TWEET."
  (let ((title (plist-get tweet :article-title))
        (text (or (plist-get tweet :text) "")))
    (when (and title
               (not (string-empty-p title))
               (not (string= title text)))
      (chirp-render--insert-face-text title 'chirp-article-title-face prefix prefix-face))
    (dolist (segment (chirp-article-segments (plist-get tweet :article-text)))
      (pcase (plist-get segment :type)
        ('text
         (chirp-render--insert-filled-text (plist-get segment :text) prefix prefix-face)
         (insert "\n"))
        ('image
         (chirp-render-insert-media-strip (list (plist-get segment :media))
                                          prefix
                                          prefix-face))))))

(defun chirp-render--insert-article-media-preview
    (tweet &optional detailp prefix prefix-face)
  "Insert article images for TWEET previews."
  (let ((images (chirp-tweet-article-images tweet (unless detailp 1))))
    (when images
      (chirp-render-insert-media-strip images prefix prefix-face))))

(defun chirp-render--truncate-link-card-text (text max-length)
  "Return TEXT truncated to MAX-LENGTH characters when needed."
  (let ((cleaned (chirp-clean-text text)))
    (if (<= (length cleaned) max-length)
        cleaned
      (concat (string-trim-right (substring cleaned 0 (max 0 (- max-length 3))))
              "..."))))

(defun chirp-render--insert-link-card (card &optional prefix prefix-face)
  "Insert one external link CARD."
  (let* ((start (point))
         (url (plist-get card :url))
         (title (plist-get card :title))
         (description (plist-get card :description))
         (image-url (plist-get card :image-url))
         (thumb (and image-url
                     (chirp-media-thumbnail-image
                      (list :type "photo"
                            :url image-url)))))
    (when thumb
      (chirp-render--insert-prefix prefix prefix-face)
      (insert-image thumb "[link preview]")
      (insert "\n"))
    (when (and title
               (not (string-empty-p title)))
      (chirp-render--insert-face-text
       (chirp-render--truncate-link-card-text title 180)
       'chirp-link-card-title-face
       prefix
       prefix-face))
    (when (and description
               (not (string-empty-p description))
               (not (string= description title)))
      (chirp-render--insert-face-text
       (chirp-render--truncate-link-card-text description 220)
       'chirp-link-card-description-face
       prefix
       prefix-face))
    (when (and url
               (not (string-empty-p url)))
      (chirp-render--insert-prefix prefix prefix-face)
      (let ((line-start (point)))
        (insert (propertize url 'face 'chirp-link-face))
        (insert "\n")
        (chirp-render--apply-wrap-prefix line-start (point) prefix prefix-face)))
    (insert "\n")
    (chirp-render--mark-url-region start (point) url)))

(defun chirp-render--insert-link-cards (tweet &optional prefix prefix-face)
  "Insert cached external link-card previews for TWEET."
  (dolist (card (chirp-media-link-cards-for-tweet tweet))
    (chirp-render--insert-link-card card prefix prefix-face)))

(defun chirp-render--insert-quoted-tweet (tweet &optional prefix prefix-face)
  "Insert the quoted tweet preview inside TWEET."
  (ignore prefix-face)
  (when-let* ((quoted (plist-get tweet :quoted-tweet)))
    (let* ((start (point))
           (quoted-prefix (concat (or prefix "") "│ "))
           (quoted-prefix-face 'chirp-quoted-tweet-block-face)
           (handle (plist-get quoted :author-handle))
           (name (plist-get quoted :author-name))
           (label (cond
                   ((and handle name)
                    (format "Quoted @%s (%s)" handle name))
                   (handle
                    (format "Quoted @%s" handle))
                   (name
                    (format "Quoted %s" name))
                   (t
                    "Quoted tweet"))))
      (chirp-render--insert-prefix prefix quoted-prefix-face)
      (let ((line-start (point)))
        (chirp-render--insert-prefix quoted-prefix quoted-prefix-face)
        (insert (propertize label
                            'face '(chirp-quoted-tweet-face
                                    chirp-quoted-tweet-block-face)))
        (insert "\n")
        (chirp-render--apply-wrap-prefix
         line-start (point) quoted-prefix quoted-prefix-face))
      (when-let* ((text (chirp-tweet-preview-text quoted 140)))
        (unless (string-empty-p text)
          (chirp-render--insert-filled-text text quoted-prefix quoted-prefix-face)))
      (chirp-render--insert-article-preview quoted nil quoted-prefix quoted-prefix-face)
      (chirp-render--insert-article-media-preview quoted nil quoted-prefix quoted-prefix-face)
      (chirp-render-insert-media-strip (plist-get quoted :media)
                                       quoted-prefix
                                       quoted-prefix-face)
      (chirp-render--insert-link-cards quoted quoted-prefix quoted-prefix-face)
      (add-face-text-property start (point) 'chirp-quoted-tweet-block-face 'append)
      (chirp-render--mark-subentry start (point) quoted))))

(defun chirp-render--insert-avatar (url &optional handle)
  "Insert an avatar for URL when possible."
  (let ((start (point)))
    (if-let* ((image (chirp-media-avatar-image url)))
        (progn
          (insert-image image " ")
          (insert " "))
      (insert "  "))
    (chirp-render--mark-author-region start (point) handle)))

(defun chirp-render--unscaled-image-copy (image)
  "Return IMAGE with scaling neutralized for slice calculations."
  (let ((copy (copy-tree image)))
    (when (and (consp copy)
               (listp (cdr copy)))
      (plist-put (cdr copy) :scale 1.0))
    copy))

(defun chirp-render--insert-sliced-image (image prefix &optional prefix-face)
  "Insert IMAGE as multiple line slices, each preceded by PREFIX."
  (let* ((char-height (max 1 (frame-char-height)))
         (display-size (image-size image t))
         (display-height (max 1 (truncate (cdr display-size))))
         (source-size (image-size (chirp-render--unscaled-image-copy image) t))
         (source-height (max 1 (truncate (cdr source-size))))
         (source-per-display (/ (float source-height) display-height))
         (nslices (max 1 (ceiling (/ display-height (float char-height))))))
    (dotimes (slice-num nslices)
      (let* ((display-y0 (min display-height (* slice-num char-height)))
             (display-y1 (min display-height (* (1+ slice-num) char-height)))
             (source-y0 (round (* display-y0 source-per-display)))
             (source-y1 (round (* display-y1 source-per-display)))
             (slice-height (max 1 (- source-y1 source-y0)))
             (slice-start nil))
        (chirp-render--insert-prefix prefix prefix-face)
        (setq slice-start (point))
        (insert " ")
        (add-text-properties
         slice-start (point)
         `(rear-nonsticky (display)
                          display ((slice 0 ,source-y0 1.0 ,slice-height) ,image))))
      (when (< (1+ slice-num) nslices)
        (let ((newline-start (point)))
          (insert "\n")
          (add-text-properties
           newline-start (point)
           '(line-height t
                         line-spacing 0)))))))

(defun chirp-render--insert-media-cell (media media-list index &optional prefix prefix-face)
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
        (if prefix
            (chirp-render--insert-sliced-image thumb prefix prefix-face)
          (insert-image thumb placeholder))
      (if-let* ((fallback (chirp-media-thumbnail-placeholder-image media)))
          (if prefix
              (chirp-render--insert-sliced-image fallback prefix prefix-face)
            (insert-image fallback placeholder))
        (insert (propertize placeholder 'face 'chirp-media-placeholder-face))))
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
    (if prefix
        (cl-loop for media in media-list
                 for index from 0
                 do (chirp-render--insert-media-cell media media-list index prefix prefix-face)
                 (insert "\n"))
      (chirp-render--insert-prefix prefix prefix-face)
      (cl-loop for media in media-list
               for index from 0
               do (chirp-render--insert-media-cell media media-list index)))
    (insert "\n\n")))

(defun chirp-render--insert-tweet (tweet &optional prefix prefix-face show-reply-context article-mode)
  "Insert TWEET at point, optionally prefixed for thread rendering."
  (let* ((start (point))
         (author (or (plist-get tweet :author-name) "Unknown"))
         (handle (plist-get tweet :author-handle))
         (retweeted-by (plist-get tweet :retweeted-by))
         (created-at (plist-get tweet :created-at))
         (meta-start nil))
    (when retweeted-by
      (chirp-render--insert-prefix prefix prefix-face)
      (insert (propertize (format "retweeted by @%s" retweeted-by)
                          'face 'chirp-social-context-face))
      (insert "\n"))
    (chirp-render--insert-prefix prefix prefix-face)
    (chirp-render--insert-avatar (plist-get tweet :author-avatar-url) handle)
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
    (when-let* ((text (plist-get tweet :text)))
      (unless (string-empty-p text)
        (chirp-render--insert-filled-text text prefix prefix-face)))
    (chirp-render--insert-quoted-tweet tweet prefix prefix-face)
    (pcase article-mode
      ('full
       (chirp-render--insert-article-body tweet prefix prefix-face))
      (_
       (chirp-render--insert-article-preview tweet article-mode prefix prefix-face)
       (chirp-render--insert-article-media-preview tweet article-mode prefix prefix-face)))
    (chirp-render--insert-link-cards tweet prefix prefix-face)
    (chirp-render--insert-expanded-urls (plist-get tweet :urls) prefix prefix-face)
    (chirp-render-insert-media-strip (plist-get tweet :media)
                                     prefix
                                     prefix-face)
    (setq meta-start (point))
    (chirp-render--insert-prefix prefix prefix-face)
    (insert (chirp-render--metric-string 'reply (plist-get tweet :reply-count)))
    (insert "   ")
    (insert (chirp-render--metric-string
             'retweet
             (plist-get tweet :retweet-count)
             (plist-get tweet :retweeted-p)))
    (insert "   ")
    (insert (chirp-render--metric-string
             'like
             (plist-get tweet :like-count)
             (plist-get tweet :liked-p)))
    (insert "   ")
    (insert (chirp-render--metric-string 'quote (plist-get tweet :quote-count)))
    (insert "   ")
    (insert (chirp-render--metric-string
             'bookmark
             (plist-get tweet :bookmark-count)
             (plist-get tweet :bookmarked-p)))
    (insert "   ")
    (insert (chirp-render--metric-string 'view (plist-get tweet :view-count)))
    (insert "\n")
    (insert "\n")
    (chirp-render--mark-entry start (point) tweet)
    (put-text-property meta-start (point) 'rear-nonsticky t)))

(defun chirp-render-insert-tweet (tweet)
  "Insert TWEET at point."
  (chirp-render--insert-tweet tweet))

(defun chirp-render-insert-thread-focus-tweet (tweet)
  "Insert the focus TWEET in a thread view."
  (chirp-render--insert-tweet tweet nil nil nil 'full))

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
    (chirp-render--insert-avatar (plist-get user :avatar-url)
                                 (plist-get user :handle))
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
