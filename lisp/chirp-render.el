;;; chirp-render.el --- Rendering helpers for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared text, link, tweet, and media rendering for Chirp view buffers.

;;; Code:

(declare-function nerd-icons-faicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))

(require 'cl-lib)
(require 'subr-x)
(require 'chirp-core)
(require 'chirp-media)
(require 'nerd-icons nil t)

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

(defface chirp-translation-face
  '((t :inherit font-lock-doc-face))
  "Face used for translated tweet text."
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
  '((t :inherit fringe
       :foreground unspecified
       :extend t))
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

(defface chirp-thread-related-context
  '((t :inherit (bold font-lock-keyword-face)))
  "Face used for related-tweet context lines in thread views."
  :group 'chirp)

(defconst chirp-render-list-reply-prefix "  "
  "Indentation used for direct replies to the previous visible tweet.")

(defface chirp-social-context-face
  '((t :inherit shadow))
  "Face used for home/following social context lines."
  :group 'chirp)

(defface chirp-thread-divider-face
  '((t :inherit shadow))
  "Face used for separators inside thread views."
  :group 'chirp)

(defface chirp-tweet-separator-face
  '((t :inherit chirp-thread-divider-face))
  "Face used for separators between tweet list entries."
  :group 'chirp)

(defcustom chirp-tweet-separator "- - - - - - - - - - - -"
  "Separator text inserted between tweets in list views.

Set this to nil or an empty string to disable tweet separators."
  :type '(choice (const :tag "No separator" nil)
                 (string :tag "Separator text"))
  :group 'chirp)

(defcustom chirp-tweet-separator-indent 6
  "Number of leading spaces before tweet list separators."
  :type 'integer
  :group 'chirp)

(defface chirp-profile-view-active-face
  '((t :inherit (mode-line-emphasis link)))
  "Face used for the active profile subview label."
  :group 'chirp)

(defface chirp-profile-view-inactive-face
  '((t :inherit shadow))
  "Face used for inactive profile subview labels."
  :group 'chirp)

(defface chirp-profile-action-face
  '((t :inherit button))
  "Face used for clickable profile action buttons."
  :group 'chirp)

(defface chirp-profile-action-secondary-face
  '((t :inherit shadow))
  "Face used for secondary profile relationship labels."
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

(defun chirp-render-mark-entry (start end entry)
  "Mark the region from START to END as ENTRY."
  (when (< start end)
    (add-text-properties
     start end
     `(chirp-entry-item ,entry
                        chirp-entry-url
                        ,(or (plist-get entry :url)
                             (plist-get entry :profile-url))
                        rear-nonsticky t))
    (put-text-property start (1+ start) 'chirp-entry-start t)))

(defun chirp-render--mark-url-region (start end url)
  "Mark the region from START to END as opening URL in a browser."
  (when (and (< start end)
             (stringp url)
             (not (string-empty-p url)))
    (add-text-properties
     start end
     `(chirp-link-url ,url
                      mouse-face highlight
                      pointer hand
                      help-echo "RET, Mouse-1 or o: open in browser"))))

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

(defun chirp-render--mark-profile-list-region (start end kind handle)
  "Mark the region from START to END as profile list KIND for HANDLE."
  (when (and handle
             (< start end))
    (add-text-properties
     start end
     `(chirp-profile-list-kind ,kind
                               chirp-profile-list-handle ,handle
                               pointer hand
                               help-echo
                               ,(pcase kind
                                  ('followers "RET: open followers")
                                  ('following "RET: open following")
                                  (_ "RET: open profile list"))))))

(defun chirp-render--mark-profile-view-region (start end mode)
  "Mark the region from START to END as profile subview MODE."
  (when (< start end)
    (add-text-properties
     start end
     `(chirp-profile-view-mode ,mode
                               pointer hand
                               help-echo "RET/TAB: switch profile view"))))

(defun chirp-render--mark-profile-action-region (start end action handle)
  "Mark the region from START to END as profile ACTION for HANDLE."
  (when (and handle
             (< start end))
    (add-text-properties
     start end
     `(chirp-profile-action ,action
                            chirp-profile-action-handle ,handle
                            pointer hand
                            help-echo "RET: toggle follow"))))

(defun chirp-render--mark-tweet-action-region (start end action)
  "Mark the region from START to END as tweet ACTION."
  (when (< start end)
    (let ((count (get-text-property (1- end) 'help-echo))
          (hint (pcase action
                  ('quote "quote this tweet")
                  ('reply "reply to this tweet")
                  ('retweet "toggle repost/retweet")
                  ('like "toggle like")
                  ('bookmark "toggle bookmark"))))
      (add-text-properties
       start end
       (list 'chirp-tweet-action action 'mouse-face 'highlight 'pointer 'hand
             'help-echo (concat "Mouse-1: " hint (and count (concat " · " count)))
             'rear-nonsticky t)))))

(defun chirp-render--profile-follow-action-label (user)
  "Return the primary follow button label for USER, or nil."
  (cond
   ((plist-get user :self-p) nil)
   ((plist-get user :viewer-following-p) "Following")
   ((plist-get user :viewer-followed-by-p) "Follow back")
   (t "Follow")))

(defun chirp-render-insert-profile-view-strip (current-mode modes)
  "Insert a lightweight profile subview strip for MODES.

CURRENT-MODE marks the active entry."
  (when modes
    (dolist (mode modes)
      (let ((start (point)))
        (insert
         (propertize
         (pcase mode
            ('posts "Posts")
            ('replies "Replies")
            ('highlights "Highlights")
            ('media "Media")
            ('likes "Likes")
            (_ (capitalize (symbol-name mode))))
          'face (if (eq mode current-mode)
                    'chirp-profile-view-active-face
                  'chirp-profile-view-inactive-face)))
        (chirp-render--mark-profile-view-region start (point) mode))
      (unless (eq mode (car (last modes)))
        (insert (propertize "  " 'face 'shadow))))
    (insert "\n\n")))

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
               (if active "RTed" "RT")))
            ('like
             (if (fboundp 'nerd-icons-faicon)
                 (nerd-icons-faicon "nf-fa-heart" :face face)
               (if active "Liked" "Likes")))
            ('quote
             (if (fboundp 'nerd-icons-mdicon)
                 (nerd-icons-mdicon "nf-md-format_quote_open" :face face)
               "Quotes"))
            ('bookmark
             (if (fboundp 'nerd-icons-mdicon)
                 (nerd-icons-mdicon "nf-md-bookmark" :face face)
               (if active "Saved" "Bookmarks")))
            ('view
             (if (fboundp 'nerd-icons-mdicon)
                 (nerd-icons-mdicon "nf-md-eye" :face face)
               "Views"))
            (_
             (format "%s" label)))))
    (propertize (format "%s %s" prefix (chirp-format-count value))
                'face face)))

(defun chirp-render--insert-metric (label value &optional active)
  "Insert the metric for LABEL and VALUE, returning its buffer region.

When ACTIVE is non-nil, emphasize the metric."
  (let ((start (point)))
    (insert (chirp-render--metric-string label value active))
    (cons start (point))))

(defun chirp-render--insert-state-metric (label tweet state-key count-key)
  "Insert LABEL for TWEET, previewing a pending STATE-KEY and its COUNT-KEY."
  (let* ((state (chirp-boolean-value (plist-get tweet state-key)))
         (pending (gethash (cons (plist-get tweet :id) state-key) chirp-pending-tweet-states))
         (active (if pending (eq pending 'on) state))
         (count (plist-get tweet count-key)))
    (chirp-render--insert-metric label (if (eq active state) count
                                         (chirp-adjust-count count (if active 1 -1)))
                                 active)))

(defun chirp-render-timestamp (stamp)
  "Format STAMP in local time, with the full timestamp on hover."
  (condition-case nil
      (let* ((time (if (stringp stamp) (date-to-time stamp) stamp))
             (age (max 0 (- (float-time) (float-time time))))
             (label (cond ((< age 60) "now")
                          ((< age 3600) (format "%dm" (floor age 60)))
                          ((< age 86400) (format "%dh" (floor age 3600)))
                          (t (format-time-string
                              (if (equal (format-time-string "%Y" time) (format-time-string "%Y"))
                                  "%b %-d" "%Y-%m-%d") time)))))
        (propertize label 'face 'chirp-meta-face 'help-echo
                    (format-time-string "%Y-%m-%d %H:%M:%S %Z" time)))
    (error (propertize stamp 'face 'chirp-meta-face))))

(defun chirp-render--insert-filled-text (text &optional prefix prefix-face)
  "Insert TEXT and let Emacs wrap it visually in the current window.

Precede each line with PREFIX using PREFIX-FACE when provided."
  (dolist (line (split-string text "\n" nil))
    (chirp-render--insert-prefix prefix prefix-face)
    (let ((start (point)))
      (insert line)
      (save-excursion
        (let ((end (point)))
          (goto-char start)
          (let ((case-fold-search nil))
            (while (re-search-forward
                    (rx (or line-start (not (in "A-Za-z0-9_@#/")))
                        (group (or (seq "@" (+ (in "A-Za-z0-9_")))
                                   (seq "#" (+ (or alnum "_"))))))
                    end t)
              (let* ((tag (match-string-no-properties 1))
                     (mention (= (aref tag 0) ?@)))
                (add-text-properties
                 (match-beginning 1) (match-end 1)
                 (list (if mention 'chirp-mention 'chirp-hashtag) tag
                       'face 'chirp-handle-face 'pointer 'hand 'mouse-face 'highlight
                       'help-echo (if mention "Open profile" "Search hashtag"))))))))
      (insert "\n")
      (chirp-render--apply-wrap-prefix start (point) prefix prefix-face))))

(defun chirp-render--insert-face-text (text face &optional prefix prefix-face)
  "Insert TEXT using FACE, optionally preceded by PREFIX.

Apply PREFIX-FACE to that prefix when provided."
  (dolist (line (split-string text "\n" nil))
    (chirp-render--insert-prefix prefix prefix-face)
    (let ((start (point)))
      (if (string-empty-p line)
          (insert "")
        (insert (propertize line 'face face)))
      (insert "\n")
      (chirp-render--apply-wrap-prefix start (point) prefix prefix-face))))

(defun chirp-render--insert-translation (tweet &optional prefix prefix-face)
  "Insert the cached translation for TWEET when present.

Precede each line with PREFIX using PREFIX-FACE when provided."
  (when-let* ((translation (plist-get tweet :translation))
              ((not (string-empty-p translation))))
    (chirp-render--insert-prefix prefix prefix-face)
    (let ((start (point))
          (language (plist-get tweet :translation-language)))
      (insert (propertize
               (if language
                   (format "Translation · %s" language)
                 "Translation")
               'face 'chirp-meta-face))
      (insert "\n")
      (chirp-render--apply-wrap-prefix start (point) prefix prefix-face))
    (chirp-render--insert-face-text
     translation 'chirp-translation-face prefix prefix-face)))

(defun chirp-render--insert-expanded-urls (urls &optional prefix prefix-face)
  "Insert URLS as separate readable lines.

Precede each line with PREFIX using PREFIX-FACE when provided."
  (when urls
    (dolist (url urls)
      (chirp-render--insert-prefix prefix prefix-face)
      (let ((start (point)))
        (insert (propertize url 'face 'chirp-link-face))
        (let ((end (point)))
          (insert "\n")
          (chirp-render--apply-wrap-prefix start (point) prefix prefix-face)
          (chirp-render--mark-url-region start end url))))))

(defun chirp-render--insert-article-preview (tweet &optional detailp prefix prefix-face)
  "Insert article metadata for TWEET.

When DETAILP is non-nil, use a longer preview.  Precede each line with PREFIX
using PREFIX-FACE when provided."
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

(defun chirp-render--insert-article-text (text prefix prefix-face)
  "Render article TEXT with PREFIX and PREFIX-FACE on each line."
  (let (code)
    (dolist (line (split-string text "\n"))
      (if (string-prefix-p "```" line)
          (setq code (not code))
        (let ((face (cond (code 'fixed-pitch)
                          ((string-match "^#+ +\\(.*\\)" line)
                           (setq line (match-string 1 line)) 'chirp-article-title-face)
                          ((string-prefix-p "> " line)
                           (setq line (concat "│ " (substring line 2))) 'shadow))))
          (chirp-render--insert-prefix prefix prefix-face)
          (let ((start (point)) (pos 0))
            (unless code
              (while (string-match
                      (rx "[" (group (+ (or (seq "\\" nonl) (not (in "]\\\n"))))) "]("
                          (group (or "https://" "http://")
                                 (+ (or (seq "\\" nonl) (not (in " )\\\n"))))) ")")
                      line pos)
                (let ((label (match-string 1 line)) (url (match-string 2 line))
                      (beg (match-beginning 0)) (end (match-end 0)))
                  (insert (substring line pos beg))
                  (let ((from (point)))
                    (insert (propertize
                             (replace-regexp-in-string (rx "\\" (group (in "[]\\"))) "\\1" label)
                             'face 'link))
                    (chirp-render--mark-url-region
                     from (point)
                     (replace-regexp-in-string (rx "\\" (group (in ")\\"))) "\\1" url)))
                  (setq pos end))))
            (insert (substring line pos))
            (when face (add-face-text-property start (point) face))
            (chirp-render--apply-wrap-prefix start (point) prefix prefix-face)
            (insert "\n")))))))

(defun chirp-render--insert-article-body (tweet &optional prefix prefix-face)
  "Insert the full article content for TWEET.

Precede each line with PREFIX using PREFIX-FACE when provided."
  (let ((title (plist-get tweet :article-title))
        (text (or (plist-get tweet :text) "")))
    (when (and title
               (not (string-empty-p title))
               (not (string= title text)))
      (chirp-render--insert-face-text title 'chirp-article-title-face prefix prefix-face))
    (dolist (segment (chirp-article-segments (plist-get tweet :article-text)))
      (pcase (plist-get segment :type)
        ('text
         (chirp-render--insert-article-text (plist-get segment :text) prefix prefix-face)
         (insert "\n"))
        ('image
         (chirp-render-insert-media-strip (list (plist-get segment :media))
                                          prefix
                                          prefix-face))))))

(defun chirp-render--insert-article-media-preview
    (tweet &optional detailp prefix prefix-face)
  "Insert article images for TWEET previews.

When DETAILP is non-nil, include every image.  Precede each line with PREFIX
using PREFIX-FACE when provided."
  (let ((images (chirp-tweet-article-images tweet (unless detailp 1))))
    (when images
      (chirp-render-insert-media-strip images prefix prefix-face))))

(defun chirp-render--article-expandable-p (tweet detailp)
  "Return non-nil when TWEET has article content beyond its preview.

DETAILP selects the same preview length used by the article renderer."
  (let* ((limit (if detailp 420 220))
         (segments (chirp-article-segments (plist-get tweet :article-text)))
         (text-count
          (cl-count-if (lambda (segment)
                         (eq (plist-get segment :type) 'text))
                       segments))
         (image-count
          (cl-count-if (lambda (segment)
                         (eq (plist-get segment :type) 'image))
                       segments))
         (preview (chirp-tweet-article-preview tweet limit))
         (full-summary
          (chirp-tweet-article-preview tweet most-positive-fixnum)))
    (and segments
         (or (> text-count 1)
             (> image-count 1)
             (and full-summary
                  (not (equal preview full-summary)))))))

(defun chirp-render--insert-show-more (tweet &optional prefix prefix-face)
  "Insert an inline expansion action for TWEET.

Precede the action with PREFIX using PREFIX-FACE when provided."
  (when-let* ((tweet-id (plist-get tweet :id)))
    (chirp-render--insert-prefix prefix prefix-face)
    (let ((start (point)))
      (insert (propertize "Show more" 'face 'link))
      (add-text-properties
       start (point)
       `(chirp-expand-tweet-id ,tweet-id
                               mouse-face highlight
                          pointer hand
                               help-echo "RET: show full content"))
      (insert "\n")
      (chirp-render--apply-wrap-prefix start (point) prefix prefix-face))))

(defun chirp-render--truncate-link-card-text (text max-length)
  "Return TEXT truncated to MAX-LENGTH characters when needed."
  (let ((cleaned (chirp-clean-text text)))
    (if (<= (length cleaned) max-length)
        cleaned
      (concat (string-trim-right (substring cleaned 0 (max 0 (- max-length 3))))
              "..."))))

(defun chirp-render--insert-link-card (card &optional prefix prefix-face)
  "Insert one external link CARD.

Precede each line with PREFIX using PREFIX-FACE when provided."
  (let* ((start (point))
         (url (plist-get card :url))
         (title (plist-get card :title))
         (description (plist-get card :description))
         (image-url (plist-get card :image-url))
         (thumb (and chirp-show-tweet-media
                     image-url
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
  "Insert cached external link-card previews for TWEET.

Precede each line with PREFIX using PREFIX-FACE when provided."
  (dolist (card (chirp-media-link-cards-for-tweet tweet))
    (chirp-render--insert-link-card card prefix prefix-face)))

(defun chirp-render--insert-quoted-tweet (tweet &optional prefix prefix-face)
  "Insert the quoted tweet preview inside TWEET.

Nest it below PREFIX.  PREFIX-FACE is accepted for caller consistency; the
quoted-tweet face is used for the nested block."
  (ignore prefix-face)
  (when-let* ((quoted (plist-get tweet :quoted-tweet)))
    (let* ((start (point))
           (quoted-prefix (concat prefix "│ "))
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
      (let ((line-start (point)))
        (chirp-render--insert-prefix quoted-prefix quoted-prefix-face)
        (insert (propertize label 'pointer 'hand 'mouse-face 'highlight
                            'help-echo "Open quoted post"
                            'face '(chirp-quoted-tweet-face
                                    chirp-quoted-tweet-block-face)))
        (insert "\n")
        (chirp-render--apply-wrap-prefix
         line-start (point) quoted-prefix quoted-prefix-face))
      (when-let* ((text (chirp-tweet-preview-text quoted 140)))
        (unless (string-empty-p text)
          (chirp-render--insert-filled-text text quoted-prefix quoted-prefix-face)))
      (chirp-render--insert-translation
       quoted quoted-prefix quoted-prefix-face)
      (chirp-render--insert-article-preview quoted nil quoted-prefix quoted-prefix-face)
      (chirp-render--insert-article-media-preview quoted nil quoted-prefix quoted-prefix-face)
      (chirp-render-insert-media-strip (plist-get quoted :media)
                                       quoted-prefix
                                       quoted-prefix-face)
      (chirp-render--insert-link-cards quoted quoted-prefix quoted-prefix-face)
      (add-face-text-property start (point) 'chirp-quoted-tweet-block-face 'append)
      (put-text-property start (point) 'chirp-subentry-item quoted))))

(defun chirp-render--insert-list-reply-context (tweet reply-parent &optional prefix prefix-face)
  "Insert a lightweight reply context line for TWEET above REPLY-PARENT.

Precede the line with PREFIX using PREFIX-FACE when provided."
  (let ((parent-id (plist-get reply-parent :id))
        (handle (or (plist-get tweet :reply-to-handle)
                    (plist-get reply-parent :author-handle))))
    (when parent-id
      (chirp-render--insert-prefix prefix prefix-face)
      (let ((start (point)))
        (if handle
            (progn
              (insert (propertize "↳ replying to "
                                  'face 'chirp-thread-reply-context-face))
              (insert (propertize (format "@%s" handle)
                                  'face 'chirp-handle-face))
              (insert (propertize " above"
                                  'face 'chirp-thread-reply-context-face)))
          (insert (propertize "↳ reply to above"
                              'face 'chirp-thread-reply-context-face)))
        (insert "\n")
        (chirp-render--apply-wrap-prefix start (point) prefix prefix-face)
        (add-text-properties
         start (max start (1- (point)))
         `(chirp-reply-parent-id ,parent-id
                                 pointer hand))))))

(defun chirp-render--list-reply-parent (tweet chain)
  "Find the parent of TWEET in the adjacent reply CHAIN, newest first."
  (let ((id (plist-get tweet :reply-to-id))
        (handle (plist-get tweet :reply-to-handle))
        (conversation (plist-get tweet :conversation-id)))
    (seq-find
     (lambda (candidate)
       (if id
           (equal id (plist-get candidate :id))
         (and handle conversation
              (string-equal-ignore-case handle (or (plist-get candidate :author-handle) ""))
              (or (equal conversation (plist-get candidate :id))
                  (equal conversation (plist-get candidate :conversation-id))))))
     chain)))

(defun chirp-render--insert-avatar (url &optional handle)
  "Insert an avatar for URL when possible.

Associate the avatar with HANDLE when provided."
  (when chirp-show-avatars
    (let ((start (point)))
      (if-let* ((image (chirp-media-avatar-image url)))
          (progn
            (insert-image image " ")
            (insert " "))
        (insert "  "))
      (chirp-render--mark-author-region start (point) handle))))

(defun chirp-render--rendered-thumbnail-row-metrics
    (text minimum-height window)
  "Return rendered metrics for TEXT using MINIMUM-HEIGHT in WINDOW, or nil."
  (when (and window
             (display-graphic-p (window-frame window)))
    (let ((remapping face-remapping-alist))
      (with-temp-buffer
        (setq-local face-remapping-alist remapping
                    line-spacing 0
                    truncate-lines t)
        (insert text (propertize "x" 'face 'default))
        (when-let* ((rendered-height
                     (ignore-errors
                       (cdr (buffer-text-pixel-size
                             (current-buffer) window t))))
                    ((numberp rendered-height))
                    ((> rendered-height 0)))
          (insert (propertize
                   " " 'display
                   `(space :height (,rendered-height) :ascent 100)))
          (when-let* ((probe-height
                       (ignore-errors
                         (cdr (buffer-text-pixel-size
                               (current-buffer) window t))))
                      ((numberp probe-height))
                      ((> probe-height 0)))
            (let* ((content-ascent (- (* 2 rendered-height) probe-height))
                   (height (max minimum-height rendered-height))
                   (ascent (+ content-ascent
                              (/ (- height rendered-height) 2)))
                   (ascent-percent
                    (max 0 (min 100 (ceiling
                                     (* 100 (/ (float ascent) height)))))))
              (cons height ascent-percent))))))))

(defun chirp-render--thumbnail-row-metrics (&optional prefix prefix-face)
  "Return metrics for one thumbnail row using PREFIX and PREFIX-FACE.

The result is (HEIGHT . ASCENT).  HEIGHT is in pixels and ASCENT is an image
ascent percentage, or nil when final-layout measurement is unavailable."
  (let* ((window (get-buffer-window (current-buffer) t))
         (frame (if window (window-frame window) (selected-frame)))
         (minimum-height
          (max 1 (ceiling
                  (or (and window
                           (ignore-errors
                             (window-font-height window 'default)))
                      (frame-char-height frame)))))
         (text (or (chirp-render--prefix-string prefix prefix-face) "")))
    (or (chirp-render--rendered-thumbnail-row-metrics
         text minimum-height window)
        (cons minimum-height nil))))

(defun chirp-render--thumbnail-slices (image row-metrics)
  "Return (SLICES . WIDTH) for IMAGE using ROW-METRICS.

SLICES use one-to-one integer pixel coordinates.  WIDTH is the resulting image
width in pixels.  IMAGE is copied and never mutated.  Geometry errors are not
caught or converted to fallback values."
  (pcase-let* ((`(,display-width . ,display-height) (image-size image t))
               (display-width (max 1 (ceiling display-width)))
               (display-height (max 1 (ceiling display-height)))
               (row-height (car row-metrics))
               (row-count
                (max 1 (ceiling (/ display-height (float row-height)))))
               (source-height (* row-count row-height))
               (target-width
                (max 1 (round (* display-width
                                 (/ (float source-height) display-height)))))
               (properties (copy-sequence (cdr image))))
    (setq properties (plist-put properties :width target-width)
          properties (plist-put properties :height source-height)
          properties (plist-put properties :scale 1.0))
    (when (cdr row-metrics)
      (setq properties (plist-put properties :ascent (cdr row-metrics))))
    (let ((prepared (cons 'image properties)))
      (cons
       (cl-loop for row below row-count
                collect
                (propertize
                 " "
                 'display `((slice 0 ,(* row row-height) 1.0 ,row-height)
                            ,prepared)
                 'line-height t
                 'rear-nonsticky '(display)))
       target-width))))

(defun chirp-render--media-placeholder-text (media &optional compactp)
  "Return a text placeholder for MEDIA.

When COMPACTP is non-nil, omit alt text and make a missing video actionable."
  (let* ((alt (chirp-first-nonblank (plist-get media :alt)))
         (kind (pcase (plist-get media :type)
                 ("video"
                  (format "video%s"
                          (if-let* ((width (plist-get media :width))
                                    (height (plist-get media :height)))
                              (format " %sx%s" width height)
                            (if compactp " open" ""))))
                 ("animated_gif" "gif")
                 (_ "image"))))
    (if (and (not compactp) alt)
        (format "[%s: %s]" kind (chirp-render--truncate-link-card-text alt 220))
      (format "[%s]" kind))))

(defun chirp-render--mark-media-region (start end media media-list index)
  "Mark START..END as MEDIA at INDEX in MEDIA-LIST."
  (add-text-properties
   start end
   `(chirp-media-item ,media
                      chirp-media-index ,index
                      chirp-media-list ,media-list
                      pointer hand
                      help-echo "RET: open media  D: download  o: browser")))

(defun chirp-render--media-grid-cell (media index row-metrics)
  "Return sliced grid data for MEDIA at INDEX using ROW-METRICS."
  (if-let* ((image (or (chirp-media-thumbnail-image media)
                       (chirp-media-thumbnail-placeholder-image media))))
      (pcase-let ((`(,rows . ,width)
                   (chirp-render--thumbnail-slices image row-metrics)))
        (list :media media
              :index index
              :width width
              :rows rows
              :padding (propertize
                        " " 'display `(space :width (,width)))))
    (let ((placeholder (chirp-render--media-placeholder-text media t)))
      (list :media media
            :index index
            :width (* (max 1 (string-width placeholder))
                      (max 1 (frame-char-width)))
            :rows (list (propertize placeholder
                                    'face 'chirp-media-placeholder-face))
            :padding (make-string (max 1 (string-width placeholder)) ?\s)))))

(defun chirp-render--media-grid-max-width (&optional prefix)
  "Return available pixel width for one media-grid band with PREFIX, or nil."
  (when-let* ((window (or (get-buffer-window (current-buffer) t)
                          (selected-window)))
              ((display-graphic-p (window-frame window)))
              (body-width (window-body-width window t))
              ((> body-width 0)))
    (let* ((char-width (max 1 (or (ignore-errors
                                    (window-font-width window 'default))
                                  (frame-char-width (window-frame window)))))
           (prefix-width (* (string-width (or prefix "")) char-width)))
      (max 1 (- body-width prefix-width char-width)))))

(defconst chirp-render--media-grid-gap 6
  "Horizontal pixel gap between adjacent media thumbnails in one row.")

(defconst chirp-render--media-overflow-card-width 48
  "Pixel width of the `+N' overflow badge card at the end of a single-row strip.")

(defun chirp-render--media-row-fits-p (cells max-width)
  "Return non-nil when all CELLS fit on one row within MAX-WIDTH pixels."
  (or (null cells)
      (not (and (numberp max-width) (> max-width 0)))
      (let ((total 0))
        (cl-loop for cell in cells
                 for index from 0
                 do (setq total (+ total
                                   (if (> index 0) chirp-render--media-grid-gap 0)
                                   (max 1 (or (plist-get cell :width) 1)))))
        (<= total max-width))))

(defun chirp-render--fit-media-single-row (cells max-width)
  "Fit CELLS onto one horizontal row within MAX-WIDTH pixels.

Return `(VISIBLE . HIDDEN-COUNT)' where VISIBLE always keeps at least the first
cell and reserves space for an overflow `+N' card whenever items are omitted."
  (if (chirp-render--media-row-fits-p cells max-width)
      (cons cells 0)
    (let* ((budget (max 1 (- max-width
                             chirp-render--media-grid-gap
                             chirp-render--media-overflow-card-width)))
           (remaining cells)
           visible
           (used 0)
           (keep-going t))
      (while (and remaining keep-going)
        (let* ((cell (car remaining))
               (width (max 1 (or (plist-get cell :width) 1)))
               (gap (if visible chirp-render--media-grid-gap 0)))
          (if (or (null visible)
                  (<= (+ used gap width) budget))
              (progn
                (push cell visible)
                (setq used (+ used gap width)
                      remaining (cdr remaining)))
            (setq keep-going nil))))
      (setq visible (nreverse visible))
      (cons visible (length remaining)))))

(defun chirp-render--media-overflow-cell (media index hidden-count row-count row-metrics)
  "Return a `+HIDDEN-COUNT' overflow cell targeting MEDIA at INDEX.

ROW-COUNT and ROW-METRICS align the badge card height with the visible strip."
  (let* ((target-height (* (max 1 row-count) (car row-metrics)))
         (badge (chirp-media-overflow-badge-image
                 hidden-count
                 chirp-render--media-overflow-card-width
                 target-height)))
    (if badge
        (pcase-let ((`(,rows . ,width)
                     (chirp-render--thumbnail-slices badge row-metrics)))
          (list :media media
                :index index
                :width width
                :rows rows
                :padding (propertize " " 'display `(space :width (,width)))
                :overflow-count hidden-count))
      (let ((label (format "+%d" hidden-count)))
        (list :media media
              :index index
              :width chirp-render--media-overflow-card-width
              :rows (list (propertize label 'face 'chirp-media-placeholder-face))
              :padding (make-string (length label) ?\s)
              :overflow-count hidden-count)))))

(defun chirp-render--insert-media-grid
    (media-list &optional prefix prefix-face)
  "Insert one horizontal row of sliced media thumbnails for MEDIA-LIST.

Precede each slice row with PREFIX using PREFIX-FACE when provided.  When more
items exist than fit on a single row, append a clickable `+N' badge card;
opening any item lets the user cycle through the full MEDIA-LIST."
  (pcase-let* ((row-metrics
                (chirp-render--thumbnail-row-metrics prefix prefix-face))
               (cells
                (cl-loop for media in media-list
                         for index from 0
                         collect (chirp-render--media-grid-cell
                                  media index row-metrics)))
               (`(,visible . ,hidden-count)
                (chirp-render--fit-media-single-row
                 cells (chirp-render--media-grid-max-width prefix)))
               (row-count (apply #'max
                                 (mapcar (lambda (cell)
                                           (length (plist-get cell :rows)))
                                         visible)))
               (band (if (> hidden-count 0)
                         (let ((first-hidden (length visible)))
                           (append visible
                                   (list (chirp-render--media-overflow-cell
                                          (nth first-hidden media-list)
                                          first-hidden
                                          hidden-count
                                          row-count
                                          row-metrics))))
                       visible))
               (final-row-count (apply #'max
                                       (mapcar (lambda (cell)
                                                 (length (plist-get cell :rows)))
                                               band)))
               (gap-spacer (propertize
                            " "
                            'display `(space :width (,chirp-render--media-grid-gap))
                            'line-height t)))
    (dotimes (row final-row-count)
      (chirp-render--insert-prefix prefix prefix-face)
      (cl-loop for cell in band
               for cell-index from 0
               do
               (when (> cell-index 0)
                 (insert gap-spacer))
               (let ((start (point)))
                 (insert (or (nth row (plist-get cell :rows))
                             (plist-get cell :padding)))
                 (chirp-render--mark-media-region
                  start (point)
                  (plist-get cell :media) media-list (plist-get cell :index))))
      (when (< (1+ row) final-row-count)
        (insert (propertize "\n" 'line-height t))))))

(defun chirp-render--insert-media-text-cell (media media-list index &optional prefix prefix-face)
  "Insert one compact text entry for hidden MEDIA.

Associate it with INDEX in MEDIA-LIST.  Precede it with PREFIX using
PREFIX-FACE when provided."
  (chirp-render--insert-prefix prefix prefix-face)
  (let ((start (point)))
    (insert (propertize (chirp-render--media-placeholder-text media)
                        'face 'chirp-media-placeholder-face))
    (chirp-render--mark-media-region start (point) media media-list index)))

(defun chirp-render-insert-media-strip (media-list &optional prefix prefix-face)
  "Insert a grid of thumbnails for MEDIA-LIST.

Precede each row with PREFIX using PREFIX-FACE when provided."
  (when media-list
    (if (not chirp-show-tweet-media)
        (cl-loop for media in media-list
                 for index from 0
                 do (unless (zerop index)
                      (insert "\n"))
                 do (chirp-render--insert-media-text-cell media media-list index prefix prefix-face))
      (chirp-render--insert-media-grid media-list prefix prefix-face))
    (insert "\n\n")))

(defun chirp-render--insert-mention-handle (handle)
  "Insert clickable @HANDLE with profile navigation properties."
  (let ((tag (format "@%s" (string-remove-prefix "@" handle))))
    (insert (propertize tag
                        'face 'chirp-handle-face
                        'chirp-mention tag
                        'mouse-face 'highlight
                        'pointer 'hand
                        'help-echo "Open profile"))))

(defun chirp-render--insert-tweet
    (tweet &optional prefix prefix-face show-reply-context article-mode reply-parent)
  "Insert TWEET at point, optionally prefixed for thread rendering.

Use PREFIX and PREFIX-FACE for indentation.  When SHOW-REPLY-CONTEXT is
non-nil, show the reply target.  ARTICLE-MODE controls full article rendering,
and REPLY-PARENT supplies the preceding parent tweet when available."
  (let* ((start (point))
         (author (or (plist-get tweet :author-name) "Unknown"))
         (handle (plist-get tweet :author-handle))
         (retweeted-by (plist-get tweet :retweeted-by))
         (created-at (plist-get tweet :created-at))
         (article-mode
          (if (or (eq article-mode 'full)
                  (chirp--tweet-expanded-p tweet))
              'full
            article-mode))
         (meta-start nil)
         (action-regions nil))
    (when reply-parent
      (chirp-render--insert-list-reply-context tweet reply-parent prefix prefix-face))
    (when (and show-reply-context
               (eq (plist-get tweet :timeline-context) 'related))
      (chirp-render--insert-prefix prefix prefix-face)
      (insert (propertize "Related tweet"
                          'face 'chirp-thread-related-context))
      (insert "\n"))
    (when retweeted-by
      (chirp-render--insert-prefix prefix prefix-face)
      (insert (propertize "retweeted by " 'face 'chirp-social-context-face))
      (chirp-render--insert-mention-handle retweeted-by)
      (insert "\n"))
    (chirp-render--insert-prefix prefix prefix-face)
    (chirp-render--insert-avatar (plist-get tweet :author-avatar-url) handle)
    (let ((author-start (point)))
      (insert (propertize author 'face 'chirp-author-face))
      (when handle
        (insert " ")
        (insert (propertize (format "@%s" handle) 'face 'chirp-handle-face)))
      (chirp-render--mark-author-region author-start (point) handle))
    (when created-at
      (insert "  ")
      (insert (chirp-render-timestamp created-at)))
    (insert "\n")
    (when (and show-reply-context
               (plist-get tweet :reply-to-handle))
      (chirp-render--insert-prefix prefix prefix-face)
      (insert (propertize "replying to "
                          'face 'chirp-thread-reply-context-face))
      (chirp-render--insert-mention-handle (plist-get tweet :reply-to-handle))
      (insert "\n"))
    (when-let* ((text (plist-get tweet :text)))
      (unless (string-empty-p text)
        (chirp-render--insert-filled-text text prefix prefix-face)))
    (chirp-render--insert-translation tweet prefix prefix-face)
    (chirp-render--insert-quoted-tweet tweet prefix prefix-face)
    (pcase article-mode
      ('full
       (chirp-render--insert-article-body tweet prefix prefix-face))
      (_
       (chirp-render--insert-article-preview tweet article-mode prefix prefix-face)
       (chirp-render--insert-article-media-preview tweet article-mode prefix prefix-face)
       (when (chirp-render--article-expandable-p tweet article-mode)
         (chirp-render--insert-show-more tweet prefix prefix-face))))
    (chirp-render--insert-link-cards tweet prefix prefix-face)
    (chirp-render--insert-expanded-urls (plist-get tweet :urls) prefix prefix-face)
    (chirp-render-insert-media-strip (plist-get tweet :media)
                                     prefix
                                     prefix-face)
    (setq meta-start (point))
    (chirp-render--insert-prefix prefix prefix-face)
    (push (cons 'reply
                (chirp-render--insert-metric
                 'reply (plist-get tweet :reply-count)))
          action-regions)
    (insert "   ")
    (push (cons 'retweet
                (chirp-render--insert-state-metric 'retweet tweet :retweeted-p :retweet-count))
          action-regions)
    (insert "   ")
    (push (cons 'like
                (chirp-render--insert-state-metric 'like tweet :liked-p :like-count))
          action-regions)
    (insert "   ")
    (push (cons 'quote (chirp-render--insert-metric 'quote (plist-get tweet :quote-count)))
          action-regions)
    (insert "   ")
    (push (cons 'bookmark
                (chirp-render--insert-state-metric 'bookmark tweet :bookmarked-p :bookmark-count))
          action-regions)
    (insert "   ")
    (chirp-render--insert-metric 'view (plist-get tweet :view-count))
    (insert "\n")
    (insert "\n")
    (chirp-render-mark-entry start (point) tweet)
    (dolist (action-region action-regions)
      (pcase-let ((`(,action ,region-start . ,region-end) action-region))
        (chirp-render--mark-tweet-action-region
         region-start region-end action)))
    (put-text-property meta-start (point) 'rear-nonsticky t)))

(defun chirp-render-insert-tweet (tweet)
  "Insert TWEET at point."
  (chirp-render--insert-tweet tweet))

(defun chirp-render--tweet-separator-line ()
  "Return the tweet separator line, or nil when disabled."
  (when (and (stringp chirp-tweet-separator)
             (not (string-empty-p chirp-tweet-separator)))
    (concat (make-string (max 0 chirp-tweet-separator-indent) ?\s)
            chirp-tweet-separator)))

(defun chirp-render-insert-tweet-separator ()
  "Insert the configured separator between tweet list entries."
  (when-let* ((line (chirp-render--tweet-separator-line)))
    ;; Shrink the blank lines above and below the separator to 1/4 line height.
    (when (and (> (point) (1+ (point-min)))
               (eq (char-before (point)) ?\n)
               (eq (char-before (1- (point))) ?\n))
      (put-text-property (1- (point)) (point) 'display '((height 0.25))))
    (insert (propertize line 'face 'chirp-tweet-separator-face))
    (insert "\n")
    (insert (propertize "\n" 'display '((height 0.25))))))

(defvar-local chirp-render--reply-chain nil
  "Adjacent reply chain at the end of the last rendered tweet list.")

(defun chirp-render-insert-tweet-list (tweets &optional previous)
  "Insert TWEETS, continuing the adjacent reply chain ending at PREVIOUS."
  (unless (and previous (eq previous (car chirp-render--reply-chain)))
    (setq chirp-render--reply-chain (and previous (list previous))))
  (dolist (tweet tweets)
    (if-let* ((parent (chirp-render--list-reply-parent tweet chirp-render--reply-chain)))
        (progn
          (chirp-render--insert-tweet tweet chirp-render-list-reply-prefix nil nil nil parent)
          (push tweet chirp-render--reply-chain))
      (when chirp-render--reply-chain (chirp-render-insert-tweet-separator))
      (chirp-render-insert-tweet tweet)
      (setq chirp-render--reply-chain (list tweet)))))

(defun chirp-render-insert-thread-focus-tweet (tweet)
  "Insert the focus TWEET in a thread view."
  (chirp-render--insert-tweet tweet nil nil nil 'full))

(defun chirp-render-insert-thread-reply (tweet &optional filtered-rule)
  "Insert a reply TWEET in a thread view.

When FILTERED-RULE is non-nil, prepend a muted badge showing the matched rule."
  (when (and (stringp filtered-rule) (not (string-empty-p filtered-rule)))
    (insert (propertize (format "[filtered: %s]\n" filtered-rule)
                        'face 'chirp-thread-reply-context-face)))
  (chirp-render--insert-tweet tweet nil nil t))

(defun chirp-render-insert-thread-divider (count hidden &optional show-hidden-p)
  "Insert a conversation heading for COUNT displayed posts and HIDDEN replies.

When SHOW-HIDDEN-P is non-nil, indicate that filtered replies are currently
expanded."
  (let* ((hidden-part
          (cond
           ((<= hidden 0) "")
           (show-hidden-p (format " · %d filtered shown (RET to hide)" hidden))
           (t (format " · %d hidden (RET to show)" hidden))))
         (label (format "── Conversation · %d shown%s ──" count hidden-part))
         (props (append (list 'face 'chirp-thread-divider-face
                              'chirp-replies-heading t)
                        (when (> hidden 0)
                          (list 'pointer 'hand
                                'mouse-face 'highlight
                                'help-echo
                                (if show-hidden-p
                                    "RET or click: hide filtered replies"
                                  "RET or click: show filtered replies"))))))
    (insert (apply #'propertize label props))
    (insert "\n\n")))

(defun chirp-render-insert-user-summary (user)
  "Insert USER summary."
  (let ((start (point)))
    (chirp-render--insert-avatar (plist-get user :avatar-url)
                                 (plist-get user :handle))
    (let ((author-start (point)))
      (insert (propertize (or (plist-get user :name) "Unknown") 'face 'chirp-author-face))
      (when-let* ((handle (plist-get user :handle)))
        (insert " " (propertize (format "@%s" handle) 'face 'chirp-handle-face))
        (chirp-render--mark-author-region author-start (point) handle)))
    (insert "\n")
    (when-let* ((bio (plist-get user :bio)))
      (unless (string-empty-p bio)
        (chirp-render--insert-filled-text bio)))
    (when-let* ((action-label (chirp-render--profile-follow-action-label user))
                (handle (plist-get user :handle)))
      (let ((action-start (point)))
        (insert (propertize action-label 'face 'chirp-profile-action-face))
        (chirp-render--mark-profile-action-region
         action-start (point) 'toggle-follow handle))
      (when (and (plist-get user :viewer-following-p)
                 (plist-get user :viewer-followed-by-p))
        (insert (propertize "  Mutuals" 'face 'chirp-profile-action-secondary-face)))
      (insert "\n"))
    (insert (propertize (format "Posts %s" (chirp-format-count (plist-get user :posts)))
                        'face 'chirp-meta-face))
    (insert (propertize "   " 'face 'chirp-meta-face))
    (let ((following-start (point)))
      (insert (propertize
               (format "Following %s" (chirp-format-count (plist-get user :following)))
               'face 'chirp-meta-face))
      (chirp-render--mark-profile-list-region
       following-start (point) 'following (plist-get user :handle)))
    (insert (propertize "   " 'face 'chirp-meta-face))
    (let ((followers-start (point)))
      (insert (propertize
               (format "Followers %s" (chirp-format-count (plist-get user :followers)))
               'face 'chirp-meta-face))
      (chirp-render--mark-profile-list-region
       followers-start (point) 'followers (plist-get user :handle)))
    (insert "\n")
    (when-let* ((joined (plist-get user :joined)))
      (insert (propertize (format "Joined %s" (condition-case nil
                                              (format-time-string "%b %Y" (date-to-time joined))
                                            (error joined))) 'face 'chirp-meta-face))
      (insert "\n"))
    (when-let* ((url (plist-get user :profile-url)))
      (let ((start (point)))
        (insert (propertize url 'face 'link))
        (chirp-render--mark-url-region start (point) url))
      (insert "\n"))
    (insert "\n")
    (chirp-render-mark-entry start (point) user)))

(provide 'chirp-render)

;;; chirp-render.el ends here
