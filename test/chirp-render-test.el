;;; chirp-render-test.el --- Tests for Chirp rendering helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-render)
(require 'chirp-thread)

(defun chirp-test--face-member-p (face value)
  "Return non-nil when FACE appears in text property VALUE."
  (cond
   ((eq value face) t)
   ((listp value)
    (or (memq face value)
        (cl-some (lambda (item)
                   (chirp-test--face-member-p face item))
                 value)))
   (t nil)))

(defun chirp-test--sample-article-tweet ()
  "Return a normalized tweet payload with article metadata."
  (chirp-normalize-tweet
   '(("id" . "123")
     ("text" . "Read this https://t.co/demo")
     ("urls" . ("https://example.com/article"))
     ("articleTitle" . "Longform title")
     ("articleText" . "First paragraph with [details](https://example.com/article).\n\nSecond paragraph.")
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice"))))))

(defun chirp-test--sample-article-tweet-with-image ()
  "Return a normalized article tweet that includes one inline image."
  (chirp-normalize-tweet
   '(("id" . "124")
     ("text" . "Longform https://t.co/demo")
     ("urls" . ("https://example.com/article"))
     ("articleTitle" . "Longform title")
     ("articleText" . "First paragraph.\n\n![Cover](https://example.com/cover.jpg)\n\nSecond paragraph.")
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice"))))))

(defun chirp-test--sample-quoted-tweet ()
  "Return a normalized tweet payload with a quoted tweet."
  (chirp-normalize-tweet
   '(("id" . "999")
     ("text" . "Commentary https://t.co/quoted")
     ("urls" . ("https://x.com/bob/status/456"))
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice")))
     ("quotedTweet" . (("id" . "456")
                       ("text" . "Quoted body text that is intentionally long enough to be shown as a short preview instead of the entire post verbatim.")
                       ("author" . (("screenName" . "bob")
                                    ("name" . "Bob"))))))))

(defun chirp-test--sample-quoted-tweet-with-media ()
  "Return a normalized tweet payload whose quoted tweet has media."
  (chirp-normalize-tweet
   '(("id" . "998")
     ("text" . "Commentary https://t.co/quoted")
     ("urls" . ("https://x.com/bob/status/456"))
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice")))
     ("quotedTweet" . (("id" . "456")
                       ("text" . "")
                       ("author" . (("screenName" . "bob")
                                    ("name" . "Bob")))
                       ("media" . ((("type" . "photo")
                                    ("url" . "https://example.com/quoted.jpg")))))))))

(defun chirp-test--sample-retweeted-tweet ()
  "Return a normalized tweet payload with retweet social context."
  (chirp-normalize-tweet
   '(("id" . "321")
     ("text" . "Boosted post")
     ("retweetedBy" . "dotey")
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice"))))))

(defun chirp-test--sample-note-tweet-with-entity-links ()
  "Return a normalized note tweet whose expanded URLs live in entity metadata."
  (chirp-normalize-tweet
   '(("id" . "777")
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice")))
     ("note_tweet" . (("note_tweet_results" . (("result" . (("text" . "GitHub仓库 https://t.co/repo\n在线阅读 https://t.co/read")
                                                            ("entity_set" . (("urls" . ((("expanded_url" . "https://github.com/example/project"))
                                                                                        (("expanded_url" . "https://example.com/read")))))))))))))))

(defun chirp-test--sample-tweet-with-incomplete-expanded-urls ()
  "Return a normalized tweet whose short links outnumber expanded URLs."
  (chirp-normalize-tweet
   '(("id" . "778")
     ("text" . "GitHub仓库 https://t.co/repo 在线阅读 https://t.co/read")
     ("urls" . ("https://github.com/example/project"))
     ("author" . (("screenName" . "alice")
                  ("name" . "Alice"))))))

(ert-deftest chirp-normalize-tweet-strips-short-urls-and-keeps-article-fields ()
  "Short links should be removed from display text while article data survives."
  (let ((tweet (chirp-test--sample-article-tweet)))
    (should (equal (plist-get tweet :text) "Read this"))
    (should (equal (plist-get tweet :raw-text) "Read this https://t.co/demo"))
    (should (equal (plist-get tweet :urls) '("https://example.com/article")))
    (should (equal (plist-get tweet :article-title) "Longform title"))
    (should (equal (chirp-tweet-article-preview tweet 80)
                   "First paragraph with details."))))

(ert-deftest chirp-article-segments-split-inline-images-out-of-body-text ()
  "Article helpers should split Markdown image paragraphs into media items."
  (let* ((tweet (chirp-test--sample-article-tweet-with-image))
         (segments (chirp-article-segments (plist-get tweet :article-text)))
         (images (chirp-tweet-article-images tweet)))
    (should (= (length segments) 3))
    (should (equal (mapcar (lambda (segment) (plist-get segment :type)) segments)
                   '(text image text)))
    (should (equal (plist-get (car images) :url)
                   "https://example.com/cover.jpg"))
    (should (equal (chirp-tweet-article-display-text tweet)
                   "First paragraph.\n\nSecond paragraph."))))

(ert-deftest chirp-normalize-tweet-keeps-quoted-tweet-and-filters-quote-link ()
  "Quoted tweets should survive normalization without duplicate permalinks."
  (let* ((tweet (chirp-test--sample-quoted-tweet))
         (quoted (plist-get tweet :quoted-tweet)))
    (should quoted)
    (should (equal (plist-get quoted :id) "456"))
    (should (equal (plist-get tweet :text) "Commentary"))
    (should (string-match-p "Quoted body text" (plist-get quoted :text)))
    (should-not (plist-get tweet :urls))))

(ert-deftest chirp-normalize-tweet-extracts-multiple-note-tweet-links ()
  "Expanded URLs should survive even when they only appear in note-tweet entities."
  (let ((tweet (chirp-test--sample-note-tweet-with-entity-links)))
    (should (equal (plist-get tweet :text) "GitHub仓库\n在线阅读"))
    (should (equal (plist-get tweet :urls)
                   '("https://github.com/example/project"
                     "https://example.com/read")))))

(ert-deftest chirp-normalize-tweet-keeps-short-urls-when-expanded-links-are-incomplete ()
  "Display text should keep `t.co` placeholders when expansion coverage is incomplete."
  (let ((tweet (chirp-test--sample-tweet-with-incomplete-expanded-urls)))
    (should (equal (plist-get tweet :text)
                   "GitHub仓库 https://t.co/repo 在线阅读 https://t.co/read"))
    (should (equal (plist-get tweet :urls)
                   '("https://github.com/example/project")))))

(ert-deftest chirp-normalize-tweet-preserves-retweeted-by-handle ()
  "Structured tweets should preserve retweet social context handles."
  (let ((tweet (chirp-test--sample-retweeted-tweet)))
    (should (equal (plist-get tweet :retweeted-by) "dotey"))))

(ert-deftest chirp-render-insert-tweet-renders-expanded-links-and-article-preview ()
  "Tweet rendering should show expanded links and article metadata."
  (let ((tweet (chirp-test--sample-article-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "Longform title" rendered))
        (should (string-match-p "First paragraph with details\\." rendered))
        (should (string-match-p "https://example.com/article" rendered))
        (should-not (string-match-p "https://t\\.co/demo" rendered))))))

(ert-deftest chirp-render-insert-tweet-renders-multiple-note-tweet-links ()
  "Tweet rendering should show multiple expanded links extracted from note entities."
  (let ((tweet (chirp-test--sample-note-tweet-with-entity-links)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "GitHub仓库" rendered))
        (should (string-match-p "在线阅读" rendered))
        (should (string-match-p "https://github.com/example/project" rendered))
        (should (string-match-p "https://example.com/read" rendered))
        (should-not (string-match-p "https://t\\.co/repo" rendered))
        (should-not (string-match-p "https://t\\.co/read" rendered))))))

(ert-deftest chirp-render-insert-tweet-keeps-short-urls-when-expanded-links-are-incomplete ()
  "Rendering should prefer visible short links over silently swallowing them."
  (let ((tweet (chirp-test--sample-tweet-with-incomplete-expanded-urls)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "https://t\\.co/repo" rendered))
        (should (string-match-p "https://t\\.co/read" rendered))
        (should (string-match-p "https://github.com/example/project" rendered))))))

(ert-deftest chirp-render-insert-tweet-renders-retweet-social-context ()
  "Tweet rendering should show who retweeted the current post."
  (let ((tweet (chirp-test--sample-retweeted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (should (search-forward "retweeted by @dotey" nil t))
      (should (chirp-test--face-member-p
               'chirp-social-context-face
               (get-text-property (match-beginning 0) 'face))))))

(ert-deftest chirp-render-insert-thread-focus-tweet-renders-full-article-body ()
  "Thread focus rendering should include the full article text."
  (let ((tweet (chirp-test--sample-article-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-thread-focus-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "Longform title" rendered))
        (should (string-match-p "First paragraph with \\[details\\]" rendered))
        (should (string-match-p "Second paragraph\\." rendered))))))

(ert-deftest chirp-render-insert-thread-focus-tweet-renders-article-images ()
  "Thread focus rendering should show inline article images instead of raw Markdown."
  (let ((tweet (chirp-test--sample-article-tweet-with-image)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-thread-focus-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "First paragraph\\." rendered))
        (should (string-match-p "Second paragraph\\." rendered))
        (should (string-match-p "\\[image\\]" rendered))
        (should-not (string-match-p "!\\[Cover\\]" rendered))))))

(ert-deftest chirp-render-insert-tweet-renders-link-card-preview ()
  "Tweet rendering should include cached external link-card previews."
  (let ((tweet
         (chirp-normalize-tweet
          '(("id" . "125")
            ("text" . "Repo https://t.co/repo")
            ("urls" . ("https://github.com/example/project"))
            ("author" . (("screenName" . "alice")
                         ("name" . "Alice")))))))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-link-cards-for-tweet)
                 (lambda (_tweet)
                   (list '(:url "https://github.com/example/project"
                           :title "microsoft/RD-Agent"
                           :description "Research and development agent"
                           :image-url "https://opengraph.githubassets.com/demo")))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "microsoft/RD-Agent" rendered))
        (should (string-match-p "Research and development agent" rendered))
        (should (string-match-p "https://github.com/example/project" rendered))))))

(ert-deftest chirp-render-metric-string-uses-action-specific-active-faces ()
  "Liked, bookmarked, and retweeted metrics should use distinct active faces."
  (should (eq (get-text-property 0 'face
                                 (chirp-render--metric-string 'like 12 t))
              'chirp-liked-metric-face))
  (should (eq (get-text-property 0 'face
                                 (chirp-render--metric-string 'bookmark 3 t))
              'chirp-bookmarked-metric-face))
  (should (eq (get-text-property 0 'face
                                 (chirp-render--metric-string 'retweet 5 t))
              'chirp-retweeted-metric-face))
  (should (eq (get-text-property 0 'face
                                 (chirp-render--metric-string 'reply 1 nil))
              'chirp-meta-face)))

(ert-deftest chirp-render-insert-tweet-renders-quoted-tweet-preview ()
  "Tweet rendering should show quoted tweet text instead of just its link."
  (let ((tweet (chirp-test--sample-quoted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((rendered (buffer-string)))
        (should (string-match-p "Quoted @bob (Bob)" rendered))
        (should (string-match-p "Quoted body text" rendered))
        (should-not (string-match-p "https://x\\.com/bob/status/456" rendered)))
      (goto-char (point-min))
      (search-forward "Quoted @bob (Bob)")
      (should (equal (plist-get (chirp-entry-at-point) :id) "456")))))

(ert-deftest chirp-render-insert-tweet-highlights-quoted-tweet-block ()
  "Quoted tweet previews should carry a distinct block face."
  (let ((tweet (chirp-test--sample-quoted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (search-forward "Quoted @bob (Bob)")
      (should (chirp-test--face-member-p
               'chirp-quoted-tweet-block-face
               (get-text-property (match-beginning 0) 'face)))
      (goto-char (point-min))
      (search-forward "│ Quoted body text")
      (should (chirp-test--face-member-p
               'chirp-quoted-tweet-block-face
               (get-text-property (match-beginning 0) 'face))))))

(ert-deftest chirp-render-quoted-tweet-lines-use-wrap-prefix ()
  "Quoted tweet body lines should keep the quote marker on visual wraps."
  (let ((tweet (chirp-test--sample-quoted-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (goto-char (point-min))
      (search-forward "Quoted body text")
      (let* ((needle "Quoted body text")
             (pos (- (point) (length needle)))
             (wrap-prefix (get-text-property pos 'wrap-prefix)))
        (should (stringp wrap-prefix))
        (should (string-match-p "^│ " wrap-prefix))))))

(ert-deftest chirp-render-quoted-tweet-media-uses-tall-quote-bar-image ()
  "Quoted tweet media should replace the bar glyph with a full-height image bar."
  (let ((tweet (chirp-test--sample-quoted-tweet-with-media))
        (fake-image '(image :type png :file "/tmp/fake.png")))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) fake-image))
                ((symbol-function 'chirp-media-thumbnail-placeholder-image) (lambda (&rest _args) nil))
                ((symbol-function 'image-size)
                 (lambda (_image &optional _pixels _frame)
                   '(128 . 128)))
                ((symbol-function 'chirp-media-quote-bar-image)
                 (lambda (_height)
                   '(image :type svg :data "<svg/>"))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet)))
      (let ((quoted-prefix-lines 0)
            media-pos)
        (dolist (line (split-string (buffer-string) "\n"))
          (when (string-prefix-p "│ " line)
            (setq quoted-prefix-lines (1+ quoted-prefix-lines))))
        (setq media-pos (next-single-property-change
                         (point-min) 'chirp-media-item nil (point-max)))
        (should (= quoted-prefix-lines 2))
        (should media-pos)
        (should (equal (car-safe (get-text-property media-pos 'display)) 'image))))))

(ert-deftest chirp-open-at-point-opens-profile-when-point-is-on-avatar ()
  "RET on an avatar should open the author profile, not the tweet thread."
  (let ((tweet (chirp-test--sample-quoted-tweet))
        opened-profile
        opened-thread)
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-profile-open)
                 (lambda (handle &optional _buffer)
                   (setq opened-profile handle)))
                ((symbol-function 'chirp-thread-open)
                 (lambda (&rest args)
                   (setq opened-thread args))))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet))
        (goto-char (point-min))
        (chirp-open-at-point)))
    (should (equal opened-profile "alice"))
    (should-not opened-thread)))

(ert-deftest chirp-enrich-quoted-tweets-upgrades-preview-and-prefetches-media ()
  "Quoted tweet enrichment should replace the preview and kick media prefetch."
  (let ((chirp-quoted-tweet-cache (make-hash-table :test #'equal))
        (chirp-quoted-tweet-pending (make-hash-table :test #'equal))
        (tweet (chirp-test--sample-quoted-tweet))
        rerendered-buffer
        prefetched-media-url)
    (unwind-protect
        (let ((buffer (generate-new-buffer " *chirp-quote-enrich-test*")))
          (with-current-buffer buffer
            (chirp-view-mode))
          (cl-letf (((symbol-function 'chirp-backend-tweet)
                     (lambda (_tweet-id callback &optional _errback)
                       (funcall
                        callback
                        (chirp-normalize-tweet
                         '(("id" . "456")
                           ("text" . "Quoted body text with image")
                           ("author" . (("screenName" . "bob")
                                        ("name" . "Bob")))
                           ("media" . ((("type" . "photo")
                                        ("url" . "https://example.com/quoted.jpg"))))))
                        nil)))
                    ((symbol-function 'chirp-request-rerender)
                     (lambda (target &optional _delay)
                       (setq rerendered-buffer target)))
                    ((symbol-function 'chirp-media-prefetch-tweet)
                     (lambda (quoted _buffer)
                       (setq prefetched-media-url
                             (plist-get (car (plist-get quoted :media)) :url)))))
            (chirp-enrich-quoted-tweets (list tweet) buffer))
          (let ((quoted (plist-get tweet :quoted-tweet)))
            (should (plist-get quoted :chirp-enriched-p))
            (should (equal rerendered-buffer buffer))
            (should (equal prefetched-media-url "https://example.com/quoted.jpg"))
            (should (equal (plist-get (car (plist-get quoted :media)) :url)
                           "https://example.com/quoted.jpg"))))
      (dolist (name '(" *chirp-quote-enrich-test*"))
        (when-let* ((buffer (get-buffer name)))
          (kill-buffer buffer))))))

(ert-deftest chirp-entry-navigation-jumps-between-top-level-tweets ()
  "Entry navigation should move between top-level tweets from nested regions."
  (let ((tweet-a (chirp-test--sample-quoted-tweet))
        (tweet-b (chirp-test--sample-article-tweet)))
    (with-temp-buffer
      (chirp-view-mode)
      (cl-letf (((symbol-function 'chirp-media-avatar-image) (lambda (&rest _args) nil))
                ((symbol-function 'chirp-media-thumbnail-image) (lambda (&rest _args) nil)))
        (let ((inhibit-read-only t))
          (chirp-render-insert-tweet tweet-a)
          (chirp-render-insert-tweet tweet-b)))
      (goto-char (point-min))
      (search-forward "Quoted @bob (Bob)")
      (chirp-next-entry)
      (should (equal (plist-get (chirp-entry-at-point) :id) "123"))
      (search-forward "First paragraph")
      (chirp-previous-entry)
      (should (equal (plist-get (chirp-entry-at-point) :id) "999")))))

(ert-deftest chirp-thread-article-fetch-needed-for-link-only-or-preview-tweets ()
  "Thread views should enrich article-like tweets when body text is missing."
  (should (chirp-thread--article-fetch-needed-p
           '(:id "123"
             :text ""
             :urls ("https://example.com/article"))))
  (should (chirp-thread--article-fetch-needed-p
           '(:id "123"
             :text "Read this"
             :article-title "Longform title")))
  (should-not (chirp-thread--article-fetch-needed-p
               '(:id "123"
                 :text "Read this"
                 :article-text "Full article body."))))

(provide 'chirp-render-test)

;;; chirp-render-test.el ends here
