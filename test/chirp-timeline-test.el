;;; chirp-timeline-test.el --- Tests for timeline loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-timeline)
(require 'chirp)

(ert-deftest chirp-opens-home-after-web-sign-in ()
  "Fetch Home on sign-in, even if a previous session has an open Home."
  (save-window-excursion
    (let ((chirp-web--ready 'first) (requests 0) complete views)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-x-session) (lambda () chirp-web--ready))
                    ((symbol-function 'chirp-web-login) (lambda (done) (setq complete done)))
                    ((symbol-function 'chirp-backend-feed)
                     (lambda (ok &rest _)
                       (cl-incf requests)
                       (funcall ok nil nil)
                       (push (current-buffer) views))))
            (chirp)
            (setq chirp-web--ready nil)
            (call-interactively #'chirp)
            (should (= requests 1))
            (setq chirp-web--ready 'second)
            (funcall complete)
            (should (= requests 2))
            (should-not (eq (car views) (cadr views)))
            (chirp)
            (should (= requests 2))
            (should (eq (window-buffer) (car views))))
        (mapc #'kill-buffer views)))))

(ert-deftest chirp-buffer-creates-fresh-view-buffers ()
  "Each `chirp-buffer' call should return a fresh buffer."
  (let ((buffer-a (chirp-buffer))
        (buffer-b (chirp-buffer)))
    (unwind-protect
        (progn
          (should (buffer-live-p buffer-a))
          (should (buffer-live-p buffer-b))
          (should-not (eq buffer-a buffer-b))
          (chirp--apply-buffer-name buffer-a "For You")
          (should (string= (buffer-name buffer-a) "*chirp: For You*")))
      (dolist (buffer (list buffer-a buffer-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-quit-current-buffer-keeps-main-timeline-buffers ()
  "Quitting For You/Following should keep the timeline buffer alive."
  (let ((previous (generate-new-buffer "*chirp-prev*"))
        (timeline (generate-new-buffer " *chirp-home*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer previous)
          (switch-to-buffer timeline)
          (with-current-buffer timeline
            (chirp-view-mode)
            (setq-local chirp--timeline-kind 'home))
          (chirp-quit-current-buffer)
          (should (eq (window-buffer (selected-window)) previous))
          (should (buffer-live-p timeline)))
      (dolist (buffer (list previous timeline))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-quit-current-buffer-kills-secondary-chirp-views ()
  "Quitting secondary Chirp views should still kill the current buffer."
  (let ((previous (generate-new-buffer "*chirp-prev*"))
        (detail (generate-new-buffer " *chirp-detail*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer previous)
          (switch-to-buffer detail)
          (with-current-buffer detail
            (chirp-view-mode)
            (setq-local chirp--timeline-kind nil)
            (setq-local chirp--view-title "Thread"))
          (chirp-quit-current-buffer)
          (should (eq (window-buffer (selected-window)) previous))
          (should-not (buffer-live-p detail)))
      (dolist (buffer (list previous detail))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-collect-top-level-tweets-hides-promoted-posts ()
  "Promoted tweets should be dropped when filtering is enabled."
  (let ((chirp-hide-promoted-posts t))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (chirp-collect-top-level-tweets
                            (list '(("id" . "1")
                                    ("text" . "normal")
                                    ("author" . (("screenName" . "alice")
                                                 ("name" . "Alice"))))
                                  '(("id" . "2")
                                    ("text" . "ad")
                                    ("isPromoted" . t)
                                    ("author" . (("screenName" . "brand")
                                                 ("name" . "Brand")))))))
                   '("1")))))

(ert-deftest chirp-collect-top-level-tweets-can-keep-promoted-posts ()
  "Promoted tweets should remain visible when filtering is disabled."
  (let ((chirp-hide-promoted-posts nil))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (chirp-collect-top-level-tweets
                            (list '(("id" . "1")
                                    ("text" . "normal")
                                    ("author" . (("screenName" . "alice")
                                                 ("name" . "Alice"))))
                                  '(("id" . "2")
                                    ("text" . "ad")
                                    ("isPromoted" . t)
                                    ("author" . (("screenName" . "brand")
                                                 ("name" . "Brand")))))))
                   '("1" "2")))))

(ert-deftest chirp-bookmarks-loads-the-next-page ()
  "Continue bookmarks from the last row and append only new entries."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (one '(:id "1" :text "First" :kind tweet))
          (two '(:id "2" :text "Second" :kind tweet))
          calls)
      (cl-letf (((symbol-function 'chirp-backend-bookmarks)
                 (lambda (success _error _limit cursor)
                   (push cursor calls)
                   (if cursor
                       (funcall success (list one two) nil)
                     (funcall success (list one)
                              '(("pagination" . (("nextCursor" . "page2"))))))))
                ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                ((symbol-function 'chirp-display-buffer) #'ignore))
        (chirp-timeline-open-bookmarks buffer)
        (chirp-next-entry)
        (should (equal calls '("page2" nil)))
        (should (equal (mapcar #'chirp-tweet-key
                               (chirp-timeline--buffer-tweets buffer))
                       '("1" "2")))
        (chirp-load-more)
        (should (= (length calls) 2))))))

(ert-deftest chirp-load-more-stops-when-timeline-is-exhausted ()
  "Loading more should short-circuit once the timeline is exhausted."
  (with-temp-buffer
    (chirp-view-mode)
    (setq-local chirp--timeline-kind 'home
                chirp--timeline-load-more-function #'chirp-timeline--load-more)
    (setq-local chirp--timeline-limit 20)
    (setq-local chirp--timeline-exhausted-p t)
    (let ((open-called nil)
          (last-message nil))
      (cl-letf (((symbol-function 'chirp-timeline--open)
                 (lambda (&rest _args)
                   (setq open-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq last-message (apply #'format format-string args)))))
        (chirp-load-more))
      (should-not open-called)
      (should (equal last-message "No older posts.")))))

(ert-deftest chirp-status-appears-in-mode-line ()
  "Persistent Chirp status should stay visible in the mode line."
  (with-temp-buffer
    (chirp-view-mode)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest _args)
                 'chirp-test-timer))
              ((symbol-function 'timerp)
               (lambda (value)
                 (eq value 'chirp-test-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'float-time)
               (lambda (&optional _time)
                 100.0)))
      (chirp-set-status (current-buffer) "Loading thread...")
      (setq-local chirp--status-start-time 97.5)
      (let ((rendered (chirp--mode-line-status-string)))
        (should (string-match-p "Loading thread\\.\\.\\." rendered))
        (should (string-match-p "2\\.5s" rendered)))
      (chirp-clear-status (current-buffer))
      (should-not (chirp--mode-line-status-string)))))

(ert-deftest chirp-refresh-error-preserves-view-and-point ()
  (with-temp-buffer
    (chirp-view-mode)
    (setq-local chirp--view-title "Thread")
    (let ((inhibit-read-only t))
      (insert (propertize "existing post" 'chirp-entry-start t)))
    (goto-char 5)
    (chirp-set-status (current-buffer) "Loading thread")
    (chirp-show-error (current-buffer) "Thread" #'ignore "HTTP 503")
    (should (equal (buffer-string) "existing post"))
    (should (= (point) 5))
    (should-not chirp--status-timer)
    (should (equal (chirp--mode-line-status-string) " · HTTP 503 · g retry"))))

(ert-deftest chirp-timeline-render-clears-persistent-status ()
  "Rendering a timeline should clear any previous loading status."
  (let ((buffer (generate-new-buffer " *chirp-status-render*")))
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _args)
                     'chirp-test-timer))
                  ((symbol-function 'timerp)
                   (lambda (value)
                     (eq value 'chirp-test-timer)))
                  ((symbol-function 'cancel-timer) #'ignore)
                  ((symbol-function 'chirp-render-insert-tweet-list) #'ignore)
                  ((symbol-function 'chirp-render-insert-empty) #'ignore)
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (with-current-buffer buffer
            (chirp-view-mode)
            (chirp-set-status buffer "Refreshing timeline..."))
          (chirp-timeline--render
           buffer
           "For You"
           #'ignore
           (list (list :id "1"))
           :kind 'home
           :limit 20)
          (with-current-buffer buffer
            (should-not chirp--status-text)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-pagination-only-renders-new-entries ()
  "Appending a page preserves existing text and cross-page reply context."
  (let* ((first '(:id "1" :kind tweet :text "Parent" :author-handle "alice"))
         (reply '(:id "2" :kind tweet :text "Reply" :reply-to-id "1"))
         (buffer (generate-new-buffer " *chirp-append*"))
         prefetched)
    (unwind-protect
        (with-current-buffer buffer
          (chirp-timeline--render buffer "Bookmarks" #'ignore (list first)
                                  :kind 'bookmarks :limit 1 :next-cursor "next")
          (let ((inhibit-read-only t))
            (put-text-property (point-min) (1+ (point-min)) 'old-page t))
          (cl-letf (((symbol-function 'chirp-display-buffer) #'ignore)
                    ((symbol-function 'chirp-media-prefetch-tweets)
                     (lambda (tweets _buffer) (setq prefetched tweets)))
                    ((symbol-function 'chirp-backend-bookmarks)
                     (lambda (ok _fail _limit cursor)
                       (should (equal cursor "next"))
                       (funcall ok (list first reply) nil))))
            (chirp-timeline--load-more))
          (should (get-text-property (point-min) 'old-page))
          (should (equal prefetched (list reply)))
          (should (equal (chirp-timeline--buffer-tweets buffer) (list first reply)))
          (let ((appended (buffer-substring-no-properties (point-min) (point-max))))
            (funcall chirp--rerender-function)
            (should (equal appended (buffer-substring-no-properties
                                     (point-min) (point-max))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-timeline-handle-feed-success-keeps-view-when-no-growth ()
  "Loading more should keep the current view when the response adds nothing."
  (let ((buffer (generate-new-buffer " *chirp-timeline-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (setq-local chirp--timeline-loading-more t)
          (setq-local chirp--request-token 'token)
          (let ((render-called nil)
                (last-message nil))
            (cl-letf (((symbol-function 'chirp-timeline--render)
                       (lambda (&rest _args)
                         (setq render-called t)))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq last-message (apply #'format format-string args)))))
              (chirp-timeline--handle-feed-success
               buffer
               "For You"
               #'ignore
               (list (list :id "1") (list :id "2"))
               :kind 'home
               :limit 40
               :anchor-id "2"
               :loading-more t
               :refreshing nil
               :previous-tweets (list (list :id "1") (list :id "2"))
               :previous-exhausted-p nil
               :previous-next-cursor nil
               :envelope nil))
            (should-not render-called)
            (should chirp--timeline-exhausted-p)
            (should-not chirp--timeline-loading-more)
            (should-not chirp--request-token)
            (should (equal last-message "No older posts."))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-refresh-merges-newer-posts-at-top ()
  "Refreshing should prepend newer tweets and keep older visible tweets."
  (let ((render-args nil)
        (last-message nil))
    (cl-letf (((symbol-function 'chirp-timeline--render)
               (lambda (&rest args)
                 (setq render-args args)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq last-message (apply #'format format-string args)))))
      (chirp-timeline--handle-feed-success
       (current-buffer)
       "For You"
       #'ignore
       (list (list :id "3") (list :id "2"))
       :kind 'home
       :limit 20
       :anchor-id "2"
       :loading-more nil
       :refreshing t
       :previous-tweets (list (list :id "2") (list :id "1"))
       :previous-exhausted-p t
       :previous-next-cursor nil
       :envelope nil))
    (should render-args)
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (nth 3 render-args))
                   '("3" "2" "1")))
    (should-not (plist-get (nthcdr 4 render-args) :anchor-id))
    (should (eq (plist-get (nthcdr 4 render-args) :exhausted-p) t))
    (should (equal last-message "1 new post."))))

(ert-deftest chirp-timeline-refresh-reports-no-new-posts ()
  "Refreshing should say when nothing new was added and avoid a rerender."
  (let ((buffer (generate-new-buffer " *chirp-refresh-no-change*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (setq-local chirp--timeline-kind 'home)
          (setq-local chirp--timeline-limit 20)
          (setq-local chirp--timeline-next-cursor nil)
          (let ((render-called nil)
                (last-message nil))
            (cl-letf (((symbol-function 'chirp-timeline--render)
                       (lambda (&rest _args)
                         (setq render-called t)))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq last-message (apply #'format format-string args)))))
              (chirp-timeline--handle-feed-success
               buffer
               "For You"
               #'ignore
               (list (list :id "2") (list :id "1"))
               :kind 'home
               :limit 20
               :anchor-id "2"
               :loading-more nil
               :refreshing t
               :previous-tweets (list (list :id "2") (list :id "1"))
               :previous-exhausted-p nil
               :previous-next-cursor nil
               :envelope nil))
            (should-not render-called)
            (should (equal last-message "No new posts."))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-refresh-counts-new-interleaved-posts ()
  "Refreshing should count unseen tweets even when the top recommendation stays put."
  (let ((render-args nil)
        (last-message nil))
    (cl-letf (((symbol-function 'chirp-timeline--render)
               (lambda (&rest args)
                 (setq render-args args)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq last-message (apply #'format format-string args)))))
      (chirp-timeline--handle-feed-success
       (current-buffer)
       "For You"
       #'ignore
       (list (list :id "2") (list :id "3") (list :id "1"))
       :kind 'home
       :limit 20
       :anchor-id "2"
       :loading-more nil
       :refreshing t
       :previous-tweets (list (list :id "2") (list :id "1"))
       :previous-exhausted-p nil
       :previous-next-cursor nil
       :envelope nil))
    (should render-args)
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (nth 3 render-args))
                   '("2" "3" "1")))
    (should-not (plist-get (nthcdr 4 render-args) :anchor-id))
    (should (equal last-message "1 new post."))))

(ert-deftest chirp-timeline-refresh-skips-rerender-when-page-is-unchanged ()
  "Refreshing should avoid a full rerender when the visible page is unchanged."
  (let ((buffer (generate-new-buffer " *chirp-refresh-skip*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (setq-local chirp--timeline-kind 'home)
          (setq-local chirp--timeline-limit 20)
          (setq-local chirp--timeline-next-cursor "cursor-next")
          (setq-local chirp--timeline-exhausted-p nil)
          (setq-local chirp--request-token 'token)
          (let ((render-called nil)
                (last-message nil))
            (cl-letf (((symbol-function 'chirp-timeline--render)
                       (lambda (&rest _args)
                         (setq render-called t)))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq last-message (apply #'format format-string args)))))
              (chirp-timeline--handle-feed-success
               buffer
               "For You"
               #'ignore
               (list (list :id "2") (list :id "1"))
               :kind 'home
               :limit 20
               :anchor-id "2"
               :loading-more nil
               :refreshing t
               :previous-tweets (list (list :id "2") (list :id "1"))
               :previous-exhausted-p nil
               :previous-next-cursor "cursor-next"
               :envelope nil))
            (should-not render-called)
            (should-not chirp--request-token)
            (should-not chirp--timeline-loading-more)
            (should (equal chirp--timeline-next-cursor "cursor-next"))
            (should (equal last-message "No new posts."))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-render-displays-buffer-when-ready ()
  "Rendering a fetched timeline should display the target buffer."
  (let ((buffer (generate-new-buffer " *chirp-display-test*"))
        displayed)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-render-into-buffer)
                   (lambda (target _title _refresh render-fn)
                     (with-current-buffer target
                       (chirp-view-mode)
                       (let ((inhibit-read-only t))
                         (erase-buffer)
                         (funcall render-fn)))))
                  ((symbol-function 'chirp-render-insert-tweet-list)
                   (lambda (_tweets)
                     (insert "tweet\n")))
                 ((symbol-function 'chirp-display-buffer)
                   (lambda (target)
                     (setq displayed target)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (chirp-timeline--render
           buffer "For You" #'ignore (list (list :id "1"))
           :kind 'home
           :limit 20
           :display-p t))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (should (eq displayed buffer))))

(ert-deftest chirp-timeline-open-likes-resolves-current-user-before-fetching ()
  "Liked view should resolve the current handle before fetching likes."
  (let ((buffer (generate-new-buffer " *chirp-liked-test*"))
        whoami-called
        likes-handle
        render-args)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer _token)
                     t))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-called t)
                     (funcall callback '(:handle "alice") nil)))
                  ((symbol-function 'chirp-backend-likes)
                   (lambda (handle callback &optional _errback _limit _cursor)
                     (setq likes-handle handle)
                     (funcall callback (list (list :id "1")) nil)))
                  ((symbol-function 'chirp-timeline--render)
                   (lambda (&rest args)
                     (setq render-args args))))
          (chirp-timeline-open-likes nil buffer)
          (should whoami-called)
          (should (equal likes-handle "alice"))
          (should (equal (nth 1 render-args) "Liked: @alice"))
          (should (equal (nth 3 render-args) '((:id "1")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-open-list-prompts-from-accessible-lists ()
  "List selection should prompt with accessible lists and open the chosen id."
  (let ((buffer (generate-new-buffer " *chirp-list-picker-test*"))
        captured-target
        render-args
        seen-prompt
        seen-collection)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-lists)
                   (lambda (callback &optional _errback)
                     (funcall callback
                     '((("id" . "1956792682412345678")
                        ("name" . "Emacs")
                        ("mode" . "private")
                        ("sources" . ("owned"))
                        ("owner" . (("screenName" . "lucius"))))) nil)))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _args)
                     (setq seen-prompt prompt
                           seen-collection collection)
                     (caar collection)))
                  ((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer _token)
                     t))
                  ((symbol-function 'chirp-backend-list)
                   (lambda (list-target callback &optional _errback _limit _cursor)
                     (setq captured-target list-target)
                     (funcall callback (list (list :id "1")) nil)))
                  ((symbol-function 'chirp-timeline--render)
                   (lambda (&rest args)
                     (setq render-args args))))
          (chirp-timeline-open-list nil buffer)
          (should (equal seen-prompt "List (1): "))
          (should (string-match-p "@lucius" (caar seen-collection)))
          (should (string-match-p "owned" (caar seen-collection)))
          (should (equal captured-target "1956792682412345678"))
          (should (equal (nth 1 render-args) "List: Emacs"))
          (should (equal (nth 3 render-args) '((:id "1")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-open-list-uses-list-title-and-renderer ()
  "List view should fetch tweets and render under a list-specific title."
  (let ((buffer (generate-new-buffer " *chirp-list-test*"))
        captured-target
        render-args)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer _token)
                     t))
                  ((symbol-function 'chirp-backend-list)
                   (lambda (list-target callback &optional _errback _limit _cursor)
                     (setq captured-target list-target)
                     (funcall callback (list (list :id "1")) nil)))
                  ((symbol-function 'chirp-timeline--render)
                   (lambda (&rest args)
                     (setq render-args args))))
          (chirp-timeline-open-list "https://x.com/i/lists/1956792682412345678" buffer)
          (should (equal captured-target "https://x.com/i/lists/1956792682412345678"))
          (should (equal (nth 1 render-args) "List: 1956792682412345678"))
          (should (equal (nth 3 render-args) '((:id "1")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-render-stores-next-cursor ()
  "Timeline render should retain the next pagination cursor."
  (let ((buffer (generate-new-buffer " *chirp-next-cursor*")))
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (chirp-timeline--render
           buffer
           "For You"
           #'ignore
           (list (list :id "1"))
           :kind 'home
           :limit 20
           :next-cursor "cursor-next")
          (with-current-buffer buffer
            (should (equal chirp--timeline-next-cursor "cursor-next"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-rerender-does-not-steal-focus ()
  "Background timeline rerenders should not redisplay the buffer."
  (let ((buffer (generate-new-buffer " *chirp-rerender-test*"))
        displayed)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-render-into-buffer)
                   (lambda (target _title _refresh render-fn)
                     (with-current-buffer target
                       (chirp-view-mode)
                       (let ((inhibit-read-only t))
                         (erase-buffer)
                         (funcall render-fn)))))
                  ((symbol-function 'chirp-render-insert-tweet-list)
                   (lambda (_tweets)
                     (insert "tweet\n")))
                 ((symbol-function 'chirp-display-buffer)
                   (lambda (target)
                     (setq displayed target)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (chirp-timeline--render
           buffer "For You" #'ignore (list (list :id "1"))
           :kind 'home
           :limit 20)
          (should-not displayed))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-timeline-rerender-preserves-point-within-entry ()
  "Background rerenders should keep point inside the same tweet body."
  (let ((buffer (generate-new-buffer " *chirp-offset-test*"))
        (tweet-a '(:id "1" :text "Alpha line one\nAlpha line two"))
        (tweet-b '(:id "2" :text "Beta line one\nBeta line two")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-timeline--render
           buffer "For You" #'ignore (list tweet-a tweet-b)
           :kind 'home
           :limit 20)
          (search-forward "Beta line two")
          (let ((before (point)))
            (funcall chirp--rerender-function)
            (should (equal (plist-get (chirp-entry-at-point) :id) "2"))
            (should (> (point) (chirp--current-entry-start)))
            (should (equal before (point)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-load-more-uses-next-cursor-instead-of-growing-max ()
  "Loading more should request the next cursor without inflating the head page size."
  (with-temp-buffer
    (chirp-view-mode)
    (setq-local chirp--timeline-kind 'home
                chirp--timeline-load-more-function #'chirp-timeline--load-more)
    (setq-local chirp--timeline-limit 20)
    (setq-local chirp--timeline-next-cursor "cursor-next")
    (let ((buffer (current-buffer))
          captured)
      (cl-letf (((symbol-function 'chirp-timeline--open)
                 (lambda (&rest args)
                   (setq captured args))))
        (chirp-load-more))
      (should (equal (nth 0 captured) 'home))
      (should (= (plist-get (cdr captured) :limit) 20))
      (should (equal (plist-get (cdr captured) :anchor-id) '(:position 1)))
      (should (eq (plist-get (cdr captured) :buffer) buffer))
      (should (eq (plist-get (cdr captured) :loading-more) t))
      (should-not (plist-get (cdr captured) :refreshing))
      (should (equal (plist-get (cdr captured) :cursor) "cursor-next")))))

(ert-deftest chirp-timeline-refresh-uses-smaller-head-window ()
  "Refreshing should fetch a smaller head page when configured."
  (with-temp-buffer
    (chirp-view-mode)
    (let ((chirp-timeline-refresh-max-results 10)
          captured)
      (cl-letf (((symbol-function 'chirp-backend-feed)
                 (lambda (_callback _following _errback max-results cursor)
                   (setq captured (list max-results cursor)))))
        (chirp-timeline--open
         'home
         :limit 20
         :buffer (current-buffer)
         :refreshing t))
      (should (equal captured '(10 nil))))))

(ert-deftest chirp-timeline-refresh-can-use-current-limit ()
  "Refreshing should keep the old fetch size when the head-window override is disabled."
  (with-temp-buffer
    (chirp-view-mode)
    (let ((chirp-timeline-refresh-max-results nil)
          captured)
      (cl-letf (((symbol-function 'chirp-backend-feed)
                 (lambda (_callback _following _errback max-results cursor)
                   (setq captured (list max-results cursor)))))
        (chirp-timeline--open
         'home
         :limit 20
         :buffer (current-buffer)
         :refreshing t))
      (should (equal captured '(20 nil))))))

(ert-deftest chirp-window-state-restore-preserves-point-and-scroll ()
  "Window-state helpers should preserve point and scroll position."
  (let ((buffer (generate-new-buffer " *chirp-window-state*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (chirp-view-mode)
            (let ((inhibit-read-only t))
              (dotimes (index 80)
                (insert (format "line %02d\n" index))))
            (goto-char (point-min))
            (forward-line 40)
            (set-window-start (selected-window) (line-beginning-position))
            (set-window-vscroll (selected-window) 8 t)
            (let ((state (chirp-capture-window-state buffer))
                  (point (point))
                  (window-start (window-start (selected-window)))
                  (vscroll (window-vscroll (selected-window) t)))
              (goto-char (point-min))
              (set-window-start (selected-window) (point-min))
              (set-window-vscroll (selected-window) 0 t)
              (chirp-restore-window-state state)
              (should (= (point) point))
              (should (= (window-start (selected-window)) window-start))
              (should (= (window-vscroll (selected-window) t) vscroll)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


(ert-deftest chirp-clean-text-decodes-html-entities ()
  "Tweet text should decode common HTML entities."
  (should (equal (chirp-clean-text "a &gt; b &amp; c &lt; d")
                 "a > b & c < d"))
  (should (equal (chirp-clean-text "say &quot;hi&quot; &#39;now&#39;")
                 "say \"hi\" 'now'"))
  (should (equal (chirp-clean-text "A&#10;B&#x21;")
                 "A\nB!")))

(ert-deftest chirp-search-load-more-skips-duplicate-page-without-changing-windows ()
  (save-window-excursion
    (let ((view (generate-new-buffer " *chirp-search-page*"))
          (other (current-buffer)) calls)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-backend-search)
                     (lambda (_query ok _err _limit cursor)
                       (push cursor calls)
                       (funcall ok (list (list :kind 'tweet :id (if (equal cursor "2") "b" "a")))
                                (pcase cursor
                                  ('nil '(("nextCursor" . "1")))
                                  ("1" '(("nextCursor" . "2"))))))))
            (chirp-timeline-open-search "emacs" view)
            (switch-to-buffer other)
            (with-current-buffer view (chirp-load-more))
            (should (eq other (window-buffer)))
            (should (equal (nreverse calls) '(nil "1" "2")))
            (should (equal (mapcar #'chirp-tweet-key (chirp-timeline--buffer-tweets view)) '("a" "b"))))
        (kill-buffer view)))))

(ert-deftest chirp-bookmarks-resumes-the-requested-view-after-web-login ()
  (let (resume opened)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
              ((symbol-function 'getenv) (lambda (&rest _) nil))
              ((symbol-function 'chirp-web-ready-p) (lambda () nil))
              ((symbol-function 'auth-source-pick-first-password)
               (lambda (&rest _) (ert-fail "Unexpected password lookup")))
              ((symbol-function 'chirp-web-login) (lambda (callback) (setq resume callback)))
              ((symbol-function 'chirp-timeline-open-bookmarks) (lambda () (setq opened t))))
      (chirp-bookmarks)
      (should-not opened)
      (funcall resume)
      (should opened))))

(ert-deftest chirp-home-retries-in-the-same-view-after-first-load-fails ()
  (save-window-excursion
    (let ((view (generate-new-buffer " *chirp-retry*")) (requests 0))
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-x-session) (lambda () 'webkit))
                    ((symbol-function 'chirp-buffer) (lambda () view))
                    ((symbol-function 'chirp-backend-feed)
                     (lambda (ok _following fail &rest _)
                       (cl-incf requests)
                       (if (= requests 1) (funcall fail "Offline")
                         (funcall ok nil nil)))))
            (chirp)
            (should (eq (current-buffer) view))
            (should (string-match-p "Offline" (buffer-string)))
            (chirp)
            (should-not (string-match-p "Offline" (buffer-string)))
            (chirp)
            (should (= requests 2)))
        (kill-buffer view)))))

(ert-deftest chirp-list-picker-only-prompts-in-the-requesting-window-and-buffer ()
  (save-window-excursion
    (with-temp-buffer
      (let ((origin (current-buffer)) callback busy opened)
        (switch-to-buffer origin)
        (cl-letf (((symbol-function 'chirp-x-session) (lambda () 'webkit))
                  ((symbol-function 'chirp-backend-lists) (lambda (ok &rest _) (setq callback ok)))
                  ((symbol-function 'active-minibuffer-window)
                   (lambda () (and busy (minibuffer-window))))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (should-not busy)
                     (should (eq origin (window-buffer)))
                     (caar choices)))
                  ((symbol-function 'chirp-timeline--open) (lambda (kind &rest _) (setq opened kind))))
          (let ((lists '(((name . "Lisp") (id . "42")))))
            (chirp-list)
            (setq busy t)
            (funcall callback lists nil)
            (should-not opened)
            (setq busy nil)
            (chirp-list)
            (switch-to-buffer (other-buffer))
            (funcall callback lists nil)
            (should-not opened)
            (switch-to-buffer origin)
            (chirp-list)
            (funcall callback lists nil)
            (should (equal opened '(list "42" "Lisp")))
            (should-error (chirp-list " ") :type 'user-error)))))))

(ert-deftest chirp-search-tab-toggles-between-top-and-latest ()
  "TAB in a search view should toggle between Top and Latest product ordering."
  (save-window-excursion
    (let ((view (generate-new-buffer " *chirp-search-toggle*"))
          products)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-backend-search)
                     (lambda (query ok _err _limit _cursor &optional product)
                       (push (cons query (or product "Top")) products)
                       (funcall ok (list (list :kind 'tweet :id "1" :text query)) nil))))
            (should-error (chirp-timeline-open-search "   " view) :type 'user-error)
            (chirp-timeline-open-search "emacs" view)
            (with-current-buffer view
              (should (equal chirp--view-title "Search: emacs"))
              (chirp-toggle-home-following)
              (should (equal chirp--view-title "Search (Latest): emacs"))
              (chirp-toggle-home-following)
              (should (equal chirp--view-title "Search: emacs")))
            (should (equal (nreverse products)
                           '(("emacs" . "Top")
                             ("emacs" . "Latest")
                             ("emacs" . "Top")))))
        (kill-buffer view)))))

(provide 'chirp-timeline-test)

;;; chirp-timeline-test.el ends here
