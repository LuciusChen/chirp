;;; chirp-timeline-test.el --- Tests for timeline loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-timeline)

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
  (let ((previous (generate-new-buffer " *chirp-prev*"))
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
  (let ((previous (generate-new-buffer " *chirp-prev*"))
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

(ert-deftest chirp-load-more-stops-when-timeline-is-exhausted ()
  "Loading more should short-circuit once the timeline is exhausted."
  (with-temp-buffer
    (chirp-view-mode)
    (setq-local chirp--timeline-kind 'home)
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
           'home
           20)
          (with-current-buffer buffer
            (should-not chirp--status-text)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
               'home
               40
               "2"
               t
               (list (list :id "1") (list :id "2"))
               2
               (list (list :id "1") (list :id "2"))
               nil
               nil
               nil))
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
       'home
       20
       "2"
       nil
       t
       nil
       (list (list :id "2") (list :id "1"))
       t
       nil
       nil))
    (should render-args)
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (nth 3 render-args))
                   '("3" "2" "1")))
    (should-not (nth 6 render-args))
    (should (eq (nth 7 render-args) t))
    (should (equal last-message "1 new post."))))

(ert-deftest chirp-timeline-refresh-reports-no-new-posts ()
  "Refreshing should say when nothing new was added and avoid a rerender."
  (let ((buffer (generate-new-buffer " *chirp-refresh-no-change*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (setq-local chirp--timeline-kind 'home)
          (setq-local chirp--timeline-limit 20)
          (setq-local chirp--timeline-count 2)
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
               'home
               20
               "2"
               nil
               t
               2
               (list (list :id "2") (list :id "1"))
               nil
               nil
               nil))
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
       'home
       20
       "2"
       nil
       t
       nil
       (list (list :id "2") (list :id "1"))
       nil
       nil
       nil))
    (should render-args)
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (nth 3 render-args))
                   '("2" "3" "1")))
    (should-not (nth 6 render-args))
    (should (equal last-message "1 new post."))))

(ert-deftest chirp-timeline-refresh-skips-rerender-when-page-is-unchanged ()
  "Refreshing should avoid a full rerender when the visible page is unchanged."
  (let ((buffer (generate-new-buffer " *chirp-refresh-skip*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (setq-local chirp--timeline-kind 'home)
          (setq-local chirp--timeline-limit 20)
          (setq-local chirp--timeline-count 2)
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
               'home
               20
               "2"
               nil
               t
               2
               (list (list :id "2") (list :id "1"))
               nil
               "cursor-next"
               nil))
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
          (chirp-timeline--render buffer "For You" #'ignore (list (list :id "1")) 'home 20 nil nil t))
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
                   (lambda (handle callback &optional _errback)
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
        (cl-letf (((symbol-function 'chirp-backend-lists-sync)
                   (lambda ()
                     '((("id" . "1956792682412345678")
                        ("name" . "Emacs")
                        ("mode" . "private")
                        ("sources" . ("owned"))
                        ("owner" . (("screenName" . "lucius")))))))
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
                   (lambda (list-target callback &optional _errback)
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
          (should (equal (nth 1 render-args) "List: 1956792682412345678"))
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
                   (lambda (list-target callback &optional _errback)
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
           'home
           20
           nil
           nil
           nil
           "cursor-next")
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
          (chirp-timeline--render buffer "For You" #'ignore (list (list :id "1")) 'home 20 nil nil nil)
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
          (chirp-timeline--render buffer "For You" #'ignore (list tweet-a tweet-b) 'home 20 nil nil nil)
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
    (setq-local chirp--timeline-kind 'home)
    (setq-local chirp--timeline-limit 20)
    (setq-local chirp--timeline-next-cursor "cursor-next")
    (let ((buffer (current-buffer))
          captured)
      (cl-letf (((symbol-function 'chirp-timeline--open)
                 (lambda (&rest args)
                   (setq captured args))))
        (chirp-load-more))
      (should (equal (nth 0 captured) 'home))
      (should (= (nth 1 captured) 20))
      (should (equal (nth 2 captured) '(:position 1)))
      (should (eq (nth 3 captured) buffer))
      (should (eq (nth 4 captured) t))
      (should-not (nth 5 captured))
      (should (equal (nth 6 captured) "cursor-next")))))

(ert-deftest chirp-timeline-refresh-uses-smaller-head-window ()
  "Refreshing should fetch a smaller head page when configured."
  (with-temp-buffer
    (chirp-view-mode)
    (let ((chirp-timeline-refresh-max-results 10)
          captured)
      (cl-letf (((symbol-function 'chirp-backend-feed)
                 (lambda (_callback _following _errback max-results cursor)
                   (setq captured (list max-results cursor)))))
        (chirp-timeline--open 'home 20 nil (current-buffer) nil t))
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
        (chirp-timeline--open 'home 20 nil (current-buffer) nil t))
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

(provide 'chirp-timeline-test)

;;; chirp-timeline-test.el ends here
