;;; chirp-profile-test.el --- Tests for Chirp profile loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp)
(require 'chirp-profile)
(require 'chirp-timeline)

(ert-deftest chirp-profile-open-starts-user-and-post-requests-in-parallel ()
  "Profile loading should request user metadata and posts concurrently."
  (let ((buffer (generate-new-buffer " *chirp-profile-test*"))
        user-callback
        posts-called)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (_callback &optional _errback)
                     nil))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle _callback &optional _errback _max-results _cursor)
                     (setq posts-called t)))
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (chirp-profile-open "alice" buffer)
          (should (functionp user-callback))
          (should posts-called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-open-followers-disables-wrap-navigation ()
  "Follower/following list buffers should stop at the ends instead of wrapping."
  (let ((buffer (generate-new-buffer " *chirp-profile-followers-test*"))
        followers-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-followers)
                   (lambda (_handle callback &optional _errback)
                     (setq followers-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (_callback &optional _errback)
                     nil))
                  ((symbol-function 'chirp-render-insert-user-list) #'ignore)
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore))
          (chirp-profile-open-followers "alice" buffer)
          (should (functionp followers-callback))
          (funcall followers-callback (list '(:kind user :handle "bob")) nil)
          (with-current-buffer buffer
            (should-not chirp--entry-wrap-navigation)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-open-renders-header-before-posts-arrive ()
  "Profile view should show the header immediately without a Recent Posts section."
  (let ((buffer (generate-new-buffer " *chirp-profile-header-test*"))
        user-callback
        posts-callback
        whoami-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-callback callback)))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle callback &optional _errback _max-results _cursor)
                     (setq posts-callback callback)))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (chirp-profile-open "alice" buffer)
          (funcall user-callback
                   '(:kind user :handle "alice" :name "Alice" :bio "" :posts 12 :following 3 :followers 4)
                   nil)
          (with-current-buffer buffer
            (should (string-match-p "@alice" (buffer-string)))
            (should (string-match-p "Loading posts..." (buffer-string)))
            (should-not (string-match-p "Recent Posts" (buffer-string))))
          (funcall whoami-callback '(:kind user :handle "alice") nil)
          (funcall posts-callback
                   (list '(:kind tweet :id "1" :text "hello" :author-handle "alice"))
                   '(("pagination" . (("nextCursor" . "cursor-next")))))
          (with-current-buffer buffer
            (should (equal chirp-profile--available-modes '(posts replies highlights media likes)))
            (should (eq chirp--timeline-load-more-function #'chirp-profile-load-more))
            (should (equal chirp--timeline-next-cursor "cursor-next"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-open-updates-persistent-status-by-phase ()
  "Profile loading should keep a persistent status as each phase completes."
  (let ((buffer (generate-new-buffer " *chirp-profile-status*"))
        user-callback
        posts-callback
        whoami-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (&rest _args)
                     'chirp-test-timer))
                  ((symbol-function 'timerp)
                   (lambda (value)
                     (eq value 'chirp-test-timer)))
                  ((symbol-function 'cancel-timer) #'ignore)
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-callback callback)))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle callback &optional _errback _max-results _cursor)
                     (setq posts-callback callback)))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (chirp-profile-open "alice" buffer)
          (with-current-buffer buffer
            (should (equal chirp--status-text "Loading profile...")))
          (funcall user-callback
                   '(:kind user :handle "alice" :name "Alice" :bio "" :posts 12 :following 3 :followers 4)
                   nil)
          (with-current-buffer buffer
            (should (equal chirp--status-text "Profile ready · loading posts...")))
          (funcall whoami-callback '(:kind user :handle "alice") nil)
          (funcall posts-callback
                   (list '(:kind tweet :id "1" :text "hello" :author-handle "alice"))
                   '(("pagination" . (("nextCursor" . "cursor-next")))))
          (with-current-buffer buffer
            (should-not chirp--status-text)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-load-more-appends-older-posts ()
  "Loading more in a profile should append older tweets using the next cursor."
  (let ((buffer (generate-new-buffer " *chirp-profile-load-more*"))
        (user '(:kind user :handle "alice" :name "Alice" :bio "" :posts 12 :following 3 :followers 4))
        (first-page (list '(:kind tweet :id "1" :text "first" :author-handle "alice")))
        callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-user-posts)
                   (lambda (handle success &optional _errback max-results cursor)
                     (should (equal handle "alice"))
                     (should (= max-results chirp-profile-post-limit))
                     (should (equal cursor "cursor-prev"))
                     (setq callback success)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (_callback &optional _errback)
                     nil))
                  ((symbol-function 'chirp-begin-request)
                   (lambda (_buffer)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (chirp-profile--render
           buffer "@alice" #'ignore user first-page 'posts '(posts) nil nil t "cursor-prev" nil)
          (with-current-buffer buffer
            (chirp-profile-load-more)
            (funcall callback
                     (list '(:kind tweet :id "2" :text "second" :author-handle "alice"))
                     '(("pagination" . (("nextCursor" . "cursor-next")))))
            (should (string-match-p "first" (buffer-string)))
            (should (string-match-p "second" (buffer-string)))
            (should (equal chirp--timeline-next-cursor "cursor-next"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-load-more-uses-current-subview-fetcher ()
  "Loading more should use the active profile subview command."
  (let ((buffer (generate-new-buffer " *chirp-profile-load-more-replies*"))
        (user '(:kind user :handle "alice" :name "Alice" :bio "" :posts 12 :following 3 :followers 4))
        (first-page (list '(:kind tweet :id "1" :text "first reply" :author-handle "alice")))
        callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-user-replies)
                   (lambda (handle success &optional _errback max-results cursor)
                     (should (equal handle "alice"))
                     (should (= max-results chirp-profile-post-limit))
                     (should (equal cursor "cursor-prev"))
                     (setq callback success)))
                  ((symbol-function 'chirp-begin-request)
                   (lambda (_buffer)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (chirp-profile--render
           buffer "@alice · Replies" #'ignore user first-page 'replies '(posts replies highlights media)
           nil nil t "cursor-prev" nil)
          (with-current-buffer buffer
            (chirp-profile-load-more)
            (funcall callback
                     (list '(:kind tweet :id "2" :text "second reply" :author-handle "alice"))
                     '(("pagination" . (("nextCursor" . "cursor-next")))))
            (should (string-match-p "first reply" (buffer-string)))
            (should (string-match-p "second reply" (buffer-string)))
            (should (equal chirp--timeline-next-cursor "cursor-next"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-open-adds-likes-mode-for-own-profile ()
  "Own profiles should expose a Likes mode in the profile strip."
  (let ((buffer (generate-new-buffer " *chirp-profile-likes-mode*"))
        user-callback
        posts-callback
        whoami-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-callback callback)))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle callback &optional _errback _max-results _cursor)
                     (setq posts-callback callback)))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (chirp-profile-open "alice" buffer)
          (funcall user-callback
                   '(:kind user :handle "alice" :name "Alice" :bio "" :posts 12 :following 3 :followers 4)
                   nil)
          (funcall posts-callback
                   (list '(:kind tweet :id "1" :text "hello" :author-handle "alice"))
                   nil)
          (funcall whoami-callback '(:kind user :handle "alice") nil)
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "Posts" nil t))
            (should (search-forward "Replies" nil t))
            (should (search-forward "Highlights" nil t))
            (should (search-forward "Media" nil t))
            (should (search-forward "Likes" nil t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-open-adds-likes-mode-case-insensitively ()
  "Own profile detection should ignore handle case when exposing Likes."
  (let ((buffer (generate-new-buffer " *chirp-profile-likes-case*"))
        user-callback
        posts-callback
        whoami-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-callback callback)))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle callback &optional _errback _max-results _cursor)
                     (setq posts-callback callback)))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (chirp-profile-open "lucius_chen" buffer)
          (funcall user-callback
                   '(:kind user :handle "lucius_chen" :name "Lucius" :bio "" :posts 12 :following 3 :followers 4)
                   nil)
          (funcall posts-callback
                   (list '(:kind tweet :id "1" :text "hello" :author-handle "lucius_chen"))
                   nil)
          (funcall whoami-callback '(:kind user :handle "Lucius_Chen") nil)
          (with-current-buffer buffer
            (should (equal chirp-profile--available-modes '(posts replies highlights media likes)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-mode-strip-ret-switches-profile-view ()
  "RET on a profile mode label should call the profile switch function."
  (with-temp-buffer
    (chirp-view-mode)
    (let (captured)
      (setq-local chirp--profile-switch-mode-function
                  (lambda (mode)
                    (setq captured mode)))
      (let ((inhibit-read-only t))
        (chirp-render-insert-profile-view-strip 'posts '(posts replies highlights media likes)))
      (goto-char (point-min))
      (search-forward "Likes")
      (backward-char 2)
      (chirp-open-at-point)
      (should (eq captured 'likes)))))

(ert-deftest chirp-profile-tab-cycles-profile-view ()
  "TAB should cycle profile subviews when a profile switch function exists."
  (with-temp-buffer
    (chirp-view-mode)
    (let (captured)
      (setq-local chirp--profile-switch-mode-function
                  (lambda (mode)
                    (setq captured mode)))
      (chirp-toggle-home-following)
      (should (eq captured :next)))))

(ert-deftest chirp-me-opens-the-authenticated-profile ()
  "The public Chirp entry point should resolve and open the current profile."
  (let (opened-handle)
    (cl-letf (((symbol-function 'chirp-backend-whoami)
               (lambda (callback &optional _errback)
                 (funcall callback '(:kind user :handle "alice") nil)))
              ((symbol-function 'chirp-profile-open)
               (lambda (handle &optional _buffer _mode)
                 (setq opened-handle handle))))
      (chirp-me)
      (should (equal opened-handle "alice")))))

(provide 'chirp-profile-test)

;;; chirp-profile-test.el ends here
