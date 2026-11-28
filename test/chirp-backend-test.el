;;; chirp-backend-test.el --- Tests for Chirp backend caching -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Exercise backend caching, direct request shaping, and response adaptation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-backend)
(require 'chirp-x-cookies)
(require 'chirp-timeline)

(defun chirp-backend-test--timeline-entry (tweet &optional promoted-p)
  "Return one HomeTimeline entry carrying raw TWEET.

When PROMOTED-P is non-nil, include the item-level promoted marker used by X."
  (let ((item `(("tweet_results" . (("result" . ,tweet))))))
    (when promoted-p
      (push '("promotedMetadata" . t) item))
    `(("entryId" . "tweet-1")
      ("content" . (("itemContent" . ,item))))))

(defun chirp-backend-test--module-entry (tweet)
  "Return one nested timeline module entry carrying TWEET."
  (let ((item-content `(("tweet_results" . (("result" . ,tweet))))))
    `(("entryId" . "conversation-thread-1")
      ("content" .
       (("items" .
         ((("item" . (("itemContent" . ,item-content)))))))))))

(defun chirp-backend-test--module-item (tweet)
  "Return one instruction-level module item carrying TWEET."
  `(("entryId" . "module-item-1")
    ("item" .
     (("itemContent" .
       (("tweet_results" . (("result" . ,tweet)))))))))

(defun chirp-backend-test--bottom-cursor (cursor)
  "Return an X timeline bottom cursor carrying CURSOR."
  `(("entryId" . "cursor-bottom")
    ("content" . (("cursorType" . "Bottom")
                  ("value" . ,cursor)))))

(defun chirp-backend-test--instructions (entries)
  "Return minimal X timeline instructions containing ENTRIES."
  `(("instructions" .
     ((("type" . "TimelineAddEntries")
       ("entries" . ,entries))))))

(defun chirp-backend-test--payload-at-path (path value)
  "Return a JSON-style payload containing VALUE at PATH."
  (dolist (key (reverse (copy-sequence path)) value)
    (setq value (list (cons key value)))))

(defun chirp-backend-test--timeline-payload (path entries)
  "Return a minimal timeline payload at PATH containing ENTRIES."
  (chirp-backend-test--payload-at-path
   path (chirp-backend-test--instructions entries)))

(defun chirp-backend-test--home-timeline-payload (entries)
  "Return a minimal HomeTimeline GraphQL payload containing ENTRIES."
  (chirp-backend-test--timeline-payload
   '("data" "home" "home_timeline_urt") entries))

(defun chirp-backend-test--user-payload (&optional handle user-id)
  "Return a minimal profile payload for HANDLE and USER-ID."
  `(("data" .
     (("user" .
       (("result" .
         (("rest_id" . ,(or user-id "42"))
          ("legacy" . (("screen_name" . ,(or handle "alice"))
                       ("name" . "Alice")))))))))))

(defun chirp-backend-test--viewer-payload (&optional handle user-id)
  "Return a minimal Viewer payload for HANDLE and USER-ID."
  `(("data" .
     (("viewer" .
       (("user_results" .
         (("result" .
           (("rest_id" . ,(or user-id "42"))
            ("core" . (("screen_name" . ,(or handle "alice"))
                       ("name" . "Alice")))))))))))))

(defun chirp-backend-test--user-timeline-payload (entries)
  "Return a minimal user timeline payload containing ENTRIES."
  (chirp-backend-test--timeline-payload
   '("data" "user" "result" "timeline" "timeline") entries))

(defun chirp-backend-test--thread-payload (entries)
  "Return a minimal TweetDetail payload containing ENTRIES."
  (chirp-backend-test--timeline-payload
   '("data" "threaded_conversation_with_injections_v2") entries))

(ert-deftest chirp-backend-thread-cache-reuses-fresh-results ()
  "Fresh cached thread results should avoid a second backend request."
  (let ((chirp-backend-read-cache-ttl 15)
        (now 1000)
        (request-count 0)
        first second third)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&rest _args)
                       now))
                    ((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables callback &rest _options)
                       (setq request-count (1+ request-count))
                       (funcall
                        callback
                        (chirp-backend-test--thread-payload
                         (list
                          (chirp-backend-test--timeline-entry
                           '(("rest_id" . "123")
                             ("legacy" . (("full_text" . "hello")))))))))))
            (chirp-backend-thread "123"
                                  (lambda (tweets _envelope)
                                    (setq first tweets)))
            (chirp-backend-thread "123"
                                  (lambda (tweets _envelope)
                                    (setq second tweets)))
            (should (= request-count 1))
            (should (equal first second))
            (should-not (eq first second))
            (setf (plist-get (car first) :text) "changed")
            (chirp-backend-thread "123"
                                  (lambda (tweets _envelope)
                                    (setq third tweets)))
            (should (equal (plist-get (car third) :text) "hello"))
            (chirp-backend-invalidate-thread "123")
            (chirp-backend-thread "123" #'ignore)
            (should (= request-count 2))
            (setq now 1016)
            (chirp-backend-thread "123" #'ignore)
            (should (= request-count 3))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-user-cache-coalesces-inflight-requests ()
  "Concurrent profile requests for the same handle should share one backend call."
  (let ((chirp-backend-read-cache-ttl 15)
        (request-count 0)
        success-callback
        results)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables callback &rest _options)
                       (setq request-count (1+ request-count)
                             success-callback callback))))
            (chirp-backend-user "@Alice"
                                (lambda (user _envelope)
                                  (push user results)))
            (chirp-backend-user "alice"
                                (lambda (user _envelope)
                                  (push user results)))
            (should (= request-count 1))
            (should (functionp success-callback))
            (funcall success-callback
                     (chirp-backend-test--user-payload "alice" "42"))
            (should (= (length results) 2))
            (should (equal (plist-get (car results) :handle) "alice"))
            (should (equal (plist-get (cadr results) :handle) "alice"))
            (should-not (eq (car results) (cadr results)))
            (chirp-backend-invalidate-user "alice")
            (chirp-backend-user "alice" #'ignore)
            (should (= request-count 2))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-feed-uses-x-graphql-and-preserves-pagination ()
  "Home feeds should use direct X GraphQL with normalized pagination metadata."
  (let (operation variables next-cursor tweets)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--home-timeline-payload
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "1")
                       ("core" . (("user_results" .
                                   (("result" .
                                     (("core" . (("screen_name" . "alice") ("name" . "Alice")))
                                      ("avatar" . (("image_url" . "https://pbs.twimg.com/avatar.jpg")))))))))
                       ("legacy" . (("full_text" . "hello")))))
                    (chirp-backend-test--bottom-cursor "cursor-next")))))))
      (chirp-backend-feed
       (lambda (items envelope)
         (setq tweets items
               next-cursor (chirp-backend-envelope-next-cursor envelope)))
       nil nil 20 "cursor-prev"))
    (should (equal (plist-get operation :name) "HomeTimeline"))
    (should (equal (alist-get "count" variables nil nil #'string=) 20))
    (should (equal (alist-get "cursor" variables nil nil #'string=)
                   "cursor-prev"))
    (should (equal (mapcar #'chirp-tweet-key tweets) '("1")))
    (should (equal (plist-get (car tweets) :author-handle) "alice"))
    (should (equal (plist-get (car tweets) :author-avatar-url) "https://pbs.twimg.com/avatar.jpg"))
    (should (equal next-cursor "cursor-next"))))

(ert-deftest chirp-backend-feed-selects-the-following-operation ()
  "Following feeds should use the chronological X timeline operation."
  (let (operation)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation _variables callback &rest _options)
                 (setq operation request-operation)
                 (funcall callback
                          (chirp-backend-test--home-timeline-payload nil))))
              ((symbol-function 'chirp-collect-top-level-tweets)
               (lambda (_items) nil)))
      (chirp-backend-feed #'ignore t))
    (should (equal (plist-get operation :name) "HomeLatestTimeline"))))

(ert-deftest chirp-backend-feed-reports-an-invalid-x-timeline ()
  "Malformed direct timeline responses should not reach feed callbacks."
  (let (success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback '(("data" . nil))))))
      (chirp-backend-feed
       (lambda (&rest _args)
         (setq success t))
       nil
       (lambda (message)
         (setq failure message))))
    (should-not success)
    (should (equal failure "X did not return a home timeline"))))

(ert-deftest chirp-backend-bookmarks-uses-direct-v2-timeline ()
  "Bookmarks should accept X's v2 timeline path without a CLI process."
  (let (operation variables tweets)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--timeline-payload
                   '("data" "bookmark_timeline_v2" "timeline")
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "1")
                       ("legacy" . (("full_text" . "saved")))))))))))
      (chirp-backend-bookmarks
       (lambda (items _envelope)
         (setq tweets items))))
    (should (equal (plist-get operation :name) "Bookmarks"))
    (should (equal (alist-get "count" variables nil nil #'string=) 20))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) tweets)
                   '("1")))))

(ert-deftest chirp-backend-search-uses-post-and-normalizes-modules ()
  "Search should use direct POST and retain tweets nested in modules."
  (let (operation variables tweets)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--timeline-payload
                   '("data" "search_by_raw_query" "search_timeline" "timeline")
                   (list
                    (chirp-backend-test--module-entry
                     '(("rest_id" . "2")
                       ("legacy" . (("full_text" . "nested")))))))))))
      (chirp-backend-search
       "emacs"
       (lambda (items _envelope)
         (setq tweets items))))
    (should (equal (plist-get operation :name) "SearchTimeline"))
    (should (eq (plist-get operation :method) 'post))
    (should (equal (alist-get "rawQuery" variables nil nil #'string=)
                   "emacs"))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) tweets)
                   '("2")))))

(ert-deftest chirp-backend-timeline-keeps-module-items-and-pinned-entry ()
  "Shared timeline adaptation should retain instruction-level special entries."
  (let* ((module-item
          (chirp-backend-test--module-item
           '(("rest_id" . "1")
             ("legacy" . (("full_text" . "module"))))))
         (pinned-entry
          (chirp-backend-test--timeline-entry
           '(("rest_id" . "2")
             ("legacy" . (("full_text" . "pinned"))))))
         (instructions
          (list
           (list '("type" . "TimelineAddToModule")
                 (cons "moduleItems" (list module-item)))
           (list '("type" . "TimelinePinEntry")
                 (cons "entry" pinned-entry))))
         (payload
          (chirp-backend-test--payload-at-path
           '("data" "timeline")
           (list (cons "instructions" instructions))))
         (page (chirp-backend--timeline-page
                payload '(("data" "timeline")) "the test timeline")))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) (car page))
                   '("1" "2")))))

(ert-deftest chirp-backend-simple-mutations-map-to-direct-x-operations ()
  "Simple state and delete actions should map directly to X mutations."
  (dolist (case '(("like" "FavoriteTweet" "tweet_id")
                  ("unlike" "UnfavoriteTweet" "tweet_id")
                  ("retweet" "CreateRetweet" "tweet_id")
                  ("unretweet" "DeleteRetweet" "source_tweet_id")
                  ("bookmark" "CreateBookmark" "tweet_id")
                  ("unbookmark" "DeleteBookmark" "tweet_id")
                  ("delete" "DeleteTweet" "tweet_id")))
    (let (operation variables success)
      (cl-letf (((symbol-function 'chirp-x-graphql-request)
                 (lambda (request-operation request-variables callback &rest _options)
                   (setq operation request-operation
                         variables request-variables)
                   (funcall callback '(("data" . nil))))))
        (chirp-backend-request
         (list (car case) "123")
         (lambda (_payload _envelope)
           (setq success t))))
      (should success)
      (should (equal (plist-get operation :name) (cadr case)))
      (should (equal (alist-get (nth 2 case) variables nil nil #'string=)
                     "123")))))

(ert-deftest chirp-backend-follow-mutations-resolve-user-and-use-rest ()
  "Follow state changes should use direct non-retried REST form requests."
  (dolist (case '(("follow" . "create") ("unfollow" . "destroy")))
    (let (path form success)
      (cl-letf (((symbol-function 'chirp-backend-user)
                 (lambda (handle callback &optional _errback)
                   (should (equal handle "Alice"))
                   (funcall callback '(:id "42") nil)))
                ((symbol-function 'chirp-x-api-request)
                 (lambda (service request-path callback &rest options)
                   (should (eq service 'web))
                   (should (eq (plist-get options :method) 'post))
                   (setq path request-path
                         form (plist-get options :form))
                   (funcall callback '(("id_str" . "42"))))))
        (chirp-backend-request
         (list (car case) "Alice")
         (lambda (_payload _envelope)
           (setq success t))))
      (should success)
      (should (equal path (format "1.1/friendships/%s.json" (cdr case))))
      (should (equal (alist-get "user_id" form nil nil #'string=) "42")))))

(ert-deftest chirp-backend-user-posts-passes-cursor-without-cache ()
  "Profile post pagination should forward the next X cursor directly."
  (let (operation variables tweets next-cursor)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (handle callback &optional _errback)
                 (should (equal handle "Alice"))
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--user-timeline-payload
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "1")
                       ("legacy" . (("full_text" . "hello")))))
                    (chirp-backend-test--bottom-cursor "cursor-next")))))))
      (chirp-backend-user-posts
       "@Alice"
       (lambda (items envelope)
         (setq tweets items
               next-cursor (chirp-backend-envelope-next-cursor envelope)))
       nil 15 "cursor-prev"))
    (should (equal (plist-get operation :name) "UserTweets"))
    (should (equal (alist-get "userId" variables nil nil #'string=) "42"))
    (should (equal (alist-get "count" variables nil nil #'string=) 15))
    (should (equal (alist-get "cursor" variables nil nil #'string=)
                   "cursor-prev"))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) tweets)
                   '("1")))
    (should (equal next-cursor "cursor-next"))))

(ert-deftest chirp-backend-lists-merge-direct-collections-and-use-cache ()
  "Accessible lists should merge direct REST collections and reuse the cache."
  (let ((chirp-backend-read-cache-ttl 15)
        (now 1000)
        (request-count 0)
        first second)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf
              (((symbol-function 'float-time)
                (lambda (&rest _args)
                  now))
               ((symbol-function 'chirp-backend-whoami)
                (lambda (callback &optional _errback)
                  (funcall callback '(:id "42") nil)))
               ((symbol-function 'chirp-x-api-request)
                (lambda (_service path callback &rest _options)
                  (setq request-count (1+ request-count))
                  (funcall
                   callback
                   (cond
                    ((string-match-p "ownerships" path)
                     '(("lists" .
                        ((("id_str" . "1") ("name" . "Owned"))
                         (("id_str" . "2") ("name" . "Shared"))))
                       ("next_cursor_str" . "0")))
                    ((string-match-p "subscriptions" path)
                     '(("lists" .
                        ((("id_str" . "2") ("name" . "Shared"))
                         (("id_str" . "3") ("name" . "Subscribed"))))
                       ("next_cursor_str" . "0")))
                    (t
                     '(("lists" .
                        ((("id_str" . "4") ("name" . "Member"))))
                       ("next_cursor_str" . "0"))))))))
            (chirp-backend-lists
             (lambda (lists _envelope)
               (setq first lists)))
            (chirp-backend-lists
             (lambda (lists _envelope)
               (setq second lists)))
            (should (= request-count 3))
            (should (equal first second))
            (should-not (eq first second))
            (should
             (equal (chirp-get
                     (cl-find "2" first
                              :key (lambda (item) (chirp-get item "id"))
                              :test #'equal)
                     "sources")
                    '("owned" "subscribed")))
            (should
             (equal (chirp-get
                     (cl-find "4" first
                              :key (lambda (item) (chirp-get item "id"))
                              :test #'equal)
                     "sources")
                    '("member")))
            (setq now 1016)
            (chirp-backend-lists #'ignore)
            (should (= request-count 6))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-search-users-uses-direct-typeahead ()
  "User completion should normalize and deduplicate direct typeahead results."
  (let (service path query users)
    (cl-letf (((symbol-function 'chirp-x-api-request)
               (lambda (request-service request-path callback &rest options)
                 (setq service request-service
                       path request-path
                       query (plist-get options :query))
                 (funcall callback
                          '(("users" .
                             ((("id_str" . "1")
                               ("screen_name" . "emacs")
                               ("name" . "Emacs"))
                              (("id_str" . "1")
                               ("screen_name" . "duplicate")
                               ("name" . "Duplicate"))
                              (("id_str" . "2")
                               ("screen_name" . "emacslife")
                               ("name" . "Emacs Life")))))))))
      (chirp-backend-search-users
       "@em"
       (lambda (items _envelope)
         (setq users items))
       nil 5))
    (should (eq service 'web))
    (should (equal path "1.1/search/typeahead.json"))
    (should (equal (alist-get "q" query nil nil #'string=) "em"))
    (should (equal (mapcar (lambda (user) (plist-get user :handle)) users)
                   '("emacs" "emacslife")))))

(ert-deftest chirp-backend-translate-uses-direct-strato-response ()
  "Tweet translation should adapt the authenticated X Strato response."
  (let (service path result failure)
    (cl-letf (((symbol-function 'chirp-x-api-request)
               (lambda (request-service request-path callback &rest _options)
                 (setq service request-service
                       path request-path)
                 (funcall callback
                          '(("id_str" . "123")
                            ("translation" . "你好 &amp;amp; 欢迎\r")
                            ("destinationLanguage" . "zh")
                            ("translationState" . "Success"))))))
      (chirp-backend-translate
       "123" "zh"
       (lambda (data _envelope)
         (setq result data))
       (lambda (message)
         (setq failure message))))
    (should-not failure)
    (should (eq service 'web))
    (should (string-match-p
             (concat "tweetId=123,destinationLanguage=Some(zh),"
                     "translationSource=Some(Google)")
             path))
    (should (equal (chirp-get result "id") "123"))
    (should (equal (chirp-get result "translation") "你好 &amp; 欢迎"))
    (should (equal (chirp-get result "destinationLanguage") "zh"))))

(ert-deftest chirp-backend-whoami-cache-reuses-fresh-results ()
  "Fresh cached whoami results should avoid a second backend request."
  (let ((chirp-backend-read-cache-ttl 15)
        (request-count 0)
        first second)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'chirp-x-graphql-request)
                     (lambda (_operation _variables callback &rest _options)
                       (setq request-count (1+ request-count))
                       (funcall callback
                                (chirp-backend-test--viewer-payload
                                 "alice" "42")))))
            (chirp-backend-whoami
             (lambda (user _envelope)
               (setq first user)))
            (chirp-backend-whoami
             (lambda (user _envelope)
               (setq second user)))
            (should (= request-count 1))
            (should (equal first second))
            (should-not (eq first second))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-likes-resolves-user-id-and-marks-results ()
  "Likes should resolve the handle and mark every direct timeline result."
  (let (operation variables tweets)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (handle callback &optional _errback)
                 (should (equal handle "@Alice"))
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--user-timeline-payload
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "1")
                       ("legacy" . (("full_text" . "one")))))
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "2")
                       ("legacy" . (("full_text" . "two")))))))))))
      (chirp-backend-likes
       "@Alice"
       (lambda (items _envelope)
         (setq tweets items))))
    (should (equal (plist-get operation :name) "Likes"))
    (should (equal (alist-get "userId" variables nil nil #'string=) "42"))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :liked-p)) tweets)
                   '(t t)))))

(ert-deftest chirp-backend-user-replies-shapes-direct-request ()
  "Replies should use X search with a handle filter and cursor."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--timeline-payload
                   '("data" "search_by_raw_query" "search_timeline" "timeline")
                   nil)))))
      (chirp-backend-user-replies "@Alice" #'ignore nil 10 "cursor-prev"))
    (should (equal (plist-get operation :name) "SearchTimeline"))
    (should (equal (alist-get "rawQuery" variables nil nil #'string=)
                   "from:Alice filter:replies"))
    (should (equal (alist-get "count" variables nil nil #'string=) 10))
    (should (equal (alist-get "cursor" variables nil nil #'string=)
                   "cursor-prev"))))

(ert-deftest chirp-backend-user-highlights-shapes-direct-request ()
  "Highlights should select their X operation and requested count."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (_handle callback &optional _errback)
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback
                          (chirp-backend-test--user-timeline-payload nil)))))
      (chirp-backend-user-highlights "@Alice" #'ignore nil 7))
    (should (equal (plist-get operation :name) "UserHighlightsTweets"))
    (should (equal (alist-get "count" variables nil nil #'string=) 7))))

(ert-deftest chirp-backend-user-media-shapes-direct-request ()
  "Media should select its X operation and disable promoted content."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (_handle callback &optional _errback)
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback
                          (chirp-backend-test--user-timeline-payload nil)))))
      (chirp-backend-user-media "@Alice" #'ignore nil 6))
    (should (equal (plist-get operation :name) "UserMedia"))
    (should (equal (alist-get "count" variables nil nil #'string=) 6))
    (should (eq (alist-get "includePromotedContent" variables nil nil #'string=)
                :json-false))))

(ert-deftest chirp-backend-followers-resolve-user-and-normalize-rest-users ()
  "Followers should resolve the handle before requesting the direct REST list."
  (let ((chirp-backend-read-cache-ttl 0)
        service path query users envelope)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (handle callback &optional _errback)
                 (should (equal handle "@Bob"))
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-api-request)
               (lambda (request-service request-path callback &rest options)
                 (setq service request-service
                       path request-path
                       query (plist-get options :query))
                 (funcall callback
                          '(("users" .
                             ((("id_str" . "1")
                               ("screen_name" . "alice")
                               ("name" . "Alice"))))
                            ("next_cursor_str" . "next"))))))
      (chirp-backend-followers
       "@Bob"
       (lambda (items response-envelope)
         (setq users items
               envelope response-envelope))))
    (should (eq service 'web))
    (should (equal path "1.1/followers/list.json"))
    (should (equal (alist-get "user_id" query nil nil #'string=) "42"))
    (should (equal (plist-get (car users) :handle) "alice"))
    (should (equal (chirp-backend-envelope-next-cursor envelope) "next"))))

(ert-deftest chirp-backend-following-users-use-the-friends-rest-list ()
  "Following users should share the direct relationship-list adapter."
  (let ((chirp-backend-read-cache-ttl 0)
        path users)
    (cl-letf (((symbol-function 'chirp-backend-user)
               (lambda (_handle callback &optional _errback)
                 (funcall callback '(:id "42") nil)))
              ((symbol-function 'chirp-x-api-request)
               (lambda (_service request-path callback &rest _options)
                 (setq path request-path)
                 (funcall callback
                          '(("users" .
                             ((("id_str" . "1")
                               ("screen_name" . "alice")
                               ("name" . "Alice")))))))))
      (chirp-backend-following-users
       "@Bob"
       (lambda (items _envelope)
         (setq users items))))
    (should (equal path "1.1/friends/list.json"))
    (should (equal (plist-get (car users) :handle) "alice"))))

(ert-deftest chirp-backend-list-normalizes-list-urls ()
  "List requests should pass a URL's numeric id to direct X GraphQL."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--timeline-payload
                   '("data" "list" "tweets_timeline" "timeline") nil)))))
      (chirp-backend-list "https://x.com/i/lists/1956792682412345678" #'ignore))
    (should (equal (plist-get operation :name) "ListLatestTweetsTimeline"))
    (should (equal (alist-get "listId" variables nil nil #'string=)
                   "1956792682412345678"))))

(ert-deftest chirp-backend-thread-passes-explicit-max-results ()
  "Thread requests should pass Chirp's explicit limit and focus id to X."
  (let ((chirp-thread-max-results 20)
        operation
        variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--thread-payload
                   (list
                    (chirp-backend-test--timeline-entry
                     '(("rest_id" . "123")
                       ("legacy" . (("full_text" . "hello")))))))))))
      (chirp-backend-thread "123" #'ignore))
    (should (equal (plist-get operation :name) "TweetDetail"))
    (should (equal (alist-get "focalTweetId" variables nil nil #'string=)
                   "123"))
    (should (equal (alist-get "count" variables nil nil #'string=) 20))))

(ert-deftest chirp-backend-thread-accepts-a-retweet-wrapper-id ()
  "A retweet permalink should match its raw wrapper after normalization."
  (let ((chirp-backend-read-cache-ttl 0)
        result
        failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (let* ((original
                         '(("rest_id" . "200")
                           ("legacy" . (("full_text" . "original")))))
                        (wrapper
                         `(("rest_id" . "100")
                           ("legacy" .
                            (("full_text" . "RT @user: original")
                             ("retweeted_status_result" .
                              (("result" . ,original))))))))
                   (funcall
                    callback
                    (chirp-backend-test--thread-payload
                     (list (chirp-backend-test--timeline-entry wrapper))))))))
      (chirp-backend-thread
       "100"
       (lambda (tweets _envelope)
         (setq result tweets))
       (lambda (message)
         (setq failure message))))
    (should-not failure)
    (should (equal (plist-get (car result) :id) "200"))))

(ert-deftest chirp-backend-article-uses-direct-rich-content-operation ()
  "Article enrichment should request and normalize direct X article data."
  (let ((chirp-backend-read-cache-ttl 0)
        operation variables result failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--payload-at-path
                   '("data" "tweetResult" "result")
                   '(("rest_id" . "123")
                     ("legacy" . (("full_text" . "Article preview")))
                     ("article" .
                      (("article_results" .
                        (("result" .
                          (("title" . "Longform")
                           ("plain_text" . "Full article body.")))))))))))))
      (chirp-backend-article
       "123"
       (lambda (tweet _envelope)
         (setq result tweet))
       (lambda (message)
         (setq failure message))))
    (should-not failure)
    (should (equal (plist-get operation :name) "TweetResultByRestId"))
    (should (equal (alist-get "tweetId" variables nil nil #'string=) "123"))
    (should (equal (plist-get result :article-title) "Longform"))
    (should (equal (plist-get result :article-text) "Full article body."))))

(ert-deftest chirp-backend-tweet-weighted-length-handles-wide-text-emoji-and-urls ()
  "Create routing should follow X weighting for common text forms."
  (should (= (chirp-backend-tweet-weighted-length (make-string 280 ?x))
             280))
  (should (= (chirp-backend-tweet-weighted-length (make-string 141 ?你))
             282))
  (should (= (chirp-backend-tweet-weighted-length "👩‍👩‍👧‍👦") 2))
  (should (= (chirp-backend-tweet-weighted-length
              (apply #'concat (make-list 57 "←‍←")))
             285))
  (should (= (chirp-backend-tweet-weighted-length
              (concat "see https://example.com/" (make-string 400 ?x)))
             27)))

(ert-deftest chirp-backend-compose-creates-a-short-direct-post ()
  "Short drafts should use CreateTweet and return the created tweet ID."
  (let (operation variables result envelope failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--payload-at-path
                   '("data" "create_tweet" "tweet_results" "result")
                   '(("rest_id" . "123")))))))
      (chirp-backend-compose
       :kind 'post :text "hello" :attachments nil
       :callback (lambda (created raw-envelope)
                   (setq result created
                         envelope raw-envelope))
       :errback (lambda (message)
                  (setq failure message))))
    (should-not failure)
    (should (equal (plist-get operation :name) "CreateTweet"))
    (should (equal (plist-get result :id) "123"))
    (should envelope)
    (should (equal (alist-get "tweet_text" variables nil nil #'string=)
                   "hello"))
    (should (vectorp
             (chirp-get-in variables '("media" "media_entities"))))
    (should (vectorp
             (chirp-get variables "semantic_annotation_ids")))))

(ert-deftest chirp-backend-compose-builds-reply-quote-and-media-variables ()
  "Replies and quotes should share create routing with structured targets."
  (let ((file (make-temp-file "chirp-backend-compose-" nil ".png")))
    (unwind-protect
        (dolist (kind '(reply quote))
          (let (variables uploaded result)
            (cl-letf (((symbol-function 'chirp-x-upload-media)
                       (lambda (path callback &rest _options)
                         (should (equal path file))
                         (setq uploaded t)
                         (funcall callback "media-1")))
                      ((symbol-function 'chirp-x-graphql-request)
                       (lambda (_operation request-variables callback
                                &rest _options)
                         (setq variables request-variables)
                         (funcall
                          callback
                          (chirp-backend-test--payload-at-path
                           '("data" "create_tweet" "tweet_results" "result")
                           '(("rest_id" . "456")))))))
              (chirp-backend-compose
               :kind kind :text "hello" :target-id "99" :target-handle "alice"
               :attachments (list file)
               :callback (lambda (created _envelope)
                           (setq result created))))
            (should uploaded)
            (should (equal (plist-get result :id) "456"))
            (let* ((entities
                    (chirp-get-in variables '("media" "media_entities")))
                   (entity (aref entities 0)))
              (should (= (length entities) 1))
              (should (equal (chirp-get entity "media_id") "media-1"))
              (should (vectorp (chirp-get entity "tagged_users"))))
            (pcase kind
              ('reply
               (should
                (equal
                 (chirp-get-in
                  variables '("reply" "in_reply_to_tweet_id"))
                 "99")))
              ('quote
               (should
                (equal (chirp-get variables "attachment_url")
                       "https://x.com/alice/status/99"))))))
      (should (equal (chirp-get (chirp-backend--compose-variables
                                 'quote "hello" "99" nil nil)
                                "attachment_url")
                     "https://x.com/i/status/99"))
      (delete-file file))))

(ert-deftest chirp-backend-compose-routes-wide-text-to-create-note-tweet ()
  "Drafts over 280 weighted units should use CreateNoteTweet."
  (let (operation variables result)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                        &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall
                  callback
                  (chirp-backend-test--payload-at-path
                   '("data" "notetweet_create" "tweet_results" "result")
                   '(("rest_id" . "789")))))))
      (chirp-backend-compose
       :kind 'post :text (make-string 141 ?你) :attachments nil
       :callback (lambda (created _envelope)
                   (setq result created))))
    (should (equal (plist-get operation :name) "CreateNoteTweet"))
    (should (assoc-string "disallowed_reply_options" variables t))
    (should (equal (plist-get result :id) "789"))
    (should (assoc-string
             "longform_notetweets_creation_enabled"
             (plist-get operation :features) t))))

(ert-deftest chirp-backend-compose-does-not-retry-an-ambiguous-create-failure ()
  "A failed create request should be surfaced after exactly one attempt."
  (let ((request-count 0)
        failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables _callback &rest options)
                 (setq request-count (1+ request-count))
                 (funcall (plist-get options :errback) "Connection reset"))))
      (chirp-backend-compose
       :kind 'post :text "hello" :attachments nil :callback #'ignore
       :errback (lambda (message)
                  (setq failure message))))
    (should (= request-count 1))
    (should (equal failure "Connection reset"))))

(ert-deftest chirp-backend-compose-rejects-a-success-without-a-tweet-id ()
  "An empty create result should be treated as a failed publication."
  (let (success failure)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback '(("data" . (("create_tweet" . nil))))))))
      (chirp-backend-compose
       :kind 'post :text "hello" :attachments nil
       :callback (lambda (&rest _args)
                   (setq success t))
       :errback (lambda (message)
                  (setq failure message))))
    (should-not success)
    (should (string-match-p "did not return a created tweet ID" failure))))

(ert-deftest chirp-backend-request-rejects-unknown-actions-without-a-process ()
  "Unknown legacy action names should fail instead of starting a process."
  (let (failure)
    (chirp-backend-request
     '("unknown") #'ignore
     (lambda (message)
       (setq failure message)))
    (should (equal failure "Unknown Chirp backend action: unknown"))))

(ert-deftest chirp-home-paginates-after-the-feed-reorders ()
  "Keep the unread tail and cursor when refreshing changes the first page."
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (chirp-default-max-results 2)
          (chirp-timeline-load-more-step 2)
          calls)
      (cl-letf (((symbol-function 'chirp-buffer) (lambda () buffer))
                ((symbol-function 'chirp-display-buffer) #'ignore)
                ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                ((symbol-function 'chirp-x-graphql-request)
                 (lambda (_op variables done &rest _)
                   (let* ((cursor (cdr (assoc "cursor" variables)))
                          (ids (cond (cursor '("4" "5")) (calls '("9")) (t '("1" "2" "3"))))
                          (next (cond (cursor nil) (calls "new-bottom") (t "original-bottom"))))
                     (push cursor calls)
                     (funcall done
                              (chirp-backend-test--home-timeline-payload
                               (append
                                (mapcar (lambda (id)
                                          (chirp-backend-test--timeline-entry
                                           `(("rest_id" . ,id) ("legacy" . (("full_text" . ,id))))))
                                        ids)
                                (when next (list (chirp-backend-test--bottom-cursor next))))))))))
        (chirp-timeline-open-home)
        (chirp-refresh)
        (should (chirp-restore-point-anchor "2"))
        (call-interactively (key-binding (kbd "n")))
        (should (equal calls '(nil nil)))
        (should (equal (chirp-entry-id-at-point) "2"))
        (should (equal chirp--timeline-next-cursor "original-bottom"))
        (call-interactively (key-binding (kbd "N")))
        (should (equal (reverse calls) '(nil nil "original-bottom")))
        (should (equal (mapcar #'chirp-tweet-key (chirp-timeline--buffer-tweets buffer))
                       '("9" "1" "2" "3" "4" "5")))
        (should chirp--timeline-exhausted-p)))))

(ert-deftest chirp-backend-bookmarks-continues-an-oversized-page ()
  "A page larger than requested must not skip bookmarks on continuation."
  (let (requests ids cursor)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_op variables cb &rest _)
                 (push (cdr (assoc "cursor" variables)) requests)
                 (funcall cb
                          (chirp-backend-test--timeline-payload
                           '("data" "bookmark_timeline_v2" "timeline")
                           (append
                            (mapcar (lambda (id)
                                      (chirp-backend-test--timeline-entry
                                       `(("rest_id" . ,id)
                                         ("legacy" . (("full_text" . "example"))))))
                                    '("71" "72"))
                            (list (chirp-backend-test--bottom-cursor "remote"))))))))
      (dotimes (_ 2)
        (chirp-backend-bookmarks
         (lambda (tweets envelope)
           (push (plist-get (car tweets) :id) ids)
           (setq cursor (chirp-backend-envelope-next-cursor envelope)))
         #'ert-fail 1 cursor)))
    (should (equal (nreverse ids) '("71" "72")))
    (should (equal requests '(nil)))
    (should (equal cursor "remote"))))

(ert-deftest chirp-backend-bookmark-transport-reads-http-response ()
  "Exercise the public bookmark action down to signed HTTP and back."
  (let ((material (list :key (make-string 48 ?a) :animation "1234abcd"))
        prepared headers user-agent method outcome)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda () '(:auth-token "example" :ct0 "csrf" :bearer-token "public")))
              ((symbol-function 'chirp-x-transaction-prepare)
               (lambda (_creds _ua cb _err)
                 (setq prepared t)
                 (funcall cb material)))
              ((symbol-function 'url-retrieve)
               (lambda (url cb &optional _args _silent inhibit-cookies)
                 (should inhibit-cookies)
                 (should (string-suffix-p "/CreateBookmark" url))
                 (should (equal url-request-method "POST"))
                 (setq method url-request-method
                       user-agent url-user-agent
                       headers url-request-extra-headers)
                 (should (equal (chirp-get-in (json-parse-string url-request-data
                                                                :object-type 'alist)
                                               '("variables" "tweet_id"))
                                "123"))
                 (with-current-buffer (generate-new-buffer " *chirp test response*")
                   (setq-local url-http-response-status 200)
                   (setq-local url-http-end-of-headers (point-marker))
                   (insert "{\"data\":{\"tweet_bookmark_put\":\"Done\"}}")
                   (funcall cb nil)))))
      (chirp-backend-request '("bookmark" "123")
                             (lambda (data _)
                               (setq outcome (chirp-get-in data '("data" "tweet_bookmark_put"))))
                             #'ert-fail))
    (should prepared)
    (should-not (multibyte-string-p method))
    (should (equal user-agent chirp-x-user-agent))
    (should-not (multibyte-string-p user-agent))
    (should-not (assoc "User-Agent" headers))
    (should-not (assoc "Accept" headers))
    (should (assoc "X-Client-Transaction-Id" headers))
    (should (assoc "X-Client-UUID" headers))
    (should (equal outcome "Done"))))

(ert-deftest chirp-x-post-with-utf8-body-builds-unibyte-http-request ()
  "Posting non-ASCII UTF-8 JSON must produce a strictly unibyte HTTP request."
  (url-do-setup)
  (let ((material (list :key (make-string 48 ?a) :animation "1234abcd"))
        raw-request)
    (with-temp-buffer
      (cl-letf (((symbol-function 'chirp-x-credentials)
                 (lambda () '(:auth-token "example" :ct0 "csrf" :bearer-token "public")))
                ((symbol-function 'chirp-x-transaction-prepare)
                 (lambda (_creds _ua cb _err) (funcall cb material)))
                ((symbol-function 'url-use-cookies) (lambda (_) nil))
                ((symbol-function 'url-retrieve)
                 (lambda (url cb &rest _)
                   (let ((m url-request-method)
                         (ua url-user-agent)
                         (priv url-privacy-level)
                         (acc url-mime-accept-string)
                         (cs url-mime-charset-string)
                         (lang url-mime-language-string)
                         (ext url-extensions-header)
                         (hdrs url-request-extra-headers)
                         (data url-request-data))
                     (with-temp-buffer
                       (setq-local url-http-target-url (url-generic-parse-url url))
                       (setq-local url-http-method m)
                       (setq-local url-user-agent ua)
                       (setq-local url-privacy-level priv)
                       (setq-local url-mime-accept-string acc)
                       (setq-local url-mime-charset-string cs)
                       (setq-local url-mime-language-string lang)
                       (setq-local url-http-extensions-header ext)
                       (setq-local url-http-proxy nil)
                       (setq-local url-http-referer nil)
                       (setq-local url-http-extra-headers hdrs)
                       (setq-local url-http-data data)
                       (setq raw-request (url-http-create-request))))
                   (with-current-buffer (generate-new-buffer " *chirp utf8*")
                     (setq-local url-http-response-status 200)
                     (setq-local url-http-end-of-headers (point-marker))
                     (insert "{\"data\":{\"create_tweet\":{\"tweet_results\":{\"result\":{\"rest_id\":\"42\"}}}}}")
                     (funcall cb nil)))))
        (chirp-backend-compose
         :kind 'quote :text "中文测试 🚀" :target-id "99" :target-handle "alice"
         :attachments nil :callback #'ignore :errback #'ert-fail)))
    (should (stringp raw-request))
    (should-not (multibyte-string-p raw-request))
    (should (= (length raw-request) (string-bytes raw-request)))
    (should (string-match-p (regexp-quote (concat "User-Agent: " chirp-x-user-agent "\r\n"))
                            raw-request))
    (should-not (string-match-p "URL/Emacs" raw-request))
    (should (equal (chirp-x--sanitize-error "Multibyte text in HTTP request: Cookie: auth_token=secret")
                   "X request encoding error"))
    (should-not (string-match-p "secret"
                                (chirp-x--sanitize-error "Cookie: auth_token=secret\nX-Csrf-Token: secret")))))

(ert-deftest chirp-backend-search-http-error-is-a-read-failure ()
  (let (failure)
    (cl-letf (((symbol-function 'chirp-x-credentials)
               (lambda () '(:auth-token "example" :ct0 "csrf" :bearer-token "public")))
              ((symbol-function 'chirp-x-transaction-prepare)
               (lambda (&rest _)
                 (ert-fail "Read-only search should not prepare transaction metadata")))
              ((symbol-function 'url-retrieve)
               (lambda (url callback &rest _)
                 (should (string-suffix-p "/SearchTimeline" url))
                 (should (equal url-request-method "POST"))
                 (with-current-buffer (generate-new-buffer " *chirp search response*")
                   (setq-local url-http-response-status 503)
                   (setq-local url-http-end-of-headers (point-marker))
                   (funcall callback nil)))))
      (chirp-backend-search "emacs" #'ert-fail (lambda (reason) (setq failure reason))))
    (should (string-match-p "HTTP 503" failure))
    (should-not (string-match-p "write" failure))))

(ert-deftest chirp-x-reports-only-the-limit-headers-of-the-failed-request ()
  "Surface quota headers on rate-limit and automation errors without leaking cookies."
  (let* ((body "{\"errors\":[{\"message\":\"Write refused\",\"code\":344}]}")
         (headers "X-Rate-Limit-Remaining: 17\r\nRetry-After: 60")
         (expected "X request failed [x-rate-limit-remaining=17, retry-after=60]: Write refused (344)"))
    (with-temp-buffer
      (insert "HTTP/1.1 200 OK\r\nSet-Cookie: private\r\n" headers "\r\n\r\n")
      (setq-local url-http-end-of-headers (point-marker) url-http-response-status 200)
      (insert body)
      (should (equal (chirp-x--decode-response nil nil 'post) (list :error expected))))))

(ert-deftest chirp-thread-paginates-after-locating-the-focus ()
  (let ((chirp-thread-max-results 2)
        (chirp-backend-read-cache-ttl 0)
        ids cursor)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_op _args cb &rest _)
                 (funcall cb (chirp-backend-test--thread-payload
                              (mapcar (lambda (id)
                                        (chirp-backend-test--timeline-entry
                                         (list (cons "rest_id" id)
                                               '("legacy" ("full_text" . "text")))))
                                      '("1" "2" "3")))))))
      (chirp-backend-thread
       "3" (lambda (items envelope)
             (setq ids (mapcar #'chirp-tweet-key items)
                   cursor (chirp-backend-envelope-next-cursor envelope))) #'ert-fail)
      (should (equal ids '("3" "1")))
      (chirp-backend-thread "3" (lambda (items _) (should (equal (mapcar #'chirp-tweet-key items) '("2"))))
                            #'ert-fail cursor))))

(ert-deftest chirp-x-distinguishes-partial-reads-from-rejected-writes ()
  (dolist (case '((200 get :success nil) (200 post :error nil)
                  (403 post :error nil) (503 post :error t)))
    (pcase-let ((`(,status ,method ,outcome ,uncertain) case))
      (with-temp-buffer
        (setq-local url-http-response-status status)
        (insert "{\"data\":{\"home\":{\"items\":[1]}},\"errors\":[{\"message\":\"Unavailable\"}]}")
        (let ((result (chirp-x--decode-response nil nil method)))
          (should (eq (car result) outcome))
          (if (eq outcome :success)
              (should (equal (chirp-get-in (cadr result) '("data" "home" "items")) '(1)))
            (should (string-match-p "Unavailable" (cadr result)))
            (should (eq uncertain (not (null (string-match-p "outcome is unknown" (cadr result))))))))))))

(ert-deftest chirp-backend-related-module-preserves-context-and-note-text ()
  (let* ((module (chirp-backend-test--module-entry
                  '((rest_id . "8") (legacy . ((full_text . "preview")))
                    (note_tweet . ((note_tweet_results . ((result . ((text . "full text")))))))))))
    (setcdr (assoc "entryId" module) "tweetdetailrelatedtweets-8")
    (let ((tweet (car (chirp-collect-top-level-tweets (chirp-backend--timeline-entry-tweets module)))))
      (should (eq (plist-get tweet :timeline-context) 'related))
      (should (equal (plist-get tweet :text) "full text")))))

(ert-deftest chirp-backend-native-follow-relationship-preserves-false-and-count ()
  (let ((rest (chirp-normalize-user '((screen_name . "a") (following . t) (followed_by . t))))
        (user (chirp-normalize-user
               '((rest_id . "42") (legacy . ((following . t) (friends_count . 12)))
                 (relationship_perspectives . ((following . nil)))))))
    (should (plist-get rest :viewer-following-p))
    (should (plist-get rest :viewer-followed-by-p))
    (should-not (plist-get rest :following))
    (should (= (plist-get user :following) 12))
    (should-not (plist-get user :viewer-following-p))))

(ert-deftest chirp-upload-validates-a-symlink-before-reading-and-chunks-its-target ()
  (let* ((dir (make-temp-file "chirp-image-" t))
         (image (expand-file-name "target.png" dir))
         (link (expand-file-name "link.png" dir))
         (chirp-x--upload-chunk-size 2)
         calls done)
    (unwind-protect
        (progn
          (with-temp-file image (insert "12345"))
          (make-symbolic-link image link)
          (cl-letf (((symbol-function 'chirp-x--request)
                     (lambda (url _method callback &rest options)
                       (should (string-prefix-p "https://upload.x.com/i/media/" url))
                       (let ((body (plist-get options :data)))
                         (cond
                          ((string-prefix-p "command=INIT" body)
                           (should (string-match-p "total_bytes=5" body))
                           (funcall callback '((media_id_string . "42"))))
                          ((string-prefix-p "command=FINALIZE" body) (funcall callback nil))
                          (t (push body calls) (funcall callback nil)))))))
            (chirp-x-upload-media link (lambda (id) (setq done id)) :errback #'ert-fail))
          (should (equal done "42"))
          (should (= (length calls) 3))
          (let ((chirp-x--upload-image-limit 4))
            (cl-letf (((symbol-function 'chirp-x--file-bytes)
                       (lambda (_) (ert-fail "Oversized file was read"))))
              (let (reason)
                (chirp-x-upload-media link #'ert-fail :errback (lambda (message) (setq reason message)))
                (should (string-match-p "at most" reason))))))
      (delete-directory dir t))))

(ert-deftest chirp-x-clears-transaction-cache-for-auth-and-automation-errors ()
  (dolist (case '((403 32 t t) (403 179 nil nil)
                  (403 353 t t) (200 89 t t)
                  (401 32 t t) (200 226 t nil)
                  (403 226 t nil)))
    (pcase-let ((`(,status ,code ,cleared ,login-hint) case)
                (chirp-x-transaction--cache '("hash" 9999999999 (:key "k" :animation "a"))))
      (with-temp-buffer
        (setq-local url-http-response-status status)
        (insert (format "{\"errors\":[{\"code\":%d,\"message\":\"Rejected\"}]}" code))
        (let ((result (chirp-x--decode-response nil nil 'post)))
          (should (eq (car result) :error))
          (should (eq (null chirp-x-transaction--cache) cleared))
          (should (eq (not (null (string-match-p "chirp-login" (cadr result)))) login-hint))
          (should-not (string-match-p "outcome is unknown" (cadr result))))))))

(ert-deftest chirp-x-query-id-overrides-replace-builtin-graphql-id ()
  "Configured `chirp-x-query-id-overrides' should replace built-in query IDs."
  (let ((chirp-x-query-id-overrides '(("HomeTimeline" . "customQueryId123")))
        requested-url)
    (cl-letf (((symbol-function 'chirp-x--request)
               (lambda (url _method cb &rest _)
                 (setq requested-url url)
                 (funcall cb '((data . nil))))))
      (chirp-x-graphql-request
       '(:name "HomeTimeline" :query-id "oldId" :method get)
       '(("count" . 20))
       #'ignore))
    (should (string-match-p "/customQueryId123/HomeTimeline\\?" requested-url))))

(defun chirp-backend-test--transaction-html ()
  "Return a minimal valid X home HTML document for transaction tests."
  (let* ((key (base64-encode-string (apply #'unibyte-string (number-sequence 0 47)) t))
         (row " 10,20 30,40 50,60 h 70 s 80,90 100,110")
         (path (concat "M 10,30 C" (string-join (make-list 16 row) "C")))
         (svgs (mapconcat
                (lambda (idx)
                  (format "<svg id=\"loading-x-anim-%d\"><g><path d=\"M0,0\"/><path d=\"%s\"/></g></svg>"
                          idx path))
                '(0 1 2 3)
                ""))
         (script "x.p=\"https://abs.twimg.com/responsive-web/client-web/\";x.u=e=>(({12:\"ondemand.s\"})[e]||e)+\".\"+({12:\"0123456789abcdef\"})[e]+\"a.js\""))
    (format "<html><head><meta name=\"twitter-site-verification\" content=\"%s\"/></head><body>%s<script>%s</script></body></html>"
            key svgs script)))

(ert-deftest chirp-x-transaction-bootstrap-material-and-id-are-deterministic ()
  "Transaction token generation should parse HTML/script fixtures and fail closed."
  (let* ((html (chirp-backend-test--transaction-html))
         (script "parseInt(k[1], 16); parseInt(k[2], 16); parseInt(k[3], 16); parseInt(k[4], 16);")
         (bootstrap (chirp-x-transaction-bootstrap html))
         (material (chirp-x-transaction-material bootstrap script))
         (id-a (chirp-x-transaction-id material "POST" "/i/api/graphql/abc/CreateTweet" 1700000000 42))
         (id-b (chirp-x-transaction-id material "POST" "/i/api/graphql/abc/CreateTweet" 1700000000 42))
         (id-c (chirp-x-transaction-id material "POST" "/i/api/graphql/abc/FavoriteTweet" 1700000000 42)))
    (should (equal (plist-get bootstrap :script-url)
                   "https://abs.twimg.com/responsive-web/client-web/ondemand.s.0123456789abcdefa.js"))
    (should (= (length (plist-get material :key)) 48))
    (should (string-match-p "\\`[A-Za-z0-9+/]+\\'" id-a))
    (should (equal id-a id-b))
    (should-not (equal id-a id-c))
    (should-error (chirp-x-transaction-bootstrap "<html><body>no meta</body></html>"))))

(ert-deftest chirp-x-transaction-prepare-caches-by-cookies-and-isolates-cdn-request ()
  "Bootstrap fetches should send cookies only to x.com and reuse fresh cache entries."
  (let ((chirp-x-transaction--cache nil)
        (chirp-x-transaction--pending nil)
        (html (chirp-backend-test--transaction-html))
        (script "parseInt(k[1], 16); parseInt(k[2], 16); parseInt(k[3], 16); parseInt(k[4], 16);")
        (creds-a '(:auth-token "token-a" :ct0 "csrf-a" :bearer-token "public"))
        (creds-b '(:auth-token "token-b" :ct0 "csrf-b" :bearer-token "public"))
        requests
        materials)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url cb &optional _args _silent inhibit-cookies)
                 (should inhibit-cookies)
                 (push (cons url url-request-extra-headers) requests)
                 (with-current-buffer (generate-new-buffer " *chirp tx*")
                   (setq-local url-http-response-status 200)
                   (insert "\n")
                   (setq-local url-http-end-of-headers (copy-marker (1- (point))))
                   (insert (if (string-prefix-p "https://x.com/" url) html script))
                   (funcall cb nil)))))
      (chirp-x-transaction-prepare creds-a "UA" (lambda (m) (push m materials)) #'ert-fail)
      (chirp-x-transaction-prepare creds-a "UA" (lambda (m) (push m materials)) #'ert-fail)
      (should (= (length requests) 2))
      (should (= (length materials) 2))
      (let ((cdn-req (car requests))
            (home-req (cadr requests)))
        (should (equal (car home-req) "https://x.com/home"))
        (should (assoc "Cookie" (cdr home-req)))
        (should (string-prefix-p "https://abs.twimg.com/" (car cdn-req)))
        (should-not (assoc "Cookie" (cdr cdn-req))))
      (chirp-x-transaction-prepare creds-b "UA" (lambda (m) (push m materials)) #'ert-fail)
      (should (= (length requests) 4)))))

(ert-deftest chirp-x-credentials-unifies-env-auth-file-and-auth-source ()
  "Credentials should follow env > auth-file > auth-source identically in GUI and TUI."
  (let* ((dir (make-temp-file "chirp-auth-" t))
         (chirp-x-auth-file (expand-file-name "auth.json" dir))
         (env nil)
         (auth-source-map nil))
    (unwind-protect
        (progn
          (should (equal (chirp-x-parse-cookie-input
                          "Cookie: guest_id=1; auth_token=tok1; ct0=cs1; twid=u%3D1")
                         '("tok1" . "cs1")))
          (should (equal (chirp-x-parse-cookie-input
                          (concat "curl 'https://x.com/i/api/graphql/id/HomeTimeline' \\\n"
                                  "  -H 'cookie: guest_id=1; auth_token=curl-tok; ct0=curl-ct0' \\\n"
                                  "  -H 'x-csrf-token: curl-ct0'"))
                         '("curl-tok" . "curl-ct0")))
          (should (equal (chirp-x-parse-cookie-input
                          (concat "curl ^\"https://x.com/i/api/graphql/id/HomeTimeline^\" ^\r\n"
                                  "  -H ^\"cookie: auth_token=win-tok; ct0=win-ct0^\""))
                         '("win-tok" . "win-ct0")))
          (should-not (chirp-x-parse-cookie-input "plain-token-only"))
          (dolist (graphic '(nil t))
            (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) graphic))
                      ((symbol-function 'getenv) (lambda (name) (cdr (assoc name env))))
                      ((symbol-function 'auth-source-pick-first-password)
                       (lambda (&rest spec)
                         (cdr (assoc (plist-get spec :user) auth-source-map)))))
              (setq env nil auth-source-map nil)
              (chirp-x-clear-credentials)
              (should-not (chirp-x-credentials))
              (setq auth-source-map '(("auth_token" . "as-tok") ("ct0" . "as-ct0")))
              (should (equal (plist-get (chirp-x-credentials) :auth-token) "as-tok"))
              (chirp-x-save-credentials "file-tok" "file-ct0")
              (unless (eq system-type 'windows-nt)
                (should (= (logand (file-modes chirp-x-auth-file) #o777) #o600)))
              (should (equal (plist-get (chirp-x-credentials) :auth-token) "file-tok"))
              (setq env '(("TWITTER_AUTH_TOKEN" . "env-tok") ("TWITTER_CT0" . "env-ct0")))
              (should (equal (plist-get (chirp-x-credentials) :auth-token) "env-tok"))
              (setq env nil)
              (chirp-x-clear-credentials)
              (should-not (file-exists-p chirp-x-auth-file))
              (should (equal (plist-get (chirp-x-credentials) :auth-token) "as-tok")))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest chirp-x-cookies-pbkdf2-and-chromium-decryption-round-trip ()
  "PBKDF2-SHA1 and Chromium cookie decryption should handle legacy and v24+ blobs."
  ;; RFC 6070 standard PBKDF2-HMAC-SHA1 vectors.
  (should (equal (chirp-x-cookies--bytes-to-hex
                  (chirp-x-cookies--pbkdf2-sha1 "password" "salt" 1 20))
                 "0c60c80f961f0e71f3a9b524af6012062fe037a6"))
  (should (equal (chirp-x-cookies--bytes-to-hex
                  (chirp-x-cookies--pbkdf2-sha1 "password" "salt" 2 20))
                 "ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957"))
  (let* ((openssl (or (and (file-executable-p "/usr/bin/openssl") "/usr/bin/openssl")
                      (executable-find "openssl")))
         (key-hex (chirp-x-cookies--bytes-to-hex
                   (chirp-x-cookies--pbkdf2-sha1 "peanuts" "saltysalt" 1 16))))
    (when openssl
      (cl-flet ((encrypt (plain)
                  (let ((coding-system-for-read 'binary)
                        (coding-system-for-write 'binary))
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert plain)
                      (should (zerop (call-process-region
                                      (point-min) (point-max) openssl t t nil
                                      "enc" "-aes-128-cbc"
                                      "-K" key-hex
                                      "-iv" chirp-x-cookies--chromium-iv)))
                      (concat "v10" (buffer-string))))))
        (let ((legacy (encrypt "tok-legacy-123"))
              (v24 (encrypt (concat (secure-hash 'sha256 ".x.com" nil nil t)
                                    "tok-v24-456"))))
          (should (equal (chirp-x-cookies--decrypt-chromium-value
                          legacy ".x.com" "peanuts" 1)
                         "tok-legacy-123"))
          (should (equal (chirp-x-cookies--decrypt-chromium-value
                          v24 ".x.com" "peanuts" 1)
                         "tok-v24-456")))))))

(ert-deftest chirp-x-cookies-import-reads-sqlite-and-prefers-newest-browser ()
  "Importer should read unexpired SQLite cookies and compare epochs accurately."
  (when (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (let* ((root (make-temp-file "chirp-browser-test-" t))
           (ff-root (expand-file-name "ff" root))
           (ff-prof (expand-file-name "abc.default" ff-root))
           (ff-db (expand-file-name "cookies.sqlite" ff-prof))
           (cr-root (expand-file-name "chrome" root))
           (cr-prof (expand-file-name "Default" cr-root))
           (cr-db (expand-file-name "Cookies" cr-prof))
           (secret-calls 0)
           (now-unix (floor (float-time)))
           (future-unix (+ now-unix 86400))
           (past-unix (- now-unix 86400))
           (now-unix-us (* now-unix 1000000))
           (older-chrome-filetime (+ (- now-unix-us 60000000)
                                     chirp-x-cookies--chromium-epoch-offset))
           (future-chrome-filetime (+ (* future-unix 1000000)
                                      chirp-x-cookies--chromium-epoch-offset))
           (past-chrome-filetime (+ (* past-unix 1000000)
                                    chirp-x-cookies--chromium-epoch-offset)))
      (unwind-protect
          (progn
            (make-directory ff-prof t)
            (make-directory cr-prof t)
            (let ((db (sqlite-open ff-db)))
              (unwind-protect
                  (progn
                    (sqlite-execute db "CREATE TABLE moz_cookies (host TEXT, name TEXT, value TEXT, expiry INTEGER, lastAccessed INTEGER)")
                    ;; Expired row with newer lastAccessed must be ignored.
                    (sqlite-execute db "INSERT INTO moz_cookies VALUES ('.x.com', 'auth_token', 'expired-ff', ?, ?)" (list past-unix (+ now-unix-us 999999)))
                    (sqlite-execute db "INSERT INTO moz_cookies VALUES ('.x.com', 'auth_token', 'ff-auth', ?, ?)" (list future-unix now-unix-us))
                    (sqlite-execute db "INSERT INTO moz_cookies VALUES ('.x.com', 'ct0', 'ff-ct0', 0, ?)" (list now-unix-us)))
                (sqlite-close db)))
            (let ((db (sqlite-open cr-db)))
              (unwind-protect
                  (progn
                    (sqlite-execute db "CREATE TABLE cookies (host_key TEXT, name TEXT, value TEXT, encrypted_value BLOB, expires_utc INTEGER, last_access_utc INTEGER)")
                    (sqlite-execute db "INSERT INTO cookies VALUES ('.x.com', 'auth_token', 'expired-cr', '', ?, ?)" (list past-chrome-filetime (+ older-chrome-filetime 999999)))
                    (sqlite-execute db "INSERT INTO cookies VALUES ('.x.com', 'auth_token', 'cr-auth', '', ?, ?)" (list future-chrome-filetime older-chrome-filetime))
                    (sqlite-execute db "INSERT INTO cookies VALUES ('.x.com', 'ct0', 'cr-ct0', '', 0, ?)" (list older-chrome-filetime)))
                (sqlite-close db)))
            (let ((chirp-x-cookie-browser nil)
                  (specs `((firefox :label "Firefox" :engine firefox :dirs (,ff-root))
                           (chrome :label "Chrome" :engine chromium :dirs (,cr-root)
                                   :keychain-services ("Chrome Safe Storage")))))
              (cl-letf (((symbol-function 'chirp-x-cookies--specs) (lambda () specs))
                        ((symbol-function 'chirp-x-cookies--running-browsers) (lambda (_) nil))
                        ((symbol-function 'chirp-x-cookies--default-browser) (lambda (_) nil))
                        ((symbol-function 'chirp-x-cookies--chromium-secret)
                         (lambda (&rest _) (cl-incf secret-calls) '("pw" . 1))))
                ;; Firefox has newer unexpired timestamp than Chrome despite 1601 epoch offset.
                (let* ((iter (chirp-x-cookies-make-iterator))
                       (first (funcall iter))
                       (second (funcall iter)))
                  (should (equal (plist-get first :browser) "Firefox"))
                  (should (equal (plist-get first :auth-token) "ff-auth"))
                  (should (equal (plist-get first :ct0) "ff-ct0"))
                  (should (equal (plist-get second :browser) "Chrome"))
                  (should (equal (plist-get second :auth-token) "cr-auth"))
                  (should-not (funcall iter))
                  (should (= secret-calls 0)))
                ;; Selecting Chrome explicitly imports Chrome's cookies.
                (let ((imported (chirp-x-cookies-import 'chrome)))
                  (should (equal (plist-get imported :browser) "Chrome"))
                  (should (equal (plist-get imported :auth-token) "cr-auth"))))))
        (when (file-directory-p root)
          (delete-directory root t))))))

(ert-deftest chirp-x-cookies-prioritizes-running-browser-and-skips-denied-dirs ()
  "Importer should try running/default browsers first, support multi-profiles, and survive TCC errors."
  (when (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (let* ((root (make-temp-file "chirp-browser-prio-" t))
           (chrome-root (expand-file-name "chrome" root))
           (dia-root (expand-file-name "dia" root))
           (dia-prof1 (expand-file-name "Personal/Network" dia-root))
           (dia-db1 (expand-file-name "Cookies" dia-prof1))
           (dia-prof2 (expand-file-name "Work/Network" dia-root))
           (dia-db2 (expand-file-name "Cookies" dia-prof2))
           (orig-dir-files (symbol-function 'directory-files))
           (chrome-touched nil)
           (future-filetime (+ (* (+ (floor (float-time)) 86400) 1000000)
                               chirp-x-cookies--chromium-epoch-offset))
           (specs `((chrome :label "Chrome" :engine chromium :dirs (,chrome-root)
                            :keychain-services ("Chrome Safe Storage"))
                    (dia :label "Dia" :engine chromium :dirs (,dia-root)
                         :keychain-services ("Dia Safe Storage" "Dia Storage")))))
      (unwind-protect
          (progn
            (make-directory chrome-root t)
            (make-directory dia-prof1 t)
            (make-directory dia-prof2 t)
            (let ((db (sqlite-open dia-db1)))
              (unwind-protect
                  (progn
                    (sqlite-execute db "CREATE TABLE cookies (host_key TEXT, name TEXT, value TEXT, encrypted_value BLOB, expires_utc INTEGER, last_access_utc INTEGER)")
                    (sqlite-execute db "INSERT INTO cookies VALUES ('.x.com', 'auth_token', 'dia-personal', '', ?, 13400000000000002)" (list future-filetime))
                    (sqlite-execute db "INSERT INTO cookies VALUES ('.x.com', 'ct0', 'ct0-personal', '', ?, 13400000000000002)" (list future-filetime)))
                (sqlite-close db)))
            (let ((db (sqlite-open dia-db2)))
              (unwind-protect
                  (progn
                    (sqlite-execute db "CREATE TABLE cookies (host_key TEXT, name TEXT, value TEXT, encrypted_value BLOB, expires_utc INTEGER, last_access_utc INTEGER)")
                    (sqlite-execute db "INSERT INTO cookies VALUES ('.x.com', 'auth_token', 'dia-work', '', ?, 13400000000000001)" (list future-filetime))
                    (sqlite-execute db "INSERT INTO cookies VALUES ('.x.com', 'ct0', 'ct0-work', '', ?, 13400000000000001)" (list future-filetime)))
                (sqlite-close db)))
            (let ((chirp-x-cookie-browser nil)
                  (running '(dia))
                  (default nil))
              (cl-letf (((symbol-function 'chirp-x-cookies--specs) (lambda () specs))
                        ((symbol-function 'chirp-x-cookies--running-browsers) (lambda (_) running))
                        ((symbol-function 'chirp-x-cookies--default-browser) (lambda (_) default))
                        ((symbol-function 'directory-files)
                         (lambda (dir &rest args)
                           (if (equal (expand-file-name dir) (expand-file-name chrome-root))
                               (progn
                                 (setq chrome-touched t)
                                 (signal 'file-error (list "Opening directory" "Operation not permitted" dir)))
                             (apply orig-dir-files dir args)))))
                ;; 1. Running Dia yields both profiles lazily without touching Chrome.
                (let* ((iter (chirp-x-cookies-make-iterator))
                       (first (funcall iter))
                       (second (funcall iter)))
                  (should (equal (plist-get first :browser) "Dia (Personal)"))
                  (should (equal (plist-get first :auth-token) "dia-personal"))
                  (should (equal (plist-get second :browser) "Dia (Work)"))
                  (should (equal (plist-get second :auth-token) "dia-work"))
                  (should-not chrome-touched)
                  (should-not (chirp-x-cookies-last-diagnostic)))
                ;; 2. Fallback scan hits TCC-denied Chrome, records diagnostic, and imports Dia.
                (setq running nil default nil chrome-touched nil)
                (let ((imported (chirp-x-cookies-import)))
                  (should chrome-touched)
                  (should (equal (chirp-x-cookies-last-denied-browsers) '("Chrome")))
                  (should (string-match-p "Chrome" (chirp-x-cookies-last-diagnostic)))
                  (should (equal (plist-get imported :auth-token) "dia-personal")))
                ;; 3. Available-sources disambiguates both Dia profiles and survives Chrome TCC.
                (let ((sources (chirp-x-cookies-available-sources)))
                  (should (= (length sources) 2))
                  (should (equal (sort (mapcar (lambda (s) (plist-get s :label)) sources) #'string<)
                                 '("Dia (Personal)" "Dia (Work)")))
                  (should (equal (chirp-x-cookies-last-denied-browsers) '("Chrome"))))
                ;; 4. Selecting one profile explicitly imports only that profile.
                (let ((imported (chirp-x-cookies-import 'dia "Work")))
                  (should (equal (plist-get imported :auth-token) "dia-work"))
                  (should (equal (plist-get imported :browser) "Dia (Work)")))
                ;; 5. Tier order places running default first, then other running, then remaining.
                (setq running '(chrome dia) default 'dia)
                (should (equal (mapcar (lambda (tier) (mapcar #'car tier))
                                       (chirp-x-cookies--prioritized-tiers specs))
                               '((dia chrome))))
                (setq running nil default 'dia)
                (should (equal (mapcar (lambda (tier) (mapcar #'car tier))
                                       (chirp-x-cookies--prioritized-tiers specs))
                               '((dia) (chrome)))))))
        (when (file-directory-p root)
          (delete-directory root t))))))

(provide 'chirp-backend-test)

;;; chirp-backend-test.el ends here
