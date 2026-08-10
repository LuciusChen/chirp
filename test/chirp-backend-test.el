;;; chirp-backend-test.el --- Tests for Chirp backend caching -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Exercise backend caching, argument shaping, structured-error dispatch, and
;; the retry boundary around one-shot twitter-cli processes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-backend)

(defun chirp-backend-test--run-process-payload (args payload)
  "Run ARGS through process dispatch using JSON string PAYLOAD."
  (let ((chirp-cli-max-retries 2)
        stdout-buffer
        sentinel
        scheduled
        success
        failure)
    (cl-letf (((symbol-function 'chirp-backend-command)
               (lambda ()
                 "/tmp/twitter"))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq stdout-buffer (plist-get plist :buffer)
                       sentinel (plist-get plist :sentinel))
                 'chirp-backend-test-process))
              ((symbol-function 'process-status)
               (lambda (_process)
                 'exit))
              ((symbol-function 'process-exit-status)
               (lambda (_process)
                 1))
              ((symbol-function 'chirp-backend--schedule-retry)
               (lambda (retry-args _callback _errback attempt)
                 (setq scheduled (list retry-args attempt)))))
      (chirp-backend-request
       args
       (lambda (data envelope)
         (setq success (cons data envelope)))
       (lambda (message)
         (setq failure message)))
      (with-current-buffer stdout-buffer
        (insert payload))
      (funcall sentinel 'chirp-backend-test-process "finished\n"))
    (list :scheduled scheduled :success success :failure failure)))

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
                    ((symbol-function 'chirp-backend-request)
                     (lambda (_args callback &optional _errback)
                       (setq request-count (1+ request-count))
                       (funcall callback 'raw '((ok . t)))))
                    ((symbol-function 'chirp-collect-tweets)
                     (lambda (_data)
                       (list (list :id "123" :text "hello")))))
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
          (cl-letf (((symbol-function 'chirp-backend-request)
                     (lambda (_args callback &optional _errback)
                       (setq request-count (1+ request-count)
                             success-callback callback)))
                    ((symbol-function 'chirp-normalize-user)
                     (lambda (_data)
                       (list :kind 'user :handle "alice"))))
            (chirp-backend-user "@Alice"
                                (lambda (user _envelope)
                                  (push user results)))
            (chirp-backend-user "alice"
                                (lambda (user _envelope)
                                  (push user results)))
            (should (= request-count 1))
            (should (functionp success-callback))
            (funcall success-callback 'raw '((ok . t)))
            (should (= (length results) 2))
            (should (equal (plist-get (car results) :handle) "alice"))
            (should (equal (plist-get (cadr results) :handle) "alice"))
            (should-not (eq (car results) (cadr results)))
            (chirp-backend-invalidate-user "alice")
            (chirp-backend-user "alice" #'ignore)
            (should (= request-count 2))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-feed-passes-cursor-and-extracts-next-cursor ()
  "Feed requests should forward cursor args and expose pagination metadata."
  (let (captured-args next-cursor tweets)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback
                          '(("id" . "1")
                            ("text" . "hello")
                            ("author" . (("screenName" . "alice")
                                         ("name" . "Alice"))))
                          '(("ok" . t)
                            ("data" . nil)
                            ("pagination" . (("nextCursor" . "cursor-next")))))))
              ((symbol-function 'chirp-collect-top-level-tweets)
               (lambda (_data)
                 (list (list :id "1")))))
      (chirp-backend-feed
       (lambda (items envelope)
         (setq tweets items
               next-cursor (chirp-backend-envelope-next-cursor envelope)))
       nil
       nil
       20
       "cursor-prev"))
    (should (equal captured-args
                   '("feed" "--cursor" "cursor-prev" "--max" "20")))
    (should (equal tweets '((:id "1"))))
    (should (equal next-cursor "cursor-next"))))

(ert-deftest chirp-backend-user-posts-passes-cursor-without-cache ()
  "Profile post pagination should forward the next cursor directly."
  (let (captured-args tweets next-cursor)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback
                          '(("id" . "1"))
                          '(("ok" . t)
                            ("pagination" . (("nextCursor" . "cursor-next")))))))
              ((symbol-function 'chirp-collect-top-level-tweets)
               (lambda (_data)
                 (list (list :id "1")))))
      (chirp-backend-user-posts
       "@Alice"
       (lambda (items envelope)
         (setq tweets items
               next-cursor (chirp-backend-envelope-next-cursor envelope)))
       nil
       15
       "cursor-prev"))
    (should (equal captured-args
                   '("user-posts" "Alice" "--cursor" "cursor-prev" "--max" "15")))
    (should (equal tweets '((:id "1"))))
    (should (equal next-cursor "cursor-next"))))

(ert-deftest chirp-backend-lists-sync-uses-cache-and-lists-command ()
  "List catalog lookups should reuse the read cache."
  (let ((chirp-backend-read-cache-ttl 15)
        (now 1000)
        (request-count 0)
        captured-args
        first second)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&rest _args)
                       now))
                    ((symbol-function 'chirp-backend--request-sync)
                     (lambda (args)
                       (setq request-count (1+ request-count)
                             captured-args args)
                       (cons '((("id" . "1")
                                ("name" . "Emacs")))
                             '((("ok" . t)))))))
            (setq first (chirp-backend-lists-sync))
            (setq second (chirp-backend-lists-sync))
            (should (equal captured-args '("lists")))
            (should (= request-count 1))
            (should (equal first second))
            (should-not (eq first second))
            (setq now 1016)
            (chirp-backend-lists-sync)
            (should (= request-count 2))))
      (chirp-backend-clear-cache))))

(ert-deftest chirp-backend-search-users-sync-uses-users-command ()
  "User completion should use the CLI typeahead command."
  (let (captured-args)
    (cl-letf (((symbol-function 'chirp-backend--request-sync)
               (lambda (args)
                 (setq captured-args args)
                 (cons '((("screenName" . "emacs")))
                       '(("ok" . t))))))
      (should (equal (chirp-backend-search-users-sync "em" 5)
                     '((("screenName" . "emacs")))))
      (should (equal captured-args '("users" "em" "--max" "5"))))))

(ert-deftest chirp-backend-translate-passes-language-and-tweet-id ()
  "Tweet translation should use the dedicated CLI command."
  (let (captured-args)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args _callback &optional _errback)
                 (setq captured-args args))))
      (chirp-backend-translate "123" "zh" #'ignore)
      (should (equal captured-args
                     '("translate" "123" "--to" "zh"))))))

(ert-deftest chirp-backend-whoami-cache-reuses-fresh-results ()
  "Fresh cached whoami results should avoid a second backend request."
  (let ((chirp-backend-read-cache-ttl 15)
        (request-count 0)
        first second)
    (unwind-protect
        (progn
          (chirp-backend-clear-cache)
          (cl-letf (((symbol-function 'chirp-backend-request)
                     (lambda (_args callback &optional _errback)
                       (setq request-count (1+ request-count))
                       (funcall callback 'raw '((ok . t)))))
                    ((symbol-function 'chirp-normalize-user)
                     (lambda (_data)
                       (list :kind 'user :handle "alice"))))
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

(ert-deftest chirp-backend-likes-passes-handle-and-max-results ()
  "Likes requests should strip @ and reuse timeline normalization."
  (let (captured-args tweets)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback 'raw '((ok . t)))))
              ((symbol-function 'chirp-collect-top-level-tweets)
               (lambda (_data)
                 (list (list :id "1")
                       (list :id "2" :liked-p nil)))))
      (chirp-backend-likes
       "@Alice"
       (lambda (items _envelope)
         (setq tweets items))))
    (should (equal captured-args
                   '("likes" "Alice" "--max" "20")))
    (should (equal (mapcar (lambda (tweet)
                             (plist-get tweet :liked-p))
                           tweets)
                   '(t t)))))

(ert-deftest chirp-backend-user-replies-passes-cursor-and-normalizes-tweets ()
  "Replies requests should pass through handle, cursor, and tweet normalization."
  (let (captured-args tweets)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback 'raw '((ok . t)))))
              ((symbol-function 'chirp-collect-top-level-tweets)
               (lambda (_data)
                 (list (list :id "1")))))
      (chirp-backend-user-replies
       "@Alice"
       (lambda (items _envelope)
         (setq tweets items))
       nil
       10
       "cursor-prev"))
    (should (equal captured-args
                   '("user-replies" "Alice" "--cursor" "cursor-prev" "--max" "10")))
    (should (equal tweets '((:id "1"))))))

(ert-deftest chirp-backend-user-highlights-passes-max-results ()
  "Highlights requests should strip @ and forward max-results."
  (let (captured-args tweets)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback 'raw '((ok . t)))))
              ((symbol-function 'chirp-collect-top-level-tweets)
               (lambda (_data)
                 (list (list :id "1")))))
      (chirp-backend-user-highlights
       "@Alice"
       (lambda (items _envelope)
         (setq tweets items))
       nil
       7))
    (should (equal captured-args
                   '("user-highlights" "Alice" "--max" "7")))
    (should (equal tweets '((:id "1"))))))

(ert-deftest chirp-backend-user-media-passes-max-results ()
  "Media requests should strip @ and forward max-results."
  (let (captured-args tweets)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback 'raw '((ok . t)))))
              ((symbol-function 'chirp-collect-top-level-tweets)
               (lambda (_data)
                 (list (list :id "1")))))
      (chirp-backend-user-media
       "@Alice"
       (lambda (items _envelope)
         (setq tweets items))
       nil
       6))
    (should (equal captured-args
                   '("user-media" "Alice" "--max" "6")))
    (should (equal tweets '((:id "1"))))))

(ert-deftest chirp-backend-followers-passes-handle-and-normalizes-users ()
  "Followers requests should strip @ and normalize returned users."
  (let (captured-args users)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback '((("screenName" . "alice")
                                      ("name" . "Alice")))
                          '((ok . t)))))
              ((symbol-function 'chirp-normalize-user)
               (lambda (_data)
                 (list :kind 'user :handle "alice"))))
      (chirp-backend-followers
       "@Bob"
       (lambda (items _envelope)
         (setq users items))))
    (should (equal captured-args
                   '("followers" "Bob" "--max" "20")))
    (should (equal users '((:kind user :handle "alice"))))))

(ert-deftest chirp-backend-following-users-passes-handle-and-normalizes-users ()
  "Following requests should strip @ and normalize returned users."
  (let (captured-args users)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback '((("screenName" . "alice")
                                      ("name" . "Alice")))
                          '((ok . t)))))
              ((symbol-function 'chirp-normalize-user)
               (lambda (_data)
                 (list :kind 'user :handle "alice"))))
      (chirp-backend-following-users
       "@Bob"
       (lambda (items _envelope)
         (setq users items))))
    (should (equal captured-args
                   '("following" "Bob" "--max" "20")))
    (should (equal users '((:kind user :handle "alice"))))))

(ert-deftest chirp-backend-list-normalizes-list-urls ()
  "List requests should accept either a raw id or a full list URL."
  (let (captured-args)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback 'raw '((ok . t)))))
              ((symbol-function 'chirp-collect-top-level-tweets)
               (lambda (_data)
                 nil)))
      (chirp-backend-list "https://x.com/i/lists/1956792682412345678" #'ignore))
    (should (equal captured-args
                   '("list" "1956792682412345678" "--max" "20")))))

(ert-deftest chirp-backend-thread-passes-explicit-max-results ()
  "Thread requests should pass Chirp's explicit thread fetch limit."
  (let ((chirp-thread-max-results 20)
        captured-args)
    (cl-letf (((symbol-function 'chirp-backend-request)
               (lambda (args callback &optional _errback)
                 (setq captured-args args)
                 (funcall callback 'raw '((ok . t)))))
              ((symbol-function 'chirp-collect-tweets)
               (lambda (_data)
                 (list (list :id "1")))))
      (chirp-backend-thread "123" #'ignore))
    (should (equal captured-args
                   '("tweet" "123" "--max" "20")))))

(ert-deftest chirp-backend-process-does-not-retry-http-zero-business-errors ()
  "HTTP zero business errors should reach the caller without a retry."
  (let ((result
         (chirp-backend-test--run-process-payload
          '("feed")
          (concat
           "{\"ok\":false,\"error\":{\"code\":\"api_error\","
           "\"message\":\"Twitter API error (HTTP 0): "
           "Authorization: daily limit reached (344)\"}}"))))
    (should-not (plist-get result :scheduled))
    (should-not (plist-get result :success))
    (should (string-match-p "daily limit reached (344)"
                            (plist-get result :failure)))
    (should-not (string-match-p "Chirp retried"
                                (plist-get result :failure)))))

(ert-deftest chirp-backend-process-retries-transient-read-errors ()
  "Explicit network and HTTP 5xx failures should retry read commands."
  (dolist (payload
           '("{\"ok\":false,\"error\":{\"code\":\"network_error\",\"message\":\"Connection reset\"}}"
             "{\"ok\":false,\"error\":{\"code\":\"api_error\",\"message\":\"Twitter API error (HTTP 503): unavailable\"}}"))
    (let ((result (chirp-backend-test--run-process-payload '("feed") payload)))
      (should (equal (plist-get result :scheduled)
                     '(("feed") 0)))
      (should-not (plist-get result :success))
      (should-not (plist-get result :failure)))))

(ert-deftest chirp-backend-process-honors-structured-nonretryable-errors ()
  "Structured retryable false should prevent an otherwise transient retry."
  (let ((result
         (chirp-backend-test--run-process-payload
          '("feed")
          (concat
           "{\"ok\":false,\"error\":{\"code\":\"network_error\","
           "\"message\":\"Connection reset\","
           "\"details\":{\"retryable\":false}}}"))))
    (should-not (plist-get result :scheduled))
    (should-not (plist-get result :success))
    (should (equal (plist-get result :failure)
                   "Connection reset (network_error)"))))

(ert-deftest chirp-backend-process-never-retries-publishing-commands ()
  "Post, reply, and quote commands should never retry unknown outcomes."
  (dolist (case
           '(("post"
              "{\"ok\":false,\"error\":{\"code\":\"network_error\",\"message\":\"Connection reset\"}}")
             ("reply"
              "{\"ok\":false,\"error\":{\"code\":\"api_error\",\"message\":\"Twitter API error (HTTP 503): unavailable\"}}")
             ("quote"
              "{\"ok\":false,\"error\":{\"code\":\"network_error\",\"message\":\"Timed out\"}}")))
    (let ((result
           (chirp-backend-test--run-process-payload
            (list (car case) "content")
            (cadr case))))
      (should-not (plist-get result :scheduled))
      (should-not (plist-get result :success))
      (should (stringp (plist-get result :failure)))
      (should-not (string-match-p "Chirp retried"
                                  (plist-get result :failure))))))

(ert-deftest chirp-backend-process-requests-enable-compact-json ()
  "One-shot backend requests should ask twitter-cli for compact structured JSON."
  (let (captured-env captured-command)
    (cl-letf (((symbol-function 'chirp-backend-command)
               (lambda ()
                 "/tmp/twitter"))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq captured-env process-environment
                       captured-command (plist-get plist :command))
                 'process)))
      (chirp-backend--request-via-process '("feed" "--max" "20") #'ignore nil 0))
    (should (equal captured-command '("/tmp/twitter" "feed" "--max" "20" "--json")))
    (should (member "TWITTER_CLI_COMPACT_JSON=1" captured-env))))

(provide 'chirp-backend-test)

;;; chirp-backend-test.el ends here
