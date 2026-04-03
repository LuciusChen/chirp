;;; chirp-backend-test.el --- Tests for Chirp backend caching -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-backend)

(declare-function chirp-backend-clear-cache "chirp-backend" ())
(declare-function chirp-backend-invalidate-user "chirp-backend" (handle))
(defvar chirp-backend-read-cache-ttl)
(defvar chirp-backend--bypass-read-cache)
(defvar chirp-backend-use-daemon)
(defvar chirp-backend--daemon-process)
(defvar chirp-backend--daemon-ready-p)
(defvar chirp-backend--daemon-shutting-down)
(defvar chirp-backend--daemon-unsupported-p)
(defvar chirp-backend--daemon-queue)
(defvar chirp-backend--daemon-stderr-buffer)
(defvar chirp-backend--daemon-shutdown-timer)
(defvar chirp-backend--daemon-pending)

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
            (let ((chirp-backend--bypass-read-cache t))
              (chirp-backend-thread "123" #'ignore))
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
                 (list (list :id "1")))))
      (chirp-backend-likes
       "@Alice"
       (lambda (items _envelope)
         (setq tweets items))))
    (should (equal captured-args
                   '("likes" "Alice" "--max" "20")))
    (should (equal tweets '((:id "1"))))))

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

(ert-deftest chirp-backend-request-routes-read-commands-through-daemon ()
  "Read commands should use the daemon while writes stay one-shot."
  (let ((chirp-backend-use-daemon t)
        (chirp-backend--daemon-unsupported-p nil)
        daemon-call
        process-call)
    (cl-letf (((symbol-function 'chirp-backend--request-via-daemon)
               (lambda (args _callback _errback attempt)
                 (setq daemon-call (list args attempt))))
              ((symbol-function 'chirp-backend--request-via-process)
               (lambda (args _callback _errback attempt)
                 (setq process-call (list args attempt)))))
      (chirp-backend-request '("feed" "--max" "20") #'ignore)
      (chirp-backend-request '("like" "123") #'ignore))
    (should (equal daemon-call '(("feed" "--max" "20") 0)))
    (should (equal process-call '(("like" "123") 0)))))

(ert-deftest chirp-backend-shutdown-daemon-sends-graceful-shutdown ()
  "Daemon shutdown should send a shutdown request before forcing a kill."
  (let ((chirp-backend--daemon-process 'daemon)
        (chirp-backend--daemon-ready-p t)
        (chirp-backend--daemon-shutting-down nil)
        (chirp-backend--daemon-shutdown-timer nil)
        sent)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (_process)
                 t))
              ((symbol-function 'process-send-string)
               (lambda (_process payload)
                 (setq sent payload)))
              ((symbol-function 'run-at-time)
               (lambda (&rest _args)
                 'timer)))
      (chirp-backend-shutdown-daemon))
    (should chirp-backend--daemon-shutting-down)
    (should (string-match-p "\"shutdown\"" sent))
    (should (eq chirp-backend--daemon-shutdown-timer 'timer))))

(ert-deftest chirp-backend-daemon-sentinel-falls-back-before-ready ()
  "A daemon startup failure should fall back to one-shot requests."
  (let ((chirp-backend--daemon-process 'daemon)
        (chirp-backend--daemon-ready-p nil)
        (chirp-backend--daemon-shutting-down nil)
        (chirp-backend--daemon-unsupported-p nil)
        (chirp-backend--daemon-queue
         (list (list :id "1"
                     :args '("feed" "--max" "20")
                     :callback #'ignore
                     :errback nil
                     :attempt 0)))
        (chirp-backend--daemon-stderr-buffer (generate-new-buffer " *chirp-daemon-stderr-test*"))
        fallback)
    (unwind-protect
        (progn
          (with-current-buffer chirp-backend--daemon-stderr-buffer
            (insert "No such command"))
          (cl-letf (((symbol-function 'process-status)
                     (lambda (_process)
                       'exit))
                    ((symbol-function 'chirp-backend--request-via-process)
                     (lambda (args _callback _errback attempt)
                       (push (list args attempt) fallback)))
                    ((symbol-function 'message)
                     (lambda (&rest _args)
                       nil)))
            (chirp-backend--daemon-sentinel 'daemon "finished"))
          (should chirp-backend--daemon-unsupported-p)
          (should (equal fallback '((("feed" "--max" "20") 0)))))
      (when (buffer-live-p chirp-backend--daemon-stderr-buffer)
        (kill-buffer chirp-backend--daemon-stderr-buffer)))))

(ert-deftest chirp-backend-daemon-invalid-json-falls-back-to-one-shot ()
  "Protocol corruption should disable daemon mode and retry pending requests."
  (let ((chirp-backend--daemon-process 'daemon)
        (chirp-backend--daemon-ready-p t)
        (chirp-backend--daemon-shutting-down nil)
        (chirp-backend--daemon-unsupported-p nil)
        (chirp-backend--daemon-queue nil)
        (chirp-backend--daemon-pending (make-hash-table :test #'equal))
        fallback)
    (puthash "1"
             (list :id "1"
                   :args '("tweet" "123" "--max" "20")
                   :callback #'ignore
                   :errback nil
                   :attempt 0)
             chirp-backend--daemon-pending)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (_process)
                 t))
              ((symbol-function 'delete-process)
               (lambda (&rest _args)
                 nil))
              ((symbol-function 'chirp-backend--request-via-process)
               (lambda (args _callback _errback attempt)
                 (push (list args attempt) fallback)))
              ((symbol-function 'message)
               (lambda (&rest _args)
                 nil)))
      (chirp-backend--daemon-handle-line "x"))
    (should chirp-backend--daemon-unsupported-p)
    (should (equal fallback '((("tweet" "123" "--max" "20") 0))))))

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
