;;; chirp-backend.el --- twitter-cli integration for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'json)
(require 'subr-x)
(require 'chirp-core)

(defconst chirp-backend--json-false (make-symbol "chirp-json-false")
  "Sentinel value used for JSON false.")

(defconst chirp-backend--default-cli-commands '("twitter" "twitter-cli")
  "Executable names Chirp tries when `chirp-cli-command' is nil.")

(defcustom chirp-backend-read-cache-ttl 15
  "Seconds to keep successful thread/profile/article reads in memory.

When zero or negative, the in-memory read cache is disabled."
  :type 'number
  :group 'chirp)

(defvar chirp-backend--read-cache (make-hash-table :test #'equal)
  "In-memory cache for successful Chirp read responses.")

(defvar chirp-backend--pending-read-requests (make-hash-table :test #'equal)
  "Map cache keys to queued Chirp read callbacks while a request is in flight.")

(defvar chirp-backend--bypass-read-cache nil
  "When non-nil, Chirp read requests bypass the in-memory cache and pending reuse.")

(defun chirp-backend--auto-detect-command-p ()
  "Return non-nil when Chirp should auto-detect the CLI command."
  (or (null chirp-cli-command)
      (string-empty-p chirp-cli-command)))

(defun chirp-backend--command-candidates ()
  "Return candidate twitter-cli executable names."
  (if (chirp-backend--auto-detect-command-p)
      chirp-backend--default-cli-commands
    (list chirp-cli-command)))

(defun chirp-backend--resolve-from-exec-path (candidates)
  "Return the first executable in CANDIDATES found via `exec-path'."
  (catch 'found
    (dolist (candidate candidates)
      (let ((command (executable-find candidate)))
        (when command
          (throw 'found command))))
    nil))

(defun chirp-backend--resolve-from-search-paths (candidates)
  "Return the first executable found in `chirp-cli-search-paths' for CANDIDATES."
  (catch 'command
    (dolist (dir chirp-cli-search-paths)
      (let ((expanded-dir (expand-file-name dir)))
        (when (file-directory-p expanded-dir)
          (dolist (candidate candidates)
            (let ((path (expand-file-name candidate expanded-dir)))
              (when (file-executable-p path)
                (throw 'command path)))))))
    nil))

(defun chirp-backend-command ()
  "Return the resolved twitter-cli executable path, or nil."
  (let ((candidates (chirp-backend--command-candidates)))
    (or (chirp-backend--resolve-from-exec-path candidates)
        (and (chirp-backend--auto-detect-command-p)
             (chirp-backend--resolve-from-search-paths candidates)))))

(defun chirp-backend-clear-cache ()
  "Clear Chirp's in-memory read cache."
  (interactive)
  (clrhash chirp-backend--read-cache))

(defun chirp-backend--clone-data (value)
  "Return VALUE copied deeply enough for safe cache reuse."
  (if (consp value)
      (copy-tree value)
    value))

(defun chirp-backend--normalize-handle (handle)
  "Return HANDLE normalized for cache lookup."
  (downcase (string-remove-prefix "@" (format "%s" handle))))

(defun chirp-backend--tweet-id-from-target (target)
  "Return a likely tweet id extracted from TARGET, or nil."
  (cond
   ((null target) nil)
   ((and (stringp target)
         (string-match "/status/\\([0-9]+\\)" target))
    (match-string 1 target))
   ((stringp target)
    target)
   ((listp target)
    (or (plist-get target :id)
        (and-let* ((url (plist-get target :url)))
          (chirp-backend--tweet-id-from-target url))))
   (t
    (format "%s" target))))

(defun chirp-backend--thread-cache-key (tweet-or-url)
  "Return the cache key for thread TARGET."
  (list :thread (or (chirp-backend--tweet-id-from-target tweet-or-url)
                    (format "%s" tweet-or-url))))

(defun chirp-backend--article-cache-key (tweet-id)
  "Return the cache key for TWEET-ID article fetches."
  (list :article (format "%s" tweet-id)))

(defun chirp-backend--user-cache-key (handle)
  "Return the cache key for HANDLE profile metadata."
  (list :user (chirp-backend--normalize-handle handle)))

(defun chirp-backend--user-posts-cache-key (handle)
  "Return the cache key for HANDLE recent posts."
  (list :user-posts (chirp-backend--normalize-handle handle)))

(defun chirp-backend-invalidate-thread (tweet-or-url)
  "Drop cached thread and article data for TWEET-OR-URL."
  (let ((thread-key (chirp-backend--thread-cache-key tweet-or-url))
        (tweet-id (chirp-backend--tweet-id-from-target tweet-or-url)))
    (remhash thread-key chirp-backend--read-cache)
    (when tweet-id
      (chirp-backend-invalidate-article tweet-id))))

(defun chirp-backend-invalidate-article (tweet-id)
  "Drop cached article data for TWEET-ID."
  (let ((key (chirp-backend--article-cache-key tweet-id)))
    (remhash key chirp-backend--read-cache)))

(defun chirp-backend-invalidate-user (handle)
  "Drop cached profile metadata and posts for HANDLE."
  (dolist (key (list (chirp-backend--user-cache-key handle)
                     (chirp-backend--user-posts-cache-key handle)))
    (remhash key chirp-backend--read-cache)))

(defun chirp-backend--cache-entry-live-p (entry now)
  "Return non-nil when cached ENTRY is still fresh at NOW."
  (and entry
       (> chirp-backend-read-cache-ttl 0)
       (numberp (plist-get entry :expires-at))
       (> (plist-get entry :expires-at) now)))

(defun chirp-backend--cached-result (key)
  "Return KEY's cached result plist, or nil when absent or expired."
  (let* ((now (float-time))
         (entry (gethash key chirp-backend--read-cache)))
    (cond
     ((chirp-backend--cache-entry-live-p entry now)
      entry)
     (entry
      (remhash key chirp-backend--read-cache)
      nil)
     (t nil))))

(defun chirp-backend--dispatch-read-success (requesters value envelope)
  "Invoke REQUESTERS with VALUE and ENVELOPE."
  (dolist (requester requesters)
    (funcall (car requester)
             (chirp-backend--clone-data value)
             (chirp-backend--clone-data envelope))))

(defun chirp-backend--dispatch-read-error (requesters message)
  "Invoke REQUESTERS with MESSAGE."
  (dolist (requester requesters)
    (funcall (or (cdr requester)
                 (lambda (text)
                   (message "%s" text)))
             message)))

(defun chirp-backend--cached-read (key fetcher callback &optional errback)
  "Fetch KEY via FETCHER and serve CALLBACK from the short-lived read cache.

FETCHER is called with success and error callbacks."
  (if-let* (((not chirp-backend--bypass-read-cache))
            (entry (chirp-backend--cached-result key)))
      (funcall callback
               (chirp-backend--clone-data (plist-get entry :value))
               (chirp-backend--clone-data (plist-get entry :envelope)))
    (let ((pending (and (not chirp-backend--bypass-read-cache)
                        (gethash key chirp-backend--pending-read-requests))))
      (if pending
          (puthash key
                   (append pending (list (cons callback errback)))
                   chirp-backend--pending-read-requests)
        (puthash key (list (cons callback errback))
                 chirp-backend--pending-read-requests)
        (condition-case err
            (funcall
             fetcher
             (lambda (value envelope)
               (let ((requesters (prog1 (gethash key chirp-backend--pending-read-requests)
                                   (remhash key chirp-backend--pending-read-requests))))
                 (when (> chirp-backend-read-cache-ttl 0)
                   (puthash key
                            (list :value (chirp-backend--clone-data value)
                                  :envelope (chirp-backend--clone-data envelope)
                                  :expires-at (+ (float-time)
                                                 chirp-backend-read-cache-ttl))
                            chirp-backend--read-cache))
                 (chirp-backend--dispatch-read-success requesters value envelope)))
             (lambda (message)
               (let ((requesters (prog1 (gethash key chirp-backend--pending-read-requests)
                                   (remhash key chirp-backend--pending-read-requests))))
                 (chirp-backend--dispatch-read-error requesters message))))
          (error
           (let ((requesters (prog1 (gethash key chirp-backend--pending-read-requests)
                               (remhash key chirp-backend--pending-read-requests))))
             (chirp-backend--dispatch-read-error
              requesters
              (error-message-string err)))))))))

(defun chirp-backend--missing-command-message ()
  "Return an error message for a missing twitter-cli executable."
  (if (chirp-backend--auto-detect-command-p)
      "Cannot find twitter or twitter-cli in PATH or `chirp-cli-search-paths`.\n\nInstall twitter-cli so it is available as `twitter` or `twitter-cli`, or set `chirp-cli-command`."
    (format "Cannot find configured command: %s.\n\nInstall twitter-cli and set `chirp-cli-command' to the executable path or name."
            chirp-cli-command)))

(defun chirp-backend-available-p ()
  "Return non-nil when Chirp can find a twitter-cli executable."
  (chirp-backend-command))

(defun chirp-backend--error-message (command args stdout-buffer stderr-buffer)
  "Build an error message for COMMAND and ARGS.

Use STDOUT-BUFFER and STDERR-BUFFER for process output."
  (let ((stderr (with-current-buffer stderr-buffer
                  (string-trim (buffer-string))))
        (stdout (with-current-buffer stdout-buffer
                  (string-trim (buffer-string)))))
    (string-join
     (delq nil
           (list (format "Command: %s %s"
                         command
                         (string-join args " "))
                 (unless (string-empty-p stderr)
                   (format "stderr: %s" stderr))
                 (unless (string-empty-p stdout)
                   (format "stdout: %s" stdout))
                 (unless (chirp-backend-available-p)
                   (chirp-backend--missing-command-message))))
     "\n\n")))

(defun chirp-backend--parse-json-buffer (buffer)
  "Parse BUFFER as JSON."
  (with-current-buffer buffer
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object chirp-backend--json-false)))

(defun chirp-backend--maybe-parse-json-buffer (buffer)
  "Parse BUFFER as JSON and return nil on failure."
  (condition-case nil
      (chirp-backend--parse-json-buffer buffer)
    (error nil)))

(defun chirp-backend--payload-error-message (payload)
  "Return a human-readable error message extracted from PAYLOAD."
  (let* ((error (chirp-get payload "error"))
         (code (chirp-get error "code"))
         (message (or (chirp-get error "message")
                      "twitter-cli reported an unknown error")))
    (if code
        (format "%s (%s)" message code)
      message)))

(defun chirp-backend--transient-error-p (payload)
  "Return non-nil when PAYLOAD describes a transient backend failure."
  (let* ((message (chirp-backend--payload-error-message payload))
         (status (when (string-match "HTTP \\([0-9]+\\)" message)
                   (string-to-number (match-string 1 message)))))
    (or (and status
             (or (= status 0)
                 (>= status 500)))
        (string-match-p "network error" message))))

(defun chirp-backend--format-retried-message (message attempt)
  "Annotate MESSAGE with retry information for ATTEMPT."
  (if (zerop attempt)
      message
    (format "%s\n\nChirp retried %d time%s."
            message
            attempt
            (if (= attempt 1) "" "s"))))

(defun chirp-backend--schedule-retry (args callback errback attempt)
  "Retry ARGS after ATTEMPT failures, then call CALLBACK or ERRBACK."
  (let ((next-attempt (1+ attempt)))
    (if (> chirp-cli-retry-delay 0)
        (run-at-time chirp-cli-retry-delay nil
                     #'chirp-backend--request
                     args callback errback next-attempt)
      (chirp-backend--request args callback errback next-attempt))))

(defun chirp-backend--dispatch (payload callback errback)
  "Dispatch PAYLOAD to CALLBACK or ERRBACK."
  (let ((ok (chirp-get payload "ok")))
    (cond
     ((eq ok chirp-backend--json-false)
      (funcall errback (chirp-backend--payload-error-message payload)))
     ((null ok)
      (funcall callback payload payload))
     (t
      (funcall callback (chirp-get payload "data") payload)))))

(defun chirp-backend--request (args callback errback attempt)
  "Run twitter-cli with ARGS and call CALLBACK.
ERRBACK receives a single human-readable string.
ATTEMPT tracks how many retries have already been used."
  (let ((error-fn (or errback
                      (lambda (message)
                        (message "%s" message))))
        (command (chirp-backend-command)))
    (if (not command)
        (funcall error-fn (chirp-backend--missing-command-message))
      (let ((stdout-buffer (generate-new-buffer " *chirp-cli-stdout*"))
            (stderr-buffer (generate-new-buffer " *chirp-cli-stderr*")))
        (make-process
         :name "chirp-cli"
         :buffer stdout-buffer
         :command (append (list command) args '("--json"))
         :stderr stderr-buffer
         :noquery t
         :sentinel
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (unwind-protect
                 (let* ((payload (chirp-backend--maybe-parse-json-buffer stdout-buffer))
                        (wrapped-error-fn
                         (lambda (message)
                           (funcall error-fn
                                    (chirp-backend--format-retried-message
                                     message attempt)))))
                   (cond
                    ((and payload
                          (< attempt chirp-cli-max-retries)
                          (chirp-backend--transient-error-p payload))
                     (chirp-backend--schedule-retry args callback errback attempt))
                    (payload
                     (chirp-backend--dispatch payload callback wrapped-error-fn))
                    ((zerop (process-exit-status process))
                     (funcall wrapped-error-fn
                              "twitter-cli exited successfully but did not return valid JSON."))
                    (t
                     (funcall wrapped-error-fn
                              (chirp-backend--error-message command
                                                            args
                                                            stdout-buffer
                                                            stderr-buffer)))))
               (kill-buffer stdout-buffer)
               (kill-buffer stderr-buffer)))))))))

(defun chirp-backend-request (args callback &optional errback)
  "Run twitter-cli with ARGS and call CALLBACK.
ERRBACK receives a single human-readable string."
  (chirp-backend--request args callback errback 0))

(defun chirp-backend-envelope-next-cursor (envelope)
  "Return the next pagination cursor from ENVELOPE, or nil."
  (or (chirp-get-in envelope '("pagination" "nextCursor"))
      (chirp-get envelope "nextCursor")))

(defun chirp-backend-feed (callback &optional following errback max-results cursor)
  "Fetch the home timeline and call CALLBACK.
When FOLLOWING is non-nil, fetch the Following timeline."
  (chirp-backend-request
   (append '("feed")
           (when following
             '("-t" "following"))
           (when cursor
             (list "--cursor" cursor))
           (list "--max" (number-to-string (or max-results
                                              chirp-default-max-results))))
   (lambda (data envelope)
     (funcall callback (chirp-collect-top-level-tweets data) envelope))
   errback))

(defun chirp-backend-bookmarks (callback &optional errback)
  "Fetch bookmarks and call CALLBACK."
  (chirp-backend-request
   (list "bookmarks" "--max" (number-to-string chirp-default-max-results))
   (lambda (data envelope)
     (funcall callback (chirp-collect-top-level-tweets data) envelope))
   errback))

(defun chirp-backend-search (query callback &optional errback)
  "Search for QUERY and call CALLBACK."
  (chirp-backend-request
   (list "search" query "--max" (number-to-string chirp-default-max-results))
   (lambda (data envelope)
     (funcall callback (chirp-collect-top-level-tweets data) envelope))
   errback))

(defun chirp-backend-thread (tweet-or-url callback &optional errback)
  "Fetch thread data for TWEET-OR-URL and call CALLBACK."
  (chirp-backend--cached-read
   (chirp-backend--thread-cache-key tweet-or-url)
   (lambda (success error)
     (chirp-backend-request
      (list "tweet" tweet-or-url)
      (lambda (data envelope)
        (funcall success (chirp-collect-tweets data) envelope))
      error))
   callback
   errback))

(defun chirp-backend-tweet (tweet-id callback &optional errback)
  "Fetch a single tweet for TWEET-ID and call CALLBACK."
  (chirp-backend-thread
   tweet-id
   (lambda (tweets envelope)
     (if-let* ((tweet (or (cl-find tweet-id tweets
                                   :key (lambda (item) (plist-get item :id))
                                   :test #'equal)
                          (car tweets))))
         (funcall callback tweet envelope)
       (funcall (or errback #'ignore)
                "twitter-cli returned tweet detail Chirp could not parse.")))
   errback))

(defun chirp-backend-article (tweet-id callback &optional errback)
  "Fetch article content for TWEET-ID and call CALLBACK."
  (chirp-backend--cached-read
   (chirp-backend--article-cache-key tweet-id)
   (lambda (success error)
     (chirp-backend-request
      (list "article" tweet-id)
      (lambda (data envelope)
        (let ((tweet (chirp-normalize-tweet data)))
          (if tweet
              (funcall success tweet envelope)
            (funcall error
                     "twitter-cli returned article data Chirp could not parse."))))
      error))
   callback
   errback))

(defun chirp-backend-user (handle callback &optional errback)
  "Fetch profile data for HANDLE and call CALLBACK."
  (chirp-backend--cached-read
   (chirp-backend--user-cache-key handle)
   (lambda (success error)
     (chirp-backend-request
      (list "user" (string-remove-prefix "@" handle))
      (lambda (data envelope)
        (let ((user (chirp-normalize-user data)))
          (if user
              (funcall success user envelope)
            (funcall error
                     "twitter-cli returned a profile payload Chirp could not parse."))))
      error))
   callback
   errback))

(defun chirp-backend-user-posts (handle callback &optional errback)
  "Fetch recent posts for HANDLE and call CALLBACK."
  (chirp-backend--cached-read
   (chirp-backend--user-posts-cache-key handle)
   (lambda (success error)
     (chirp-backend-request
      (list "user-posts"
            (string-remove-prefix "@" handle)
            "--max" (number-to-string chirp-profile-post-limit))
      (lambda (data envelope)
        (funcall success (chirp-collect-top-level-tweets data) envelope))
      error))
   callback
   errback))

(provide 'chirp-backend)

;;; chirp-backend.el ends here
