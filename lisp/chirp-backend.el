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

(defconst chirp-backend--compact-json-env "TWITTER_CLI_COMPACT_JSON=1"
  "Environment flag that asks twitter-cli for compact structured JSON.")

(defcustom chirp-backend-read-cache-ttl 15
  "Seconds to keep successful thread/profile/article reads in memory.

When zero or negative, the in-memory read cache is disabled."
  :type 'number
  :group 'chirp)

(defcustom chirp-backend-use-daemon nil
  "When non-nil, reuse a persistent twitter-cli daemon for read requests."
  :type 'boolean
  :group 'chirp)

(defcustom chirp-backend-daemon-idle-timeout 90
  "Seconds to keep the twitter-cli daemon alive after its last read activity.

When zero or negative, Chirp keeps the daemon alive until Emacs exits or Chirp
explicitly shuts it down."
  :type 'number
  :group 'chirp)

(defcustom chirp-backend-daemon-shutdown-grace-period 0.2
  "Seconds to wait for graceful twitter-cli daemon shutdown before killing it."
  :type 'number
  :group 'chirp)

(defvar chirp-backend--read-cache (make-hash-table :test #'equal)
  "In-memory cache for successful Chirp read responses.")

(defvar chirp-backend--pending-read-requests (make-hash-table :test #'equal)
  "Map cache keys to queued Chirp read callbacks while a request is in flight.")

(defvar chirp-backend--bypass-read-cache nil
  "When non-nil, Chirp read requests bypass the in-memory cache and pending reuse.")

(defconst chirp-backend--daemon-read-commands
  '("feed" "bookmarks" "search" "tweet" "article" "user" "user-posts")
  "twitter-cli commands that Chirp can route through the daemon.")

(defvar chirp-backend--daemon-process nil
  "Persistent twitter-cli daemon process used for read requests.")

(defvar chirp-backend--daemon-stdout-buffer nil
  "Buffer collecting stdout from the twitter-cli daemon.")

(defvar chirp-backend--daemon-stderr-buffer nil
  "Buffer collecting stderr from the twitter-cli daemon.")

(defvar chirp-backend--daemon-line-buffer ""
  "Partial stdout line accumulated from the twitter-cli daemon.")

(defvar chirp-backend--daemon-pending (make-hash-table :test #'equal)
  "Map daemon request ids to pending Chirp callbacks.")

(defvar chirp-backend--daemon-queue nil
  "Daemon requests queued until the daemon reports ready.")

(defvar chirp-backend--daemon-request-counter 0
  "Monotonic counter for daemon request ids.")

(defvar chirp-backend--daemon-ready-p nil
  "Non-nil once the twitter-cli daemon has emitted its ready event.")

(defvar chirp-backend--daemon-shutting-down nil
  "Non-nil while Chirp is intentionally shutting down the daemon.")

(defvar chirp-backend--daemon-unsupported-p nil
  "Non-nil when the current twitter-cli does not support daemon mode.")

(defvar chirp-backend--daemon-idle-timer nil
  "Idle timer used to close the daemon after inactivity.")

(defvar chirp-backend--daemon-shutdown-timer nil
  "Grace-period timer used to force-kill a stuck daemon.")

(defvar chirp-backend--daemon-command nil
  "Resolved twitter-cli executable path backing the current daemon.")

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

(defun chirp-backend--whoami-cache-key ()
  "Return the cache key for the current authenticated user."
  '(:whoami))

(defun chirp-backend--user-posts-cache-key (handle)
  "Return the cache key for HANDLE recent posts."
  (list :user-posts (chirp-backend--normalize-handle handle)))

(defun chirp-backend--list-id-from-target (target)
  "Return a likely list id extracted from TARGET, or TARGET as-is."
  (let ((text (string-trim (format "%s" target))))
    (if (string-match "/lists?/\\([0-9]+\\)" text)
        (match-string 1 text)
      text)))

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

(defun chirp-backend--daemon-request-p (args)
  "Return non-nil when ARGS can be served by the twitter-cli daemon."
  (and chirp-backend-use-daemon
       (not chirp-backend--daemon-unsupported-p)
       (consp args)
       (member (car args) chirp-backend--daemon-read-commands)))

(defun chirp-backend--daemon-live-p ()
  "Return non-nil when the daemon process is alive."
  (process-live-p chirp-backend--daemon-process))

(defun chirp-backend--daemon-cancel-idle-timer ()
  "Cancel the daemon idle timer."
  (when (timerp chirp-backend--daemon-idle-timer)
    (cancel-timer chirp-backend--daemon-idle-timer))
  (setq chirp-backend--daemon-idle-timer nil))

(defun chirp-backend--daemon-cancel-shutdown-timer ()
  "Cancel the daemon shutdown grace timer."
  (when (timerp chirp-backend--daemon-shutdown-timer)
    (cancel-timer chirp-backend--daemon-shutdown-timer))
  (setq chirp-backend--daemon-shutdown-timer nil))

(defun chirp-backend--daemon-clear-state ()
  "Clear daemon state and kill any associated buffers."
  (chirp-backend--daemon-cancel-idle-timer)
  (chirp-backend--daemon-cancel-shutdown-timer)
  (let ((stdout chirp-backend--daemon-stdout-buffer)
        (stderr chirp-backend--daemon-stderr-buffer))
    (setq chirp-backend--daemon-process nil
          chirp-backend--daemon-stdout-buffer nil
          chirp-backend--daemon-stderr-buffer nil
          chirp-backend--daemon-line-buffer ""
          chirp-backend--daemon-queue nil
          chirp-backend--daemon-ready-p nil
          chirp-backend--daemon-shutting-down nil
          chirp-backend--daemon-command nil)
    (clrhash chirp-backend--daemon-pending)
    (when (buffer-live-p stdout)
      (kill-buffer stdout))
    (when (buffer-live-p stderr)
      (kill-buffer stderr))))

(defun chirp-backend--daemon-drain-pending ()
  "Return and clear pending daemon request entries."
  (let (entries)
    (maphash (lambda (_id entry)
               (push entry entries))
             chirp-backend--daemon-pending)
    (clrhash chirp-backend--daemon-pending)
    (nreverse entries)))

(defun chirp-backend--daemon-drain-queue ()
  "Return and clear daemon startup queue entries."
  (prog1 chirp-backend--daemon-queue
    (setq chirp-backend--daemon-queue nil)))

(defun chirp-backend--daemon-entry-error-fn (entry)
  "Return ENTRY's wrapped error callback."
  (let ((error-fn (or (plist-get entry :errback)
                      (lambda (message)
                        (message "%s" message))))
        (attempt (plist-get entry :attempt)))
    (lambda (message)
      (funcall error-fn
               (chirp-backend--format-retried-message message attempt)))))

(defun chirp-backend--daemon-entry-json (entry)
  "Return ENTRY encoded as a single JSON request line."
  (concat
   (json-serialize
    `((id . ,(plist-get entry :id))
      (args . ,(apply #'vector (plist-get entry :args)))))
   "\n"))

(defun chirp-backend--daemon-force-kill (process)
  "Force-kill daemon PROCESS if it is still alive."
  (when (process-live-p process)
    (delete-process process)))

(defun chirp-backend-shutdown-daemon (&optional immediate)
  "Gracefully shut down the twitter-cli daemon.

When IMMEDIATE is non-nil, kill the daemon process without waiting."
  (interactive)
  (chirp-backend--daemon-cancel-idle-timer)
  (let ((process chirp-backend--daemon-process))
    (cond
     ((not (process-live-p process))
      (chirp-backend--daemon-clear-state))
     ((or immediate
          (not chirp-backend--daemon-ready-p))
      (setq chirp-backend--daemon-shutting-down t)
      (delete-process process))
     (t
      (setq chirp-backend--daemon-shutting-down t)
      (chirp-backend--daemon-cancel-shutdown-timer)
      (process-send-string
       process
       (concat
        (json-serialize
         `((id . "__shutdown__")
           (args . ["shutdown"])))
        "\n"))
      (setq chirp-backend--daemon-shutdown-timer
            (run-at-time chirp-backend-daemon-shutdown-grace-period
                         nil
                         #'chirp-backend--daemon-force-kill
                         process))))))

(defun chirp-backend--daemon-maybe-shutdown ()
  "Shut down the daemon when it has gone idle."
  (setq chirp-backend--daemon-idle-timer nil)
  (cond
   ((not (chirp-backend--daemon-live-p))
    (chirp-backend--daemon-clear-state))
   ((or (not chirp-backend--daemon-ready-p)
        (> (hash-table-count chirp-backend--daemon-pending) 0)
        chirp-backend--daemon-queue)
    (chirp-backend--daemon-reset-idle-timer))
   (t
    (chirp-backend-shutdown-daemon))))

(defun chirp-backend--daemon-reset-idle-timer ()
  "Restart the daemon idle timer."
  (chirp-backend--daemon-cancel-idle-timer)
  (when (and (> chirp-backend-daemon-idle-timeout 0)
             (chirp-backend--daemon-live-p))
    (setq chirp-backend--daemon-idle-timer
          (run-at-time chirp-backend-daemon-idle-timeout
                       nil
                       #'chirp-backend--daemon-maybe-shutdown))))

(defun chirp-backend--daemon-fallback-entries (entries)
  "Retry daemon ENTRIES via one-shot twitter-cli processes."
  (dolist (entry entries)
    (chirp-backend--request-via-process
     (plist-get entry :args)
     (plist-get entry :callback)
     (plist-get entry :errback)
     (plist-get entry :attempt))))

(defun chirp-backend--daemon-crash (message)
  "Disable the daemon after a protocol MESSAGE and fall back pending reads."
  (let ((queued (chirp-backend--daemon-drain-queue))
        (pending (chirp-backend--daemon-drain-pending))
        (process chirp-backend--daemon-process))
    (setq chirp-backend--daemon-shutting-down t)
    (when (process-live-p process)
      (delete-process process))
    (chirp-backend--daemon-clear-state)
    (setq chirp-backend--daemon-unsupported-p t)
    (message "%s Falling back to one-shot twitter-cli processes." message)
    (chirp-backend--daemon-fallback-entries (append queued pending))))

(defun chirp-backend--daemon-handle-ready ()
  "Mark the daemon ready and flush queued requests."
  (setq chirp-backend--daemon-ready-p t
        chirp-backend--daemon-unsupported-p nil)
  (dolist (entry (chirp-backend--daemon-drain-queue))
    (condition-case nil
        (progn
          (puthash (plist-get entry :id) entry chirp-backend--daemon-pending)
          (process-send-string chirp-backend--daemon-process
                               (chirp-backend--daemon-entry-json entry)))
      (error
       (remhash (plist-get entry :id) chirp-backend--daemon-pending)
       (chirp-backend--request-via-process
        (plist-get entry :args)
        (plist-get entry :callback)
        (plist-get entry :errback)
        (plist-get entry :attempt)))))
  (chirp-backend--daemon-reset-idle-timer))

(defun chirp-backend--daemon-handle-response (payload)
  "Dispatch daemon PAYLOAD to the matching Chirp callback."
  (let* ((request-id (chirp-get payload "id"))
         (entry (and request-id
                     (gethash request-id chirp-backend--daemon-pending))))
    (when request-id
      (remhash request-id chirp-backend--daemon-pending))
    (when entry
      (cond
       ((and (< (plist-get entry :attempt) chirp-cli-max-retries)
             (chirp-backend--transient-error-p payload))
        (chirp-backend--schedule-retry
         (plist-get entry :args)
         (plist-get entry :callback)
         (plist-get entry :errback)
         (plist-get entry :attempt)))
       (t
        (chirp-backend--dispatch
         payload
         (plist-get entry :callback)
         (chirp-backend--daemon-entry-error-fn entry))))))
  (chirp-backend--daemon-reset-idle-timer))

(defun chirp-backend--daemon-handle-line (line)
  "Handle one JSON LINE emitted by the daemon."
  (unless (string-empty-p line)
    (condition-case err
        (let ((payload (json-parse-string line
                                          :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object chirp-backend--json-false)))
          (if (equal (chirp-get payload "event") "ready")
              (chirp-backend--daemon-handle-ready)
            (chirp-backend--daemon-handle-response payload)))
      (error
       (chirp-backend--daemon-crash
        (format "Chirp daemon emitted invalid JSON: %s\n\nOutput: %s"
                (error-message-string err)
                (truncate-string-to-width line 160 nil nil t)))))))

(defun chirp-backend--daemon-filter (_process chunk)
  "Consume daemon stdout CHUNK."
  (let ((combined (concat chirp-backend--daemon-line-buffer chunk))
        (start 0))
    (while (string-match "\n" combined start)
      (chirp-backend--daemon-handle-line
       (string-trim-right (substring combined start (match-beginning 0))))
      (setq start (match-end 0)))
    (setq chirp-backend--daemon-line-buffer (substring combined start))))

(defun chirp-backend--daemon-sentinel (process _event)
  "Handle daemon PROCESS lifecycle changes."
  (when (and (memq (process-status process) '(exit signal))
             (eq process chirp-backend--daemon-process))
    (let* ((intentional chirp-backend--daemon-shutting-down)
           (was-ready chirp-backend--daemon-ready-p)
           (stderr (when (buffer-live-p chirp-backend--daemon-stderr-buffer)
                     (with-current-buffer chirp-backend--daemon-stderr-buffer
                       (string-trim (buffer-string)))))
           (queued (chirp-backend--daemon-drain-queue))
           (pending (chirp-backend--daemon-drain-pending)))
      (chirp-backend--daemon-clear-state)
      (when (and (not intentional)
                 (not was-ready))
        (setq chirp-backend--daemon-unsupported-p t)
        (message "Chirp daemon mode unavailable; falling back to one-shot twitter-cli processes.%s"
                 (if (string-empty-p (or stderr ""))
                     ""
                   (format " %s" stderr))))
      (unless intentional
        (chirp-backend--daemon-fallback-entries (append queued pending))))))

(defun chirp-backend--start-daemon (command)
  "Start the twitter-cli daemon for COMMAND and return non-nil on success."
  (when (and (chirp-backend--daemon-live-p)
             (not (equal command chirp-backend--daemon-command)))
    (chirp-backend-shutdown-daemon t))
  (unless (chirp-backend--daemon-live-p)
    (when (or chirp-backend--daemon-process
              chirp-backend--daemon-stdout-buffer
              chirp-backend--daemon-stderr-buffer)
      (chirp-backend--daemon-clear-state)))
  (unless (chirp-backend--daemon-live-p)
    (let ((stdout-buffer (generate-new-buffer " *chirp-cli-daemon-stdout*"))
          (stderr-buffer (generate-new-buffer " *chirp-cli-daemon-stderr*")))
      (condition-case err
          (setq chirp-backend--daemon-process
                (make-process
                 :name "chirp-cli-daemon"
                 :buffer stdout-buffer
                 :command (list command "daemon" "--stdio")
                 :stderr stderr-buffer
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :filter #'chirp-backend--daemon-filter
                 :sentinel #'chirp-backend--daemon-sentinel
                 :noquery t)
                chirp-backend--daemon-stdout-buffer stdout-buffer
                chirp-backend--daemon-stderr-buffer stderr-buffer
                chirp-backend--daemon-command command
                chirp-backend--daemon-ready-p nil
                chirp-backend--daemon-shutting-down nil
                chirp-backend--daemon-line-buffer "")
        (error
         (when (buffer-live-p stdout-buffer)
           (kill-buffer stdout-buffer))
         (when (buffer-live-p stderr-buffer)
           (kill-buffer stderr-buffer))
         (message "Failed to start Chirp daemon: %s" (error-message-string err))
         (setq chirp-backend--daemon-unsupported-p t
               chirp-backend--daemon-process nil
               chirp-backend--daemon-command nil)))))
  (chirp-backend--daemon-live-p))

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

(defun chirp-backend--request-via-process (args callback errback attempt)
  "Run one-shot twitter-cli with ARGS and call CALLBACK.

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
        (let ((process-environment (cons chirp-backend--compact-json-env
                                         process-environment)))
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
                 (kill-buffer stderr-buffer))))))))))

(defun chirp-backend--request-via-daemon (args callback errback attempt)
  "Run daemon-backed read request with ARGS and call CALLBACK.

ERRBACK receives a single human-readable string.
ATTEMPT tracks how many retries have already been used."
  (let ((error-fn (or errback
                      (lambda (message)
                        (message "%s" message))))
        (command (chirp-backend-command)))
    (if (not command)
        (funcall error-fn (chirp-backend--missing-command-message))
      (if (not (chirp-backend--start-daemon command))
          (chirp-backend--request-via-process args callback errback attempt)
        (let ((entry (list :id (format "%d" (cl-incf chirp-backend--daemon-request-counter))
                           :args args
                           :callback callback
                           :errback errback
                           :attempt attempt)))
          (if chirp-backend--daemon-ready-p
              (condition-case nil
                  (progn
                    (puthash (plist-get entry :id) entry chirp-backend--daemon-pending)
                    (process-send-string chirp-backend--daemon-process
                                         (chirp-backend--daemon-entry-json entry))
                    (chirp-backend--daemon-reset-idle-timer))
                (error
                 (remhash (plist-get entry :id) chirp-backend--daemon-pending)
                 (chirp-backend--request-via-process args callback errback attempt)))
            (setq chirp-backend--daemon-queue
                  (append chirp-backend--daemon-queue (list entry)))))))))

(defun chirp-backend--request (args callback errback attempt)
  "Run twitter-cli with ARGS and call CALLBACK.

ERRBACK receives a single human-readable string.
ATTEMPT tracks how many retries have already been used."
  (if (chirp-backend--daemon-request-p args)
      (chirp-backend--request-via-daemon args callback errback attempt)
    (chirp-backend--request-via-process args callback errback attempt)))

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

(defun chirp-backend-whoami (callback &optional errback)
  "Fetch the authenticated user profile and call CALLBACK."
  (chirp-backend--cached-read
   (chirp-backend--whoami-cache-key)
   (lambda (success error)
     (chirp-backend-request
      '("whoami")
      (lambda (data envelope)
        (let ((user (chirp-normalize-user data)))
          (if user
              (funcall success user envelope)
            (funcall error
                     "twitter-cli returned a whoami payload Chirp could not parse."))))
      error))
   callback
   errback))

(defun chirp-backend-likes (handle callback &optional errback)
  "Fetch liked tweets for HANDLE and call CALLBACK."
  (chirp-backend-request
   (list "likes"
         (string-remove-prefix "@" handle)
         "--max" (number-to-string chirp-default-max-results))
   (lambda (data envelope)
     (funcall callback (chirp-collect-top-level-tweets data) envelope))
   errback))

(defun chirp-backend-list (list-target callback &optional errback)
  "Fetch list timeline data for LIST-TARGET and call CALLBACK."
  (chirp-backend-request
   (list "list"
         (chirp-backend--list-id-from-target list-target)
         "--max" (number-to-string chirp-default-max-results))
   (lambda (data envelope)
     (funcall callback (chirp-collect-top-level-tweets data) envelope))
   errback))

(defun chirp-backend-thread (tweet-or-url callback &optional errback)
  "Fetch thread data for TWEET-OR-URL and call CALLBACK."
  (chirp-backend--cached-read
   (chirp-backend--thread-cache-key tweet-or-url)
   (lambda (success error)
     (chirp-backend-request
      (list "tweet"
            tweet-or-url
            "--max" (number-to-string chirp-thread-max-results))
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

(add-hook 'kill-emacs-hook #'chirp-backend-shutdown-daemon)

(provide 'chirp-backend)

;;; chirp-backend.el ends here
