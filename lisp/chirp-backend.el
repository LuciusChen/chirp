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

(defun chirp-backend-feed (callback &optional following errback max-results)
  "Fetch the home timeline and call CALLBACK.
When FOLLOWING is non-nil, fetch the Following timeline."
  (chirp-backend-request
   (append '("feed")
           (when following
             '("-t" "following"))
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
  (chirp-backend-request
   (list "tweet" tweet-or-url)
   (lambda (data envelope)
     (funcall callback (chirp-collect-tweets data) envelope))
   errback))

(defun chirp-backend-user (handle callback &optional errback)
  "Fetch profile data for HANDLE and call CALLBACK."
  (chirp-backend-request
   (list "user" (string-remove-prefix "@" handle))
   (lambda (data envelope)
     (let ((user (chirp-normalize-user data)))
       (if user
           (funcall callback user envelope)
         (funcall (or errback #'ignore)
                  "twitter-cli returned a profile payload Chirp could not parse."))))
   errback))

(defun chirp-backend-user-posts (handle callback &optional errback)
  "Fetch recent posts for HANDLE and call CALLBACK."
  (chirp-backend-request
   (list "user-posts"
         (string-remove-prefix "@" handle)
         "--max" (number-to-string chirp-profile-post-limit))
   (lambda (data envelope)
     (funcall callback (chirp-collect-top-level-tweets data) envelope))
   errback))

(provide 'chirp-backend)

;;; chirp-backend.el ends here
