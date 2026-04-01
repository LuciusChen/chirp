;;; chirp-core.el --- Shared state and utilities for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)

(declare-function chirp-profile-open "chirp-profile" (handle))
(declare-function chirp-thread-open "chirp-thread" (tweet-or-url &optional focus-id))
(declare-function chirp-dispatch "chirp-actions" ())
(declare-function chirp-load-more "chirp-timeline" (&optional anchor-id))
(declare-function chirp-toggle-home-following "chirp-timeline" ())
(declare-function chirp-media-at-point "chirp-media" ())
(declare-function chirp-media-open "chirp-media" (media-list index &optional title push-history))
(declare-function chirp-media-open-at-point "chirp-media" ())
(declare-function chirp-media-image-mode "chirp-media" ())
(declare-function chirp-media-view-mode "chirp-media" ())

(defgroup chirp nil
  "Browse X/Twitter from Emacs."
  :group 'applications)

(defcustom chirp-cli-command nil
  "Command used to invoke twitter-cli.

When nil, Chirp searches for `twitter' and `twitter-cli' in `exec-path'."
  :type '(choice (const :tag "Auto-detect" nil)
                 string)
  :group 'chirp)

(defcustom chirp-cli-search-paths
  '("~/.local/bin"
    "~/bin"
    "/usr/local/bin"
    "/opt/homebrew/bin"
    "/home/linuxbrew/.linuxbrew/bin")
  "Extra directories Chirp checks for twitter-cli executables.

These paths are consulted after `exec-path' when `chirp-cli-command' is nil."
  :type '(repeat directory)
  :group 'chirp)

(defcustom chirp-buffer-name "*chirp*"
  "Base shared buffer name used for Chirp views."
  :type 'string
  :group 'chirp)

(defcustom chirp-cli-max-retries 2
  "Number of times Chirp retries transient twitter-cli failures."
  :type 'integer
  :group 'chirp)

(defcustom chirp-cli-retry-delay 1.0
  "Seconds to wait before retrying a transient twitter-cli failure."
  :type 'number
  :group 'chirp)

(defcustom chirp-default-max-results 20
  "Default number of posts requested for list views."
  :type 'integer
  :group 'chirp)

(defcustom chirp-timeline-load-more-step 20
  "Number of additional posts fetched when loading more timeline items."
  :type 'integer
  :group 'chirp)

(defcustom chirp-profile-post-limit 15
  "Number of recent posts to fetch for profile buffers."
  :type 'integer
  :group 'chirp)

(defvar-local chirp--refresh-function nil
  "Function used to refresh the current Chirp buffer.")

(defvar-local chirp--view-title nil
  "Human-readable title for the current Chirp buffer.")

(defvar-local chirp--history nil
  "Navigation history stack for the current Chirp buffer.")

(defvar-local chirp--request-token nil
  "Latest async request token for the current Chirp buffer.")

(defvar-local chirp--timeline-kind nil
  "Timeline kind shown in the current Chirp buffer.")

(defvar-local chirp--timeline-limit nil
  "Current max post count for the active timeline buffer.")

(defvar-local chirp--timeline-load-more-function nil
  "Function used to fetch older posts for the current timeline.")

(defvar-local chirp--timeline-loading-more nil
  "Non-nil while Chirp is fetching older timeline posts.")

(defvar-local chirp--rerender-function nil
  "Function used to redraw the current Chirp view without refetching data.")

(defvar-local chirp--rerender-timer nil
  "Pending timer used to coalesce lightweight Chirp rerenders.")

(put 'chirp--history 'permanent-local t)
(put 'chirp--request-token 'permanent-local t)
(put 'chirp--timeline-kind 'permanent-local t)
(put 'chirp--timeline-limit 'permanent-local t)
(put 'chirp--timeline-load-more-function 'permanent-local t)
(put 'chirp--rerender-function 'permanent-local t)

(defvar chirp--suspend-history nil
  "When non-nil, Chirp navigation should not push history entries.")

(defvar chirp--shared-buffer nil
  "The shared Chirp buffer object, even when it is renamed per view.")

(defvar chirp-tweet-state-overrides (make-hash-table :test #'equal)
  "Map tweet ids to local state overrides such as likes and bookmarks.")

(defvar chirp-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'chirp-refresh)
    (define-key map (kbd "b") #'chirp-back)
    (define-key map (kbd "TAB") #'chirp-toggle-home-following)
    (define-key map (kbd "n") #'chirp-next-entry)
    (define-key map (kbd "p") #'chirp-previous-entry)
    (define-key map (kbd "N") #'chirp-load-more)
    (define-key map (kbd "RET") #'chirp-open-at-point)
    (define-key map (kbd "t") #'chirp-open-at-point)
    (define-key map (kbd "m") #'chirp-open-primary-media)
    (define-key map (kbd "A") #'chirp-open-author-at-point)
    (define-key map (kbd "x") #'chirp-dispatch)
    (define-key map (kbd "o") #'chirp-browse-at-point)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `chirp-view-mode'.")

(define-derived-mode chirp-view-mode special-mode "Chirp"
  "Major mode for Chirp buffers."
  (setq-local truncate-lines nil)
  (setq-local line-spacing 0.1))

(defun chirp--base-buffer-stem ()
  "Return the display stem used for Chirp buffer names."
  (let ((name chirp-buffer-name))
    (if (string-match "\\`\\*\\(.*?\\)\\*\\'" name)
        (match-string 1 name)
      name)))

(defun chirp--format-buffer-name (&optional title)
  "Return a shared buffer name for TITLE."
  (if (and (stringp title)
           (not (string-empty-p title)))
      (format "*%s: %s*" (chirp--base-buffer-stem) title)
    chirp-buffer-name))

(defun chirp--apply-buffer-name (buffer &optional title)
  "Rename BUFFER to match TITLE and return BUFFER."
  (with-current-buffer buffer
    (let ((new-name (chirp--format-buffer-name title)))
      (unless (string= (buffer-name buffer) new-name)
        (setq chirp--shared-buffer (rename-buffer new-name t)))))
  buffer)

(defun chirp-buffer ()
  "Return the shared Chirp buffer."
  (setq chirp--shared-buffer
        (if (buffer-live-p chirp--shared-buffer)
            chirp--shared-buffer
          (get-buffer-create chirp-buffer-name))))

(defun chirp-display-buffer (buffer)
  "Display BUFFER in the selected window."
  (unless (eq (window-buffer (selected-window)) buffer)
    (switch-to-buffer buffer)))

(defun chirp--active-buffer-p ()
  "Return non-nil when the current buffer is a Chirp view buffer."
  (or (derived-mode-p 'chirp-view-mode)
      (derived-mode-p 'chirp-media-image-mode)
      (derived-mode-p 'chirp-media-view-mode)))

(defun chirp--snapshot-state ()
  "Capture the current Chirp buffer state."
  (list :mode major-mode
        :content (buffer-substring (point-min) (point-max))
        :point (point)
        :locals
        (mapcar
         (lambda (symbol)
           (cons symbol
                 (and (boundp symbol)
                      (local-variable-p symbol)
                      (symbol-value symbol))))
         '(chirp--view-title
           chirp--refresh-function
           chirp--timeline-kind
           chirp--timeline-limit
           chirp--timeline-load-more-function
           chirp--rerender-function
           chirp--media-list
           chirp--media-index
           chirp--media-title))))

(defun chirp--maybe-push-history ()
  "Save the current Chirp view so `chirp-back' can restore it."
  (when (and (not chirp--suspend-history)
             (chirp--active-buffer-p)
             (eq (current-buffer) (chirp-buffer))
             (or chirp--view-title
                 (> (buffer-size) 0)))
    (push (chirp--snapshot-state) chirp--history)))

(defun chirp--restore-state (state)
  "Restore a Chirp buffer STATE snapshot."
  (let ((inhibit-read-only t)
        (history chirp--history)
        (mode (plist-get state :mode))
        (locals (plist-get state :locals)))
    (pcase mode
      ('chirp-media-image-mode
       (erase-buffer)
       (insert (plist-get state :content))
       (chirp-media-image-mode))
      ('chirp-media-view-mode
       (chirp-media-view-mode))
      (_
       (chirp-view-mode)))
    (setq-local chirp--history history)
    (setq-local chirp--request-token nil)
    (setq-local header-line-format nil)
    (dolist (binding locals)
      (set (make-local-variable (car binding)) (cdr binding)))
    (unless (eq mode 'chirp-media-image-mode)
      (erase-buffer)
      (insert (plist-get state :content)))
    (chirp--apply-buffer-name (current-buffer) chirp--view-title)
    (goto-char (min (point-max) (plist-get state :point)))))

(defun chirp-back ()
  "Return to the previous Chirp view."
  (interactive)
  (unless chirp--history
    (user-error "No previous Chirp view"))
  (chirp--restore-state (pop chirp--history)))

(defun chirp-show-loading (buffer title refresh)
  "Display a loading message in BUFFER for TITLE.

Return a token that identifies the current request."
  (let ((token (chirp-begin-request buffer)))
    (chirp-render-into-buffer
     buffer title refresh
     (lambda ()
       (insert "Loading...\n")))
    (chirp-display-buffer buffer)
    token))

(defun chirp-begin-request (buffer)
  "Return a new request token for BUFFER."
  (let ((token (gensym "chirp-request-")))
    (with-current-buffer buffer
      (setq-local chirp--request-token token))
    token))

(defun chirp-request-current-p (buffer token)
  "Return non-nil when TOKEN still matches BUFFER's active request."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (eq chirp--request-token token))))

(defun chirp-render-into-buffer (buffer title refresh render-fn)
  "Render BUFFER with TITLE using REFRESH and RENDER-FN."
  (with-current-buffer buffer
    (chirp-view-mode)
    (setq-local chirp--view-title title)
    (setq-local chirp--refresh-function refresh)
    (setq-local chirp--timeline-kind nil)
    (setq-local chirp--timeline-limit nil)
    (setq-local chirp--timeline-load-more-function nil)
    (setq-local chirp--timeline-loading-more nil)
    (setq-local chirp--rerender-function nil)
    (when (timerp chirp--rerender-timer)
      (cancel-timer chirp--rerender-timer))
    (setq-local chirp--rerender-timer nil)
    (setq-local header-line-format nil)
    (chirp--apply-buffer-name buffer title)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (funcall render-fn)
      (goto-char (point-min)))))

(defun chirp-request-rerender (&optional buffer delay)
  "Schedule a lightweight rerender of BUFFER after DELAY seconds."
  (let ((target (or buffer (current-buffer)))
        (wait (or delay 0.1)))
    (when (buffer-live-p target)
      (with-current-buffer target
        (when (timerp chirp--rerender-timer)
          (cancel-timer chirp--rerender-timer))
        (setq-local
         chirp--rerender-timer
         (run-at-time
          wait nil
          (lambda (buf)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq-local chirp--rerender-timer nil)
                (when chirp--rerender-function
                  (let ((chirp--suspend-history t))
                    (funcall chirp--rerender-function))))))
          target))))))

(defun chirp-show-error (buffer title refresh message)
  "Display MESSAGE in BUFFER for TITLE."
  (chirp-render-into-buffer
   buffer title refresh
   (lambda ()
     (insert "Unable to load data.\n\n")
     (insert message)
     (insert "\n"))))

(defun chirp-refresh ()
  "Refresh the current Chirp buffer."
  (interactive)
  (if chirp--refresh-function
      (let ((chirp--suspend-history
             (not (memq chirp--timeline-kind '(home following)))))
        (funcall chirp--refresh-function))
    (user-error "No refresh function for this buffer")))

(defun chirp--entry-position-forward (start)
  "Return the next entry start at or after START."
  (or (and (< start (point-max))
           (eq (get-text-property start 'chirp-entry-start) t)
           start)
      (text-property-any start (point-max) 'chirp-entry-start t)))

(defun chirp--entry-position-backward (start)
  "Return the previous entry start before START."
  (let ((search-end (max (point-min) start))
        (probe (text-property-any (point-min) (max (point-min) start)
                                  'chirp-entry-start t))
        last)
    (while probe
      (setq last probe
            probe (text-property-any (min (point-max) (1+ probe))
                                     search-end
                                     'chirp-entry-start t)))
    last))

(defun chirp-next-entry ()
  "Move to the next entry."
  (interactive)
  (let* ((origin (point))
         (pos (chirp--entry-position-forward (min (point-max) (1+ origin)))))
    (cond
     (pos
      (goto-char pos))
     ((and chirp--timeline-load-more-function
           (chirp-entry-at-point))
      (funcall chirp--timeline-load-more-function (chirp-entry-id-at-point)))
     ((setq pos (chirp--entry-position-forward (point-min)))
      (goto-char pos))
     (t
      (user-error "No entries in this buffer")))))

(defun chirp-previous-entry ()
  "Move to the previous entry."
  (interactive)
  (let* ((origin (point))
         (start (max (point-min) (1- origin)))
         (pos (chirp--entry-position-backward start)))
    (unless pos
      (setq pos (chirp--entry-position-backward (point-max))))
    (if pos
        (goto-char pos)
      (user-error "No entries in this buffer"))))

(defun chirp-entry-at-point ()
  "Return the Chirp entry stored at point, or nil."
  (or (get-text-property (point) 'chirp-entry-item)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-entry-item))))

(defun chirp-entry-id-at-point ()
  "Return the current Chirp entry id, or nil."
  (plist-get (chirp-entry-at-point) :id))

(defun chirp-entry-position-by-id (id)
  "Return the start position of the entry whose id equals ID."
  (when id
    (let ((pos (chirp--entry-position-forward (point-min)))
          found)
      (while (and pos (not found))
        (when (equal (plist-get (get-text-property pos 'chirp-entry-item) :id) id)
          (setq found pos))
        (unless found
          (setq pos (chirp--entry-position-forward (min (point-max) (1+ pos))))))
      found)))

(defun chirp-goto-entry-id (id)
  "Move point to the entry whose id equals ID."
  (when-let* ((pos (chirp-entry-position-by-id id)))
    (goto-char pos)
    t))

(defun chirp-entry-url-at-point ()
  "Return the hidden URL stored on the current Chirp entry, or nil."
  (or (get-text-property (point) 'chirp-entry-url)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-entry-url))))

(defun chirp-open-at-point ()
  "Open the entry at point."
  (interactive)
  (if (chirp-media-at-point)
      (chirp-media-open-at-point)
    (pcase (plist-get (chirp-entry-at-point) :kind)
      ('tweet (chirp-thread-open (chirp-entry-at-point)
                                 (plist-get (chirp-entry-at-point) :id)))
      ('user (chirp-profile-open (plist-get (chirp-entry-at-point) :handle)))
      (_ (user-error "No entry at point")))))

(defun chirp-open-primary-media ()
  "Open the media at point or the first media of the current entry."
  (interactive)
  (if (chirp-media-at-point)
      (chirp-media-open-at-point)
    (let* ((entry (chirp-entry-at-point))
           (media-list (plist-get entry :media)))
      (if media-list
          (chirp-media-open media-list
                            0
                            (or chirp--view-title "Chirp Media")
                            t)
        (user-error "No media available at point")))))

(defun chirp-open-author-at-point ()
  "Open the author profile for the current entry."
  (interactive)
  (let* ((entry (chirp-entry-at-point))
         (handle (or (plist-get entry :author-handle)
                     (plist-get entry :handle))))
    (if handle
        (chirp-profile-open handle)
      (user-error "No profile available at point"))))

(defun chirp-browse-at-point ()
  "Open the current entry in a browser."
  (interactive)
  (let* ((media (chirp-media-at-point))
         (entry (chirp-entry-at-point))
         (url (or (plist-get media :url)
                  (chirp-entry-url-at-point)
                  (plist-get entry :url)
                  (plist-get entry :profile-url))))
    (if url
        (browse-url url)
      (user-error "No URL available at point"))))

(defun chirp-object-p (value)
  "Return non-nil when VALUE looks like an alist-style JSON object."
  (and (listp value)
       (or (null value)
           (let ((head (car value)))
             (and (consp head)
                  (or (stringp (car head))
                      (symbolp (car head))))))))

(defun chirp-get (object &rest keys)
  "Return the first matching value in OBJECT for KEYS."
  (when (chirp-object-p object)
    (cl-loop for key in keys
             for cell = (assoc-string key object t)
             when cell
             return (cdr cell))))

(defun chirp-get-in (object path)
  "Return the value at PATH inside OBJECT."
  (let ((value object))
    (catch 'missing
      (dolist (key path value)
        (setq value
              (cond
               ((chirp-object-p value)
                (let ((cell (assoc-string key value t)))
                  (if cell
                      (cdr cell)
                    (throw 'missing nil))))
               (t
                (throw 'missing nil))))))))

(defun chirp-coalesce (&rest values)
  "Return the first non-nil element in VALUES."
  (cl-loop for value in values
           when value
           return value))

(defun chirp-boolean-value (value)
  "Normalize VALUE into a Lisp boolean."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((numberp value) (not (zerop value)))
   ((stringp value)
    (not (null (member (downcase value) '("1" "true" "yes" "on")))))
   ((symbolp value)
    (not (string= (symbol-name value) "chirp-json-false")))
   (t t)))

(defun chirp-first-nonblank (&rest values)
  "Return the first non-blank string in VALUES."
  (cl-loop for value in values
           when (and (stringp value)
                     (not (string-blank-p value)))
           return value))

(defun chirp-clean-text (value)
  "Normalize VALUE into a human-readable string."
  (cond
   ((stringp value)
    (string-trim (replace-regexp-in-string "\r" "" value)))
   ((null value) "")
   (t
    (string-trim (format "%s" value)))))

(defun chirp-format-count (value)
  "Return a display string for VALUE."
  (cond
   ((null value) "-")
   ((numberp value) (number-to-string value))
   ((stringp value) value)
   (t (format "%s" value))))

(defun chirp-adjust-count (value delta)
  "Return VALUE adjusted by DELTA when it looks numeric."
  (let ((number
         (cond
          ((numberp value) value)
          ((and (stringp value)
                (string-match-p "\\`[0-9]+\\'" value))
           (string-to-number value))
          (t nil))))
    (if number
        (max 0 (+ number delta))
      value)))

(defun chirp-plist-override (plist prop fallback)
  "Return PROP from PLIST when present, otherwise FALLBACK."
  (if (plist-member plist prop)
      (plist-get plist prop)
    fallback))

(defun chirp-set-tweet-state-override (tweet-id prop value)
  "Store VALUE as local PROP override for TWEET-ID."
  (when tweet-id
    (let ((state (copy-sequence (gethash tweet-id chirp-tweet-state-overrides))))
      (setq state (plist-put state prop value))
      (puthash tweet-id state chirp-tweet-state-overrides))))

(defun chirp-clear-tweet-state-overrides (tweet-id)
  "Clear local state overrides for TWEET-ID."
  (when tweet-id
    (remhash tweet-id chirp-tweet-state-overrides)))

(defun chirp--map-buffer-tweets (buffer fn)
  "Call FN for each distinct tweet entry visible in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((pos (chirp--entry-position-forward (point-min)))
            (seen (make-hash-table :test #'eq)))
        (while pos
          (let ((entry (get-text-property pos 'chirp-entry-item)))
            (when (and (eq (plist-get entry :kind) 'tweet)
                       (not (gethash entry seen)))
              (puthash entry t seen)
              (funcall fn entry)))
          (setq pos (chirp--entry-position-forward
                     (min (point-max) (1+ pos)))))))))

(defun chirp-update-tweet-by-id (buffer tweet-id fn &optional rerender)
  "Apply FN to every tweet matching TWEET-ID in BUFFER.

When RERENDER is non-nil, request a lightweight rerender afterwards."
  (let (updated)
    (chirp--map-buffer-tweets
     buffer
     (lambda (tweet)
       (when (equal (plist-get tweet :id) tweet-id)
         (setq updated t)
         (funcall fn tweet))))
    (when (and updated rerender)
      (chirp-request-rerender buffer))
    updated))

(defun chirp-user-like-p (object)
  "Return non-nil when OBJECT resembles a user payload."
  (and (chirp-object-p object)
       (or (chirp-first-nonblank
            (chirp-get object "screen_name" "screenName" "username" "handle")
            (chirp-get-in object '("legacy" "screen_name")))
           (chirp-get object "id" "rest_id"))
       (or (chirp-first-nonblank
            (chirp-get object "name" "display_name" "description" "bio")
            (chirp-get-in object '("legacy" "name"))
            (chirp-get-in object '("legacy" "description")))
           (chirp-get object "followers_count" "friends_count" "statuses_count")
           (chirp-get-in object '("legacy" "followers_count"))
           (chirp-get-in object '("legacy" "friends_count")))))

(defun chirp-tweet-like-p (object)
  "Return non-nil when OBJECT resembles a tweet payload."
  (let* ((legacy (chirp-get object "legacy"))
         (metrics (chirp-get object "metrics"))
         (id (chirp-first-nonblank
              (chirp-get object "id" "id_str" "rest_id")
              (chirp-get legacy "id_str")))
         (text (chirp-first-nonblank
                (chirp-get object "full_text" "text")
                (chirp-get legacy "full_text" "text")
                (chirp-get-in object '("note_tweet" "note_tweet_results" "result" "text"))
                (chirp-get-in object '("note_tweet" "text"))))
         (stats (chirp-coalesce
                 (chirp-get object "favorite_count" "retweet_count" "reply_count"
                            "quote_count" "bookmark_count" "view_count")
                 (and metrics
                      (chirp-get metrics "likes" "retweets" "replies"
                                 "quotes" "bookmarks" "views"))
                 (and legacy
                      (chirp-get legacy "favorite_count" "retweet_count"
                                 "reply_count" "quote_count")))))
    (and (chirp-object-p object)
         id
         (or text stats (chirp-get object "conversationId" "conversation_id"))
         (not (chirp-user-like-p object)))))

(defun chirp-find-first-object (value predicate)
  "Return the first object inside VALUE that satisfies PREDICATE."
  (let (result)
    (cl-labels ((walk (node)
                  (cond
                   (result nil)
                   ((chirp-object-p node)
                    (when (funcall predicate node)
                      (setq result node))
                    (unless result
                      (dolist (cell node)
                        (walk (cdr cell)))))
                   ((vectorp node)
                    (mapc #'walk (append node nil)))
                   ((listp node)
                    (mapc #'walk node)))))
      (walk value))
    result))

(defun chirp-extract-user-object (object)
  "Extract the most relevant user object from OBJECT."
  (let ((direct (or (chirp-get object "user" "author")
                    (chirp-get-in object '("core" "user_results" "result"))
                    (chirp-get-in object '("author_results" "result"))
                    (chirp-get-in object '("user_results" "result"))
                    (chirp-get-in object '("result")))))
    (cond
     ((chirp-user-like-p object) object)
     ((chirp-user-like-p direct) direct)
     (t (chirp-find-first-object object #'chirp-user-like-p)))))

(defun chirp-normalize-user (object)
  "Normalize OBJECT into a user plist."
  (let* ((user (chirp-extract-user-object object))
         (legacy (chirp-get user "legacy"))
         (handle (chirp-first-nonblank
                  (chirp-get user "screen_name" "screenName" "username" "handle")
                  (chirp-get legacy "screen_name")))
         (name (chirp-first-nonblank
                (chirp-get user "name" "display_name")
                (chirp-get legacy "name")
                handle))
         (id (chirp-first-nonblank
              (chirp-get user "id" "rest_id")
              (chirp-get legacy "id_str")))
         (bio (chirp-clean-text
               (chirp-first-nonblank
                (chirp-get user "description" "bio")
                (chirp-get legacy "description"))))
         (followers (chirp-coalesce
                     (chirp-get user "followers_count" "followers")
                     (chirp-get legacy "followers_count")))
         (following (chirp-coalesce
                     (chirp-get user "friends_count" "following_count" "following")
                     (chirp-get legacy "friends_count")))
         (posts (chirp-coalesce
                 (chirp-get user "statuses_count" "tweets_count" "tweets")
                 (chirp-get legacy "statuses_count")))
         (joined (chirp-first-nonblank
                  (chirp-get user "createdAtLocal" "createdAtISO" "createdAt" "created_at")
                  (chirp-get legacy "created_at")))
         (avatar-url (chirp-first-nonblank
                      (chirp-get user "profileImageUrl" "profile_image_url_https" "profile_image_url")
                      (chirp-get legacy "profile_image_url_https" "profile_image_url"))))
    (when (or handle id name)
      (list :kind 'user
            :id id
            :name name
            :handle (and handle (string-remove-prefix "@" handle))
            :bio bio
            :avatar-url avatar-url
            :followers followers
            :following following
            :posts posts
            :joined joined
            :profile-url (and handle
                              (format "https://x.com/%s"
                                      (string-remove-prefix "@" handle)))
            :raw object))))

(defun chirp-normalize-media-item (object)
  "Normalize media OBJECT into a plist."
  (let ((type (chirp-first-nonblank (chirp-get object "type")))
        (url (chirp-first-nonblank (chirp-get object "url")))
        (preview-url (chirp-first-nonblank
                      (chirp-get object
                                 "preview_url" "previewUrl"
                                 "preview_image_url" "previewImageUrl"
                                 "thumbnail_url" "thumbnailUrl"
                                 "poster_url" "posterUrl")
                      (chirp-get-in object '("preview" "url"))
                      (chirp-get-in object '("thumbnail" "url"))
                      (chirp-get-in object '("poster" "url"))))
        (width (chirp-get object "width"))
        (height (chirp-get object "height")))
    (when (and type url)
      (list :type type
            :url url
            :preview-url preview-url
            :width width
            :height height))))

(defun chirp-normalize-media-list (value)
  "Normalize media VALUE into a list of plists."
  (if (listp value)
      (delq nil (mapcar #'chirp-normalize-media-item value))
    nil))

(defun chirp-normalize-tweet (object)
  "Normalize OBJECT into a tweet plist."
  (let* ((legacy (chirp-get object "legacy"))
         (metrics (chirp-get object "metrics"))
         (author (chirp-extract-user-object object))
         (author-user (chirp-normalize-user author))
         (id (chirp-first-nonblank
              (chirp-get object "id" "id_str" "rest_id")
              (chirp-get legacy "id_str")))
         (text (chirp-clean-text
                (chirp-first-nonblank
                 (chirp-get object "full_text" "text")
                 (chirp-get legacy "full_text" "text")
                 (chirp-get-in object '("note_tweet" "note_tweet_results" "result" "text"))
                 (chirp-get-in object '("note_tweet" "text")))))
         (author-handle (plist-get author-user :handle))
         (media (chirp-normalize-media-list (chirp-get object "media")))
         (reply-to-handle (let ((handle (chirp-first-nonblank
                                         (chirp-get object "inReplyToScreenName"
                                                    "in_reply_to_screen_name")
                                         (chirp-get legacy "in_reply_to_screen_name"))))
                            (and handle
                                 (string-remove-prefix "@" handle))))
         (reply-to-id (chirp-first-nonblank
                       (chirp-get object "inReplyToStatusId"
                                  "in_reply_to_status_id_str"
                                  "in_reply_to_status_id")
                       (chirp-get legacy "in_reply_to_status_id_str"
                                  "in_reply_to_status_id")))
         (retweeted-p (chirp-boolean-value
                       (chirp-coalesce
                        (chirp-get object "retweeted" "isRetweeted")
                        (chirp-get-in object '("viewer" "retweeted"))
                        (chirp-get legacy "retweeted"))))
         (liked-p (chirp-boolean-value
                   (chirp-coalesce
                    (chirp-get object "liked" "favorited" "isLiked" "isFavorited")
                    (chirp-get-in object '("viewer" "liked"))
                    (chirp-get-in object '("viewer" "favorited"))
                    (chirp-get legacy "liked" "favorited"))))
         (bookmarked-p (chirp-boolean-value
                        (chirp-coalesce
                         (chirp-get object "bookmarked" "isBookmarked")
                         (chirp-get-in object '("viewer" "bookmarked"))
                         (chirp-get legacy "bookmarked"))))
         (state-overrides (and id (gethash id chirp-tweet-state-overrides)))
         (url (chirp-first-nonblank
               (chirp-get object "url")
               (chirp-get legacy "url")
               (and id author-handle
                    (format "https://x.com/%s/status/%s" author-handle id))
               (and id (format "https://x.com/i/status/%s" id)))))
    (when (or id (not (string-empty-p text)))
      (list :kind 'tweet
            :id id
            :text text
            :created-at (chirp-first-nonblank
                         (chirp-get object "createdAtLocal" "createdAtISO" "createdAt" "created_at")
                         (chirp-get legacy "created_at"))
            :url url
            :conversation-id (chirp-first-nonblank
                              (chirp-get object "conversationId" "conversation_id")
                              (chirp-get legacy "conversation_id_str"))
            :reply-to-id reply-to-id
            :reply-to-handle reply-to-handle
            :author-name (plist-get author-user :name)
            :author-handle author-handle
            :author-avatar-url (plist-get author-user :avatar-url)
            :media media
            :retweeted-p (chirp-plist-override state-overrides :retweeted-p retweeted-p)
            :liked-p (chirp-plist-override state-overrides :liked-p liked-p)
            :bookmarked-p (chirp-plist-override state-overrides :bookmarked-p bookmarked-p)
            :reply-count (chirp-coalesce
                          (chirp-get object "reply_count")
                          (chirp-get metrics "replies")
                          (chirp-get legacy "reply_count"))
            :retweet-count (chirp-coalesce
                            (chirp-get object "retweet_count")
                            (chirp-get metrics "retweets")
                            (chirp-get legacy "retweet_count"))
            :like-count (chirp-coalesce
                         (chirp-get object "favorite_count" "like_count")
                         (chirp-get metrics "likes")
                         (chirp-get legacy "favorite_count"))
            :quote-count (chirp-coalesce
                          (chirp-get object "quote_count")
                          (chirp-get metrics "quotes")
                          (chirp-get legacy "quote_count"))
            :bookmark-count (chirp-coalesce
                             (chirp-get object "bookmark_count")
                             (chirp-get metrics "bookmarks"))
            :view-count (chirp-coalesce
                         (chirp-get object "view_count" "views")
                         (chirp-get metrics "views")
                         (chirp-get-in object '("views" "count")))
            :raw object))))

(defun chirp-collect-top-level-tweets (value)
  "Collect normalized tweets from the top level of VALUE."
  (cond
   ((chirp-tweet-like-p value)
    (let ((tweet (chirp-normalize-tweet value)))
      (if tweet (list tweet) nil)))
   ((listp value)
    (delq nil
          (mapcar (lambda (item)
                    (when (chirp-tweet-like-p item)
                      (chirp-normalize-tweet item)))
                  value)))
   (t nil)))

(defun chirp-collect-tweets (value)
  "Collect normalized tweets recursively from VALUE."
  (let ((seen (make-hash-table :test #'equal))
        tweets)
    (cl-labels ((walk (node)
                  (cond
                   ((chirp-tweet-like-p node)
                    (let* ((tweet (chirp-normalize-tweet node))
                           (id (plist-get tweet :id))
                           (key (or id (plist-get tweet :url))))
                      (when (and tweet
                                 (or (not key)
                                     (not (gethash key seen))))
                        (when key
                          (puthash key t seen))
                        (push tweet tweets))))
                   ((chirp-object-p node)
                    (dolist (cell node)
                      (walk (cdr cell))))
                   ((vectorp node)
                    (mapc #'walk (append node nil)))
                   ((listp node)
                    (mapc #'walk node)))))
      (walk value))
    (nreverse tweets)))

(defun chirp-move-point-to-first-entry ()
  "Move point to the first entry in the current buffer."
  (when-let* ((pos (chirp--entry-position-forward (point-min))))
    (goto-char pos)))

(provide 'chirp-core)

;;; chirp-core.el ends here
