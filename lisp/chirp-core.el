;;; chirp-core.el --- Shared state and utilities for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)

(declare-function chirp-backend-tweet "chirp-backend" (tweet-id callback &optional errback))
(declare-function chirp-profile-open "chirp-profile" (handle &optional buffer))
(declare-function chirp-profile-followers "chirp-profile" (handle &optional buffer))
(declare-function chirp-profile-following-users "chirp-profile" (handle &optional buffer))
(declare-function chirp-thread-open "chirp-thread" (tweet-or-url &optional focus-id buffer))
(declare-function chirp-dispatch "chirp-actions" ())
(declare-function chirp-toggle-follow-user-at-point "chirp-actions" ())
(declare-function chirp-load-more "chirp-timeline" (&optional anchor-id))
(declare-function chirp-toggle-home-following "chirp-timeline" ())
(declare-function chirp-media-at-point "chirp-media" ())
(declare-function chirp-media-open "chirp-media" (media-list index &optional title buffer))
(declare-function chirp-media-open-at-point "chirp-media" ())
(declare-function chirp-media-download-at-point "chirp-media" ())
(declare-function chirp-media-prefetch-tweet "chirp-media" (tweet buffer))
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
  "Base buffer name used for newly created Chirp views."
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

(defcustom chirp-timeline-refresh-max-results 10
  "Number of head posts requested when refreshing a timeline with `g'.

Refreshing only needs a recent head window to detect and merge newer posts, so
this can stay smaller than `chirp-default-max-results' for better latency.
Set it to nil to refresh using the current timeline size instead."
  :type '(choice (const :tag "Use current timeline size" nil)
                 integer)
  :group 'chirp)

(defcustom chirp-rerender-idle-delay 0.2
  "Seconds Chirp waits for Emacs to go idle before background rerenders.

This applies to lightweight redraws triggered by async media, link-card, and
quoted-tweet enrichment.  Using an idle timer keeps cursor and mouse movement
smoother while background data arrives."
  :type 'number
  :group 'chirp)

(defcustom chirp-timeline-load-more-step 20
  "Number of additional posts fetched when loading more timeline items."
  :type 'integer
  :group 'chirp)

(defcustom chirp-profile-post-limit 15
  "Number of posts fetched per profile page."
  :type 'integer
  :group 'chirp)

(defcustom chirp-thread-max-results 20
  "Number of tweets to request when opening a thread.

This caps the initial `twitter tweet --max` fetch so Chirp does not inherit the
CLI's larger default reply count."
  :type 'integer
  :group 'chirp)

(defcustom chirp-hide-promoted-posts t
  "When non-nil, hide tweets explicitly marked as promoted by twitter-cli."
  :type 'boolean
  :group 'chirp)

(defvar-local chirp--refresh-function nil
  "Function used to refresh the current Chirp buffer.")

(defvar-local chirp--view-title nil
  "Human-readable title for the current Chirp buffer.")

(defvar-local chirp--request-token nil
  "Latest async request token for the current Chirp buffer.")

(defvar-local chirp--timeline-kind nil
  "Timeline kind shown in the current Chirp buffer.")

(defvar-local chirp--timeline-limit nil
  "Current max post count for the active timeline buffer.")

(defvar-local chirp--timeline-count nil
  "Current number of posts shown in the active timeline buffer.")

(defvar-local chirp--timeline-next-cursor nil
  "Pagination cursor used to fetch older posts for the active timeline buffer.")

(defvar-local chirp--timeline-load-more-function nil
  "Function used to fetch older posts for the current timeline.")

(defvar-local chirp--timeline-exhausted-p nil
  "Non-nil when the active timeline has no more older posts to fetch.")

(defvar-local chirp--timeline-loading-more nil
  "Non-nil while Chirp is fetching older timeline posts.")

(defvar-local chirp--rerender-function nil
  "Function used to redraw the current Chirp view without refetching data.")

(defvar-local chirp--rerender-timer nil
  "Pending timer used to coalesce lightweight Chirp rerenders.")

(defvar-local chirp--entry-wrap-navigation t
  "When non-nil, entry navigation wraps around at buffer boundaries.")

(defvar-local chirp--profile-handle nil
  "Profile handle represented by the current profile buffer, or nil.")

(defvar-local chirp--profile-view-mode nil
  "Current profile subview mode for the active profile buffer.")

(defvar-local chirp--profile-view-modes nil
  "Available profile subview modes for the active profile buffer.")

(defvar-local chirp--profile-switch-mode-function nil
  "Function used to switch the current profile buffer to another subview.")

(defvar-local chirp--status-text nil
  "Persistent status text shown for the current Chirp buffer.")

(defvar-local chirp--status-kind nil
  "Kind of status currently shown for the current Chirp buffer.")

(defvar-local chirp--status-start-time nil
  "Timestamp when the current Chirp status started.")

(defvar-local chirp--status-timer nil
  "Timer used to refresh Chirp's persistent status display.")

(put 'chirp--request-token 'permanent-local t)
(put 'chirp--timeline-kind 'permanent-local t)
(put 'chirp--timeline-limit 'permanent-local t)
(put 'chirp--timeline-count 'permanent-local t)
(put 'chirp--timeline-next-cursor 'permanent-local t)
(put 'chirp--timeline-load-more-function 'permanent-local t)
(put 'chirp--timeline-exhausted-p 'permanent-local t)
(put 'chirp--rerender-function 'permanent-local t)
(put 'chirp--entry-wrap-navigation 'permanent-local t)
(put 'chirp--profile-handle 'permanent-local t)
(put 'chirp--profile-view-mode 'permanent-local t)
(put 'chirp--profile-view-modes 'permanent-local t)
(put 'chirp--profile-switch-mode-function 'permanent-local t)
(put 'chirp--status-text 'permanent-local t)
(put 'chirp--status-kind 'permanent-local t)
(put 'chirp--status-start-time 'permanent-local t)
(put 'chirp--status-timer 'permanent-local t)
(defvar chirp-tweet-state-overrides (make-hash-table :test #'equal)
  "Map tweet ids to local state overrides such as likes and bookmarks.")

(defvar chirp-quoted-tweet-cache (make-hash-table :test #'equal)
  "Map quoted tweet ids to enriched Chirp tweet plists.")

(defvar chirp-quoted-tweet-pending (make-hash-table :test #'equal)
  "Map quoted tweet ids to pending enrichment callbacks.")

(defconst chirp--quoted-tweet-fetch-failed (make-symbol "chirp-quoted-tweet-fetch-failed")
  "Sentinel value used when quoted tweet enrichment fails.")

(defvar chirp-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'chirp-refresh)
    (define-key map (kbd "TAB") #'chirp-toggle-home-following)
    (define-key map (kbd "n") #'chirp-next-entry)
    (define-key map (kbd "p") #'chirp-previous-entry)
    (define-key map (kbd "N") #'chirp-load-more)
    (define-key map (kbd "RET") #'chirp-open-at-point)
    (define-key map (kbd "t") #'chirp-open-at-point)
    (define-key map (kbd "m") #'chirp-open-primary-media)
    (define-key map (kbd "D") #'chirp-media-download-at-point)
    (define-key map (kbd "A") #'chirp-open-author-at-point)
    (define-key map (kbd "x") #'chirp-dispatch)
    (define-key map (kbd "o") #'chirp-browse-at-point)
    (define-key map (kbd "q") #'chirp-quit-current-buffer)
    map)
  "Keymap for `chirp-view-mode'.")

(define-derived-mode chirp-view-mode special-mode "Chirp"
  "Major mode for Chirp buffers."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local line-spacing 0.1)
  (setq-local mode-line-process
              '((:eval (chirp--mode-line-status-string))))
  (visual-line-mode 1))

(defun chirp--status-face (kind)
  "Return a mode-line face for status KIND."
  (pcase kind
    ('error 'error)
    (_ 'mode-line-emphasis)))

(defun chirp--mode-line-status-string ()
  "Return the mode-line string for the current Chirp status."
  (when (and chirp--status-text
             (not (string-empty-p chirp--status-text)))
    (let* ((elapsed (if chirp--status-start-time
                        (max 0.0 (- (float-time) chirp--status-start-time))
                      0.0))
           (text (format " · %s %.1fs" chirp--status-text elapsed)))
      (propertize text 'face (chirp--status-face chirp--status-kind)))))

(defun chirp--ensure-status-timer (buffer)
  "Ensure BUFFER has a status refresh timer."
  (with-current-buffer buffer
    (unless (timerp chirp--status-timer)
      (let (timer)
        (setq timer
              (run-with-timer
               0.0 0.5
               (lambda ()
                 (if (not (buffer-live-p buffer))
                     (cancel-timer timer)
                   (with-current-buffer buffer
                     (if chirp--status-text
                         (force-mode-line-update t)
                       (cancel-timer timer)
                       (setq-local chirp--status-timer nil)))))))
        (setq-local chirp--status-timer timer)))))

(defun chirp-set-status (buffer text &optional kind)
  "Set BUFFER's persistent status TEXT and KIND."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local chirp--status-text text)
      (setq-local chirp--status-kind (or kind 'loading))
      (setq-local chirp--status-start-time (float-time)))
    (chirp--ensure-status-timer buffer)
    (with-current-buffer buffer
      (force-mode-line-update t))))

(defun chirp-clear-status (&optional buffer)
  "Clear persistent status in BUFFER."
  (let ((target (or buffer (current-buffer))))
    (when (buffer-live-p target)
      (with-current-buffer target
        (when (timerp chirp--status-timer)
          (cancel-timer chirp--status-timer))
        (setq-local chirp--status-text nil)
        (setq-local chirp--status-kind nil)
        (setq-local chirp--status-start-time nil)
        (setq-local chirp--status-timer nil)
        (force-mode-line-update t)))))

(defun chirp--base-buffer-stem ()
  "Return the display stem used for Chirp buffer names."
  (let ((name chirp-buffer-name))
    (if (string-match "\\`\\*\\(.*?\\)\\*\\'" name)
        (match-string 1 name)
      name)))

(defun chirp--format-buffer-name (&optional title)
  "Return a display buffer name for TITLE."
  (if (and (stringp title)
           (not (string-empty-p title)))
      (format "*%s: %s*" (chirp--base-buffer-stem) title)
    chirp-buffer-name))

(defun chirp--apply-buffer-name (buffer &optional title)
  "Rename BUFFER to match TITLE and return BUFFER."
  (with-current-buffer buffer
    (let ((new-name (chirp--format-buffer-name title)))
      (unless (string= (buffer-name buffer) new-name)
        (rename-buffer new-name t))))
  buffer)

(defun chirp-buffer ()
  "Create and return a fresh Chirp buffer."
  (generate-new-buffer chirp-buffer-name))

(defun chirp-display-buffer (buffer)
  "Display BUFFER in the selected window."
  (unless (eq (window-buffer (selected-window)) buffer)
    (switch-to-buffer buffer)))

(defun chirp--active-buffer-p ()
  "Return non-nil when the current buffer is a Chirp view buffer."
  (or (derived-mode-p 'chirp-view-mode)
      (derived-mode-p 'chirp-media-image-mode)
      (derived-mode-p 'chirp-media-view-mode)))

(defun chirp--persistent-timeline-buffer-p (&optional buffer)
  "Return non-nil when BUFFER is a main timeline Chirp buffer.

For You and Following stay alive when the user quits the window so they can be
revisited later."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (memq chirp--timeline-kind '(home following)))))

(defun chirp-quit-current-buffer ()
  "Close the current Chirp buffer."
  (interactive)
  (quit-window (not (chirp--persistent-timeline-buffer-p))))

(defun chirp-show-loading (buffer title refresh)
  "Display a loading message in BUFFER for TITLE.

Return a token that identifies the current request."
  (let ((token (chirp-begin-request buffer)))
    (chirp-set-status buffer (format "Loading %s..." title))
    (chirp-render-into-buffer
     buffer title refresh
     (lambda ()
       (insert "Loading...\n")))
    (chirp-display-buffer buffer)
    token))

(defun chirp-begin-background-request (buffer title)
  "Start an async request for BUFFER titled TITLE without displaying it yet."
  (chirp-set-status buffer (format "Loading %s..." title))
  (chirp-begin-request buffer))

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
    (setq-local chirp--timeline-count nil)
    (setq-local chirp--timeline-next-cursor nil)
    (setq-local chirp--timeline-load-more-function nil)
    (setq-local chirp--timeline-exhausted-p nil)
    (setq-local chirp--timeline-loading-more nil)
    (setq-local chirp--rerender-function nil)
    (setq-local chirp--entry-wrap-navigation t)
    (setq-local chirp--profile-handle nil)
    (setq-local chirp--profile-view-mode nil)
    (setq-local chirp--profile-view-modes nil)
    (setq-local chirp--profile-switch-mode-function nil)
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
        (wait (or delay chirp-rerender-idle-delay)))
    (when (buffer-live-p target)
      (with-current-buffer target
        (when (timerp chirp--rerender-timer)
          (cancel-timer chirp--rerender-timer))
        (setq-local
         chirp--rerender-timer
         (run-with-idle-timer
          wait nil
          (lambda (buf)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq-local chirp--rerender-timer nil)
                (when chirp--rerender-function
                  (let ((window-state (chirp-capture-window-state buf)))
                    (funcall chirp--rerender-function)
                    (chirp-restore-window-state window-state))))))
          target))))))

(defun chirp-author-handle-at-point ()
  "Return the author handle stored at point, or nil."
  (or (get-text-property (point) 'chirp-author-handle)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-author-handle))))

(defun chirp-author-profile-url-at-point ()
  "Return the author profile URL stored at point, or nil."
  (or (get-text-property (point) 'chirp-author-profile-url)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-author-profile-url))))

(defun chirp-reply-parent-id-at-point ()
  "Return the inline reply parent id stored at point, or nil."
  (or (get-text-property (point) 'chirp-reply-parent-id)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-reply-parent-id))))

(defun chirp-profile-list-kind-at-point ()
  "Return the profile list kind stored at point, or nil."
  (or (get-text-property (point) 'chirp-profile-list-kind)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-profile-list-kind))))

(defun chirp-profile-list-handle-at-point ()
  "Return the profile list handle stored at point, or nil."
  (or (get-text-property (point) 'chirp-profile-list-handle)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-profile-list-handle))))

(defun chirp-profile-view-mode-at-point ()
  "Return the profile subview mode stored at point, or nil."
  (or (get-text-property (point) 'chirp-profile-view-mode)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-profile-view-mode))))

(defun chirp-profile-action-at-point ()
  "Return the profile action stored at point, or nil."
  (or (get-text-property (point) 'chirp-profile-action)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-profile-action))))

(defun chirp-open-reply-parent-at-point ()
  "Jump to the visible parent tweet referenced at point."
  (interactive)
  (if-let* ((parent-id (chirp-reply-parent-id-at-point)))
      (or (chirp-goto-entry-id parent-id)
          (let ((entry (chirp-entry-at-point)))
            (when (eq (plist-get entry :kind) 'tweet)
              (chirp-thread-open entry parent-id)
              t))
          (user-error "No parent tweet available at point"))
    (user-error "No reply parent available at point")))

(defun chirp-show-error (buffer title refresh message)
  "Display MESSAGE in BUFFER for TITLE."
  (chirp-set-status buffer "Load failed" 'error)
  (chirp-render-into-buffer
   buffer title refresh
   (lambda ()
     (insert "Unable to load data.\n\n")
     (insert message)
     (insert "\n")))
  (chirp-display-buffer buffer))

(defun chirp-refresh ()
  "Refresh the current Chirp buffer."
  (interactive)
  (if chirp--refresh-function
      (funcall chirp--refresh-function)
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

(defun chirp--current-entry-start ()
  "Return the top-level entry start that contains point, or nil."
  (let ((search-end (min (point-max) (1+ (point)))))
    (when (> search-end (point-min))
      (chirp--entry-position-backward search-end))))

(defun chirp-capture-point-anchor ()
  "Return a stable anchor describing the current point location."
  (if-let* ((entry-start (chirp--current-entry-start))
            (entry (get-text-property entry-start 'chirp-entry-item)))
      (list :entry-id (plist-get entry :id)
            :offset (- (point) entry-start))
    (list :position (point))))

(defun chirp-point-position-from-anchor (anchor)
  "Return a buffer position for ANCHOR, or nil when it cannot be restored."
  (cond
   ((null anchor) nil)
   ((and (listp anchor)
         (plist-member anchor :position))
    (max (point-min)
         (min (point-max)
              (plist-get anchor :position))))
   ((listp anchor)
    (when-let* ((entry-id (plist-get anchor :entry-id))
                (entry-pos (chirp-entry-position-by-id entry-id)))
      (let* ((offset (max 0 (or (plist-get anchor :offset) 0)))
             (next-start (chirp--entry-position-forward (min (point-max) (1+ entry-pos))))
             (entry-end (or next-start (point-max))))
        (max entry-pos
             (min (+ entry-pos offset)
                  (max entry-pos (1- entry-end)))))))
   ((stringp anchor)
    (chirp-entry-position-by-id anchor))
   (t nil)))

(defun chirp-restore-point-anchor (anchor)
  "Restore point from ANCHOR, returning non-nil on success."
  (when-let* ((pos (chirp-point-position-from-anchor anchor)))
    (goto-char pos)
    t))

(defun chirp-buffer-window (&optional buffer)
  "Return the interactive window showing BUFFER, or nil."
  (let ((target (or buffer (current-buffer))))
    (or (and (window-live-p (selected-window))
             (eq (window-buffer (selected-window)) target)
             (selected-window))
        (get-buffer-window target t))))

(defun chirp-capture-window-state (&optional buffer)
  "Return the current window state for BUFFER, or nil when not visible."
  (when-let* ((window (chirp-buffer-window buffer))
              ((window-live-p window)))
    (with-current-buffer (window-buffer window)
      (list :window window
            :point-anchor (save-excursion
                            (goto-char (window-point window))
                            (chirp-capture-point-anchor))
            :start-anchor (save-excursion
                            (goto-char (window-start window))
                            (chirp-capture-point-anchor))
            :hscroll (window-hscroll window)
            :vscroll (window-vscroll window t)))))

(defun chirp-restore-window-state (state)
  "Restore window STATE captured by `chirp-capture-window-state'."
  (when-let* ((window (plist-get state :window))
              ((window-live-p window)))
    (with-current-buffer (window-buffer window)
      (when-let* ((start (chirp-point-position-from-anchor
                          (plist-get state :start-anchor))))
        (set-window-start window start t))
      (set-window-hscroll window (or (plist-get state :hscroll) 0))
      (set-window-vscroll window (or (plist-get state :vscroll) 0) t)
      (when-let* ((pos (chirp-point-position-from-anchor
                        (plist-get state :point-anchor))))
        (set-window-point window pos)))))

(defun chirp-next-entry ()
  "Move to the next entry."
  (interactive)
  (let* ((current-start (chirp--current-entry-start))
         (pos (if current-start
                  (chirp--entry-position-forward
                   (min (point-max) (1+ current-start)))
                (chirp--entry-position-forward (point-min)))))
    (cond
     (pos
     (goto-char pos))
     ((and chirp--timeline-load-more-function
           (chirp-entry-at-point))
      (funcall chirp--timeline-load-more-function (chirp-entry-id-at-point)))
     ((not chirp--entry-wrap-navigation)
      (user-error "Already at last entry"))
     ((setq pos (chirp--entry-position-forward (point-min)))
      (goto-char pos))
     (t
      (user-error "No entries in this buffer")))))

(defun chirp-previous-entry ()
  "Move to the previous entry."
  (interactive)
  (let* ((current-start (chirp--current-entry-start))
         (pos (and current-start
                   (chirp--entry-position-backward current-start))))
    (when (and (not pos)
               (not chirp--entry-wrap-navigation))
      (user-error "Already at first entry"))
    (unless pos
      (setq pos (chirp--entry-position-backward (point-max))))
    (if pos
        (goto-char pos)
      (user-error "No entries in this buffer"))))

(defun chirp-entry-at-point ()
  "Return the Chirp entry stored at point, or nil."
  (or (get-text-property (point) 'chirp-subentry-item)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-subentry-item))
      (get-text-property (point) 'chirp-entry-item)
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
  (or (get-text-property (point) 'chirp-subentry-url)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-subentry-url))
      (get-text-property (point) 'chirp-entry-url)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'chirp-entry-url))))

(defun chirp-open-at-point ()
  "Open the entry at point."
  (interactive)
  (if-let* ((profile-view-mode (chirp-profile-view-mode-at-point))
            ((functionp chirp--profile-switch-mode-function)))
      (funcall chirp--profile-switch-mode-function profile-view-mode)
    (if-let* ((profile-action (chirp-profile-action-at-point)))
        (pcase profile-action
          ('toggle-follow (chirp-toggle-follow-user-at-point))
          (_ (user-error "Unknown profile action at point")))
      (if-let* ((author-handle (chirp-author-handle-at-point)))
          (let* ((entry (chirp-entry-at-point))
                 (clean-author (string-remove-prefix "@" author-handle))
                 (clean-profile (and chirp--profile-handle
                                     (string-remove-prefix "@" chirp--profile-handle))))
            (if (and (eq (plist-get entry :kind) 'tweet)
                     clean-profile
                     (equal clean-author clean-profile))
                (chirp-thread-open entry (plist-get entry :id))
              (chirp-profile-open author-handle)))
        (if-let* ((profile-list-kind (chirp-profile-list-kind-at-point))
                  (profile-list-handle (chirp-profile-list-handle-at-point)))
            (pcase profile-list-kind
              ('followers (chirp-profile-followers profile-list-handle))
              ('following (chirp-profile-following-users profile-list-handle))
              (_ (user-error "Unknown profile list at point")))
          (if-let* (((chirp-reply-parent-id-at-point)))
              (chirp-open-reply-parent-at-point)
            (if (chirp-media-at-point)
                (chirp-media-open-at-point)
              (pcase (plist-get (chirp-entry-at-point) :kind)
                ('tweet (chirp-thread-open (chirp-entry-at-point)
                                           (plist-get (chirp-entry-at-point) :id)))
                ('user (chirp-profile-open (plist-get (chirp-entry-at-point) :handle)))
                (_ (user-error "No entry at point"))))))))))

(defun chirp-open-primary-media ()
  "Open the media at point or the first media of the current entry."
  (interactive)
  (if (chirp-media-at-point)
      (chirp-media-open-at-point)
    (let* ((entry (chirp-entry-at-point))
           (media-list (or (plist-get entry :media)
                           (chirp-tweet-article-images entry))))
      (if media-list
          (chirp-media-open media-list
                            0
                            (or chirp--view-title "Chirp Media"))
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
                  (chirp-author-profile-url-at-point)
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

(defun chirp-decode-html-entities (text)
  "Decode common HTML entities in TEXT."
  (let ((decoded text))
    (setq decoded
          (replace-regexp-in-string
           "&#\\([0-9]+\\);"
           (lambda (match)
             (string (string-to-number (match-string 1 match))))
           decoded t t))
    (setq decoded
          (replace-regexp-in-string
           "&#x\\([0-9A-Fa-f]+\\);"
           (lambda (match)
             (string (string-to-number (match-string 1 match) 16)))
           decoded t t))
    (setq decoded (replace-regexp-in-string "&gt;" ">" decoded t t))
    (setq decoded (replace-regexp-in-string "&lt;" "<" decoded t t))
    (setq decoded (replace-regexp-in-string "&amp;" "&" decoded t t))
    (setq decoded (replace-regexp-in-string "&quot;" (string 34) decoded t t))
    (setq decoded (replace-regexp-in-string "&#39;" "'" decoded t t))
    (setq decoded (replace-regexp-in-string "&nbsp;" " " decoded t t))
    decoded))

(defun chirp-clean-text (value)
  "Normalize VALUE into a human-readable string."
  (cond
   ((stringp value)
    (string-trim
     (chirp-decode-html-entities
      (replace-regexp-in-string "\r" "" value))))
   ((null value) "")
   (t
    (string-trim (format "%s" value)))))

(defconst chirp--short-url-regexp "https?://t\\.co/[[:alnum:]]+"
  "Regexp that matches short X/Twitter URLs in tweet text.")

(defconst chirp--markdown-image-regexp "!\\[\\([^]\n]*\\)\\](\\([^)\n]+\\))"
  "Regexp that matches one Markdown image.")

(defun chirp-normalize-string-list (value)
  "Normalize VALUE into a de-duplicated list of non-blank strings."
  (when (listp value)
    (let ((seen (make-hash-table :test #'equal))
          items)
      (dolist (item value (nreverse items))
        (when-let* ((text (and (stringp item)
                               (string-trim item))))
          (unless (or (string-empty-p text)
                      (gethash text seen))
            (puthash text t seen)
            (push text items)))))))

(defun chirp-normalize-url-item (value)
  "Normalize one URL VALUE into an expanded string, or nil."
  (cond
   ((stringp value)
    (let ((text (string-trim value)))
      (unless (string-empty-p text)
        text)))
   ((chirp-object-p value)
    (chirp-first-nonblank
     (chirp-get value
                "expanded_url"
                "expandedUrl"
                "expanded"
                "url"
                "shortUrl")))
   (t nil)))

(defun chirp-normalize-url-list (&rest values)
  "Normalize URL VALUES into a de-duplicated list of expanded strings."
  (let ((seen (make-hash-table :test #'equal))
        items)
    (dolist (value values (nreverse items))
      (when (listp value)
        (dolist (item value)
          (when-let* ((url (chirp-normalize-url-item item)))
            (unless (gethash url seen)
              (puthash url t seen)
              (push url items))))))))

(defun chirp-extract-tweet-urls (object &optional legacy)
  "Extract expanded URLs for tweet OBJECT and optional LEGACY payload."
  (chirp-normalize-url-list
   (chirp-get object "urls")
   (chirp-get-in object '("note_tweet" "note_tweet_results" "result" "entity_set" "urls"))
   (chirp-get-in object '("note_tweet" "entity_set" "urls"))
   (chirp-get-in object '("entities" "urls"))
   (and legacy
        (chirp-get-in legacy '("entities" "urls")))))

(defun chirp-tweet-candidate-urls (tweet)
  "Return likely canonical URLs for TWEET."
  (let ((id (plist-get tweet :id))
        (handle (plist-get tweet :author-handle))
        urls)
    (when-let* ((url (plist-get tweet :url)))
      (push url urls))
    (when id
      (push (format "https://x.com/i/status/%s" id) urls)
      (when handle
        (push (format "https://x.com/%s/status/%s" handle id) urls)))
    (delete-dups (delq nil urls))))

(defun chirp-tweet-fixupx-url (tweet)
  "Return a fixupx.com URL for TWEET, or nil when unavailable."
  (when-let* ((url (car (chirp-tweet-candidate-urls tweet))))
    (replace-regexp-in-string
     "\\`https?://\\(?:www\\.\\)?\\(?:x\\.com\\|twitter\\.com\\)"
     "https://fixupx.com"
     url)))

(defun chirp-tweet-preview-text (tweet &optional max-length)
  "Return a short one-paragraph preview for TWEET."
  (let* ((limit (or max-length 160))
         (text (or (plist-get tweet :text)
                   (chirp-tweet-article-preview tweet limit)
                   ""))
         (cleaned (replace-regexp-in-string "[ \t\n\r]+" " " (chirp-clean-text text) t)))
    (if (<= (length cleaned) limit)
        cleaned
      (concat (string-trim-right (substring cleaned 0 (max 0 (- limit 3))))
              "..."))))

(defun chirp-filter-display-urls (urls &optional quoted-tweet)
  "Return URLS after removing duplicates and quoted-tweet permalinks."
  (let ((quoted-urls (and quoted-tweet
                          (chirp-tweet-candidate-urls quoted-tweet))))
    (cl-remove-if (lambda (url)
                    (member url quoted-urls))
                  urls)))

(defun chirp-short-url-count (text)
  "Return how many `t.co` placeholders appear in TEXT."
  (let ((start 0)
        (count 0)
        (value (or text "")))
    (while (string-match chirp--short-url-regexp value start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun chirp-strip-short-urls (text)
  "Remove `t.co` placeholders from TEXT while keeping paragraph structure."
  (let ((cleaned (or text "")))
    (setq cleaned (replace-regexp-in-string chirp--short-url-regexp "" cleaned t t))
    (setq cleaned (replace-regexp-in-string "[ \t]+\\(\n\\)" "\\1" cleaned t))
    (setq cleaned (replace-regexp-in-string "\\(\n\\)[ \t]+" "\\1" cleaned t))
    (setq cleaned (replace-regexp-in-string "[ \t]\\{2,\\}" " " cleaned t))
    (setq cleaned (replace-regexp-in-string "\n\\{3,\\}" "\n\n" cleaned t))
    (string-trim cleaned)))

(defun chirp--normalize-markdown-summary (text)
  "Flatten markdown-ish TEXT into a readable single paragraph."
  (let ((summary (or text "")))
    (setq summary (replace-regexp-in-string "!\\[[^]]*\\](\\([^)]*\\))" "" summary t))
    (setq summary (replace-regexp-in-string "\\[\\([^]]+\\)\\](\\([^)]*\\))" "\\1" summary t))
    (setq summary (replace-regexp-in-string "`\\([^`]+\\)`" "\\1" summary t))
    (setq summary (replace-regexp-in-string "^[#>*-]+[ \t]*" "" summary t))
    (setq summary (replace-regexp-in-string "[ \t\n\r]+" " " summary t))
    (string-trim summary)))

(defun chirp-tweet-article-preview (tweet &optional max-length)
  "Return a short readable article preview for TWEET.

When MAX-LENGTH is non-nil, truncate the preview to that many characters."
  (let* ((limit (or max-length 240))
         (paragraphs (split-string (or (plist-get tweet :article-text) "")
                                   "\n[ \t]*\n+"
                                   t))
         (summary
          (cl-loop for paragraph in paragraphs
                   for cleaned = (chirp--normalize-markdown-summary paragraph)
                   unless (or (string-empty-p cleaned)
                              (string-prefix-p "```" cleaned))
                   return cleaned)))
    (when summary
      (if (<= (length summary) limit)
          summary
        (concat (string-trim-right (substring summary 0 (max 0 (- limit 3))))
                "...")))))

(defun chirp--markdown-image-media (text)
  "Return a photo plist when TEXT is exactly one Markdown image paragraph."
  (let ((paragraph (string-trim (or text ""))))
    (when (string-match (format "\\`%s\\'" chirp--markdown-image-regexp) paragraph)
      (let ((url (chirp-clean-text (match-string 2 paragraph)))
            (alt (chirp-clean-text (match-string 1 paragraph))))
        (when (and (not (string-empty-p url))
                   (string-match-p "\\`https?://" url))
          (list :type "photo"
                :url url
                :alt alt
                :article-image-p t))))))

(defun chirp-article-segments (text)
  "Split article TEXT into renderable text and image segments."
  (let (segments)
    (dolist (paragraph (split-string (or text "")
                                     "\n[ \t]*\n+"
                                     t))
      (if-let* ((media (chirp--markdown-image-media paragraph)))
          (push (list :type 'image :media media) segments)
        (let ((cleaned (chirp-clean-text paragraph)))
          (unless (string-empty-p cleaned)
            (push (list :type 'text :text cleaned) segments)))))
    (nreverse segments)))

(defun chirp-tweet-article-images (tweet &optional max-count)
  "Return article images parsed from TWEET.

When MAX-COUNT is non-nil, return at most that many images."
  (let ((images
         (cl-loop for segment in (chirp-article-segments
                                  (plist-get tweet :article-text))
                  when (eq (plist-get segment :type) 'image)
                  collect (plist-get segment :media))))
    (if (and (integerp max-count)
             (>= max-count 0))
        (cl-subseq images 0 (min max-count (length images)))
      images)))

(defun chirp-tweet-article-display-text (tweet)
  "Return article body text for TWEET with Markdown image paragraphs removed."
  (string-join
   (cl-loop for segment in (chirp-article-segments
                            (plist-get tweet :article-text))
            when (eq (plist-get segment :type) 'text)
            collect (plist-get segment :text))
   "\n\n"))

(defun chirp-normalize-quoted-tweet (value)
  "Normalize VALUE into a quoted-tweet plist, or nil."
  (when-let* ((quoted
               (cond
                ((chirp-object-p value)
                 (or (chirp-get value "quotedTweet" "quoted_tweet")
                     (chirp-get-in value '("quoted_status_result" "result"))))
                (t nil))))
    (or (and-let* ((quoted-id (chirp-first-nonblank
                               (chirp-get quoted "id" "id_str" "rest_id")))
                   (cached (gethash quoted-id chirp-quoted-tweet-cache))
                   ((not (eq cached chirp--quoted-tweet-fetch-failed))))
         cached)
        (chirp-normalize-tweet quoted))))

(defun chirp-quoted-tweet-enriched-p (tweet)
  "Return non-nil when quoted TWEET already carries full fetched detail."
  (plist-get tweet :chirp-enriched-p))

(defun chirp--dispatch-quoted-tweet-callbacks (tweet-id payload)
  "Run pending callbacks for TWEET-ID with PAYLOAD."
  (let ((callbacks (prog1 (gethash tweet-id chirp-quoted-tweet-pending)
                     (remhash tweet-id chirp-quoted-tweet-pending))))
    (dolist (callback callbacks)
      (when callback
        (ignore-errors (funcall callback payload))))))

(defun chirp--request-quoted-tweet (tweet-id callback)
  "Fetch quoted tweet TWEET-ID and run CALLBACK with the result."
  (let ((cached (gethash tweet-id chirp-quoted-tweet-cache)))
    (cond
     ((eq cached chirp--quoted-tweet-fetch-failed)
      nil)
     (cached
      (funcall callback cached))
     ((gethash tweet-id chirp-quoted-tweet-pending)
      (puthash tweet-id
               (cons callback (gethash tweet-id chirp-quoted-tweet-pending))
               chirp-quoted-tweet-pending))
     (t
      (puthash tweet-id (list callback) chirp-quoted-tweet-pending)
      (chirp-backend-tweet
       tweet-id
       (lambda (tweet _envelope)
         (puthash tweet-id tweet chirp-quoted-tweet-cache)
         (chirp--dispatch-quoted-tweet-callbacks tweet-id tweet))
       (lambda (_message)
         (puthash tweet-id chirp--quoted-tweet-fetch-failed chirp-quoted-tweet-cache)
         (chirp--dispatch-quoted-tweet-callbacks tweet-id nil)))))))

(defun chirp-enrich-quoted-tweets (tweets buffer)
  "Asynchronously enrich quoted tweets inside TWEETS and rerender BUFFER."
  (dolist (tweet tweets)
    (when-let* ((quoted (plist-get tweet :quoted-tweet))
                (quoted-id (plist-get quoted :id))
                ((not (chirp-quoted-tweet-enriched-p quoted))))
      (chirp--request-quoted-tweet
       quoted-id
       (lambda (full-quoted)
         (when (and full-quoted
                    (buffer-live-p buffer))
           (plist-put full-quoted :chirp-enriched-p t)
           (plist-put tweet :quoted-tweet full-quoted)
           (chirp-media-prefetch-tweet full-quoted buffer)
           (chirp-request-rerender buffer)))))))

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
            (chirp-get object "screen_name")
            (chirp-get object "screenName")
            (chirp-get object "username")
            (chirp-get object "handle")
            (chirp-get-in object '("legacy" "screen_name")))
           (chirp-get object "id" "rest_id"))
       (or (chirp-first-nonblank
            (chirp-get object "name")
            (chirp-get object "display_name")
            (chirp-get object "description")
            (chirp-get object "bio")
            (chirp-get-in object '("legacy" "name"))
            (chirp-get-in object '("legacy" "description")))
           (chirp-get object "followers_count" "friends_count" "statuses_count")
           (chirp-get object "followers" "following" "tweets")
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
                  (chirp-get user "screen_name")
                  (chirp-get user "screenName")
                  (chirp-get user "username")
                  (chirp-get user "handle")
                  (chirp-get legacy "screen_name")))
         (name (chirp-first-nonblank
                (chirp-get user "name")
                (chirp-get user "display_name")
                (chirp-get legacy "name")
                handle))
         (id (chirp-first-nonblank
              (chirp-get user "id" "rest_id")
              (chirp-get legacy "id_str")))
         (bio (chirp-clean-text
               (chirp-first-nonblank
                (chirp-get user "description")
                (chirp-get user "bio")
                (chirp-get legacy "description"))))
         (followers (chirp-coalesce
                     (chirp-get user "followers_count")
                     (chirp-get user "followers")
                     (chirp-get legacy "followers_count")))
         (following (chirp-coalesce
                     (chirp-get user "friends_count")
                     (chirp-get user "following_count")
                     (chirp-get user "following")
                     (chirp-get legacy "friends_count")))
         (posts (chirp-coalesce
                 (chirp-get user "statuses_count")
                 (chirp-get user "tweets_count")
                 (chirp-get user "tweets")
                 (chirp-get legacy "statuses_count")))
         (joined (chirp-first-nonblank
                  (chirp-get user "createdAtLocal")
                  (chirp-get user "createdAtISO")
                  (chirp-get user "createdAt")
                  (chirp-get user "created_at")
                  (chirp-get legacy "created_at")))
         (avatar-url (chirp-first-nonblank
                      (chirp-get user "profileImageUrl")
                      (chirp-get user "profile_image_url_https")
                      (chirp-get user "profile_image_url")
                      (chirp-get legacy "profile_image_url_https" "profile_image_url")))
         (viewer-following-p (chirp-boolean-value
                              (chirp-coalesce
                               (chirp-get user "viewerFollowing" "viewer_following")
                               (chirp-get-in user '("relationship_perspectives" "following")))))
         (viewer-followed-by-p (chirp-boolean-value
                                (chirp-coalesce
                                 (chirp-get user "viewerFollowedBy" "viewer_followed_by")
                                 (chirp-get-in user '("relationship_perspectives" "followed_by")))))
         (viewer-blocking-p (chirp-boolean-value
                             (chirp-coalesce
                              (chirp-get user "viewerBlocking" "viewer_blocking")
                              (chirp-get-in user '("relationship_perspectives" "blocking")))))
         (viewer-muting-p (chirp-boolean-value
                           (chirp-coalesce
                            (chirp-get user "viewerMuting" "viewer_muting")
                            (chirp-get-in user '("relationship_perspectives" "muting"))))))
    (when (or handle id name)
      (list :kind 'user
            :id id
            :name name
            :handle (and handle (string-remove-prefix "@" handle))
            :bio bio
            :avatar-url avatar-url
            :followers followers
            :following following
            :viewer-following-p viewer-following-p
            :viewer-followed-by-p viewer-followed-by-p
            :viewer-blocking-p viewer-blocking-p
            :viewer-muting-p viewer-muting-p
            :posts posts
            :joined joined
            :profile-url (and handle
                              (format "https://x.com/%s"
                                      (string-remove-prefix "@" handle)))
            :raw object))))

(defun chirp-normalize-media-variant (object)
  "Normalize media variant OBJECT into a plist."
  (let ((url (chirp-first-nonblank (chirp-get object "url")))
        (bitrate (chirp-get object "bitrate")))
    (when url
      (list :url url
            :bitrate bitrate))))

(defun chirp-normalize-media-variants (value)
  "Normalize media variant VALUE into a list of plists."
  (if (listp value)
      (delq nil (mapcar #'chirp-normalize-media-variant value))
    nil))

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
        (variants (chirp-normalize-media-variants
                   (chirp-get object "variants")))
        (width (chirp-get object "width"))
        (height (chirp-get object "height")))
    (when (and type url)
      (list :type type
            :url url
            :preview-url preview-url
            :variants variants
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
         (quoted-tweet (chirp-normalize-quoted-tweet object))
         (all-urls (chirp-extract-tweet-urls object legacy))
         (display-text (if (>= (length all-urls)
                               (chirp-short-url-count text))
                           (chirp-strip-short-urls text)
                         text))
         (urls (chirp-filter-display-urls all-urls quoted-tweet))
         (article-title (chirp-first-nonblank
                         (chirp-get object "articleTitle" "article_title")))
         (article-text-raw (chirp-first-nonblank
                            (chirp-get object "articleText" "article_text")))
         (article-text (and article-text-raw
                            (chirp-clean-text article-text-raw)))
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
         (retweeted-by (let ((handle (chirp-first-nonblank
                                      (chirp-get object "retweetedBy" "retweeted_by"))))
                         (and handle
                              (string-remove-prefix "@" handle))))
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
         (promoted-p (chirp-boolean-value
                      (chirp-coalesce
                       (chirp-get object "isPromoted" "is_promoted" "promoted")
                       (chirp-get-in object '("itemContent" "promotedMetadata"))
                       (chirp-get object "promotedMetadata"))))
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
            :text display-text
            :raw-text text
            :created-at (chirp-first-nonblank
                         (chirp-get object "createdAtLocal" "createdAtISO" "createdAt" "created_at")
                         (chirp-get legacy "created_at"))
            :url url
            :urls urls
            :conversation-id (chirp-first-nonblank
                              (chirp-get object "conversationId" "conversation_id")
                              (chirp-get legacy "conversation_id_str"))
            :reply-to-id reply-to-id
            :reply-to-handle reply-to-handle
            :retweeted-by retweeted-by
            :author-name (plist-get author-user :name)
            :author-handle author-handle
            :author-avatar-url (plist-get author-user :avatar-url)
            :quoted-tweet quoted-tweet
            :article-title article-title
            :article-text article-text
            :promoted-p promoted-p
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

(defun chirp-tweet-visible-p (tweet)
  "Return non-nil when TWEET should be shown in Chirp."
  (or (not chirp-hide-promoted-posts)
      (not (plist-get tweet :promoted-p))))

(defun chirp-collect-top-level-tweets (value)
  "Collect normalized tweets from the top level of VALUE."
  (cond
   ((chirp-tweet-like-p value)
    (let ((tweet (chirp-normalize-tweet value)))
      (if (and tweet
               (chirp-tweet-visible-p tweet))
          (list tweet)
        nil)))
   ((listp value)
    (delq nil
          (mapcar (lambda (item)
                    (when (chirp-tweet-like-p item)
                      (let ((tweet (chirp-normalize-tweet item)))
                        (when (and tweet
                                   (chirp-tweet-visible-p tweet))
                          tweet))))
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
                                 (chirp-tweet-visible-p tweet)
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
