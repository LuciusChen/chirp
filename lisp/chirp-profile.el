;;; chirp-profile.el --- Profile view for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch and render profile summaries, timelines, and account lists.

;;; Code:

(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)

(defconst chirp-profile--base-modes '(posts replies highlights media)
  "Profile subviews shown for all profiles.")
(defvar-local chirp-profile--user nil
  "Buffer-local cached profile plist for the active profile view.")
(defvar-local chirp-profile--tweets nil
  "Buffer-local cached tweet list for the active profile view.")
(defvar-local chirp-profile--available-modes chirp-profile--base-modes
  "Buffer-local list of available profile subview modes.")

(defun chirp-profile--list-title (kind handle)
  "Return a title for KIND list belonging to HANDLE."
  (format "%s: @%s"
          (pcase kind
            ('followers "Followers")
            ('following "Following")
            (_ "Users"))
          (string-remove-prefix "@" handle)))

(defun chirp-profile--mode-label (mode)
  "Return a human-readable label for profile MODE."
  (pcase mode
    ('posts "Posts")
    ('replies "Replies")
    ('highlights "Highlights")
    ('media "Media")
    ('likes "Likes")
    (_ (capitalize (symbol-name mode)))))

(defun chirp-profile--paginated-mode-p (mode)
  "Return non-nil when profile MODE supports cursor-based pagination."
  (memq mode '(posts replies highlights media)))

(defun chirp-profile--title (handle mode)
  "Return a title for HANDLE profile subview MODE."
  (let ((base (format "@%s" (string-remove-prefix "@" handle))))
    (if (eq mode 'posts)
        base
      (format "%s · %s" base (chirp-profile--mode-label mode)))))

(defun chirp-profile--next-mode (current modes)
  "Return the mode following CURRENT inside MODES."
  (let* ((modes (or modes '(posts)))
         (index (or (cl-position current modes) 0)))
    (nth (mod (1+ index) (length modes)) modes)))

(defun chirp-profile--open-user-list (kind handle &optional buffer)
  "Open KIND user list for HANDLE in BUFFER."
  (let* ((clean-handle (string-remove-prefix "@" handle))
         (title (chirp-profile--list-title kind clean-handle))
         (buffer (or buffer (chirp-buffer)))
         (refresh (lambda ()
                    (chirp-backend-invalidate-user clean-handle)
                    (chirp-profile--open-user-list kind clean-handle buffer)))
         (fetch-fn (pcase kind
                     ('followers #'chirp-backend-followers)
                     ('following #'chirp-backend-following-users)
                     (_ (error "Unknown profile list kind: %S" kind)))))
    (let ((token (chirp-begin-background-request buffer title)))
      (funcall fetch-fn
               clean-handle
               (lambda (users _envelope)
                 (when (chirp-request-current-p buffer token)
                   (chirp-render-into-buffer
                    buffer title refresh
                    (lambda ()
                      (chirp-render-insert-user-list users)))
                   (with-current-buffer buffer
                     (setq-local chirp--entry-wrap-navigation nil)
                     (setq-local chirp--profile-handle nil))
                   (chirp-clear-status buffer)
                   (chirp-display-buffer buffer)
                   (dolist (user users)
                     (chirp-media-prefetch-user user buffer))))
               (lambda (message)
                 (when (chirp-request-current-p buffer token)
                   (chirp-show-error buffer title refresh message)))))))

(cl-defun chirp-profile--render
    (buffer title refresh user tweets current-mode modes
            &key anchor-id display-p timeline-ready next-cursor status-message)
  "Render USER and TWEETS into BUFFER with TITLE and REFRESH metadata.

CURRENT-MODE selects one of MODES.  ANCHOR-ID restores point, and DISPLAY-P
shows the buffer.  TIMELINE-READY controls the empty state.  NEXT-CURSOR and
STATUS-MESSAGE record pagination and loading status."
  (chirp-render-into-buffer
   buffer title refresh
   (lambda ()
     (chirp-render-insert-user-summary user)
     (chirp-render-insert-profile-view-strip
      current-mode
      modes)
      (cond
      (tweets
       (chirp-render-insert-tweet-list tweets))
      (status-message
       (insert (propertize status-message 'face 'shadow))
       (insert "\n"))
      (timeline-ready
       (chirp-render-insert-empty
        (format "No %s returned."
                (downcase (chirp-profile--mode-label current-mode)))))
      (t
       (insert (propertize
                (format "Loading %s..."
                        (downcase (chirp-profile--mode-label current-mode)))
                'face 'shadow))
       (insert "\n")))))
  (with-current-buffer buffer
    (setq-local chirp-profile--user user)
    (setq-local chirp-profile--tweets tweets)
    (setq-local chirp--profile-view-mode current-mode)
    (setq-local chirp-profile--available-modes modes)
    (setq-local chirp--profile-handle (plist-get user :handle))
    (setq-local chirp--profile-switch-mode-function
                (lambda (target)
                  (chirp-profile--switch-mode (plist-get user :handle) target buffer)))
    (setq-local chirp--timeline-count (length tweets))
    (setq-local chirp--timeline-next-cursor next-cursor)
    (setq-local chirp--timeline-load-more-function
                (and timeline-ready
                     (chirp-profile--paginated-mode-p chirp--profile-view-mode)
                     next-cursor
                     #'chirp-profile-load-more))
    (setq-local chirp--timeline-exhausted-p (and timeline-ready (not next-cursor)))
    (setq-local chirp--timeline-loading-more nil)
    (setq-local chirp--rerender-function
                (let ((saved-user user)
                      (saved-tweets tweets)
                      (saved-title title)
                      (saved-refresh refresh)
                      (saved-mode current-mode)
                      (saved-modes modes)
                      (saved-next-cursor next-cursor)
                      (saved-timeline-ready timeline-ready)
                      (saved-status-message status-message))
                  (lambda ()
                    (chirp-profile--render
                     buffer
                     saved-title
                     saved-refresh
                     saved-user
                     saved-tweets
                     saved-mode
                     saved-modes
                     :anchor-id (chirp-capture-point-anchor)
                     :timeline-ready saved-timeline-ready
                     :next-cursor saved-next-cursor
                     :status-message saved-status-message))))
    (or (and anchor-id
             (chirp-restore-point-anchor anchor-id))
        (chirp-move-point-to-first-entry)))
  (when display-p
    (chirp-display-buffer buffer)))

(defun chirp-profile-load-more (&optional anchor-id)
  "Load older items and restore point to ANCHOR-ID when supplied."
  (interactive)
  (unless chirp--profile-handle
    (user-error "Current view does not support loading more items"))
  (cond
   (chirp--timeline-loading-more
    (message "Already loading older posts..."))
   (chirp--timeline-exhausted-p
    (message "No older posts."))
   ((not chirp--timeline-next-cursor)
    (message "No older posts."))
   (t
    (let* ((buffer (current-buffer))
           (title (or chirp--view-title (format "@%s" chirp--profile-handle)))
           (refresh chirp--refresh-function)
           (saved-user chirp-profile--user)
           (current chirp-profile--tweets)
           (saved-mode chirp--profile-view-mode)
           (saved-modes chirp-profile--available-modes)
           (mode-label (downcase (chirp-profile--mode-label saved-mode)))
           (cursor chirp--timeline-next-cursor)
           (token (chirp-begin-request buffer))
           (anchor (or anchor-id (chirp-capture-point-anchor))))
      (setq-local chirp--timeline-loading-more t)
      (chirp-set-status buffer (format "Loading older %s..." mode-label))
      (message "Loading older %s..." mode-label)
      (chirp-profile--fetch-content
       saved-mode
       chirp--profile-handle
       (lambda (tweets envelope)
         (when (chirp-request-current-p buffer token)
           (let* ((next-cursor (chirp-backend-envelope-next-cursor envelope))
                  (merged (chirp-append-unique-tweets current tweets))
                  (added-p (> (length merged) (length current))))
             (with-current-buffer buffer
               (setq-local chirp--request-token nil))
             (chirp-profile--render
              buffer title refresh saved-user merged saved-mode saved-modes
              :anchor-id anchor
              :display-p t
              :timeline-ready t
              :next-cursor next-cursor)
             (chirp-clear-status buffer)
             (chirp-media-prefetch-tweets tweets buffer)
             (chirp-enrich-quoted-tweets tweets buffer)
             (unless added-p
               (message "No older %s." mode-label)))))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (with-current-buffer buffer
             (setq-local chirp--timeline-loading-more nil)
             (setq-local chirp--request-token nil))
           (chirp-set-status buffer "Load more failed" 'error)
           (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))))
       chirp-profile-post-limit
       cursor)))))

(defun chirp-profile--fetch-content (mode handle callback errback &optional max-results cursor)
  "Fetch profile MODE content for HANDLE and call CALLBACK.

Call ERRBACK on failure.  MAX-RESULTS limits the response.  CURSOR is only used
for paginated modes."
  (pcase mode
    ('posts
     (chirp-backend-user-posts handle callback errback max-results cursor))
    ('replies
     (chirp-backend-user-replies handle callback errback max-results cursor))
    ('highlights
     (chirp-backend-user-highlights handle callback errback max-results cursor))
    ('media
     (chirp-backend-user-media handle callback errback max-results cursor))
    ('likes
     (chirp-backend-likes handle callback errback))
    (_
     (funcall (or errback
                  (lambda (message)
                    (message "%s" message)))
              (format "Unsupported profile mode: %S" mode)))))

(defun chirp-profile--switch-mode (handle target &optional buffer)
  "Switch HANDLE profile BUFFER to TARGET mode.

When TARGET is `:next', cycle through the available profile modes."
  (let* ((buffer (or buffer (current-buffer)))
         (mode (with-current-buffer buffer
                 (if (eq target :next)
                     (chirp-profile--next-mode
                      chirp--profile-view-mode
                      chirp-profile--available-modes)
                   target))))
    (unless (eq mode (with-current-buffer buffer chirp--profile-view-mode))
      (chirp-profile-open handle buffer mode))))

(defun chirp-profile-open (handle &optional buffer mode)
  "Open HANDLE's profile in BUFFER.

MODE selects the active profile subview and defaults to `posts'."
  (interactive "sProfile handle: ")
  (let* ((clean-handle (string-remove-prefix "@" handle))
         (mode (or mode 'posts))
         (title (chirp-profile--title clean-handle mode))
         (buffer (or buffer (chirp-buffer)))
         (refresh (lambda ()
                    (chirp-backend-invalidate-user clean-handle)
                    (chirp-profile-open clean-handle buffer mode)))
         (token nil)
         (saved-user nil)
         (saved-tweets nil)
         (available-modes chirp-profile--base-modes)
         (timeline-next-cursor nil)
         (user-ready nil)
         (timeline-ready nil)
         (whoami-ready nil)
         (timeline-error-message nil)
         (content-prefetched nil)
         (displayed nil))
    (cl-labels
        ((present-current ()
           (when user-ready
             (chirp-profile--render
              buffer
              title
              refresh
              saved-user
              saved-tweets
              mode
              available-modes
              :anchor-id (and displayed
                              (with-current-buffer buffer
                                (chirp-capture-point-anchor)))
              :display-p displayed
              :timeline-ready timeline-ready
              :next-cursor timeline-next-cursor
              :status-message (and timeline-ready timeline-error-message))
             (unless displayed
               (setq displayed t)
               (chirp-display-buffer buffer)))
           (when (and user-ready
                      timeline-ready
                      (not content-prefetched))
             (setq content-prefetched t)
             (when saved-tweets
               (chirp-media-prefetch-tweets saved-tweets buffer)
               (chirp-enrich-quoted-tweets saved-tweets buffer)))
           (when (and user-ready timeline-ready whoami-ready)
             (with-current-buffer buffer
               (setq-local chirp--request-token nil))))
         (update-status ()
           (cond
            (timeline-error-message
             (chirp-set-status
              buffer
              (format "%s failed" (chirp-profile--mode-label mode))
              'error))
            ((and user-ready timeline-ready)
             (chirp-clear-status buffer))
            (user-ready
             (chirp-set-status
              buffer
              (format "Profile ready · loading %s..."
                      (downcase (chirp-profile--mode-label mode)))))
            (timeline-ready
             (chirp-set-status
              buffer
              (format "%s ready · loading profile..."
                      (chirp-profile--mode-label mode)))))))
      (setq token (chirp-begin-background-request buffer title))
      (chirp-set-status buffer "Loading profile...")
      (with-current-buffer buffer
        (setq-local chirp--profile-switch-mode-function
                    (lambda (target)
                      (chirp-profile--switch-mode clean-handle target buffer))))
      (chirp-backend-user
       clean-handle
       (lambda (user _envelope)
         (when (chirp-request-current-p buffer token)
           (setq saved-user user
                 user-ready t)
           (plist-put saved-user :self-p (memq 'likes available-modes))
           (update-status)
           (present-current)
           (chirp-media-prefetch-user saved-user buffer)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (with-current-buffer buffer
             (setq-local chirp--request-token nil))
           (chirp-show-error buffer title refresh message))))
      (chirp-backend-whoami
       (lambda (user _envelope)
         (when (chirp-request-current-p buffer token)
           (let* ((self-handle (plist-get user :handle))
                  (modes (if (and self-handle
                                  (string-equal
                                   (downcase (string-remove-prefix "@" self-handle))
                                   (downcase clean-handle)))
                             (append chirp-profile--base-modes '(likes))
                           chirp-profile--base-modes)))
             (setq available-modes modes
                   whoami-ready t)
             (when saved-user
               (plist-put saved-user :self-p (memq 'likes modes)))
             (when (not (memq mode modes))
               (setq mode 'posts
                     title (chirp-profile--title clean-handle mode)))
             (when user-ready
               (update-status)
               (present-current)))))
       (lambda (_message)
         (when (chirp-request-current-p buffer token)
           (setq whoami-ready t)
           (present-current))))
      (chirp-profile--fetch-content
       mode
       clean-handle
       (lambda (tweets timeline-envelope)
         (when (chirp-request-current-p buffer token)
           (setq saved-tweets tweets
                 timeline-ready t
                 timeline-error-message nil
                 timeline-next-cursor
                 (and (chirp-profile--paginated-mode-p mode)
                      (chirp-backend-envelope-next-cursor timeline-envelope)))
           (update-status)
           (present-current)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (setq saved-tweets nil
                 timeline-ready t
                 timeline-next-cursor nil
                 timeline-error-message
                 (format "Unable to load %s.\n\n%s"
                         (downcase (chirp-profile--mode-label mode))
                         message))
           (update-status)
           (present-current)))
       chirp-profile-post-limit))))

;;;###autoload
(defun chirp-profile-open-followers (handle &optional buffer)
  "Open followers for HANDLE in BUFFER."
  (interactive "sProfile handle: ")
  (chirp-profile--open-user-list 'followers handle buffer))

;;;###autoload
(defun chirp-profile-open-following-users (handle &optional buffer)
  "Open followed accounts for HANDLE in BUFFER."
  (interactive "sProfile handle: ")
  (chirp-profile--open-user-list 'following handle buffer))

(provide 'chirp-profile)

;;; chirp-profile.el ends here
