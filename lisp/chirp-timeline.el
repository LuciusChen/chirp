;;; chirp-timeline.el --- Timeline views for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch, merge, paginate, refresh, and render Chirp timeline views.

;;; Code:

(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)

(defun chirp-timeline--set-kind (buffer kind)
  "Record timeline KIND in BUFFER."
  (with-current-buffer buffer
    (setq-local chirp--timeline-kind kind)))

(defun chirp-timeline--title (kind)
  "Return the buffer title for timeline KIND."
  (pcase kind
    ('home "For You")
    ('following "Following")
    (_ "Timeline")))

(defun chirp-timeline--likes-title (handle)
  "Return the buffer title for liked tweets by HANDLE."
  (if (and handle (not (string-empty-p handle)))
      (format "Liked: @%s" handle)
    "Liked"))

(defun chirp-timeline--list-id (target)
  "Return a display-friendly list id extracted from TARGET."
  (let ((text (string-trim (format "%s" target))))
    (if (string-match "/lists?/\\([0-9]+\\)" text)
        (match-string 1 text)
      text)))

(defun chirp-timeline--list-title (target)
  "Return the buffer title for list TARGET."
  (format "List: %s" (chirp-timeline--list-id target)))

(defun chirp-timeline--list-source-labels (list-info)
  "Return human-readable source labels for LIST-INFO."
  (mapcar (lambda (source)
            (pcase source
              ("owned" "owned")
              ("subscribed" "subscribed")
              ("member" "member")
              (_ source)))
          (or (chirp-get list-info "sources") '())))

(defun chirp-timeline--list-candidate (list-info)
  "Return a minibuffer display candidate for LIST-INFO."
  (let* ((name (or (chirp-get list-info "name")
                   (chirp-get list-info "fullName")
                   (chirp-get list-info "id")))
         (owner (chirp-get-in list-info '("owner" "screenName")))
         (mode (chirp-get list-info "mode"))
         (sources (chirp-timeline--list-source-labels list-info))
         (list-id (chirp-get list-info "id"))
         (parts (delq nil
                      (list (and owner (format "@%s" owner))
                            (and mode (not (string-empty-p mode)) mode)
                            (and sources (string-join sources ", "))
                            list-id))))
    (if parts
        (format "%s (%s)" name (string-join parts " · "))
      name)))

(defun chirp-timeline--read-list-target ()
  "Prompt for one accessible list and return its id."
  (let* ((lists (chirp-backend-lists-sync))
         (choices (mapcar (lambda (list-info)
                            (cons (chirp-timeline--list-candidate list-info)
                                  (chirp-get list-info "id")))
                          lists)))
    (unless choices
      (user-error "No accessible lists found"))
    (cdr (assoc (completing-read
                 (format "List (%d): " (length choices))
                 choices
                 nil
                 t)
                choices))))

(defun chirp-timeline--refresh-function (kind buffer)
  "Return a refresh function for timeline KIND in BUFFER."
  (lambda ()
    (chirp-timeline--open
     kind
     :limit (or chirp--timeline-limit chirp-default-max-results)
     :anchor-id (chirp-capture-point-anchor)
     :buffer buffer
     :refreshing t)))

(defun chirp-timeline--current-count (buffer)
  "Return the current timeline post count for BUFFER."
  (with-current-buffer buffer
    (or chirp--timeline-count
        (let ((count 0)
              (pos (chirp--entry-position-forward (point-min))))
          (while pos
            (setq count (1+ count)
                  pos (chirp--entry-position-forward
                       (min (point-max) (1+ pos)))))
          count))))

(defun chirp-timeline--buffer-tweets (buffer)
  "Return distinct tweets currently visible in BUFFER."
  (let (tweets)
    (chirp--map-buffer-tweets
     buffer
     (lambda (tweet)
       (push tweet tweets)))
    (nreverse tweets)))

(defun chirp-timeline--prepended-new-count (current fetched)
  "Return how many unique FETCHED tweets are not already present in CURRENT.

For algorithmic timelines, especially \"For You\", the top recommendation can
stay fixed while newer posts are inserted below it.  Count every unseen tweet in
the refreshed head page so refresh feedback matches what the user will actually
see after the merge."
  (if (null current)
      0
    (let ((current-keys (make-hash-table :test #'equal))
          (seen-keys (make-hash-table :test #'equal))
          (count 0))
      (dolist (tweet current)
        (puthash (chirp-tweet-key tweet) t current-keys))
      (dolist (tweet fetched count)
        (let ((key (chirp-tweet-key tweet)))
          (unless (gethash key seen-keys)
            (puthash key t seen-keys)
            (unless (gethash key current-keys)
              (setq count (1+ count)))))))))

(defun chirp-timeline--merge-refreshed-tweets (current fetched)
  "Return a plist describing how FETCHED should merge over CURRENT."
  (let ((current-keys (make-hash-table :test #'equal))
        (merged nil)
        (merged-keys (make-hash-table :test #'equal))
        (new-count (chirp-timeline--prepended-new-count current fetched)))
    (dolist (tweet current)
      (puthash (chirp-tweet-key tweet) t current-keys))
    (dolist (tweet fetched)
      (let ((key (chirp-tweet-key tweet)))
        (unless (gethash key merged-keys)
          (puthash key t merged-keys)
          (push tweet merged))))
    (dolist (tweet current)
      (let ((key (chirp-tweet-key tweet)))
        (unless (gethash key merged-keys)
          (puthash key t merged-keys)
          (push tweet merged))))
    (list :tweets (nreverse merged)
          :new-count new-count)))

(defun chirp-timeline--refresh-message (new-count)
  "Return a status message for NEW-COUNT refreshed tweets."
  (if (zerop new-count)
      "No new posts."
    (format "%d new post%s."
            new-count
            (if (= new-count 1) "" "s"))))

(defun chirp-timeline--refresh-anchor-id (new-count anchor-id)
  "Return the entry id to anchor after a refresh.

When NEW-COUNT is positive, return nil so the refreshed view shows the
newly inserted posts at the top.  Otherwise preserve ANCHOR-ID."
  (and (zerop new-count) anchor-id))

(cl-defun chirp-timeline--render
    (buffer title refresh tweets
            &key kind limit anchor-id exhausted-p display-p next-cursor)
  "Render TWEETS into BUFFER with TITLE and REFRESH metadata.

KIND and LIMIT describe the timeline.  ANCHOR-ID restores point.  EXHAUSTED-P,
DISPLAY-P, and NEXT-CURSOR control pagination and presentation."
  (let ((tweet-count (length tweets)))
    (chirp-render-into-buffer
     buffer title refresh
     (lambda ()
       (if tweets
           (chirp-render-insert-tweet-list tweets)
         (chirp-render-insert-empty "No posts returned."))))
    (with-current-buffer buffer
      (setq-local chirp--timeline-kind kind)
      (setq-local chirp--timeline-limit (and kind limit))
      (setq-local chirp--timeline-count (and kind tweet-count))
      (setq-local chirp--timeline-next-cursor (and (memq kind '(home following))
                                                   next-cursor))
      (setq-local chirp--timeline-load-more-function
                  (and (memq kind '(home following))
                       #'chirp-load-more))
      (setq-local chirp--timeline-exhausted-p (and (memq kind '(home following))
                                                   exhausted-p))
      (setq-local chirp--rerender-function
                  (let ((saved-tweets tweets)
                        (saved-title title)
                        (saved-refresh refresh)
                        (saved-kind kind)
                        (saved-limit limit)
                        (saved-exhausted exhausted-p)
                        (saved-next-cursor next-cursor))
                    (lambda ()
                      (chirp-timeline--render
                       buffer
                       saved-title
                       saved-refresh
                       saved-tweets
                       :kind saved-kind
                       :limit saved-limit
                       :anchor-id (chirp-capture-point-anchor)
                       :exhausted-p saved-exhausted
                       :next-cursor saved-next-cursor))))
      (setq-local chirp--timeline-loading-more nil)
      (or (and anchor-id
               (chirp-restore-point-anchor anchor-id))
          (chirp-move-point-to-first-entry)))
    (chirp-clear-status buffer)
    (when display-p
      (chirp-display-buffer buffer))
    (chirp-media-prefetch-tweets tweets buffer)
    (chirp-enrich-quoted-tweets tweets buffer)))

(cl-defun chirp-timeline--handle-feed-success
    (buffer title refresh tweets
            &key kind limit anchor-id loading-more refreshing previous-count
            previous-tweets previous-exhausted-p previous-next-cursor envelope)
  "Handle a successful feed response for BUFFER with TITLE and REFRESH.

TWEETS, KIND, LIMIT, and ANCHOR-ID describe the new view.  LOADING-MORE and
REFRESHING select merge behavior.  PREVIOUS-COUNT, PREVIOUS-TWEETS,
PREVIOUS-EXHAUSTED-P, and PREVIOUS-NEXT-CURSOR describe the old view.  ENVELOPE
contains response pagination metadata."
  (ignore previous-count)
  (with-current-buffer buffer
    (setq-local chirp--request-token nil))
  (let ((next-cursor (chirp-backend-envelope-next-cursor envelope)))
    (cond
     (loading-more
      (let* ((current (or previous-tweets
                          (chirp-timeline--buffer-tweets buffer)))
             (merged-tweets (chirp-append-unique-tweets current tweets))
             (new-items-added (> (length merged-tweets) (length current)))
             (exhausted-p (not next-cursor)))
        (with-current-buffer buffer
          (setq-local chirp--timeline-loading-more nil)
          (setq-local chirp--request-token nil)
          (setq-local chirp--timeline-exhausted-p exhausted-p)
          (setq-local chirp--timeline-next-cursor next-cursor))
        (if new-items-added
            (chirp-timeline--render
             buffer
             title
             refresh
             merged-tweets
             :kind kind
             :limit limit
             :anchor-id anchor-id
             :exhausted-p exhausted-p
             :display-p t
             :next-cursor next-cursor)
          (when exhausted-p
            (message "No older posts.")))
        (chirp-clear-status buffer)))
     (refreshing
      (let* ((merged (chirp-timeline--merge-refreshed-tweets previous-tweets tweets))
             (merged-tweets (plist-get merged :tweets))
             (new-count (plist-get merged :new-count))
             (effective-next-cursor
              (if (> (length previous-tweets) limit)
                  previous-next-cursor
                (or next-cursor previous-next-cursor)))
             (render-needed (not (equal previous-tweets merged-tweets))))
        (if render-needed
            (chirp-timeline--render
             buffer
             title
             refresh
             merged-tweets
             :kind kind
             :limit limit
             :anchor-id (chirp-timeline--refresh-anchor-id new-count anchor-id)
             :exhausted-p previous-exhausted-p
             :display-p t
             :next-cursor effective-next-cursor)
          (with-current-buffer buffer
            (setq-local chirp--timeline-loading-more nil)
            (setq-local chirp--timeline-exhausted-p previous-exhausted-p)
            (setq-local chirp--timeline-next-cursor effective-next-cursor)
            (setq-local chirp--timeline-count (length previous-tweets))))
        (chirp-clear-status buffer)
        (message "%s" (chirp-timeline--refresh-message new-count))))
     (t
      (chirp-timeline--render
       buffer
       title
       refresh
       tweets
       :kind kind
       :limit limit
       :anchor-id anchor-id
       :exhausted-p (and (memq kind '(home following))
                         (not next-cursor))
       :display-p t
       :next-cursor next-cursor)
      (when (and loading-more
                 (not next-cursor))
        (message "No older posts."))))))

(cl-defun chirp-timeline--open
    (kind &key limit anchor-id buffer loading-more refreshing cursor)
  "Open timeline KIND with LIMIT posts.

When ANCHOR-ID is non-nil, restore point to that entry after rendering.
When BUFFER is non-nil, render into that existing buffer.
When LOADING-MORE is non-nil, keep the current buffer visible while fetching.
When REFRESHING is non-nil, merge newer tweets at the top on success.  CURSOR
requests a specific pagination page."
  (let* ((buffer (or buffer (chirp-buffer)))
         (title (chirp-timeline--title kind))
         (limit (or limit chirp-default-max-results))
         (refresh-count (and refreshing
                             (or (and chirp-timeline-refresh-max-results
                                      (max 1 chirp-timeline-refresh-max-results))
                                 limit)))
         (fetch-count (cond
                       (loading-more
                        (max 1 chirp-timeline-load-more-step))
                       (refreshing
                        (min limit refresh-count))
                       (t
                        limit)))
         (refresh (chirp-timeline--refresh-function kind buffer))
         (previous-count (and (or loading-more refreshing)
                              (chirp-timeline--current-count buffer)))
         (previous-tweets (and (or loading-more refreshing)
                               (chirp-timeline--buffer-tweets buffer)))
         (previous-exhausted-p (and refreshing
                                    (with-current-buffer buffer
                                      chirp--timeline-exhausted-p)))
         (previous-next-cursor (and (or loading-more refreshing)
                                    (with-current-buffer buffer
                                      chirp--timeline-next-cursor)))
         (token (if (or loading-more refreshing)
                    (progn
                      (with-current-buffer buffer
                        (setq-local chirp--timeline-loading-more t))
                      (chirp-begin-request buffer))
                  (chirp-begin-background-request buffer title))))
    (cond
     (loading-more
      (chirp-set-status buffer "Loading older posts...")
      (message "Loading older posts..."))
     (refreshing
      (chirp-set-status buffer "Refreshing timeline...")
      (message "Refreshing timeline...")))
    (chirp-timeline--set-kind buffer kind)
    (chirp-backend-feed
     (lambda (tweets envelope)
       (when (chirp-request-current-p buffer token)
         (chirp-timeline--handle-feed-success
          buffer title refresh tweets
          :kind kind
          :limit limit
          :anchor-id anchor-id
          :loading-more loading-more
          :refreshing refreshing
          :previous-count previous-count
          :previous-tweets previous-tweets
          :previous-exhausted-p previous-exhausted-p
          :previous-next-cursor previous-next-cursor
          :envelope envelope)))
     (eq kind 'following)
     (lambda (message)
       (when (chirp-request-current-p buffer token)
         (with-current-buffer buffer
           (setq-local chirp--timeline-loading-more nil)
           (setq-local chirp--request-token nil))
         (chirp-timeline--set-kind buffer kind)
         (if (or loading-more refreshing)
             (progn
               (chirp-set-status
                buffer
                (if refreshing
                    "Refresh failed"
                  "Load more failed")
                'error)
               (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message)))
           (chirp-show-error buffer title refresh message))))
     fetch-count
     cursor)))

(defun chirp-timeline-open-home ()
  "Open the home timeline."
  (interactive)
  (chirp-timeline--open 'home :limit chirp-default-max-results))

(defun chirp-timeline-open-following ()
  "Open the following timeline."
  (interactive)
  (chirp-timeline--open 'following :limit chirp-default-max-results))

(defun chirp-load-more (&optional anchor-id)
  "Load older posts and restore point to ANCHOR-ID when supplied."
  (interactive)
  (unless (memq chirp--timeline-kind '(home following))
    (user-error "Current view does not support loading more posts"))
  (cond
   (chirp--timeline-loading-more
    (message "Already loading older posts..."))
   (chirp--timeline-exhausted-p
    (message "No older posts."))
   ((not chirp--timeline-next-cursor)
    (message "No older posts."))
  (t
    (chirp-timeline--open
     chirp--timeline-kind
     :limit (or chirp--timeline-limit chirp-default-max-results)
     :anchor-id (or anchor-id (chirp-capture-point-anchor))
     :buffer (current-buffer)
     :loading-more t
     :cursor chirp--timeline-next-cursor))))

(defun chirp-timeline-open-bookmarks (&optional buffer)
  "Open bookmarks in BUFFER."
  (interactive)
  (let* ((buffer (or buffer (chirp-buffer)))
         (refresh (lambda () (chirp-timeline-open-bookmarks buffer))))
    (let ((token (chirp-begin-background-request buffer "Bookmarks")))
      (chirp-timeline--set-kind buffer nil)
      (chirp-backend-bookmarks
       (lambda (tweets _envelope)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--render
            buffer "Bookmarks" refresh tweets :display-p t)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--set-kind buffer nil)
           (chirp-show-error buffer "Bookmarks" refresh message)))))))

(defun chirp-timeline-open-likes (&optional handle buffer)
  "Open liked tweets for HANDLE in BUFFER.

When HANDLE is nil, resolve the currently authenticated account first."
  (interactive)
  (let* ((buffer (or buffer (chirp-buffer)))
         (clean-handle (and handle
                            (string-remove-prefix "@"
                                                  (string-trim (format "%s" handle)))))
         (title (chirp-timeline--likes-title clean-handle)))
    (let ((token (chirp-begin-background-request buffer title)))
      (chirp-timeline--set-kind buffer nil)
      (if clean-handle
          (let ((refresh (lambda () (chirp-timeline-open-likes clean-handle buffer))))
            (chirp-backend-likes
             clean-handle
             (lambda (tweets _envelope)
               (when (chirp-request-current-p buffer token)
                 (chirp-timeline--render
                  buffer title refresh tweets :display-p t)))
             (lambda (message)
               (when (chirp-request-current-p buffer token)
                 (chirp-timeline--set-kind buffer nil)
                 (chirp-show-error buffer title refresh message)))))
        (chirp-backend-whoami
         (lambda (user _envelope)
           (when (chirp-request-current-p buffer token)
             (if-let* ((resolved-handle (plist-get user :handle)))
                 (let* ((resolved-title (chirp-timeline--likes-title resolved-handle))
                        (refresh (lambda ()
                                   (chirp-timeline-open-likes resolved-handle buffer))))
                   (chirp-backend-likes
                    resolved-handle
                    (lambda (tweets _likes-envelope)
                      (when (chirp-request-current-p buffer token)
                        (chirp-timeline--render
                         buffer resolved-title refresh tweets :display-p t)))
                    (lambda (message)
                      (when (chirp-request-current-p buffer token)
                        (chirp-timeline--set-kind buffer nil)
                        (chirp-show-error buffer resolved-title refresh message)))))
               (chirp-show-error
                buffer
                title
                (lambda () (chirp-timeline-open-likes nil buffer))
                "twitter-cli returned a whoami payload Chirp could not parse."))))
         (lambda (message)
           (when (chirp-request-current-p buffer token)
             (chirp-timeline--set-kind buffer nil)
             (chirp-show-error
              buffer
              title
              (lambda () (chirp-timeline-open-likes nil buffer))
              message))))))))

(defun chirp-timeline-open-list (&optional list-target buffer)
  "Open the timeline for LIST-TARGET in BUFFER.

LIST-TARGET may be a numeric list id or a full list URL."
  (interactive)
  (let* ((buffer (or buffer (chirp-buffer)))
         (target (or list-target
                     (chirp-timeline--read-list-target)))
         (clean-target (string-trim (format "%s" target)))
         (title (chirp-timeline--list-title clean-target))
         (refresh (lambda () (chirp-timeline-open-list clean-target buffer))))
    (when (string-empty-p clean-target)
      (user-error "List ID or URL cannot be empty"))
    (let ((token (chirp-begin-background-request buffer title)))
      (chirp-timeline--set-kind buffer nil)
      (chirp-backend-list
       clean-target
       (lambda (tweets _envelope)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--render
            buffer title refresh tweets :display-p t)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--set-kind buffer nil)
           (chirp-show-error buffer title refresh message)))))))

(defun chirp-timeline-open-search (query &optional buffer)
  "Open search results for QUERY in BUFFER."
  (interactive "sSearch X: ")
  (let* ((title (format "Search: %s" query))
         (buffer (or buffer (chirp-buffer)))
         (refresh (lambda () (chirp-timeline-open-search query buffer))))
    (let ((token (chirp-begin-background-request buffer title)))
      (chirp-timeline--set-kind buffer nil)
      (chirp-backend-search
       query
       (lambda (tweets _envelope)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--render
            buffer title refresh tweets :display-p t)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--set-kind buffer nil)
           (chirp-show-error buffer title refresh message)))))))

(defun chirp-toggle-home-following ()
  "Toggle between primary Chirp subviews.

On Home/Following, switch between the two timelines.  In profile buffers that
expose subviews, cycle the current profile mode."
  (interactive)
  (cond
   ((functionp chirp--profile-switch-mode-function)
    (funcall chirp--profile-switch-mode-function :next))
   (t
    (pcase chirp--timeline-kind
      ('home
       (chirp-timeline--open
        'following
        :limit chirp-default-max-results
        :buffer (current-buffer)))
      ('following
       (chirp-timeline--open
        'home
        :limit chirp-default-max-results
        :buffer (current-buffer)))
      (_
       (user-error "Current view does not support TAB switching"))))))

(provide 'chirp-timeline)

;;; chirp-timeline.el ends here
