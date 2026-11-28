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
(require 'chirp-web)

(defvar-local chirp-timeline--session nil
  "WebKit session that requested this timeline.")
(put 'chirp-timeline--session 'permanent-local t)

(defun chirp-timeline--set-kind (buffer kind)
  "Record timeline KIND in BUFFER."
  (with-current-buffer buffer
    (setq-local chirp--timeline-kind kind)))

(defun chirp-timeline--title (kind)
  "Return the buffer title for timeline KIND."
  (pcase kind
    ('home "For You")
    ('following "Following")
    ('bookmarks "Bookmarks")
    (`(search ,query) (format "Search: %s" query))
    (`(likes ,handle) (format "Liked: @%s" handle))
    (`(list ,target ,name) (format "List: %s" (or name (chirp-backend-list-id target))))
    (_ "Timeline")))

(defun chirp-timeline--list-candidate (list-info)
  "Return a minibuffer display candidate for LIST-INFO."
  (let* ((name (or (chirp-get list-info "name")
                   (chirp-get list-info "fullName")
                   (chirp-get list-info "id")))
         (owner (chirp-get-in list-info '("owner" "screenName")))
         (mode (chirp-get list-info "mode"))
         (sources (chirp-get list-info "sources"))
         (list-id (chirp-get list-info "id"))
         (parts (delq nil
                      (list (and owner (format "@%s" owner))
                            (and mode (not (string-empty-p mode)) mode)
                            (and sources (string-join sources ", "))
                            list-id))))
    (if parts
        (format "%s (%s)" name (string-join parts " · "))
      name)))

(defun chirp-timeline--refresh-function (kind buffer)
  "Return a refresh function for timeline KIND in BUFFER."
  (lambda ()
    (chirp-timeline--open
     kind
     :limit (or chirp--timeline-limit chirp-default-max-results)
     :anchor-id (chirp-capture-point-anchor)
     :buffer buffer
     :refreshing (memq kind '(home following)))))

(defun chirp-timeline--buffer-tweets (buffer)
  "Return distinct tweets currently visible in BUFFER."
  (let (tweets)
    (chirp--map-buffer-tweets
     buffer
     (lambda (tweet)
       (push tweet tweets)))
    (nreverse tweets)))

(defun chirp-timeline--merge-refreshed-tweets (current fetched)
  "Merge FETCHED over CURRENT, counting distinct newly visible tweets."
  (let ((seen (make-hash-table :test #'equal))
        (new-count 0)
        merged)
    (dolist (tweet fetched)
      (let ((key (chirp-tweet-key tweet)))
        (unless (gethash key seen)
          (puthash key 'fetched seen)
          (cl-incf new-count)
          (push tweet merged))))
    (dolist (tweet current)
      (let* ((key (chirp-tweet-key tweet))
             (state (gethash key seen)))
        (unless (eq state 'current)
          (if state (cl-decf new-count) (push tweet merged))
          (puthash key 'current seen))))
    (list :tweets (nreverse merged)
          :new-count (if current new-count 0))))

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
            &key kind limit anchor-id exhausted-p display-p next-cursor append-after)
  "Render TWEETS into BUFFER with TITLE and REFRESH metadata.

KIND and LIMIT describe the timeline.  ANCHOR-ID restores point.  EXHAUSTED-P,
DISPLAY-P, and NEXT-CURSOR control pagination and presentation.  APPEND-AFTER
is the tail of TWEETS starting with the last previously rendered tweet."
  (let ((new-tweets (if append-after (cdr append-after) tweets)))
    (if append-after
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (chirp-render-insert-tweet-list new-tweets (car append-after)))))
      (chirp-render-into-buffer
       buffer title refresh
       (lambda ()
         (if tweets
             (chirp-render-insert-tweet-list tweets)
           (chirp-render-insert-empty
            (if (eq kind 'bookmarks) "No saved bookmarks.  x actions · g refresh"
              "No posts returned.  x actions · g refresh"))))))
    (with-current-buffer buffer
      (setq chirp--timeline-kind kind
            chirp--timeline-limit limit
            chirp--timeline-next-cursor next-cursor
            chirp--timeline-load-more-function (and kind #'chirp-timeline--load-more)
            chirp--timeline-exhausted-p exhausted-p)
      (setq-local chirp--rerender-function
                  (lambda ()
                    (chirp-timeline--render
                     buffer title refresh tweets
                     :kind kind
                     :limit limit
                     :anchor-id (chirp-capture-point-anchor)
                     :exhausted-p chirp--timeline-exhausted-p
                     :next-cursor chirp--timeline-next-cursor)))
      (setq-local chirp--timeline-loading-more nil)
      (or (and anchor-id
               (chirp-restore-point-anchor anchor-id))
          (chirp-move-point-to-first-entry)))
    (chirp-clear-status buffer)
    (when display-p
      (chirp-display-buffer buffer))
    (chirp-media-prefetch-tweets new-tweets buffer)
    (chirp-enrich-quoted-tweets new-tweets buffer)))

(cl-defun chirp-timeline--handle-feed-success
    (buffer title refresh tweets
            &key kind limit anchor-id loading-more refreshing
            previous-tweets previous-exhausted-p previous-next-cursor envelope background)
  "Handle a successful feed response for BUFFER with TITLE and REFRESH.

TWEETS, KIND, LIMIT, and ANCHOR-ID describe the new view.  LOADING-MORE and
REFRESHING select merge behavior.  PREVIOUS-TWEETS,
PREVIOUS-EXHAUSTED-P, and PREVIOUS-NEXT-CURSOR describe the old view.  ENVELOPE
contains pagination metadata.  BACKGROUND keeps the selected window."
  (with-current-buffer buffer
    (setq-local chirp--request-token nil))
  (let ((next-cursor (chirp-backend-envelope-next-cursor envelope)))
    (when (and loading-more (equal next-cursor previous-next-cursor))
      (setq next-cursor nil))
    (cond
     (loading-more
      (let* ((current (or previous-tweets
                          (chirp-timeline--buffer-tweets buffer)))
             (merged-tweets (chirp-append-unique-tweets current tweets))
             (boundary (and current (nthcdr (1- (length current)) merged-tweets)))
             (additions (if boundary (cdr boundary) merged-tweets))
             (exhausted-p (not next-cursor)))
        (with-current-buffer buffer
          (setq-local chirp--timeline-loading-more nil)
          (setq-local chirp--request-token nil)
          (setq-local chirp--timeline-exhausted-p exhausted-p)
          (setq-local chirp--timeline-next-cursor next-cursor))
        (if additions
            (chirp-timeline--render
             buffer
             title
             refresh
             merged-tweets
             :kind kind
             :limit limit
             :anchor-id anchor-id
             :exhausted-p exhausted-p
             :append-after boundary
             :next-cursor next-cursor)
          (message (if exhausted-p "No older posts." "No new entries on this page · N continues")))
        (if exhausted-p
            (chirp-set-status buffer "End of timeline · g refresh" 'info)
          (chirp-clear-status buffer))))
     (refreshing
      (let* ((merged (chirp-timeline--merge-refreshed-tweets previous-tweets tweets))
             (merged-tweets (plist-get merged :tweets))
             (new-count (plist-get merged :new-count))
             (effective-next-cursor
              (if previous-tweets previous-next-cursor next-cursor))
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
             :next-cursor effective-next-cursor)
          (with-current-buffer buffer
            (setq-local chirp--timeline-loading-more nil)
            (setq-local chirp--timeline-exhausted-p previous-exhausted-p)
            (setq-local chirp--timeline-next-cursor effective-next-cursor)))
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
       :exhausted-p (and kind
                         (not next-cursor))
       :display-p (not background)
       :next-cursor next-cursor)))))

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
         (previous-tweets (and (or refreshing loading-more)
                               (chirp-timeline--buffer-tweets buffer)))
         (previous-keys (when loading-more
                          (let ((keys (make-hash-table :test #'equal)))
                            (dolist (tweet previous-tweets keys)
                              (puthash (chirp-tweet-key tweet) t keys)))))
         (previous-exhausted-p (and refreshing
                                    (with-current-buffer buffer
                                      chirp--timeline-exhausted-p)))
         (previous-next-cursor (and (or loading-more refreshing)
                                    (with-current-buffer buffer
                                      chirp--timeline-next-cursor)))
         (same-view (with-current-buffer buffer
                      (and (equal chirp--timeline-kind kind) chirp--timeline-limit)))
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
    (let (visited)
      (cl-labels
          ((fetch (page)
             (push page visited)
             (pcase kind
               ('bookmarks (chirp-backend-bookmarks #'success #'failure fetch-count page))
               ((or 'home 'following)
                (chirp-backend-feed #'success (eq kind 'following) #'failure fetch-count page))
               (_ (funcall (pcase (car kind)
                             ('search #'chirp-backend-search)
                             ('list #'chirp-backend-list)
                             ('likes #'chirp-backend-likes))
                           (cadr kind) #'success #'failure fetch-count page))))
           (success (tweets envelope)
             (when (chirp-request-current-p buffer token)
               (let ((next (chirp-backend-envelope-next-cursor envelope)))
                 (if (and loading-more next (< (length visited) 3)
                          (not (member next visited))
                          (not (seq-some (lambda (tweet)
                                           (not (gethash (chirp-tweet-key tweet) previous-keys)))
                                         tweets)))
                     (fetch next)
                   (chirp-timeline--handle-feed-success
                    buffer title refresh tweets
                    :kind kind
                    :limit limit
                    :anchor-id anchor-id
                    :loading-more loading-more
                    :refreshing refreshing
                    :previous-tweets previous-tweets
                    :background same-view
                    :previous-exhausted-p previous-exhausted-p
                    :previous-next-cursor previous-next-cursor
                    :envelope (unless (member next visited) envelope))))))
           (failure (reason)
             (when (chirp-request-current-p buffer token)
               (with-current-buffer buffer
                 (setq-local chirp--timeline-loading-more nil)
                 (setq-local chirp--request-token nil))
               (if (or loading-more refreshing same-view)
                   (progn
                     (chirp-set-status buffer
                                       (format "%s · %s · %s retry"
                                               (if loading-more "Load more failed" "Refresh failed")
                                               (string-trim reason)
                                               (if loading-more "N" "g"))
                                       'error)
                     (message "%s" (replace-regexp-in-string "[\r\n]+" "  " reason)))
                 (chirp-show-error buffer title refresh reason))
               (chirp-timeline--set-kind buffer kind))))
        (fetch cursor)))))

(defun chirp-timeline--open-primary (kind)
  "Revisit the persistent timeline for KIND, or load it."
  (let ((buffer (seq-find (lambda (buf)
                            (with-current-buffer buf
                              (and (eq chirp--timeline-kind kind)
                                   (eq chirp-timeline--session chirp-web--ready))))
                          (buffer-list))))
    (if (and buffer (with-current-buffer buffer (or chirp--timeline-limit chirp--request-token)))
        (chirp-display-buffer buffer)
      (setq buffer (or buffer (chirp-buffer)))
      (with-current-buffer buffer (setq chirp-timeline--session chirp-web--ready))
      (chirp-timeline--open kind :buffer buffer))))

(defun chirp-timeline-open-home ()
  "Open the home timeline."
  (chirp-timeline--open-primary 'home))

(defun chirp-timeline-open-following ()
  "Open the following timeline."
  (interactive)
  (chirp-timeline--open-primary 'following))

(defun chirp-timeline--load-more (&optional anchor-id)
  "Load older posts and restore point to ANCHOR-ID when supplied."
  (cond
   (chirp--timeline-loading-more
    (message "Already loading older posts..."))
   ((or chirp--timeline-exhausted-p (null chirp--timeline-next-cursor))
    (chirp-set-status (current-buffer) "End of timeline · g refresh" 'info)
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
  (chirp-timeline--open 'bookmarks :buffer buffer))

(defun chirp-timeline-open-likes (&optional handle buffer)
  "Open liked tweets for HANDLE in BUFFER.

When HANDLE is nil, resolve the currently authenticated account first."
  (interactive)
  (if handle
      (chirp-timeline--open (list 'likes (string-remove-prefix "@" handle)) :buffer buffer)
    (chirp-backend-whoami
     (lambda (user _)
       (if-let* ((name (plist-get user :handle)))
           (chirp-timeline-open-likes name buffer)
         (message "X returned no account handle"))))))

(defun chirp-timeline-open-list (&optional list-target buffer)
  "Open LIST-TARGET in BUFFER, or choose an accessible list by name."
  (interactive)
  (if list-target
      (if (and (stringp list-target) (string-blank-p list-target)) (user-error "Enter a list ID or URL")
        (chirp-timeline--open (list 'list list-target nil) :buffer buffer))
    (let* ((window (selected-window))
           (origin (window-buffer window)))
      (message "Loading lists…")
      (chirp-backend-lists
       (lambda (lists _)
         (cond
          ((null lists) (message "No accessible lists found"))
          ((or (active-minibuffer-window) (not (eq window (selected-window)))
               (not (eq origin (window-buffer window))))
           (message "Lists are ready. Run M-x chirp-list to choose one."))
          (t
           (let* ((choices (mapcar (lambda (item)
                                     (cons (chirp-timeline--list-candidate item) item))
                                   lists))
                  (choice (completing-read (format "List (%d): " (length lists)) choices nil t))
                  (item (cdr (assoc choice choices))))
             (when item
               (chirp-timeline--open
                (list 'list (chirp-get item "id") (chirp-get item "name")) :buffer buffer))))))))))

(defun chirp-timeline-open-search (query &optional buffer)
  "Open search results for QUERY in BUFFER."
  (interactive "sSearch X: ")
  (chirp-timeline--open (list 'search query) :buffer buffer))

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
