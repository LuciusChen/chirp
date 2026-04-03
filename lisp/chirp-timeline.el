;;; chirp-timeline.el --- Timeline views for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

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

(defun chirp-timeline--following-p (kind)
  "Return non-nil when KIND is the Following timeline."
  (eq kind 'following))

(defun chirp-timeline--refresh-function (kind buffer)
  "Return a refresh function for timeline KIND."
  (lambda ()
    (chirp-timeline--open kind
                          (or chirp--timeline-limit
                              chirp-default-max-results)
                          (chirp-capture-point-anchor)
                          buffer
                          nil
                          t)))

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

(defun chirp-timeline--tweet-key (tweet)
  "Return a stable merge key for TWEET."
  (or (plist-get tweet :id)
      (plist-get tweet :url)
      (plist-get tweet :text)))

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
        (puthash (chirp-timeline--tweet-key tweet) t current-keys))
      (dolist (tweet fetched count)
        (let ((key (chirp-timeline--tweet-key tweet)))
          (unless (gethash key seen-keys)
            (puthash key t seen-keys)
            (unless (gethash key current-keys)
              (setq count (1+ count)))))))))

(defun chirp-timeline--same-tweets-p (left right)
  "Return non-nil when LEFT and RIGHT render the same timeline content."
  (equal left right))

(defun chirp-timeline--merge-refreshed-tweets (current fetched)
  "Return a plist describing how FETCHED should merge over CURRENT."
  (let ((current-keys (make-hash-table :test #'equal))
        (merged nil)
        (merged-keys (make-hash-table :test #'equal))
        (new-count (chirp-timeline--prepended-new-count current fetched)))
    (dolist (tweet current)
      (puthash (chirp-timeline--tweet-key tweet) t current-keys))
    (dolist (tweet fetched)
      (let ((key (chirp-timeline--tweet-key tweet)))
        (unless (gethash key merged-keys)
          (puthash key t merged-keys)
          (push tweet merged))))
    (dolist (tweet current)
      (let ((key (chirp-timeline--tweet-key tweet)))
        (unless (gethash key merged-keys)
          (puthash key t merged-keys)
          (push tweet merged))))
    (list :tweets (nreverse merged)
          :new-count new-count)))

(defun chirp-timeline--append-older-tweets (current fetched)
  "Return CURRENT with older FETCHED tweets appended without duplicates."
  (let ((seen (make-hash-table :test #'equal))
        (merged (copy-sequence current)))
    (dolist (tweet current)
      (puthash (chirp-timeline--tweet-key tweet) t seen))
    (dolist (tweet fetched merged)
      (let ((key (chirp-timeline--tweet-key tweet)))
        (unless (gethash key seen)
          (puthash key t seen)
          (setq merged (append merged (list tweet))))))))

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

(defun chirp-timeline--render
    (buffer title refresh tweets &optional kind limit anchor-id exhausted-p display-p next-cursor)
  "Render TWEETS into BUFFER."
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
                       saved-kind
                       saved-limit
                       (chirp-capture-point-anchor)
                       saved-exhausted
                       nil
                       saved-next-cursor))))
      (setq-local chirp--timeline-loading-more nil)
      (or (and anchor-id
               (chirp-restore-point-anchor anchor-id))
          (chirp-move-point-to-first-entry)))
    (when display-p
      (chirp-display-buffer buffer))
    (chirp-media-prefetch-tweets tweets buffer)
    (chirp-enrich-quoted-tweets tweets buffer)))

(defun chirp-timeline--handle-feed-success
    (buffer title refresh tweets kind limit anchor-id
            loading-more refreshing previous-count previous-tweets previous-exhausted-p
            previous-next-cursor envelope)
  "Handle a successful feed response for BUFFER."
  (ignore previous-count)
  (with-current-buffer buffer
    (setq-local chirp--request-token nil))
  (let ((next-cursor (chirp-backend-envelope-next-cursor envelope)))
    (cond
     (loading-more
      (let* ((current (or previous-tweets
                          (chirp-timeline--buffer-tweets buffer)))
             (merged-tweets (chirp-timeline--append-older-tweets current tweets))
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
             kind
             limit
             anchor-id
             exhausted-p
             t
             next-cursor)
          (when exhausted-p
            (message "No older posts.")))))
     (refreshing
      (let* ((merged (chirp-timeline--merge-refreshed-tweets previous-tweets tweets))
             (merged-tweets (plist-get merged :tweets))
             (new-count (plist-get merged :new-count))
             (effective-next-cursor
              (if (> (length previous-tweets) limit)
                  previous-next-cursor
                (or next-cursor previous-next-cursor)))
             (render-needed (not (chirp-timeline--same-tweets-p
                                  previous-tweets
                                  merged-tweets))))
        (if render-needed
            (chirp-timeline--render
             buffer
             title
             refresh
             merged-tweets
             kind
             limit
             (chirp-timeline--refresh-anchor-id new-count anchor-id)
             previous-exhausted-p
             t
             effective-next-cursor)
          (with-current-buffer buffer
            (setq-local chirp--timeline-loading-more nil)
            (setq-local chirp--timeline-exhausted-p previous-exhausted-p)
            (setq-local chirp--timeline-next-cursor effective-next-cursor)
            (setq-local chirp--timeline-count (length previous-tweets))))
        (message "%s" (chirp-timeline--refresh-message new-count))))
     (t
      (chirp-timeline--render
       buffer
       title
       refresh
       tweets
       kind
       limit
       anchor-id
       (and (memq kind '(home following))
            (not next-cursor))
       t
       next-cursor)
      (when (and loading-more
                 (not next-cursor))
        (message "No older posts."))))))

(defun chirp-timeline--open
    (kind &optional limit anchor-id buffer loading-more refreshing cursor)
  "Open timeline KIND with LIMIT posts.

When ANCHOR-ID is non-nil, restore point to that entry after rendering.
When BUFFER is non-nil, render into that existing buffer.
When LOADING-MORE is non-nil, keep the current buffer visible while fetching.
When REFRESHING is non-nil, merge newer tweets at the top on success."
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
      (message "Loading older posts..."))
     (refreshing
      (message "Refreshing timeline...")))
    (chirp-timeline--set-kind buffer kind)
    (chirp-backend-feed
     (lambda (tweets envelope)
       (when (chirp-request-current-p buffer token)
         (chirp-timeline--handle-feed-success
          buffer title refresh tweets kind limit anchor-id
          loading-more refreshing previous-count previous-tweets previous-exhausted-p
          previous-next-cursor envelope)))
     (chirp-timeline--following-p kind)
     (lambda (message)
       (when (chirp-request-current-p buffer token)
         (with-current-buffer buffer
           (setq-local chirp--timeline-loading-more nil)
           (setq-local chirp--request-token nil))
         (chirp-timeline--set-kind buffer kind)
         (if (or loading-more refreshing)
             (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))
           (chirp-show-error buffer title refresh message))))
     fetch-count
     cursor)))

(defun chirp-timeline-open-home ()
  "Open the home timeline."
  (interactive)
  (chirp-timeline--open 'home chirp-default-max-results))

(defun chirp-timeline-open-following ()
  "Open the following timeline."
  (interactive)
  (chirp-timeline--open 'following chirp-default-max-results))

(defun chirp-load-more (&optional anchor-id)
  "Load older posts for the current Home or Following timeline."
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
     (or chirp--timeline-limit chirp-default-max-results)
     (or anchor-id (chirp-capture-point-anchor))
     (current-buffer)
     t
     nil
     chirp--timeline-next-cursor))))

(defun chirp-timeline-open-bookmarks (&optional buffer)
  "Open bookmarks."
  (interactive)
  (let* ((buffer (or buffer (chirp-buffer)))
         (refresh (lambda () (chirp-timeline-open-bookmarks buffer))))
    (let ((token (chirp-begin-background-request buffer "Bookmarks")))
      (chirp-timeline--set-kind buffer nil)
      (chirp-backend-bookmarks
       (lambda (tweets _envelope)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--render buffer "Bookmarks" refresh tweets nil nil nil nil t)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--set-kind buffer nil)
           (chirp-show-error buffer "Bookmarks" refresh message)))))))

(defun chirp-timeline-open-search (query &optional buffer)
  "Open search results for QUERY."
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
           (chirp-timeline--render buffer title refresh tweets nil nil nil nil t)))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-timeline--set-kind buffer nil)
           (chirp-show-error buffer title refresh message)))))))

(defun chirp-toggle-home-following ()
  "Toggle between the home and following timelines."
  (interactive)
  (pcase chirp--timeline-kind
    ('home
     (chirp-timeline--open 'following
                           chirp-default-max-results
                           nil
                           (current-buffer)))
    ('following
     (chirp-timeline--open 'home
                           chirp-default-max-results
                           nil
                           (current-buffer)))
    (_
     (user-error "Current view is not a home timeline"))))

(provide 'chirp-timeline)

;;; chirp-timeline.el ends here
