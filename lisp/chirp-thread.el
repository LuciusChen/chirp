;;; chirp-thread.el --- Thread view for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch, enrich, order, and render a focused tweet conversation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)
(require 'chirp-spam-rules)

(defcustom chirp-thread-spam-keywords
  (copy-tree chirp-spam-rules-default)
  "Keywords used to hide replies in thread views.

Each nonempty string is matched case-insensitively against reply text, expanded
URLs, quoted tweet text, author bio, display name, and handle.  Strings also
support `@handle' exact author mutes, `host:example.com' link domain filters,
`A && B' combined literal rules, and `/regexp/' patterns.  A nested list matches
only when all of its strings are nonempty and occur.  The thread focus tweet,
the focus author's own replies, your own posts, and accounts you follow are
exempt from built-in spam rules.  Set this option to nil to disable filtering."
  :type '(repeat (choice string (repeat string)))
  :group 'chirp)

(defcustom chirp-thread-spam-rules-file
  (locate-user-emacs-file "chirp/spam-rules.txt")
  "File containing persistent user spam and mute rules.

Store one rule per line.  Empty lines and lines beginning with `#' are ignored.
Supported forms are plain literal phrases, `@handle' exact author mutes,
`host:domain' URL host filters, `phrase1 && phrase2' combined rules, and
`/regexp/' regular expressions.  Persistent user rules apply to thread replies
as well as Home, Following, Search, and List timelines."
  :type 'file
  :group 'chirp)

(defvar chirp-thread--user-rules-cache nil
  "Cached `(FILE MTIME SIZE RULES)' for `chirp-thread-spam-rules-file'.")

(defun chirp-thread--normalize-spam-rule (rule)
  "Return RULE as one trimmed line, or nil when it is empty."
  (when (stringp rule)
    (let ((normalized
           (string-trim
            (replace-regexp-in-string "[[:space:]]+" " " rule))))
      (unless (string-empty-p normalized)
        normalized))))

(defun chirp-thread--normalize-match-text (text)
  "Normalize TEXT to defeat zero-width, fullwidth ASCII, and whitespace evasion."
  (when (stringp text)
    (let* ((stripped (replace-regexp-in-string
                      "[\u200b-\u200f\u202a-\u202e\u2060-\u206f\ufeff\u00ad]+"
                      ""
                      text))
           (chars (mapcar (lambda (ch)
                            (cond
                             ((= ch #x3000) ?\s)
                             ((<= #xff01 ch #xff5e) (- ch #xfee0))
                             (t ch)))
                          stripped)))
      (string-trim
       (replace-regexp-in-string "[[:space:]]+" " " (apply #'string chars))))))

(defun chirp-thread--literal-rule-present-p (rule rules)
  "Return non-nil when string RULE already occurs in RULES ignoring case."
  (when-let* ((key (chirp-thread--normalize-spam-rule rule)))
    (setq key (downcase key))
    (cl-some
     (lambda (candidate)
       (and (stringp candidate)
            (equal key
                   (downcase
                    (or (chirp-thread--normalize-spam-rule candidate) "")))))
     rules)))

(defun chirp-thread--read-user-spam-rules ()
  "Return user spam rules read from `chirp-thread-spam-rules-file' with caching."
  (when (and (stringp chirp-thread-spam-rules-file)
             (file-readable-p chirp-thread-spam-rules-file)
             (not (file-directory-p chirp-thread-spam-rules-file)))
    (let* ((file (expand-file-name chirp-thread-spam-rules-file))
           (attrs (file-attributes file))
           (mtime (and attrs (file-attribute-modification-time attrs)))
           (size (and attrs (file-attribute-size attrs))))
      (if (and chirp-thread--user-rules-cache
               (equal (nth 0 chirp-thread--user-rules-cache) file)
               (equal (nth 1 chirp-thread--user-rules-cache) mtime)
               (equal (nth 2 chirp-thread--user-rules-cache) size))
          (copy-sequence (nth 3 chirp-thread--user-rules-cache))
        (let ((rules
               (with-temp-buffer
                 (insert-file-contents file)
                 (let ((seen (make-hash-table :test #'equal))
                       parsed)
                   (dolist (line (split-string (buffer-string) "\n"))
                     (when-let* ((rule (chirp-thread--normalize-spam-rule line))
                                 ((not (string-prefix-p "#" rule)))
                                 (key (downcase rule))
                                 ((not (gethash key seen))))
                       (puthash key t seen)
                       (push rule parsed)))
                   (nreverse parsed)))))
          (setq chirp-thread--user-rules-cache (list file mtime size rules))
          (copy-sequence rules))))))

(defun chirp-thread--effective-spam-rules ()
  "Return built-in, customized, and persistent spam rules."
  (let ((rules (copy-tree chirp-thread-spam-keywords)))
    (dolist (rule (chirp-thread--read-user-spam-rules) rules)
      (unless (chirp-thread--literal-rule-present-p rule rules)
        (setq rules (append rules (list rule)))))))

(defun chirp-thread--append-user-spam-rule (rule)
  "Append RULE to `chirp-thread-spam-rules-file' and clear the file cache."
  (let* ((file (expand-file-name chirp-thread-spam-rules-file))
         (directory (file-name-directory file))
         (needs-newline
          (and (file-readable-p file)
               (> (file-attribute-size (file-attributes file)) 0)
               (with-temp-buffer
                 (insert-file-contents file)
                 (not (eq (char-before (point-max)) ?\n))))))
    (make-directory directory t)
    (with-temp-buffer
      (set-buffer-file-coding-system 'utf-8-unix)
      (when needs-newline
        (insert "\n"))
      (insert rule "\n")
      (write-region (point-min) (point-max) file t 'silent))
    (setq chirp-thread--user-rules-cache nil)))

(defun chirp-thread--spam-rule-suggestion (authorp)
  "Return a spam-rule suggestion from point.

When AUTHORP is non-nil, prefer `@handle' for the current author and fall back
to the author's display name.  Otherwise prefer the active region and then the
current tweet text."
  (let ((entry (chirp-entry-at-point)))
    (chirp-thread--normalize-spam-rule
     (cond
      (authorp
       (if-let* ((handle (plist-get entry :author-handle)))
           (format "@%s" (string-remove-prefix "@" handle))
         (plist-get entry :author-name)))
      ((use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end)))
      ((eq (plist-get entry :kind) 'tweet)
       (plist-get entry :text))))))

(defun chirp-thread-add-spam-rule (&optional authorp)
  "Persist one spam or mute rule and refresh the current view.

Use the active region as the initial input, or the current tweet text when no
region is active.  With prefix argument AUTHORP, suggest `@handle' (or display
name) for the current author.  Rules support plain phrases, `@handle',
`host:domain', `A && B', and `/regexp/'."
  (interactive "P")
  (let* ((suggestion (chirp-thread--spam-rule-suggestion authorp))
         (rule (chirp-thread--normalize-spam-rule
                (read-string "Filter rule (phrase, @handle, host:domain, A && B, /re/): "
                             suggestion))))
    (unless rule
      (user-error "Spam rule cannot be empty"))
    (when (string-prefix-p "#" rule)
      (user-error "Spam rule cannot begin with #"))
    (if (chirp-thread--literal-rule-present-p
         rule (chirp-thread--effective-spam-rules))
        (message "Spam rule already exists: %s" rule)
      (chirp-thread--append-user-spam-rule rule)
      (when (functionp chirp--refresh-function)
        (chirp-refresh))
      (message "Added spam rule: %s" rule))))

(defun chirp-thread-edit-spam-rules ()
  "Open `chirp-thread-spam-rules-file' for manual editing."
  (interactive)
  (unless (and (stringp chirp-thread-spam-rules-file)
               (not (string-empty-p chirp-thread-spam-rules-file)))
    (user-error "No user spam rules file is configured"))
  (let ((file (expand-file-name chirp-thread-spam-rules-file)))
    (make-directory (file-name-directory file) t)
    (find-file file)))

(defun chirp-thread--key (tweet)
  "Return a stable key for TWEET."
  (or (plist-get tweet :id)
      (plist-get tweet :url)))

(defun chirp-thread--reorder (tweets focus-id)
  "Move the tweet matching FOCUS-ID to the front of TWEETS."
  (if (not focus-id)
      tweets
    (let* ((focus (cl-find focus-id tweets
                           :key #'chirp-thread--key
                           :test #'equal))
           (rest (cl-remove focus-id tweets
                            :key #'chirp-thread--key
                            :test #'equal)))
      (if focus
          (cons focus rest)
        tweets))))

(defun chirp-thread--parse-spam-rule (rule)
  "Parse RULE into a normalized matcher plist, or return nil if invalid."
  (cond
   ((and (listp rule) rule)
    (let ((fragments (delq nil (mapcar #'chirp-thread--normalize-match-text rule))))
      (when (and fragments (= (length fragments) (length rule)))
        (list :kind 'and
              :label (string-join (mapcar #'string-trim rule) " && ")
              :value fragments))))
   ((stringp rule)
    (when-let* ((trimmed (chirp-thread--normalize-spam-rule rule)))
      (cond
       ((string-match "\\`@\\([^[:space:]]+\\)\\'" trimmed)
        (list :kind 'handle
              :label trimmed
              :value (downcase (match-string 1 trimmed))))
       ((let ((case-fold-search t))
          (string-match "\\`host:\\([^[:space:]/]+\\)\\'" trimmed))
        (list :kind 'host
              :label trimmed
              :value (downcase (match-string 1 trimmed))))
       ((and (>= (length trimmed) 3)
             (string-prefix-p "/" trimmed)
             (string-suffix-p "/" trimmed))
        (let ((pattern (substring trimmed 1 -1)))
          (when (ignore-errors (string-match-p pattern "") t)
            (list :kind 'regexp :label trimmed :value pattern))))
       ((string-match-p " && " trimmed)
        (let* ((parts (split-string trimmed " && "))
               (fragments (delq nil (mapcar #'chirp-thread--normalize-match-text parts))))
          (when (and fragments (= (length fragments) (length parts)))
            (list :kind 'and :label trimmed :value fragments))))
       (t
        (when-let* ((norm (chirp-thread--normalize-match-text trimmed))
                    ((not (string-empty-p norm))))
          (list :kind 'literal :label trimmed :value norm))))))))

(defun chirp-thread--compile-spam-rules (rules)
  "Compile RULES into a fast matcher structure."
  (let ((handles (make-hash-table :test #'equal))
        hosts literals complex)
    (dolist (raw rules)
      (when-let* ((parsed (chirp-thread--parse-spam-rule raw)))
        (pcase (plist-get parsed :kind)
          ('handle (puthash (plist-get parsed :value) (plist-get parsed :label) handles))
          ('host (push (cons (plist-get parsed :value) (plist-get parsed :label)) hosts))
          ('literal (push (cons (plist-get parsed :value) (plist-get parsed :label)) literals))
          (_ (push parsed complex)))))
    (setq literals (nreverse literals))
    (list :handles handles
          :hosts (nreverse hosts)
          :literals literals
          :literal-regexp (when literals
                            (regexp-opt (mapcar #'car literals)))
          :complex (nreverse complex))))

(defun chirp-thread--tweet-urls (tweet)
  "Return expanded URLs from TWEET and its quoted tweet."
  (let ((quoted (plist-get tweet :quoted-tweet)))
    (append (plist-get tweet :urls)
            (and quoted (plist-get quoted :urls)))))

(defun chirp-thread--url-host-matches-p (url target-host)
  "Return non-nil when URL belongs to TARGET-HOST or a subdomain."
  (when (stringp url)
    (let* ((parsed (ignore-errors (url-generic-parse-url url)))
           (host (and parsed (url-host parsed))))
      (when (and (stringp host) (not (string-empty-p host)))
        (let ((down (downcase host)))
          (or (equal down target-host)
              (string-suffix-p (concat "." target-host) down)))))))

(defun chirp-thread--tweet-match-content (tweet)
  "Return normalized searchable text for TWEET, its author, and quoted tweet."
  (let ((quoted (plist-get tweet :quoted-tweet)))
    (chirp-thread--normalize-match-text
     (string-join
      (cl-remove-if-not
       #'stringp
       (append (list (plist-get tweet :text)
                     (plist-get tweet :author-name)
                     (plist-get tweet :author-handle)
                     (plist-get tweet :author-bio)
                     (and quoted (plist-get quoted :text))
                     (and quoted (plist-get quoted :author-name))
                     (and quoted (plist-get quoted :author-handle)))
               (chirp-thread--tweet-urls tweet)))
      "\n"))))

(defun chirp-thread--match-compiled-rules (tweet compiled)
  "Return the first matching rule label from COMPILED for TWEET, or nil."
  (let ((case-fold-search t)
        (handle (plist-get tweet :author-handle)))
    (or (and (stringp handle)
             (gethash (downcase (string-remove-prefix "@" handle))
                      (plist-get compiled :handles)))
        (when-let* ((hosts (plist-get compiled :hosts))
                    (urls (chirp-thread--tweet-urls tweet)))
          (cl-loop for (target . label) in hosts
                   when (cl-some (lambda (u) (chirp-thread--url-host-matches-p u target))
                                 urls)
                   return label))
        (let ((content (chirp-thread--tweet-match-content tweet)))
          (when (and content (not (string-empty-p content)))
            (or (when-let* ((lit-re (plist-get compiled :literal-regexp))
                            ((string-match-p lit-re content)))
                  (cl-loop for (value . label) in (plist-get compiled :literals)
                           when (string-match-p (regexp-quote value) content)
                           return label))
                (cl-loop for rule in (plist-get compiled :complex)
                         for kind = (plist-get rule :kind)
                         for value = (plist-get rule :value)
                         when (if (eq kind 'and)
                                  (cl-every (lambda (frag)
                                              (string-match-p (regexp-quote frag) content))
                                            value)
                                (ignore-errors (string-match-p value content)))
                         return (plist-get rule :label))))))))

(defun chirp-thread--reply-exempt-p (tweet &optional focus-handle user-only-p)
  "Return non-nil when TWEET should never be hidden by spam filtering.

FOCUS-HANDLE identifies the thread's original author.  When USER-ONLY-P is
non-nil, followed accounts may still match explicit user mute rules."
  (or (eq (plist-get tweet :timeline-context) 'related)
      (plist-get tweet :author-self-p)
      (and (not user-only-p)
           (or (plist-get tweet :author-following-p)
               (and (stringp focus-handle)
                    (stringp (plist-get tweet :author-handle))
                    (string-equal-ignore-case
                     (string-remove-prefix "@" focus-handle)
                     (string-remove-prefix "@" (plist-get tweet :author-handle))))))))

(defun chirp-thread--spam-reply-p (tweet &optional rules focus-handle)
  "Return the matching spam rule label for reply TWEET, or nil.

Use RULES instead of `chirp-thread-spam-keywords' when non-nil.  FOCUS-HANDLE
protects replies written by the thread's original author."
  (unless (chirp-thread--reply-exempt-p tweet focus-handle)
    (chirp-thread--match-compiled-rules
     tweet
     (chirp-thread--compile-spam-rules (or rules chirp-thread-spam-keywords)))))

(defun chirp-thread--partition-spam-replies (tweets)
  "Partition TWEETS into `(VISIBLE . HIDDEN-ALIST)' while preserving the focus.

HIDDEN-ALIST holds `(TWEET . MATCHED-RULE)' pairs for filtered replies."
  (if (or (null tweets)
          (null chirp-thread-spam-keywords))
      (cons tweets nil)
    (let* ((focus (car tweets))
           (focus-handle (plist-get focus :author-handle))
           (builtin-compiled (chirp-thread--compile-spam-rules chirp-thread-spam-keywords))
           (user-rules (chirp-thread--read-user-spam-rules))
           (user-compiled (and user-rules (chirp-thread--compile-spam-rules user-rules)))
           kept hidden)
      (dolist (reply (cdr tweets))
        (let ((matched
               (or (and user-compiled
                        (not (chirp-thread--reply-exempt-p reply focus-handle t))
                        (chirp-thread--match-compiled-rules reply user-compiled))
                   (and (not (chirp-thread--reply-exempt-p reply focus-handle nil))
                        (chirp-thread--match-compiled-rules reply builtin-compiled)))))
          (if matched
              (push (cons reply matched) hidden)
            (push reply kept))))
      (cons (cons focus (nreverse kept))
            (nreverse hidden)))))

(defun chirp-thread--filter-spam-replies (tweets)
  "Hide keyword-matching replies from TWEETS while preserving the focus."
  (car (chirp-thread--partition-spam-replies tweets)))

(defun chirp-thread-filter-user-tweets (tweets)
  "Filter TWEETS against persistent user rules in `chirp-thread-spam-rules-file'.

Built-in thread spam defaults are not applied to timeline views.  Setting
`chirp-thread-spam-keywords' to nil disables user rules as well."
  (if (or (null tweets)
          (null chirp-thread-spam-keywords))
      tweets
    (let ((user-rules (chirp-thread--read-user-spam-rules)))
      (if (null user-rules)
          tweets
        (let ((compiled (chirp-thread--compile-spam-rules user-rules)))
          (cl-remove-if
           (lambda (tweet)
             (and (not (plist-get tweet :author-self-p))
                  (chirp-thread--match-compiled-rules tweet compiled)))
           tweets))))))

(defun chirp-thread--title (tweet-or-url)
  "Return a display title for TWEET-OR-URL."
  (if (and (stringp tweet-or-url)
           (string-match "/status/\\([0-9]+\\)" tweet-or-url))
      (format "Thread: %s" (match-string 1 tweet-or-url))
    (format "Thread: %s"
            (if (stringp tweet-or-url)
                tweet-or-url
              (or (plist-get tweet-or-url :id) "tweet")))))

(defun chirp-thread--seed-tweets (tweet-or-url focus-id)
  "Return a renderable list for TWEET-OR-URL matching FOCUS-ID, or nil."
  (when (and (listp tweet-or-url)
             (eq (plist-get tweet-or-url :kind) 'tweet)
             (or (null focus-id)
                 (equal (plist-get tweet-or-url :id) focus-id)))
    (list tweet-or-url)))

(defun chirp-thread--article-fetch-needed-p (tweet)
  "Return non-nil when TWEET needs its full article content."
  (and (plist-get tweet :id)
       (not (chirp-first-nonblank (plist-get tweet :article-text)))
       (or (chirp-first-nonblank (plist-get tweet :article-title))
           (and (string-empty-p (or (plist-get tweet :text) ""))
                (plist-get tweet :urls)))))

(defun chirp-thread--maybe-apply-article (tweets article-tweet)
  "Return TWEETS with ARTICLE-TWEET replacing the current focus when ids match."
  (if (and tweets
           article-tweet
           (equal (plist-get (car tweets) :id)
                  (plist-get article-tweet :id)))
      (cons article-tweet (cdr tweets))
    tweets))

(defun chirp-thread--render-view
    (buffer title refresh ordered
            &optional anchor-id display-p tail hidden-entries show-hidden-p)
  "Render ORDERED thread tweets into BUFFER.

TITLE and REFRESH are the usual buffer metadata.  When ANCHOR-ID is non-nil,
restore point to that entry.  DISPLAY-P shows the buffer.  When TAIL is
non-nil, append replies after its first entry.  HIDDEN-ENTRIES is either a list
of `(TWEET . RULE)' pairs or an integer count; when SHOW-HIDDEN-P is non-nil,
append filtered replies with their matched rule labels."
  (let* ((focus (car ordered))
         (replies (cdr ordered))
         (hidden-count (if (listp hidden-entries)
                           (length hidden-entries)
                         (or hidden-entries 0))))
    (if (and tail (not show-hidden-p))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (save-excursion
              (if-let* ((heading (text-property-any (point-min) (point-max) 'chirp-replies-heading t)))
                  (progn (goto-char heading)
                         (delete-region heading (line-beginning-position 3)))
                (goto-char (point-max)))
              (chirp-render-insert-thread-divider (length replies) hidden-count nil)
              (goto-char (point-max))
              (dolist (tweet (cdr tail)) (chirp-render-insert-thread-reply tweet)))))
      (chirp-render-into-buffer
       buffer title refresh
       (lambda ()
         (if focus
             (progn
               (chirp-render-insert-thread-focus-tweet focus)
               (when (or replies (> hidden-count 0))
                 (chirp-render-insert-thread-divider
                  (length replies) hidden-count show-hidden-p)
                 (dolist (tweet replies)
                   (chirp-render-insert-thread-reply tweet))
                 (when (and show-hidden-p (consp hidden-entries))
                   (dolist (entry hidden-entries)
                     (chirp-render-insert-thread-reply (car entry) (cdr entry))))))
           (chirp-render-insert-empty "No thread data returned.")))))
    (with-current-buffer buffer
      (or (and (eq anchor-id 'chirp-replies-heading)
               (when-let* ((pos (text-property-any (point-min) (point-max)
                                                   'chirp-replies-heading t)))
                 (goto-char pos)
                 t))
          (and anchor-id
               (chirp-restore-point-anchor anchor-id))
          (chirp-move-point-to-first-entry)))
    (when display-p
      (chirp-display-buffer buffer))))

(defun chirp-thread-open (tweet-or-url &optional focus-id buffer)
  "Open a thread for TWEET-OR-URL focused on FOCUS-ID in BUFFER."
  (interactive "sTweet ID or URL: ")
  (let* ((request-target (cond
                          ((stringp tweet-or-url) tweet-or-url)
                          ((plist-get tweet-or-url :url))
                          ((plist-get tweet-or-url :id))
                          (t (user-error "Need a tweet id or URL"))))
         (title (chirp-thread--title request-target))
         (buffer (or buffer (chirp-buffer)))
         (refresh (lambda ()
                    (chirp-backend-invalidate-thread request-target)
                    (when focus-id
                      (chirp-backend-invalidate-article focus-id))
                    (chirp-thread-open request-target focus-id buffer)))
         (saved-ordered nil)
         (hidden-entries nil)
         (show-hidden-p nil)
         (all-tweets nil)
         (shown nil)
         (next-cursor nil)
         (loading-more nil)
         (ready nil)
         (prefetched-article nil)
         (article-requested-p nil)
         (token nil))
    (cl-labels
        ((partition-current ()
           (pcase-let ((`(,visible . ,hidden)
                        (chirp-thread--partition-spam-replies all-tweets)))
             (setq saved-ordered visible
                   hidden-entries hidden)))
         (toggle-hidden ()
           (setq show-hidden-p (not show-hidden-p))
           (render-current 'chirp-replies-heading)
           (when show-hidden-p
             (let ((hidden-tweets (mapcar #'car hidden-entries)))
               (chirp-media-prefetch-tweets hidden-tweets buffer)
               (chirp-enrich-quoted-tweets hidden-tweets buffer))))
         (render-current (&optional anchor-id display-p tail)
           (chirp-thread--render-view
            buffer title refresh saved-ordered anchor-id (and display-p (not shown))
            tail hidden-entries show-hidden-p)
           (when display-p (setq shown t))
           (with-current-buffer buffer
             (setq-local chirp--timeline-next-cursor next-cursor
                         chirp--timeline-exhausted-p (and ready (not next-cursor))
                         chirp--timeline-load-more-function
                         (and next-cursor #'load-more)
                         chirp--toggle-hidden-replies-function
                         (and hidden-entries #'toggle-hidden))
             (setq-local chirp--rerender-function
                         (lambda ()
                           (render-current
                            (with-current-buffer buffer
                              (chirp-capture-point-anchor)))))))
         (load-more (anchor-id)
           (unless (or loading-more (null next-cursor))
             (setq loading-more t)
             (chirp-set-status buffer "Loading replies...")
             (chirp-backend-thread
              request-target
              (lambda (tweets envelope)
                (when (chirp-request-current-p buffer token)
                  (let* ((cursor (chirp-backend-envelope-next-cursor envelope))
                         (old-count (length saved-ordered)))
                    (setq next-cursor (unless (equal cursor next-cursor) cursor)
                          loading-more nil
                          all-tweets (chirp-append-unique-tweets all-tweets tweets))
                    (partition-current)
                    (apply-prefetched-article)
                    (let ((tail (and (not show-hidden-p)
                                     (> old-count 0)
                                     (nthcdr (1- old-count) saved-ordered))))
                      (render-current anchor-id nil tail)
                      (chirp-media-prefetch-tweets (cdr tail) buffer)
                      (chirp-enrich-quoted-tweets (cdr tail) buffer)))
                  (if next-cursor (chirp-clear-status buffer)
                    (chirp-set-status buffer "End of thread" 'info))))
              (lambda (reason)
                (when (chirp-request-current-p buffer token)
                  (setq loading-more nil)
                  (chirp-set-status buffer (concat reason " · N retry") 'error)))
              next-cursor)))
         (prefetch-current ()
           (chirp-media-prefetch-tweets saved-ordered buffer)
           (chirp-enrich-quoted-tweets saved-ordered buffer))
         (present-current (&optional anchor-id display-p)
           (render-current anchor-id display-p)
           (prefetch-current))
         (apply-prefetched-article ()
           (setq saved-ordered
                 (chirp-thread--maybe-apply-article
                  saved-ordered
                  prefetched-article)))
         (handle-article-success (article-tweet _envelope)
           (when (chirp-request-current-p buffer token)
             (setq prefetched-article article-tweet)
             (when saved-ordered
               (let ((anchor-id (with-current-buffer buffer
                                  (chirp-capture-point-anchor))))
                 (apply-prefetched-article)
                 (present-current anchor-id)
                 (chirp-clear-status buffer)))))
         (maybe-request-article (tweet)
           (when (and (not article-requested-p)
                      (chirp-thread--article-fetch-needed-p tweet))
             (setq article-requested-p t)
             (chirp-set-status buffer "Thread ready · loading article...")
             (chirp-backend-article
              (plist-get tweet :id)
              #'handle-article-success
              (lambda (_message)
                (when (chirp-request-current-p buffer token)
                  (chirp-clear-status buffer)))))))
      (setq token (chirp-begin-background-request buffer title))
      (chirp-set-status buffer "Loading thread...")
      (when-let* ((seed (chirp-thread--seed-tweets tweet-or-url focus-id)))
        (setq saved-ordered seed)
        (present-current nil t))
      (when (and (listp tweet-or-url)
                 (plist-get tweet-or-url :id))
        (maybe-request-article tweet-or-url))
      (chirp-backend-thread
       request-target
       (lambda (tweets envelope)
         (when (chirp-request-current-p buffer token)
           (setq ready t
                 next-cursor (chirp-backend-envelope-next-cursor envelope)
                 all-tweets (chirp-thread--reorder tweets focus-id))
           (partition-current)
           (apply-prefetched-article)
           (present-current nil t)
           (if-let* ((focus (car saved-ordered)))
               (progn
                 (maybe-request-article focus)
                 (unless article-requested-p
                   (chirp-clear-status buffer)))
             (chirp-clear-status buffer))))
       (lambda (message)
         (when (chirp-request-current-p buffer token)
           (chirp-show-error buffer title refresh message)))))))

(provide 'chirp-thread)

;;; chirp-thread.el ends here
