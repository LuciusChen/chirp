;;; chirp-backend.el --- Backend adapters for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Adapt X web responses into Chirp's normalized model, share short-lived read
;; results, and orchestrate authenticated write workflows.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ucs-normalize)
(require 'chirp-core)
(require 'chirp-x)

(defconst chirp-backend--lists-cache-key '(:lists)
  "Cache key for the authenticated account's list catalog.")

(defconst chirp-backend--standard-tweet-weight-limit 280
  "Maximum weighted length routed through CreateTweet.")

(defconst chirp-backend--transformed-url-length 23
  "Weighted length assigned to each URL by X.")

(defconst chirp-backend--weight-one-ranges
  '((#x0000 . #x10ff)
    (#x2000 . #x200d)
    (#x2010 . #x201f)
    (#x2032 . #x2037))
  "Unicode ranges whose ordinary X text weight is one.")

(defconst chirp-backend--emoji-base-ranges
  '((#x2190 . #x21ff)
    (#x2300 . #x23ff)
    (#x2600 . #x27bf)
    (#x1f000 . #x1faff)
    (#x1fc00 . #x1ffff))
  "Unicode ranges recognized as emoji sequence bases.")

(defconst chirp-backend--emoji-bases
  '(#x00a9 #x00ae #x203c #x2049 #x2122 #x2139
           #x3030 #x303d #x3297 #x3299)
  "Individual Unicode code points recognized as emoji sequence bases.")

(defconst chirp-backend--tweet-url-regexp
  (concat
   "\\(?:https?://[^[:space:]<>{}\\\"']+"
   "\\|[[:alnum:]][[:alnum:]-]*"
   "\\(?:\\.[[:alnum:]-]+\\)+"
   "\\(?::[0-9]+\\)?"
   "\\(?:[/#?][^[:space:]<>{}\\\"']*\\)?\\)")
  "Compact URL regexp used only for tweet operation routing.")

(defconst chirp-backend--tweet-features
  '(("rweb_cashtags_enabled" . t)
    ("profile_label_improvements_pcf_label_in_post_enabled" . t)
    ("responsive_web_profile_redirect_enabled" . t)
    ("creator_subscriptions_tweet_preview_api_enabled" . t)
    ("responsive_web_graphql_timeline_navigation_enabled" . t)
    ("communities_web_enable_tweet_community_results_fetch" . t)
    ("c9s_tweet_anatomy_moderator_badge_enabled" . t)
    ("rweb_cashtags_composer_attachment_enabled" . t)
    ("responsive_web_jetfuel_frame" . t)
    ("responsive_web_grok_share_attachment_enabled" . t)
    ("responsive_web_grok_annotations_enabled" . t)
    ("articles_preview_enabled" . t)
    ("responsive_web_edit_tweet_api_enabled" . t)
    ("graphql_is_translatable_rweb_tweet_is_translatable_enabled" . t)
    ("view_counts_everywhere_api_enabled" . t)
    ("longform_notetweets_consumption_enabled" . t)
    ("responsive_web_twitter_article_tweet_consumption_enabled" . t)
    ("content_disclosure_indicator_enabled" . t)
    ("content_disclosure_ai_generated_indicator_enabled" . t)
    ("responsive_web_grok_show_grok_translated_post" . t)
    ("responsive_web_grok_analysis_button_from_backend" . t)
    ("freedom_of_speech_not_reach_fetch_enabled" . t)
    ("standardized_nudges_misinfo" . t)
    ("tweet_with_visibility_results_prefer_gql_limited_actions_policy_enabled" . t)
    ("longform_notetweets_rich_text_read_enabled" . t)
    ("responsive_web_grok_image_annotation_enabled" . t)
    ("responsive_web_grok_imagine_annotation_enabled" . t)
    ("responsive_web_grok_community_note_auto_translation_is_enabled" . t))
  "Feature switches documented for current tweet GraphQL operations.")

(defconst chirp-backend--timeline-field-toggles
  '(("withPayments" . t)
    ("withAuxiliaryUserLabels" . t)
    ("withArticleRichContentState" . t)
    ("withArticlePlainText" . t)
    ("withArticleSummaryText" . t)
    ("withArticleVoiceOver" . t)
    ("withGrokAnalyze" . t)
    ("withDisallowedReplyControls" . t))
  "Field toggles documented for HomeTimeline operations.")

(defconst chirp-backend--note-tweet-features
  (append '(("longform_notetweets_creation_enabled" . t) ("longform_notetweets_richtext_consumption_enabled" . t))
          chirp-backend--tweet-features)
  "Feature switches required by CreateNoteTweet.")

(defconst chirp-backend--user-features
  '(("hidden_profile_subscriptions_enabled" . t)
    ("rweb_tipjar_consumption_enabled" . t)
    ("responsive_web_graphql_exclude_directive_enabled" . t)
    ("subscriptions_verification_info_is_identity_verified_enabled" . t)
    ("subscriptions_verification_info_verified_since_enabled" . t)
    ("highlights_tweets_tab_ui_enabled" . t)
    ("responsive_web_twitter_article_notes_tab_enabled" . t)
    ("subscriptions_feature_can_gift_premium" . t)
    ("creator_subscriptions_tweet_preview_api_enabled" . t)
    ("responsive_web_graphql_timeline_navigation_enabled" . t))
  "Feature switches used to resolve an X profile.")

(defconst chirp-backend--operations
  `((home
     :query-id "wp06oo3fRGU4P1sK8rECqQ" :name "HomeTimeline"
     :features ,chirp-backend--tweet-features
     :field-toggles ,chirp-backend--timeline-field-toggles)
    (following
     :query-id "BLQWpfVqtgBqAqwRRJcJjA" :name "HomeLatestTimeline"
     :features ,chirp-backend--tweet-features
     :field-toggles ,chirp-backend--timeline-field-toggles)
    (user
     :query-id "1VOOyvKkiI3FMmkeDNxM9A" :name "UserByScreenName"
     :features ,chirp-backend--user-features)
    (viewer
     :query-id "5XShkXk2oO2J7SYmTu6pvw" :name "Viewer"
     :features (("profile_label_improvements_pcf_label_in_post_enabled" . t)
                ("responsive_web_profile_redirect_enabled" . t)
                ("creator_subscriptions_tweet_preview_api_enabled" . t)
                ("responsive_web_graphql_timeline_navigation_enabled" . t)))
    (user-tweets
     :query-id "q6xj5bs0hapm9309hexA_g" :name "UserTweets"
     :features ,chirp-backend--tweet-features)
    (user-highlights
     :query-id "70Yf8aSyhGOXaKRLJdVA2A" :name "UserHighlightsTweets"
     :features ,chirp-backend--tweet-features)
    (user-media
     :query-id "1H9ibIdchWO0_vz3wJLDTA" :name "UserMedia"
     :features ,chirp-backend--tweet-features)
    (likes
     :query-id "lIDpu_NWL7_VhimGGt0o6A" :name "Likes"
     :features ,chirp-backend--tweet-features)
    (search
     :query-id "VhUd6vHVmLBcw0uX-6jMLA" :name "SearchTimeline" :method post :query t
     :features ,chirp-backend--tweet-features)
    (bookmarks
     :query-id "2neUNDqrrFzbLui8yallcQ" :name "Bookmarks"
     :features ,chirp-backend--tweet-features)
    (notifications
     :query-id "-S_pMlnJKTY3uUdlVKpK9w" :name "NotificationsTimeline"
     :features ,chirp-backend--tweet-features)
    (list
     :query-id "RlZzktZY_9wJynoepm8ZsA" :name "ListLatestTweetsTimeline"
     :features ,chirp-backend--tweet-features)
    (thread
     :query-id "XMOz5h24KAZ86qKffKTLdQ" :name "TweetDetail"
     :features ,chirp-backend--tweet-features
     :field-toggles (("withArticleRichContentState" . t)))
    (article
     :query-id "GZsN2Pc4knAoit6pXa4HSA" :name "TweetResultByRestId"
     :features ,chirp-backend--tweet-features
     :field-toggles (("withArticleRichContentState" . t)
                     ("withArticlePlainText" . t)))
    (create-tweet
     :query-id "l4hk8uVGvR-a8kzQ4nOhPA" :name "CreateTweet" :method post
     :features ,(append '(("post_ctas_fetch_enabled" . t)
                          ("responsive_web_grok_analyze_post_followups_enabled" . t)
                          ("responsive_web_nested_quote_preview_enabled" . t)
                          ("rweb_sports_post_context_enabled" . t)
                          ("x_elections_post_context_enabled" . t))
                        (cl-remove "creator_subscriptions_tweet_preview_api_enabled"
                                   chirp-backend--tweet-features :key #'car :test #'equal)))
    (create-note-tweet
     :query-id "dAlh5Gh9rR5pKk4HU4vW8g" :name "CreateNoteTweet" :method post
     :features ,chirp-backend--note-tweet-features)
    (delete-tweet
     :query-id "VaenaVgh5q5ih7kvyVjgtg" :name "DeleteTweet" :method post)
    (favorite
     :query-id "lI07N6Otwv1PhnEgXILM7A" :name "FavoriteTweet" :method post)
    (unfavorite
     :query-id "ZYKSe-w7KEslx3JhSIk5LA" :name "UnfavoriteTweet" :method post)
    (retweet
     :query-id "ojPdsZsimiJrUGLR1sjUtA" :name "CreateRetweet" :method post)
    (unretweet
     :query-id "iQtK4dl5hBmXewYZuEOKVw" :name "DeleteRetweet" :method post)
    (bookmark
     :query-id "aoDbu3RHznuiSkQ9aNM67Q" :name "CreateBookmark" :method post)
    (unbookmark
     :query-id "Wlmlj2-xzyS1GN3a6cj-mQ" :name "DeleteBookmark" :method post))
  "Persisted X web operations used by Chirp's direct backend.")

(defconst chirp-backend--user-timeline-paths
  '(("data" "user" "result" "timeline" "timeline")
    ("data" "user" "result" "timeline_v2" "timeline"))
  "Alternative paths to an X user timeline object.")

(defcustom chirp-backend-query-ids nil
  "Optional overrides for X operation IDs, keyed by operation symbol."
  :type '(alist :key-type symbol :value-type string)
  :group 'chirp)

(defun chirp-backend--operation (key)
  "Return the persisted X operation identified by KEY."
  (let ((operation (or (cdr (assq key chirp-backend--operations))
                       (error "Unknown X operation: %S" key))))
    (if-let* ((id (alist-get key chirp-backend-query-ids)))
        (plist-put (copy-sequence operation) :query-id id)
      operation)))

(defcustom chirp-backend-read-cache-ttl 15
  "Seconds to keep successful thread/profile/article reads in memory.

When zero or negative, the in-memory read cache is disabled."
  :type 'number
  :group 'chirp)

(defvar chirp-backend--read-cache (make-hash-table :test #'equal)
  "In-memory cache for successful Chirp read responses.")

(defvar chirp-backend--pending-read-requests (make-hash-table :test #'equal)
  "Map cache keys to queued Chirp read callbacks while a request is in flight.")

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
  "Return the cache key for thread TWEET-OR-URL."
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

(defun chirp-backend--profile-timeline-cache-key (handle mode)
  "Return the cache key for HANDLE profile timeline MODE."
  (list :profile-timeline
        (chirp-backend--normalize-handle handle)
        mode))

(defun chirp-backend--followers-cache-key (handle)
  "Return the cache key for HANDLE followers."
  (list :followers (chirp-backend--normalize-handle handle)))

(defun chirp-backend--following-users-cache-key (handle)
  "Return the cache key for HANDLE following users."
  (list :following-users (chirp-backend--normalize-handle handle)))

(defun chirp-backend-list-id (target)
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
                     (chirp-backend--user-posts-cache-key handle)
                     (chirp-backend--profile-timeline-cache-key handle 'replies)
                     (chirp-backend--profile-timeline-cache-key handle 'highlights)
                     (chirp-backend--profile-timeline-cache-key handle 'media)
                     (chirp-backend--followers-cache-key handle)
                     (chirp-backend--following-users-cache-key handle)))
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

ERRBACK handles failures.  FETCHER is called with success and error callbacks."
  (if-let* ((entry (chirp-backend--cached-result key)))
      (funcall callback
               (chirp-backend--clone-data (plist-get entry :value))
               (chirp-backend--clone-data (plist-get entry :envelope)))
    (let ((pending (gethash key chirp-backend--pending-read-requests)))
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
                   (chirp-cache-put key
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

(defun chirp-backend--codepoint-in-ranges-p (codepoint ranges)
  "Return non-nil when CODEPOINT belongs to one of RANGES."
  (cl-some (lambda (range)
             (<= (car range) codepoint (cdr range)))
           ranges))

(defun chirp-backend--emoji-base-p (codepoint)
  "Return non-nil when CODEPOINT can start an emoji sequence."
  (or (memq codepoint chirp-backend--emoji-bases)
      (chirp-backend--codepoint-in-ranges-p
       codepoint chirp-backend--emoji-base-ranges)))

(defun chirp-backend--consume-emoji-suffix (text index limit)
  "Return the end of emoji suffix characters in TEXT from INDEX to LIMIT."
  (while (and (< index limit)
              (let ((codepoint (aref text index)))
                (or (memq codepoint '(#xfe0e #xfe0f))
                    (<= #x1f3fb codepoint #x1f3ff)
                    (<= #xe0020 codepoint #xe007f))))
    (setq index (1+ index)))
  index)

(defun chirp-backend--registered-emoji-sequence-end (text index)
  "Return the registered Unicode emoji sequence end in TEXT at INDEX."
  (let ((rules (char-table-range composition-function-table
                                 (aref text index)))
        end)
    (dolist (rule rules end)
      (when (and (vectorp rule)
                 (> (length rule) 2)
                 (stringp (aref rule 0))
                 (memq (aref rule 2) '(compose-gstring-for-graphic compose-gstring-and-emoji)))
        (save-match-data
          (when (and (string-match (aref rule 0) text index)
                     (= (match-beginning 0) index))
            (setq end (max (or end 0) (match-end 0)))))))))

(defun chirp-backend--emoji-sequence-end (text index limit)
  "Return the emoji sequence end in TEXT at INDEX before LIMIT, or nil."
  (let ((codepoint (aref text index)))
    (cond
     ((memq codepoint '(35 42 48 49 50 51 52 53 54 55 56 57))
      (let ((cursor (1+ index)))
        (when (and (< cursor limit) (= (aref text cursor) #xfe0f))
          (setq cursor (1+ cursor)))
        (and (< cursor limit)
             (= (aref text cursor) #x20e3)
             (1+ cursor))))
     ((<= #x1f1e6 codepoint #x1f1ff)
      (if (and (< (1+ index) limit)
               (<= #x1f1e6 (aref text (1+ index)) #x1f1ff))
          (+ index 2)
        (1+ index)))
     ((chirp-backend--emoji-base-p codepoint)
      (or (let ((registered-end
                 (chirp-backend--registered-emoji-sequence-end text index)))
            (and registered-end
                 (<= registered-end limit)
                 registered-end))
          (chirp-backend--consume-emoji-suffix
           text (1+ index) limit))))))

(defun chirp-backend--tweet-character-weight (codepoint)
  "Return X's ordinary text weight for CODEPOINT."
  (if (chirp-backend--codepoint-in-ranges-p
       codepoint chirp-backend--weight-one-ranges)
      1
    2))

(defun chirp-backend--weighted-text-segment (text start end)
  "Return X's weighted length for TEXT between START and END."
  (let ((index start)
        (weight 0))
    (while (< index end)
      (if-let* ((emoji-end
                 (chirp-backend--emoji-sequence-end text index end)))
          (setq weight (+ weight 2)
                index emoji-end)
        (setq weight
              (+ weight
                 (chirp-backend--tweet-character-weight (aref text index)))
              index (1+ index))))
    weight))

(defun chirp-backend--trim-tweet-url-end (text start end)
  "Return URL END in TEXT after trimming prose punctuation from START."
  (while (and (> end start)
              (memq (aref text (1- end))
                    '(46 44 33 63 58 59 39 34)))
    (setq end (1- end)))
  (dolist (pair '((40 . 41) (91 . 93) (123 . 125)))
    (while (and (> end start)
                (= (aref text (1- end)) (cdr pair))
                (> (cl-count (cdr pair) text :start start :end end)
                   (cl-count (car pair) text :start start :end end)))
      (setq end (1- end))))
  end)

(defun chirp-backend--tweet-url-spans (text)
  "Return likely URL (START . END) spans in TEXT."
  (let ((case-fold-search t)
        (cursor 0)
        spans)
    (while (and (< cursor (length text))
                (string-match chirp-backend--tweet-url-regexp text cursor))
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (if (and (> start 0)
                 (let ((previous (aref text (1- start))))
                   (or (= previous ?@)
                       (= (char-syntax previous) ?w)
                       (= (char-syntax previous) ?_))))
            (setq cursor (1+ start))
          (setq end (chirp-backend--trim-tweet-url-end text start end)
                cursor (max (1+ start) end))
          (when (> end start)
            (push (cons start end) spans)))))
    (nreverse spans)))

(defun chirp-backend-tweet-weighted-length (text)
  "Return the X weighted length used to select a create operation for TEXT."
  (let* ((normalized (ucs-normalize-NFC-string text))
         (cursor 0)
         (weight 0))
    (dolist (span (chirp-backend--tweet-url-spans normalized))
      (setq weight
            (+ weight
               (chirp-backend--weighted-text-segment
                normalized cursor (car span))
               chirp-backend--transformed-url-length)
            cursor (cdr span)))
    (+ weight
       (chirp-backend--weighted-text-segment
        normalized cursor (length normalized)))))

(defun chirp-backend--created-tweet-id (payload)
  "Return a created tweet identifier from GraphQL PAYLOAD, or nil."
  (cl-loop for path in '(("data" "create_tweet" "tweet_results" "result"
                          "rest_id")
                         ("data" "notetweet_create" "tweet_results" "result"
                          "rest_id")
                         ("data" "create_note_tweet" "tweet_results" "result"
                          "rest_id"))
           for identifier = (chirp-get-in payload path)
           when (and identifier
                     (not (string-empty-p (format "%s" identifier))))
           return (format "%s" identifier)))

(defun chirp-backend--compose-variables
    (kind text target-id media-ids note-tweet-p &optional target-handle)
  "Build X create variables for KIND, TEXT, TARGET-ID, and MEDIA-IDS.

When NOTE-TWEET-P is non-nil, include the long-form-only variables.  Optional
TARGET-HANDLE formats quote attachment URLs with the author's handle."
  (let ((variables
         `(("tweet_text" . ,text)
           ("media" .
            (("media_entities" .
              ,(vconcat
                (mapcar
                 (lambda (media-id)
                   `(("media_id" . ,media-id) ("tagged_users" . [])))
                 media-ids)))
             ("possibly_sensitive" . :json-false)))
           ("semantic_annotation_ids" . [])
           ("dark_request" . :json-false)
           ("includePromotedContent" . :json-false))))
    (pcase kind
      ('reply
       (push `("reply" .
               (("in_reply_to_tweet_id" . ,target-id)
                ("exclude_reply_user_ids" . [])))
             variables))
      ('quote
       (let ((handle (and (stringp target-handle)
                          (let ((clean (string-remove-prefix "@" (string-trim target-handle))))
                            (and (string-match-p "\\`[A-Za-z0-9_]+\\'" clean)
                                 clean)))))
         (push `("attachment_url" . ,(format "https://x.com/%s/status/%s"
                                             (or handle "i") target-id))
               variables))))
    (when note-tweet-p
      (push '("disallowed_reply_options" . nil) variables))
    variables))

(defun chirp-backend--upload-compose-media
    (files callback errback &optional media-ids)
  "Upload FILES sequentially and call CALLBACK with their media IDs.

ERRBACK receives the first failure.  MEDIA-IDS carries recursive state."
  (if (null files)
      (funcall callback (nreverse media-ids))
    (chirp-x-upload-media
     (car files)
     (lambda (media-id)
       (chirp-backend--upload-compose-media
        (cdr files) callback errback (cons media-id media-ids)))
     :errback errback)))

(cl-defun chirp-backend-compose
    (&key kind text target-id target-handle attachments callback errback)
  "Publish a KIND draft containing TEXT and ATTACHMENTS through X.

KIND is `post', `reply', or `quote'.  TARGET-ID is required for replies and
quotes; optional TARGET-HANDLE sets the author path on quote URLs.  CALLBACK
receives a plist containing the created tweet ID and the raw GraphQL envelope.
ERRBACK receives upload or create failures.  Create and upload mutations are
never retried automatically."
  (unless (functionp callback)
    (error "Compose callback is not callable"))
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (unless (functionp error-fn)
      (error "Compose error callback is not callable"))
    (condition-case err
        (progn
          (unless (memq kind '(post reply quote))
            (error "Compose kind is invalid: %S" kind))
          (unless (and (stringp text) (not (string-blank-p text)))
            (error "Compose text cannot be empty"))
          (when (memq kind '(reply quote))
            (unless (and target-id
                         (string-match-p "\\`[0-9]+\\'"
                                         (format "%s" target-id)))
              (error "%s requires a numeric target tweet ID"
                     (capitalize (symbol-name kind)))))
          (unless (and (listp attachments) (<= (length attachments) 4))
            (error "Compose attachments must contain at most four files"))
          (dolist (file attachments)
            (unless (and (stringp file) (file-regular-p file)
                         (file-readable-p file))
              (error "Image file is not readable: %s" file)))
          (chirp-backend--upload-compose-media
           attachments
           (lambda (media-ids)
             (let* ((note-tweet-p
                     (> (chirp-backend-tweet-weighted-length text)
                        chirp-backend--standard-tweet-weight-limit))
                    (operation-key
                     (if note-tweet-p 'create-note-tweet 'create-tweet))
                    (variables
                     (chirp-backend--compose-variables
                      kind text (and target-id (format "%s" target-id))
                      media-ids note-tweet-p target-handle)))
               (chirp-x-graphql-request
                (chirp-backend--operation operation-key)
                variables
                (lambda (payload)
                  (if-let* ((tweet-id
                             (chirp-backend--created-tweet-id payload)))
                      (funcall callback (list :id tweet-id) payload)
                    (funcall error-fn (chirp-x-unknown-write-outcome
                                       "X did not return a created tweet ID"))))
                :errback error-fn)))
           error-fn))
      (error
       (funcall error-fn (error-message-string err))
       nil))))

(defun chirp-backend--mutation-request (args)
  "Return a direct X mutation request for action ARGS, or nil."
  (pcase (car args)
    ("like"
     (list :operation 'favorite
           :variables `(("tweet_id" . ,(cadr args)))))
    ("unlike"
     (list :operation 'unfavorite
           :variables `(("tweet_id" . ,(cadr args))
                        ("dark_request" . :json-false))))
    ("retweet"
     (list :operation 'retweet
           :variables `(("tweet_id" . ,(cadr args))
                        ("dark_request" . :json-false))))
    ("unretweet"
     (list :operation 'unretweet
           :variables `(("source_tweet_id" . ,(cadr args))
                        ("dark_request" . :json-false))))
    ("bookmark"
     (list :operation 'bookmark
           :variables `(("tweet_id" . ,(cadr args)))))
    ("unbookmark"
     (list :operation 'unbookmark
           :variables `(("tweet_id" . ,(cadr args)))))
    ("delete"
     (list :operation 'delete-tweet
           :variables `(("tweet_id" . ,(cadr args))
                        ("dark_request" . :json-false))))))

(defun chirp-backend--relationship-request
    (command handle callback &optional errback)
  "Run follow relationship COMMAND for HANDLE and call CALLBACK.
ERRBACK receives request failures."
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (if (not (and (stringp handle) (not (string-empty-p handle))))
        (funcall error-fn "Follow action requires a user handle")
      (chirp-backend-user
       handle
       (lambda (user _envelope)
         (if-let* ((user-id (plist-get user :id))
                   ((string-match-p "\\`[0-9]+\\'" user-id)))
             (chirp-x-api-request
              'web
              (format "1.1/friendships/%s.json"
                      (if (equal command "follow") "create" "destroy"))
              (lambda (payload)
                (funcall callback payload nil))
              :method 'post
              :form `(("user_id" . ,user-id)
                      ("include_profile_interstitial_type" . "1"))
              :errback error-fn)
           (funcall error-fn "X profile did not include a numeric user ID")))
       error-fn))))

(defun chirp-backend-request (args callback &optional errback)
  "Run direct backend action ARGS and call CALLBACK.

ERRBACK receives a single human-readable string."
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (pcase (car args)
      ((or "follow" "unfollow")
       (chirp-backend--relationship-request
        (car args) (cadr args) callback error-fn))
      (_
       (if-let* ((request (chirp-backend--mutation-request args)))
           (chirp-x-graphql-request
            (chirp-backend--operation (plist-get request :operation))
            (plist-get request :variables)
            (lambda (payload)
              (funcall callback payload nil))
            :errback error-fn)
         (funcall error-fn
                  (format "Unknown Chirp backend action: %s" (car args))))))))

(defun chirp-backend-envelope-next-cursor (envelope)
  "Return the next pagination cursor from ENVELOPE, or nil."
  (or (chirp-get-in envelope '("pagination" "nextCursor"))
      (chirp-get envelope "nextCursor")))

(defun chirp-backend--timeline-limit (max-results)
  "Return a valid positive timeline limit from MAX-RESULTS."
  (let ((limit (or max-results chirp-default-max-results)))
    (unless (and (integerp limit) (> limit 0))
      (error "Timeline result limit must be a positive integer: %S" limit))
    limit))

(defun chirp-backend--timeline-tweet (result promoted-p &optional related-p)
  "Return the raw tweet in RESULT, tagged for PROMOTED-P or RELATED-P."
  (let ((tweet (or (chirp-get result "tweet") result)))
    (when (and tweet related-p) (push '("timelineContext" . "related") tweet))
    (when (and tweet promoted-p) (push '("isPromoted" . t) tweet))
    tweet))

(defun chirp-backend--timeline-entry-tweets (entry)
  "Return raw tweets carried directly or inside module ENTRY."
  (let* ((content (chirp-get entry "content"))
         (item-content (or (chirp-get content "itemContent")
                           (chirp-get-in entry '("item" "itemContent"))))
         (entry-id (chirp-get entry "entryId"))
         (related (string-prefix-p "tweetdetailrelatedtweets-" (or entry-id "")))
         (promoted-p (or (chirp-get item-content "promotedMetadata")
                         (and (stringp entry-id)
                              (string-prefix-p "promoted-" entry-id))))
         tweets)
    (when-let* ((result (chirp-get-in item-content
                                      '("tweet_results" "result"))))
      (push (chirp-backend--timeline-tweet result promoted-p related) tweets))
    (dolist (nested (chirp-get content "items"))
      (when-let* ((nested-content (chirp-get-in nested
                                                '("item" "itemContent")))
                  (result (chirp-get-in nested-content
                                        '("tweet_results" "result"))))
        (push (chirp-backend--timeline-tweet
               result
               (or (chirp-get nested-content "promotedMetadata")
                   (let ((nested-id (chirp-get nested "entryId")))
                     (and (stringp nested-id)
                          (string-prefix-p "promoted-" nested-id))))
               related)
              tweets)))
    (nreverse tweets)))

(defun chirp-backend--timeline-entries (instructions)
  "Return entries from add, replace and module updates in INSTRUCTIONS."
  (cl-loop for instruction in instructions
           append (or (chirp-get instruction "entries")
                      (when-let* ((items (chirp-get instruction "moduleItems")))
                        (list (list (cons "entryId" (chirp-get instruction "moduleEntryId"))
                                    (list "content" (cons "items" items)))))
                      (when-let* ((entry (chirp-get instruction "entry")))
                        (list entry)))))

(defun chirp-backend--timeline-next-cursor (entries)
  "Return the bottom cursor or a continuation for collapsed replies in ENTRIES."
  (let (more)
    (catch 'bottom
      (dolist (entry entries)
        (let* ((content (chirp-get entry "content"))
               (items (cons content
                            (mapcar (lambda (node) (chirp-get-in node '("item" "itemContent")))
                                    (chirp-get content "items")))))
          (dolist (item items)
            (pcase (chirp-get item "cursorType")
              ("Bottom" (throw 'bottom (chirp-get item "value")))
              ((or "ShowMore" "ShowMoreThreads")
               (unless more (setq more (chirp-get item "value"))))))))
      more)))

(defun chirp-backend--timeline-page (payload paths label)
  "Return normalized tweets paired with the bottom cursor from PAYLOAD.
PATHS locates the timeline.  LABEL names it in errors."
  (let* ((timeline
          (cl-loop for path in paths
                   for value = (chirp-get-in payload path)
                   when (and value (chirp-object-p value))
                   return value))
         (instructions (and timeline (chirp-get timeline "instructions"))))
    (unless (and (assoc-string "instructions" timeline) (listp instructions))
      (error "X did not return %s" label))
    (let ((entries (chirp-backend--timeline-entries instructions)))
      (cons (chirp-collect-top-level-tweets
             (cl-mapcan #'chirp-backend--timeline-entry-tweets entries))
            (chirp-backend--timeline-next-cursor entries)))))

(cl-defun chirp-backend--request-timeline
    (operation-key variables paths limit callback &key errback label focus-id)
  "Request OPERATION-KEY and adapt its timeline at PATHS.
VARIABLES specify the request.  LIMIT caps the tweets passed to CALLBACK.
The continuation holds any unconsumed tweets and the remote bottom cursor.
ERRBACK receives errors.  LABEL names the timeline in errors.  When supplied,
FOCUS-ID identifies the post that must appear first on this page."
  (let* ((cursor (cdr (assoc "cursor" variables)))
         (error-fn (or errback #'message)))
    (cl-flet ((deliver (tweets remote)
                (let* ((rest (nthcdr limit tweets))
                       (next (if rest (list :remaining rest :remote remote) remote)))
                  (funcall callback (if rest (seq-take tweets limit) tweets)
                           (and next `(("pagination" . (("nextCursor" . ,next)))))))))
      (if (consp cursor)
          (deliver (plist-get cursor :remaining) (plist-get cursor :remote))
        (let ((operation (chirp-backend--operation operation-key)))
          (chirp-x-graphql-request
           operation variables
           (lambda (payload)
             (condition-case err
                 (pcase-let ((`(,tweets . ,next)
                              (chirp-backend--timeline-page
                               payload paths (or label (plist-get operation :name)))))
                   (when focus-id
                     (if-let* ((focus (seq-find (lambda (tweet)
                                                  (chirp-backend--tweet-matches-id-p tweet focus-id))
                                                tweets)))
                         (setq tweets (cons focus (delq focus tweets)))
                       (error "X did not return tweet %s in its thread" focus-id)))
                   (deliver tweets (unless (equal next cursor) next)))
               (error (funcall error-fn (error-message-string err)))))
           :errback error-fn))))))

(defun chirp-backend-feed
    (callback &optional following errback max-results cursor)
  "Fetch a Home or Following timeline through X's web GraphQL API.

CALLBACK receives normalized tweets and a pagination envelope.  FOLLOWING
selects the chronological Following timeline.  ERRBACK handles transport or
response failures, MAX-RESULTS limits the response, CURSOR continues
pagination."
  (let ((limit (chirp-backend--timeline-limit max-results)))
    (chirp-backend--request-timeline
     (if following 'following 'home)
     (append `(("count" . ,limit)
               ("includePromotedContent" . :json-false)
               ("latestControlAvailable" . t)
               ("requestContext" . "launch"))
             (when cursor
               `(("cursor" . ,cursor))))
     '(("data" "home" "home_timeline_urt"))
     limit callback :errback errback :label "a home timeline")))

(defconst chirp-backend--notification-kinds
  '(("heart_icon" . "like")
    ("person_icon" . "follow")
    ("retweet_icon" . "retweet")
    ("mention_icon" . "mention")
    ("reply_icon" . "reply")
    ("quote_icon" . "quote"))
  "Map X notification icon identifiers to Chirp activity kinds.")

(defun chirp-backend--notification-tweet-id (item)
  "Return the first target tweet ID referenced by notification ITEM."
  (cl-loop for target in (chirp-get-in item '("template" "target_objects"))
           for result = (chirp-get-in target '("tweet_results" "result"))
           for tweet = (and result (chirp-normalize-tweet result))
           when tweet return (plist-get tweet :id)))

(defun chirp-backend--normalize-notification (entry)
  "Normalize one X notification timeline ENTRY, or return nil."
  (when-let* ((item (chirp-get-in entry '("content" "itemContent")))
              (id (chirp-first-nonblank
                   (chirp-get item "id")
                   (chirp-get entry "entryId"))))
    (let* ((icon (chirp-get item "notification_icon"))
           (kind (or (cdr (assoc-string icon
                                        chirp-backend--notification-kinds))
                     "unknown"))
           (message (chirp-first-nonblank
                     (chirp-get-in item '("rich_message" "text"))))
           (tweet-id (chirp-backend--notification-tweet-id item))
           (timestamp (chirp-get item "timestamp_ms")))
      (append `(("id" . ,id)
                ("type" . ,kind))
              (when message `(("message" . ,message)))
              (when timestamp `(("timestampMs" . ,timestamp)))
              (when tweet-id `(("tweetId" . ,tweet-id)))))))

(defun chirp-backend-notifications (callback &optional errback max-results cursor)
  "Fetch account activity notifications and call CALLBACK.
ERRBACK handles failures, MAX-RESULTS limits the page, and CURSOR continues it."
  (let ((limit (chirp-backend--timeline-limit max-results))
        (error-fn (or errback (lambda (message) (message "%s" message)))))
    (chirp-x-graphql-request
     (chirp-backend--operation 'notifications)
     `(("timeline_type" . "All")
       ("count" . ,limit)
       ,@(when cursor `(("cursor" . ,cursor))))
     (lambda (payload)
       (if-let* ((timeline
                  (chirp-get-in
                   payload
                   '("data" "viewer_v2" "user_results" "result"
                     "notification_timeline" "timeline")))
                 (instructions (assoc-string "instructions" timeline)))
           (let* ((entries (chirp-backend--timeline-entries (cdr instructions)))
                  (notifications
                   (delq nil (mapcar #'chirp-backend--normalize-notification
                                     entries)))
                  (next-cursor (chirp-backend--timeline-next-cursor entries)))
             (when (> (length notifications) limit)
               (setq notifications (cl-subseq notifications 0 limit)))
             (funcall callback
                      notifications
                      (and next-cursor
                           `(("pagination" .
                              (("nextCursor" . ,next-cursor)))))))
         (funcall error-fn "X did not return a notification timeline")))
     :errback error-fn)))

(defun chirp-backend-bookmarks (callback &optional errback max-results cursor)
  "Fetch bookmarks for CALLBACK, reporting failures to ERRBACK.
MAX-RESULTS bounds the page size and CURSOR continues a previous page."
  (let ((limit (chirp-backend--timeline-limit max-results)))
    (chirp-backend--request-timeline
     'bookmarks
     `(,@(when cursor (list (cons "cursor" cursor))) ("count" . ,limit)
       ("includePromotedContent" . :json-false)
       ("latestControlAvailable" . t)
       ("requestContext" . "launch"))
     '(("data" "bookmark_timeline" "timeline")
       ("data" "bookmark_timeline_v2" "timeline"))
     limit callback :errback errback :label "the bookmarks timeline")))

(defun chirp-backend-search
    (query callback &optional errback max-results cursor product)
  "Search for QUERY and call CALLBACK, or ERRBACK on failure.
MAX-RESULTS sets the page size, CURSOR selects the next page, and
PRODUCT selects X search ordering (defaults to \"Top\")."
  (let ((limit (chirp-backend--timeline-limit max-results)))
    (chirp-backend--request-timeline
     'search
     `(,@(when cursor (list (cons "cursor" cursor))) ("count" . ,limit)
       ("rawQuery" . ,query)
       ("querySource" . "typed_query")
       ("product" . ,(or product "Top")))
     '(("data" "search_by_raw_query" "search_timeline" "timeline"))
     limit callback :errback errback :label "the search timeline")))

(defun chirp-backend-search-users
    (query callback &optional errback max-results)
  "Search users matching QUERY and call CALLBACK with up to MAX-RESULTS users.
ERRBACK receives request failures."
  (let* ((clean-query (string-trim (string-remove-prefix "@" query)))
         (limit (or max-results 10))
         (error-fn (or errback (lambda (message) (message "%s" message)))))
    (cond
     ((string-empty-p clean-query)
      (funcall callback nil nil))
     ((not (and (integerp limit) (> limit 0)))
      (funcall error-fn "User search limit must be a positive integer"))
     (t
      (chirp-x-api-request
       'web "1.1/search/typeahead.json"
       (lambda (payload)
         (let ((seen (make-hash-table :test #'equal))
               users)
           (dolist (user (chirp-backend--collect-users
                          (chirp-get payload "users")))
             (let ((key (or (plist-get user :id)
                            (plist-get user :handle))))
               (when (and key (not (gethash key seen)))
                 (puthash key t seen)
                 (push user users))))
           (setq users (nreverse users))
           (when (> (length users) limit)
             (setq users (cl-subseq users 0 limit)))
           (funcall callback users nil)))
       :query `(("q" . ,clean-query)
                ("src" . "search_box")
                ("result_type" . "users")
                ("count" . ,limit))
       :errback error-fn)))))

(defun chirp-backend-translate (tweet-id language callback &optional errback)
  "Translate TWEET-ID into LANGUAGE and call CALLBACK, or ERRBACK on failure."
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (cond
     ((not (and (stringp tweet-id)
                (string-match-p "\\`[0-9]+\\'" tweet-id)))
      (funcall error-fn "Tweet translation requires a numeric tweet ID"))
     ((not (and (stringp language)
                (string-match-p
                 "\\`[[:alpha:]]\\{2,3\\}\\(?:-[[:alnum:]]\\{2,8\\}\\)*\\'"
                 language)))
      (funcall error-fn "Tweet translation requires an ISO language code"))
     (t
      (chirp-x-api-request
       'web
       (format
        (concat
         "1.1/strato/column/None/tweetId=%s,destinationLanguage=Some(%s),"
         "translationSource=Some(Google),feature=None,timeout=None,"
         "onlyCached=None/translation/service/translateTweet")
        tweet-id language)
       (lambda (payload)
         (if-let* ((translation
                    (chirp-first-nonblank
                     (chirp-get payload "translation"))))
             (funcall
              callback
              (append
               `(("id" . ,(or (chirp-first-nonblank
                               (chirp-get payload "id_str" "id"))
                              tweet-id))
                 ("translation" . ,(chirp-clean-text translation)))
               (cl-loop for key in '("sourceLanguage"
                                     "localizedSourceLanguage"
                                     "destinationLanguage"
                                     "translationSource"
                                     "translationState")
                        for value = (chirp-get payload key)
                        when value collect (cons key value)))
              nil)
           (let ((state (chirp-get payload "translationState")))
             (funcall error-fn
                      (if state
                          (format "X did not return a translation (%s)" state)
                        "X did not return a translation")))))
       :errback error-fn)))))

(defun chirp-backend--viewer-request (callback errback &optional credentials)
  "Fetch the authenticated Viewer profile and pass it to CALLBACK or ERRBACK.

Optional CREDENTIALS overrides `chirp-x-credentials'."
  (chirp-x-graphql-request
   (chirp-backend--operation 'viewer) nil
   (lambda (payload)
     (let ((user (chirp-normalize-user
                  (chirp-get-in
                   payload '("data" "viewer" "user_results" "result")))))
       (if user
           (funcall callback user nil)
         (funcall (or errback #'message)
                  "X did not return the authenticated profile"))))
   :errback errback
   :credentials credentials))

(defun chirp-backend-verify-credentials (credentials callback &optional errback)
  "Verify CREDENTIALS against X's Viewer endpoint.

CALLBACK receives the normalized user plist and nil envelope; ERRBACK receives
a human-readable error message."
  (chirp-backend--viewer-request callback errback credentials))

(defun chirp-backend-whoami (callback &optional errback)
  "Fetch the authenticated user profile and call CALLBACK, or ERRBACK on failure."
  (chirp-backend--cached-read
   '(:whoami)
   (lambda (success error)
     (chirp-backend--viewer-request success error))
   callback
   errback))

(defun chirp-backend-likes (handle callback &optional errback max-results cursor)
  "Fetch liked tweets for HANDLE and call CALLBACK, or ERRBACK on failure.
MAX-RESULTS sets the page size and CURSOR selects the next page."
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (chirp-backend-user
     handle
     (lambda (user _envelope)
       (if-let* ((user-id (plist-get user :id)))
           (let ((limit (chirp-backend--timeline-limit max-results)))
             (chirp-backend--request-timeline
              'likes
              `(,@(when cursor (list (cons "cursor" cursor))) ("userId" . ,user-id)
                ("count" . ,limit)
                ("includePromotedContent" . :json-false)
                ("withClientEventToken" . :json-false)
                ("withBirdwatchNotes" . :json-false)
                ("withVoice" . t))
              chirp-backend--user-timeline-paths limit
              (lambda (tweets envelope)
                (dolist (tweet tweets)
                  (plist-put tweet :liked-p t))
                (funcall callback tweets envelope))
              :errback error-fn :label "the likes timeline"))
         (funcall error-fn "X profile did not include a user ID")))
     error-fn)))

(defun chirp-backend--normalize-list-info (item source)
  "Normalize X list ITEM tagged with SOURCE, or return nil."
  (when-let* ((id (chirp-first-nonblank
                   (chirp-get item "id_str" "id"))))
    (let ((owner (chirp-get item "user")))
      `(("id" . ,id)
        ("name" . ,(or (chirp-get item "name") id))
        ("slug" . ,(or (chirp-get item "slug") ""))
        ("description" . ,(or (chirp-get item "description") ""))
        ("mode" . ,(or (chirp-get item "mode") ""))
        ("memberCount" . ,(or (chirp-get item "member_count") 0))
        ("subscriberCount" . ,(or (chirp-get item "subscriber_count") 0))
        ("uri" . ,(or (chirp-get item "uri") ""))
        ("fullName" . ,(or (chirp-get item "full_name") ""))
        ("owner" . (("name" . ,(or (chirp-get owner "name") ""))
                    ("screenName" . ,(or (chirp-get owner "screen_name") ""))
                    ("profileImageUrl" .
                     ,(or (chirp-get owner "profile_image_url_https"
                                     "profile_image_url")
                          ""))))
        ("following" . ,(chirp-boolean-value
                         (chirp-get item "following")))
        ("sources" . (,source))))))

(defun chirp-backend--request-list-collection
    (endpoint source user-id callback errback
              &optional cursor page accumulated)
  "Fetch list collection ENDPOINT tagged SOURCE for USER-ID.

CALLBACK receives all normalized pages.  ERRBACK receives request failures.
CURSOR, PAGE, and ACCUMULATED carry private pagination state."
  (let ((cursor (or cursor "-1"))
        (page (or page 0)))
    (chirp-x-api-request
     'web (format "1.1/lists/%s.json" endpoint)
     (lambda (payload)
       (let* ((items
               (delq nil
                     (mapcar (lambda (item)
                               (chirp-backend--normalize-list-info item source))
                             (chirp-get payload "lists"))))
              (all-items (append accumulated items))
              (next (chirp-get payload "next_cursor_str" "next_cursor")))
         (if (and next
                  (not (equal (format "%s" next) "0"))
                  (not (equal (format "%s" next) cursor))
                  (< page 19))
             (chirp-backend--request-list-collection
              endpoint source user-id callback errback
              (format "%s" next) (1+ page) all-items)
           (funcall callback all-items))))
     :query `(("user_id" . ,user-id)
              ("count" . 1000)
              ("cursor" . ,cursor))
     :errback errback)))

(defun chirp-backend--merge-list-collections (owned subscribed member)
  "Merge OWNED, SUBSCRIBED, and MEMBER list metadata without duplicate IDs."
  (let ((by-id (make-hash-table :test #'equal))
        result)
    (dolist (item (append owned subscribed member))
      (let* ((id (chirp-get item "id"))
             (existing (and id (gethash id by-id))))
        (if existing
            (setcdr (assoc-string "sources" existing t)
                    (delete-dups
                     (append (chirp-get existing "sources")
                             (chirp-get item "sources"))))
          (when id
            (puthash id item by-id)
            (push item result)))))
    (nreverse result)))

(defun chirp-backend-lists (callback &optional errback)
  "Fetch accessible list metadata and call CALLBACK, or ERRBACK on failure."
  (chirp-backend--cached-read
   chirp-backend--lists-cache-key
   (lambda (success error)
     (chirp-backend-whoami
      (lambda (user _envelope)
        (if-let* ((user-id (plist-get user :id)))
            (let ((remaining 3)
                  owned subscribed member failures)
              (cl-labels
                  ((complete (source items)
                     (pcase source
                       ('owned (setq owned items))
                       ('subscribed (setq subscribed items))
                       ('member (setq member items)))
                     (when (zerop (cl-decf remaining))
                       (if (= (length failures) 3)
                           (funcall error (car failures))
                         (when failures (message "Some X lists could not be loaded: %s" (car failures)))
                         (funcall success
                                  (chirp-backend--merge-list-collections owned subscribed member)
                                  nil))))
                   (fail (reason)
                     (push reason failures)
                     (complete nil nil)))
                (chirp-backend--request-list-collection
                 "ownerships" "owned" user-id
                 (lambda (items) (complete 'owned items)) #'fail)
                (chirp-backend--request-list-collection
                 "subscriptions" "subscribed" user-id
                 (lambda (items) (complete 'subscribed items)) #'fail)
                (chirp-backend--request-list-collection
                 "memberships" "member" user-id
                 (lambda (items) (complete 'member items)) #'fail)))
          (funcall error "X profile did not include a user ID")))
      error))
   callback
   errback))

(defun chirp-backend-list (list-target callback &optional errback max-results cursor)
  "Fetch LIST-TARGET timeline data and call CALLBACK, or ERRBACK on failure.
MAX-RESULTS sets the page size and CURSOR selects the next page."
  (let ((limit (chirp-backend--timeline-limit max-results)))
    (chirp-backend--request-timeline
     'list
     `(,@(when cursor (list (cons "cursor" cursor))) ("listId" . ,(chirp-backend-list-id list-target))
       ("count" . ,limit))
     '(("data" "list" "tweets_timeline" "timeline"))
     limit callback :errback errback :label "the list timeline")))

(defun chirp-backend--tweet-matches-id-p (tweet tweet-id)
  "Return non-nil when TWEET or its raw wrapper identifies TWEET-ID."
  (or (equal (plist-get tweet :id) tweet-id)
      (equal (plist-get tweet :wrapper-id) tweet-id)))

(defun chirp-backend-thread (tweet-or-url callback &optional errback cursor)
  "Fetch TWEET-OR-URL for CALLBACK, reporting failures to ERRBACK.
CURSOR continues a previously fetched thread."
  (let ((tweet-id (chirp-backend--tweet-id-from-target tweet-or-url))
        (limit (chirp-backend--timeline-limit chirp-thread-max-results)))
    (funcall (if cursor
                 (lambda (_key fetch ok fail) (funcall fetch ok fail))
               #'chirp-backend--cached-read)
             (chirp-backend--thread-cache-key tweet-or-url)
             (lambda (success error)
               (if (not tweet-id)
                   (funcall error "Tweet target does not contain a numeric ID")
                 (chirp-backend--request-timeline
                  'thread
                  `(,@(when cursor (list (cons "cursor" cursor))) ("focalTweetId" . ,tweet-id)
                    ("count" . ,limit)
                    ("referrer" . "tweet")
                    ("with_rux_injections" . :json-false)
                    ("includePromotedContent" . t)
                    ("rankingMode" . "Relevance")
                    ("withCommunity" . :json-false)
                    ("withQuickPromoteEligibilityTweetFields" . :json-false)
                    ("withBirdwatchNotes" . :json-false)
                    ("withVoice" . :json-false))
                  '(("data" "tweetResult" "result" "timeline")
                    ("data" "threaded_conversation_with_injections_v2"))
                  limit success :focus-id (unless cursor tweet-id)
                  :errback error :label "the tweet conversation")))
             callback
             errback)))

(defun chirp-backend-tweet (tweet-id callback &optional errback)
  "Fetch TWEET-ID and call CALLBACK, or ERRBACK on failure."
  (chirp-backend-thread
   tweet-id
   (lambda (tweets envelope)
     (if-let* ((tweet (or (cl-find-if
                           (lambda (item)
                             (chirp-backend--tweet-matches-id-p item tweet-id))
                           tweets)
                          (car tweets))))
         (funcall callback tweet envelope)
       (funcall (or errback #'ignore)
                "X returned tweet detail Chirp could not parse.")))
   errback))

(defun chirp-backend-article (tweet-id callback &optional errback)
  "Fetch article content for TWEET-ID and call CALLBACK, or ERRBACK on failure."
  (chirp-backend--cached-read
   (chirp-backend--article-cache-key tweet-id)
   (lambda (success error)
     (chirp-x-graphql-request
      (chirp-backend--operation 'article)
      `(("tweetId" . ,tweet-id)
        ("withCommunity" . :json-false)
        ("includePromotedContent" . :json-false)
        ("withVoice" . :json-false))
      (lambda (payload)
        (let ((tweet (chirp-normalize-tweet
                      (chirp-get-in
                       payload '("data" "tweetResult" "result")))))
          (if (and tweet
                   (or (plist-get tweet :article-title)
                       (plist-get tweet :article-text)))
              (funcall success tweet nil)
            (funcall error "X did not return article content"))))
      :errback error))
   callback
   errback))

(defun chirp-backend-user (handle callback &optional errback)
  "Fetch profile data for HANDLE and call CALLBACK, or ERRBACK on failure."
  (let ((clean-handle (string-remove-prefix "@" handle)))
    (chirp-backend--cached-read
     (chirp-backend--user-cache-key clean-handle)
     (lambda (success error)
       (chirp-x-graphql-request
        (chirp-backend--operation 'user)
        `(("screen_name" . ,clean-handle)
          ("withSafetyModeUserFields" . t))
        (lambda (payload)
          (let ((user (chirp-normalize-user
                       (chirp-get-in payload '("data" "user" "result")))))
            (if user
                (funcall success user nil)
              (funcall error (format "X did not return profile @%s"
                                     clean-handle)))))
        :errback error))
     callback
     errback)))

(defun chirp-backend--profile-timeline-variables
    (operation-key user-id limit cursor)
  "Return profile OPERATION-KEY variables for USER-ID, LIMIT, and CURSOR."
  (append
   (pcase operation-key
     ('user-tweets
      `(("userId" . ,user-id)
        ("count" . ,limit)
        ("includePromotedContent" . :json-false)
        ("latestControlAvailable" . t)
        ("requestContext" . "launch")
        ("withQuickPromoteEligibilityTweetFields" . t)
        ("withVoice" . t)))
     ('user-highlights
      `(("userId" . ,user-id)
        ("count" . ,limit)
        ("includePromotedContent" . t)
        ("withVoice" . t)))
     ('user-media
      `(("userId" . ,user-id)
        ("count" . ,limit)
        ("includePromotedContent" . :json-false)
        ("withClientEventToken" . :json-false)
        ("withBirdwatchNotes" . :json-false)
        ("withVoice" . t)))
     (_ (error "Unknown profile timeline operation: %S" operation-key)))
   (when cursor
     `(("cursor" . ,cursor)))))

(defun chirp-backend--profile-timeline
    (timeline-kind cache-key handle callback &optional errback max-results cursor)
  "Fetch profile TIMELINE-KIND using CACHE-KEY for HANDLE.

CALLBACK receives tweets and an envelope.  ERRBACK handles failures,
MAX-RESULTS limits the response, and CURSOR bypasses the initial-page cache."
  (let* ((clean-handle (string-remove-prefix "@" handle))
         (limit (chirp-backend--timeline-limit
                 (or max-results chirp-profile-post-limit)))
         (fetch-page
          (lambda (success error)
            (if (eq timeline-kind 'user-replies)
                (chirp-backend--request-timeline
                 'search
                 (append `(("count" . ,limit)
                           ("rawQuery" . ,(format "from:%s filter:replies"
                                                  clean-handle))
                           ("querySource" . "typed_query")
                           ("product" . "Latest"))
                         (when cursor `(("cursor" . ,cursor))))
                 '(("data" "search_by_raw_query"
                    "search_timeline" "timeline"))
                 limit success :errback error :label "the replies timeline")
              (chirp-backend-user
               clean-handle
               (lambda (user _envelope)
                 (if-let* ((user-id (plist-get user :id)))
                     (chirp-backend--request-timeline
                      timeline-kind
                      (chirp-backend--profile-timeline-variables
                       timeline-kind user-id limit cursor)
                      chirp-backend--user-timeline-paths limit success
                      :errback error :label "the profile timeline")
                   (funcall error "X profile did not include a user ID")))
               error)))))
    (if cursor
        (funcall fetch-page callback
                 (or errback (lambda (message) (message "%s" message))))
      (chirp-backend--cached-read cache-key fetch-page callback errback))))

(defun chirp-backend-user-posts (handle callback &optional errback max-results cursor)
  "Fetch posts for HANDLE and call CALLBACK.

ERRBACK handles failures and MAX-RESULTS limits the response.  When CURSOR is
non-nil, continue from it without using the short-lived read cache."
  (chirp-backend--profile-timeline
   'user-tweets (chirp-backend--user-posts-cache-key handle)
   handle callback errback max-results cursor))

(defun chirp-backend-user-replies (handle callback &optional errback max-results cursor)
  "Fetch replies by HANDLE and call CALLBACK.

ERRBACK handles failures, MAX-RESULTS limits the response, and CURSOR continues
pagination."
  (chirp-backend--profile-timeline
   'user-replies (chirp-backend--profile-timeline-cache-key handle 'replies)
   handle callback errback max-results cursor))

(defun chirp-backend-user-highlights (handle callback &optional errback max-results cursor)
  "Fetch highlights for HANDLE and call CALLBACK.

ERRBACK handles failures, MAX-RESULTS limits the response, and CURSOR continues
pagination."
  (chirp-backend--profile-timeline
   'user-highlights
   (chirp-backend--profile-timeline-cache-key handle 'highlights)
   handle callback errback max-results cursor))

(defun chirp-backend-user-media (handle callback &optional errback max-results cursor)
  "Fetch media posts for HANDLE and call CALLBACK.

ERRBACK handles failures, MAX-RESULTS limits the response, and CURSOR continues
pagination."
  (chirp-backend--profile-timeline
   'user-media (chirp-backend--profile-timeline-cache-key handle 'media)
   handle callback errback max-results cursor))

(defun chirp-backend--collect-users (data)
  "Normalize DATA into a list of user plists."
  (if (listp data)
      (delq nil (mapcar #'chirp-normalize-user data))
    nil))

(defun chirp-backend--user-collection
    (kind cache-key handle callback &optional errback cursor)
  "Fetch user collection KIND through CACHE-KEY for HANDLE.

CALLBACK receives normalized users and an optional pagination envelope.
ERRBACK receives failures.  CURSOR selects the next page."
  (chirp-backend--cached-read
   (if cursor (cons cursor cache-key) cache-key)
   (lambda (success error)
     (chirp-backend-user
      handle
      (lambda (user _envelope)
        (if-let* ((user-id (plist-get user :id)))
            (chirp-x-api-request
             'web
             (format "1.1/%s/list.json"
                     (pcase kind
                       ('followers "followers")
                       ('following "friends")
                       (_ (error "Unknown user collection: %S" kind))))
             (lambda (payload)
               (let* ((users (chirp-backend--collect-users
                              (chirp-get payload "users")))
                      (cursor (chirp-get payload
                                         "next_cursor_str" "next_cursor")))
                 (funcall
                  success users
                  (and cursor (not (equal (format "%s" cursor) "0"))
                       `(("pagination" . (("nextCursor" . ,cursor))))))))
             :query `(("user_id" . ,user-id)
                      ("count" . ,chirp-default-max-results)
                      ("cursor" . ,(or cursor "-1"))
                      ("skip_status" . "true")
                      ("include_user_entities" . "false"))
             :errback error)
          (funcall error "X profile did not include a user ID")))
      error))
   callback
   errback))

(defun chirp-backend-followers (handle callback &optional errback cursor)
  "Fetch followers for HANDLE at CURSOR, calling CALLBACK or ERRBACK."
  (chirp-backend--user-collection
   'followers (chirp-backend--followers-cache-key handle)
   handle callback errback cursor))

(defun chirp-backend-following-users (handle callback &optional errback cursor)
  "Fetch accounts followed by HANDLE at CURSOR, calling CALLBACK or ERRBACK."
  (chirp-backend--user-collection
   'following (chirp-backend--following-users-cache-key handle)
   handle callback errback cursor))

(provide 'chirp-backend)

;;; chirp-backend.el ends here
