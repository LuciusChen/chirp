;;; chirp-x.el --- X web API transport for Chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Authenticate and issue persisted GraphQL and allowlisted REST requests to
;; X's web API.  This module owns credentials, HTTP details, and remote error
;; decoding; callers own operation selection and response adaptation.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'auth-source)
(require 'chirp-core)
(require 'chirp-web)

(defgroup chirp-x nil
  "X web API transport for Chirp."
  :group 'chirp)

(defconst chirp-x--api-base-url "https://x.com/i/api/graphql"
  "Trusted X web GraphQL endpoint that receives session credentials.")

(defconst chirp-x--rest-base-urls
  '((web . "https://x.com/i/api/")
    (upload . "https://upload.x.com/i/media/"))
  "Trusted X web API roots that may receive session credentials.")

(defconst chirp-x--upload-chunk-size (* 1024 1024)
  "Number of bytes in one chunked media APPEND request.")

(defconst chirp-x--upload-image-limit (* 5 1024 1024)
  "Maximum accepted JPEG, PNG, or WebP upload size in bytes.")

(defconst chirp-x--upload-gif-limit (* 15 1024 1024)
  "Maximum accepted GIF upload size in bytes.")

(defconst chirp-x--upload-status-limit 20
  "Maximum number of media processing status checks.")

(defconst chirp-x--read-response-limit (* 64 1024 1024)
  "Maximum accepted response body size for one authenticated X read.")

(defconst chirp-x--write-response-limit (* 4 1024 1024)
  "Maximum accepted response body size for one authenticated X write.")

(defcustom chirp-x-bearer-token
  (concat "AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs"
          "%3D1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA")
  "Public X web bearer token used for authenticated web requests.

Set `CHIRP_X_BEARER_TOKEN' to override this value when X rotates its web
client token.  This is not an account credential."
  :type 'string
  :group 'chirp-x)

(defun chirp-x--nonblank-string (value)
  "Return VALUE when it is a nonblank string, otherwise nil."
  (and (stringp value)
       (not (string-blank-p value))
       value))

(defun chirp-x--environment-value (name)
  "Return nonblank environment variable NAME, or nil."
  (chirp-x--nonblank-string (getenv name)))

(defun chirp-x--session-cookie-values-valid-p (auth-token ct0)
  "Return non-nil when AUTH-TOKEN and CT0 are safe X cookie values."
  (and (chirp-x--nonblank-string auth-token)
       (chirp-x--nonblank-string ct0)
       (not (string-match-p "[;\r\n]" auth-token))
       (not (string-match-p "[;\r\n]" ct0))))


(defun chirp-x-credentials ()
  "Return configured X credentials, or nil for WebKit sign-in."
  (let ((terminal (not (display-graphic-p))))
    (when-let* ((auth (or (chirp-x--environment-value "TWITTER_AUTH_TOKEN")
                          (and terminal (auth-source-pick-first-password
                                         :host "x.com" :user "auth_token"))))
                (csrf (or (chirp-x--environment-value "TWITTER_CT0")
                          (and terminal (auth-source-pick-first-password
                                         :host "x.com" :user "ct0"))))
                ((chirp-x--session-cookie-values-valid-p auth csrf)))
      (list :auth-token auth :ct0 csrf
            :bearer-token (or (chirp-x--environment-value "CHIRP_X_BEARER_TOKEN")
                              chirp-x-bearer-token)))))

(defun chirp-x-session ()
  "Return `webkit' or configured credentials for the current display."
  (if (chirp-web-ready-p) 'webkit (chirp-x-credentials)))

(defun chirp-x--operation-method (operation)
  "Return the validated HTTP method selected by OPERATION."
  (pcase (or (plist-get operation :method) 'get)
    ('get 'get)
    ('post 'post)
    (method (error "X GraphQL method is invalid: %S" method))))

(defun chirp-x--operation-features (operation)
  "Return OPERATION's feature alist after omitting false defaults."
  (let ((features (plist-get operation :features)))
    (unless (or (null features) (listp features))
      (error "X GraphQL features are invalid: %S" features))
    (delq nil
          (mapcar (lambda (entry)
                    (unless (and (consp entry)
                                 (stringp (car entry)))
                      (error "X GraphQL feature is invalid: %S" entry))
                    (unless (memq (cdr entry) '(nil :json-false))
                      entry))
                  features))))

(defun chirp-x--operation-field-toggles (operation)
  "Return the optional field-toggle alist selected by OPERATION."
  (let ((field-toggles (plist-get operation :field-toggles)))
    (unless (or (null field-toggles) (listp field-toggles))
      (error "X GraphQL field toggles are invalid: %S" field-toggles))
    field-toggles))

(defun chirp-x--json-encode (value)
  "Encode VALUE as compact JSON."
  (let ((json-encoding-pretty-print nil))
    (json-encode value)))

(defun chirp-x--query-parameter (name value)
  "Return encoded URL query parameter NAME with JSON VALUE."
  (format "%s=%s"
          (url-hexify-string name)
          (url-hexify-string (chirp-x--json-encode value))))

(defun chirp-x--urlencode (parameters)
  "Encode string-keyed alist PARAMETERS for a query or form body."
  (unless (listp parameters)
    (error "X request parameters are invalid: %S" parameters))
  (mapconcat
   (lambda (entry)
     (unless (and (consp entry) (stringp (car entry)))
       (error "X request parameter is invalid: %S" entry))
     (format "%s=%s"
             (url-hexify-string (car entry))
             (url-hexify-string (format "%s" (or (cdr entry) "")))))
   parameters
   "&"))

(defun chirp-x--rest-url (service path query)
  "Return a trusted X REST URL for SERVICE, PATH, and QUERY parameters."
  (let ((base-url (cdr (assq service chirp-x--rest-base-urls))))
    (unless base-url
      (error "Unknown X API service: %S" service))
    (unless (and (chirp-x--nonblank-string path)
                 (not (string-prefix-p "/" path))
                 (not (string-match-p "[?#\r\n\\\\]" path))
                 (not (string-match-p
                       "\\(?:\\`\\|/\\)\\.\\.?\\(?:/\\|\\'\\)" path)))
      (error "X API path is invalid: %S" path))
    (concat base-url path
            (when query
              (concat "?" (chirp-x--urlencode query))))))

(defun chirp-x--trusted-url-p (url)
  "Return non-nil when URL belongs to an authenticated X API root."
  (or (string-prefix-p (concat chirp-x--api-base-url "/") url)
      (cl-some (lambda (entry)
                 (string-prefix-p (cdr entry) url))
               chirp-x--rest-base-urls)))

(defun chirp-x--graphql-url (query-id operation-name method variables features field-toggles)
  "Build the persisted GraphQL URL for QUERY-ID and OPERATION-NAME.

METHOD selects whether VARIABLES, FEATURES, and FIELD-TOGGLES belong in the
GET query string or in the POST body."
  (unless (string-match-p "\\`[[:alnum:]_-]+\\'" query-id)
    (error "X GraphQL query ID is invalid"))
  (unless (string-match-p "\\`[[:alnum:]_]+\\'" operation-name)
    (error "X GraphQL operation name is invalid"))
  (let ((url (format "%s/%s/%s"
                     chirp-x--api-base-url query-id operation-name)))
    (if (eq method 'get)
        (concat url "?"
                (mapconcat
                 #'identity
                 (append
                  (list (chirp-x--query-parameter "variables" variables))
                  (when features
                    (list (chirp-x--query-parameter "features" features)))
                  (when field-toggles
                    (list (chirp-x--query-parameter
                           "fieldToggles" field-toggles))))
                 "&"))
      url)))

(defun chirp-x--graphql-post-body (query-id variables features field-toggles)
  "Return a persisted GraphQL POST body for QUERY-ID and VARIABLES.

FEATURES and FIELD-TOGGLES are included when non-nil."
  (chirp-x--json-encode
   (append `(("variables" . ,variables)
             ("queryId" . ,query-id))
           (when features
             `(("features" . ,features)))
           (when field-toggles
             `(("fieldToggles" . ,field-toggles))))))

(defun chirp-x--ascii-header (name value)
  "Return an unibyte HTTP header cons from NAME and VALUE."
  (unless (and (stringp name) (stringp value)
               (not (string-match-p "[^[:ascii:]]\\|[[:cntrl:]]" name))
               (not (string-match-p "[^[:ascii:]]\\|[[:cntrl:]]" value)))
    (error "X request header is unsafe: %s" name))
  (cons (encode-coding-string name 'us-ascii)
        (encode-coding-string value 'us-ascii)))

(defun chirp-x--headers (credentials &optional content-type)
  "Return authenticated X headers from CREDENTIALS and CONTENT-TYPE."
  (mapcar
   (lambda (header)
     (chirp-x--ascii-header (car header) (cdr header)))
   (append
    `(("Authorization" . ,(concat "Bearer "
                                  (plist-get credentials :bearer-token)))
      ("Cookie" . ,(format "auth_token=%s; ct0=%s"
                           (plist-get credentials :auth-token)
                           (plist-get credentials :ct0)))
      ("X-Csrf-Token" . ,(plist-get credentials :ct0))
      ("X-Twitter-Active-User" . "yes")
      ("X-Twitter-Auth-Type" . "OAuth2Session")
      ("X-Twitter-Client-Language" . "en")
      ("Origin" . "https://x.com")
      ("Referer" . "https://x.com/")
      ("Accept" . "*/*"))
    (when content-type
      `(("Content-Type" . ,content-type))))))

(defun chirp-x--response-body (limit)
  "Return the current HTTP response body when no larger than LIMIT bytes."
  (let* ((header-end (and (boundp 'url-http-end-of-headers)
                          url-http-end-of-headers))
         (start
          (cond
           ((markerp header-end) (marker-position header-end))
           ((integerp header-end) header-end)
           (t (point-min))))
         (end (point-max))
         (bytes (- (position-bytes end) (position-bytes start))))
    (when (> bytes limit)
      (error "X response body exceeds %d bytes" limit))
    (buffer-substring-no-properties start end)))

(defun chirp-x--response-status (request-status)
  "Return HTTP status from REQUEST-STATUS in the current retrieval buffer."
  (or (and (boundp 'url-http-response-status)
           (integerp url-http-response-status)
           url-http-response-status)
      (let ((error-data (plist-get request-status :error)))
        (and (listp error-data)
             (eq (nth 1 error-data) 'http)
             (integerp (nth 2 error-data))
             (nth 2 error-data)))))

(defun chirp-x--graphql-errors (payload)
  "Return top-level or mutation-nested GraphQL errors from PAYLOAD."
  (or (chirp-get payload "errors")
      (when-let* ((data (chirp-get payload "data")))
        (cl-loop for (_key . value) in data
                 for errors = (chirp-get value "errors")
                 when (consp errors)
                 return errors))))

(defun chirp-x--graphql-error-message (payload)
  "Return a human-readable GraphQL error from PAYLOAD, or nil."
  (when-let* ((errors (chirp-x--graphql-errors payload)))
    (let* ((first-error
            (or (cl-find-if
                 (lambda (item)
                   (chirp-x--nonblank-string (chirp-get item "message")))
                 errors)
                (car errors)))
           (message
            (or (chirp-x--nonblank-string
                 (chirp-get first-error "message"))
                "GraphQL returned an error")))
      (if-let* ((code (chirp-get first-error "code")))
          (if (string-match-p
               (format "(%s)\\'" (regexp-quote (format "%s" code)))
               message)
              message
            (format "%s (%s)" message code))
        message))))

(defconst chirp-x--limit-headers
  '("retry-after" "x-rate-limit-limit" "x-rate-limit-remaining" "x-rate-limit-reset"
    "x-user-limit-24hour-limit" "x-user-limit-24hour-remaining" "x-user-limit-24hour-reset"
    "x-app-limit-24hour-limit" "x-app-limit-24hour-remaining" "x-app-limit-24hour-reset")
  "Response headers describing request and posting limits.")

(defun chirp-x--limit-details ()
  "Return the quota fields present in the current HTTP response headers."
  (when (bound-and-true-p url-http-end-of-headers)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
            (pattern (concat "^" (regexp-opt chirp-x--limit-headers t)
                             ":[ \t]*\\([^\r\n]+\\)"))
            fields)
        (while (re-search-forward pattern url-http-end-of-headers t)
          (push (format "%s=%s" (downcase (match-string 1))
                        (string-trim (match-string 2))) fields))
        (when fields (string-join (nreverse fields) ", "))))))

(defun chirp-x--failure-message (http-status payload request-status)
  "Return a readable error from HTTP-STATUS, PAYLOAD, and REQUEST-STATUS."
  (let* ((graphql-error (chirp-x--graphql-error-message payload))
         (details (and (or (eq http-status 429)
                           (seq-some (lambda (err) (memq (chirp-get err "code") '(226 344)))
                                     (chirp-x--graphql-errors payload)))
                       (chirp-x--limit-details)))
         (prefix (if details (format "X request failed [%s]" details) "X request failed")))
    (cond
     (http-status
      (format "%s (HTTP %d)%s" prefix http-status
              (if graphql-error
                  (format ": %s" graphql-error)
                "")))
     ((plist-get request-status :error)
      (format "%s: %s" prefix
              (error-message-string (plist-get request-status :error))))
     (graphql-error
      (format "%s: %s" prefix graphql-error))
     (t "X request failed without an HTTP response"))))

(defun chirp-x-unknown-write-outcome (message)
  "Mark write failure MESSAGE as having an unknown remote outcome."
  (concat
   "X write outcome is unknown; the request may have succeeded. "
   "Check X before trying again. " message))

(defun chirp-x--decode-response (request-status &optional allow-empty method web-session)
  "Decode the current X response buffer for REQUEST-STATUS.

When ALLOW-EMPTY is non-nil, accept an empty successful response as an empty
object.  METHOD identifies writes whose transport outcome can be uncertain.
WEB-SESSION identifies the browser login that sent this request.
Return either `(:success PAYLOAD)' or `(:error MESSAGE)'."
  (let* ((http-status (chirp-x--response-status request-status))
         (body (chirp-x--response-body
                (if (eq method 'post)
                    chirp-x--write-response-limit
                  chirp-x--read-response-limit)))
         (empty-p (string-empty-p (string-trim body)))
         (payload
          (unless empty-p
            (condition-case nil
                (json-parse-string body
                                   :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil)
              (error nil)))))
    (cond
     ((or (eq http-status 401)
          (and (memq http-status '(200 403))
               (seq-some (lambda (item) (memq (chirp-get item "code") '(32 89 353)))
                         (chirp-x--graphql-errors payload))))
      (when (and web-session (eq web-session chirp-web--ready))
        (setq chirp-web--ready nil))
      (list :error (concat (chirp-x--failure-message
                            (unless (eq http-status 200) http-status) payload request-status)
                           (if web-session ". Sign in with M-x chirp-login"
                             ". Update your X session credentials in the environment or auth-source"))))
     ((or (not http-status)
          (< http-status 200)
          (>= http-status 300))
      (let ((message
             (chirp-x--failure-message
              http-status payload request-status)))
        (list :error
              (if (and (eq method 'post)
                       (or (null http-status) (= http-status 408) (>= http-status 500)))
                  (chirp-x-unknown-write-outcome message)
                message))))
     ((and empty-p allow-empty)
      (list :success (make-hash-table :test #'equal)))
     ((not payload)
      (list :error
            (if (eq method 'post)
                (chirp-x-unknown-write-outcome "X returned invalid JSON")
              "X returned invalid JSON")))
     ((and (chirp-x--graphql-errors payload)
           (or (eq method 'post) (not (seq-some #'cdr-safe (chirp-get payload "data")))))
      (list :error (chirp-x--failure-message nil payload request-status)))
     (t (list :success payload)))))


(cl-defun chirp-x--request (url method callback &key data content-type errback allow-empty query-p)
  "Request URL with METHOD and deliver decoded JSON to CALLBACK.
DATA and CONTENT-TYPE specify the body.  ERRBACK receives errors.
ALLOW-EMPTY accepts a successful response without a body.
QUERY-P identifies read-only POST operations."
  (unless (chirp-x--trusted-url-p url) (error "Invalid X request URL"))
  (let* ((session (chirp-x-session))
         (web-session (and (eq session 'webkit) chirp-web--ready))
         (verb (upcase (symbol-name method)))
         (bytes (if (and data (multibyte-string-p data))
                    (encode-coding-string data 'utf-8) data)))
    (unless session
      (user-error (if (display-graphic-p) "Sign in with M-x chirp-login"
                    "Configure your X session credentials in the environment or auth-source")))
    (cl-labels ((deliver (status)
                  (let ((result (condition-case err
                                    (chirp-x--decode-response
                                     status allow-empty (unless query-p method) web-session)
                                  (error (list :error (error-message-string err))))))
                    (if (eq (car result) :success)
                        (funcall callback (cadr result))
                      (funcall (or errback #'message) (cadr result))))))
      (if (listp session)
          (let* ((url-request-method verb)
                 (url-request-extra-headers (chirp-x--headers session content-type))
                 (url-request-data bytes)
                 (url-max-redirections 0)
                 (url-http-attempt-keepalives (and (eq method 'get) url-http-attempt-keepalives))
                 timer
                 finished
                 (buffer (url-retrieve
                          url (lambda (status)
                                (let ((response (current-buffer)))
                                  (unless finished
                                    (setq finished t)
                                    (when timer (cancel-timer timer))
                                    (unwind-protect (deliver status) (kill-buffer response)))))
                          ;; INHIBIT-COOKIES skips url.el's cookie jar for both request and response.
                          nil t t)))
            (when (and (not finished) (buffer-live-p buffer))
              (with-current-buffer buffer (setq-local url-max-redirections 0))
              (setq timer
                    (run-at-time
                     65 nil
                     (lambda ()
                       (setq finished t)
                       (when (buffer-live-p buffer)
                         (when-let* ((process (get-buffer-process buffer))) (delete-process process))
                         (kill-buffer buffer))
                       (funcall (or errback #'message)
                                (if (and (eq method 'post) (not query-p))
                                    (chirp-x-unknown-write-outcome "X request timed out")
                                  "X request timed out"))))))
            buffer)
        (chirp-web-request
         url verb
         (lambda (status body &optional headers)
           (with-temp-buffer
             (when headers (insert headers "\n"))
             (setq-local url-http-response-status (and (> status 0) status)
                         url-http-end-of-headers (point-marker))
             (insert body)
             (deliver (and (zerop status) (list :error (list 'error body))))))
         :bearer (or (chirp-x--environment-value "CHIRP_X_BEARER_TOKEN")
                     chirp-x-bearer-token)
         :type content-type :data bytes :response-headers chirp-x--limit-headers
         :limit (if (and (eq method 'post) (not query-p))
                    chirp-x--write-response-limit chirp-x--read-response-limit))))))

(cl-defun chirp-x-graphql-request (operation variables callback &key errback)
  "Issue OPERATION with VARIABLES and deliver the result to CALLBACK.
ERRBACK receives a single error message."
  (condition-case err
      (let* ((variables (or variables (make-hash-table :test #'equal)))
             (method (chirp-x--operation-method operation))
             (id (plist-get operation :query-id))
             (features (chirp-x--operation-features operation))
             (fields (chirp-x--operation-field-toggles operation))
             (url (chirp-x--graphql-url id (plist-get operation :name) method
                                        variables features fields)))
        (chirp-x--request url method callback :errback errback
                          :query-p (plist-get operation :query)
                          :content-type (and (eq method 'post) "application/json")
                          :data (and (eq method 'post)
                                     (chirp-x--graphql-post-body id variables features fields))))
    (error (funcall (or errback #'message) (error-message-string err)))))

(cl-defun chirp-x-api-request
    (service path callback &key (method 'get) query form errback)
  "Request an authenticated X API PATH from SERVICE asynchronously.

SERVICE selects an X REST root.  METHOD may be
`get' or `post'.  QUERY and FORM are string-keyed alists; FORM is valid only
for POST requests.  CALLBACK receives decoded JSON, and ERRBACK receives one
readable error string."
  (unless (functionp callback)
    (error "X API callback is not callable"))
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (unless (functionp error-fn)
      (error "X API error callback is not callable"))
    (condition-case err
        (progn
          (unless (memq method '(get post))
            (error "X API method is invalid: %S" method))
          (when (and form (not (eq method 'post)))
            (error "X API form data requires POST"))
          (let ((request-url (chirp-x--rest-url service path query)))
            (chirp-x--request
             request-url method callback
             :data (and form (chirp-x--urlencode form))
             :content-type (and form "application/x-www-form-urlencoded")
             :errback error-fn)))
      (error
       (funcall error-fn (error-message-string err))
       nil))))

(defun chirp-x--upload-media-type (file)
  "Return the supported X media MIME type for FILE."
  (pcase (downcase (or (file-name-extension file) ""))
    ((or "jpg" "jpeg") "image/jpeg")
    ("png" "image/png")
    ("gif" "image/gif")
    ("webp" "image/webp")
    (_ (user-error "Unsupported image format: %s" file))))

(defun chirp-x-image-type (file)
  "Validate upload FILE and return its MIME type."
  (unless (and (file-regular-p file) (file-readable-p file))
    (user-error "Image file is not readable: %s" file))
  (let* ((type (chirp-x--upload-media-type file))
         (size (file-attribute-size (file-attributes (file-truename file))))
         (limit (if (equal type "image/gif") chirp-x--upload-gif-limit
                  chirp-x--upload-image-limit)))
    (unless (<= 1 size limit)
      (user-error "Image must be nonempty and at most %d MiB: %s" (/ limit 1048576) file))
    type))

(defun chirp-x--file-bytes (file)
  "Return FILE contents as an unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun chirp-x--upload-segments (bytes)
  "Split BYTES into media upload segments."
  (if (> (length bytes) chirp-x--upload-chunk-size)
      (cl-loop for start from 0 below (length bytes)
               by chirp-x--upload-chunk-size
               collect (substring bytes start
                                  (min (length bytes)
                                       (+ start chirp-x--upload-chunk-size))))
    (list bytes)))

(defun chirp-x--multipart-data (media-id segment-index bytes)
  "Return (BODY . CONTENT-TYPE) for an APPEND of BYTES.

MEDIA-ID identifies the upload and SEGMENT-INDEX is its zero-based part."
  (let ((boundary
         (format "----------------chirp-%x-%x"
                 (random most-positive-fixnum)
                 (random most-positive-fixnum))))
    (while (string-match-p (regexp-quote boundary) bytes)
      (setq boundary
            (format "----------------chirp-%x-%x"
                    (random most-positive-fixnum)
                    (random most-positive-fixnum))))
    (let ((body
           (concat
            (encode-coding-string
             (format
              (concat "--%s\r\n"
                      "Content-Disposition: form-data; name=\"command\"\r\n\r\n"
                      "APPEND\r\n"
                      "--%s\r\n"
                      "Content-Disposition: form-data; name=\"media_id\"\r\n\r\n"
                      "%s\r\n"
                      "--%s\r\n"
                      "Content-Disposition: form-data; name=\"segment_index\"\r\n\r\n"
                      "%d\r\n"
                      "--%s\r\n"
                      "Content-Disposition: form-data; name=\"media\"; "
                      "filename=\"media\"\r\n"
                      "Content-Type: application/octet-stream\r\n\r\n")
              boundary boundary media-id boundary segment-index boundary)
             'us-ascii)
            bytes
            (encode-coding-string
             (format "\r\n--%s--\r\n" boundary) 'us-ascii))))
      (cons body (format "multipart/form-data; boundary=%s" boundary)))))

(defun chirp-x--upload-media-id (payload)
  "Return the media identifier from INIT PAYLOAD, or nil."
  (let ((identifier (or (chirp-get payload "media_id_string")
                        (chirp-get payload "media_id"))))
    (and identifier (format "%s" identifier))))

(defun chirp-x--upload-processing-error (processing-info)
  "Return a readable media processing error from PROCESSING-INFO."
  (let* ((remote-error (chirp-get processing-info "error"))
         (message (or (chirp-get remote-error "message")
                      (chirp-get remote-error "name")
                      "X could not process the uploaded media"))
         (code (chirp-get remote-error "code")))
    (if code
        (format "%s (%s)" message code)
      message)))

(cl-defun chirp-x-upload-media (file callback &key errback)
  "Upload image FILE to X and call CALLBACK with its media ID.

JPEG, PNG, and WebP files may be at most 5 MiB; GIF files may be at most 15
MiB.  ERRBACK receives setup, upload, cancellation, or asynchronous processing
failures.  No upload
mutation is retried automatically."
  (unless (functionp callback)
    (error "X media upload callback is not callable"))
  (let ((error-fn (or errback (lambda (message) (message "%s" message)))))
    (unless (functionp error-fn)
      (error "X media upload error callback is not callable"))
    (condition-case err
        (progn
          (let* ((media-type (chirp-x-image-type file))
                 (bytes (chirp-x--file-bytes file))
                 (file-size (length bytes))
                 (segments (chirp-x--upload-segments bytes))
                 (upload-url (chirp-x--rest-url 'upload "upload.json" nil)))
            (let (settled-p)
              (cl-labels
                  ((fail
                     (message)
                     (unless settled-p
                       (setq settled-p t)
                       (funcall error-fn message)))
                   (succeed
                     (media-id)
                     (unless settled-p
                       (setq settled-p t)
                       (funcall callback media-id)))
                   (check-status
                     (media-id attempt)
                     (if (>= attempt chirp-x--upload-status-limit)
                         (fail "X media processing did not finish in time")
                       (chirp-x--request
                        (chirp-x--rest-url
                         'upload "upload.json"
                         `(("command" . "STATUS")
                           ("media_id" . ,media-id)))
                        'get
                        (lambda (payload)
                          (handle-processing media-id payload (1+ attempt)))
                        :errback #'fail)))
                   (schedule-status
                     (media-id attempt delay)
                     (run-at-time delay nil (lambda () (check-status media-id attempt))))
                   (handle-processing
                     (media-id payload attempt)
                     (if-let* ((processing-info
                                (chirp-get payload "processing_info")))
                         (pcase (chirp-get processing-info "state")
                           ("succeeded" (succeed media-id))
                           ("failed"
                            (fail
                             (chirp-x--upload-processing-error
                              processing-info)))
                           ((or "pending" "in_progress")
                            (let ((delay
                                   (chirp-get processing-info
                                              "check_after_secs")))
                              (schedule-status
                               media-id attempt
                               (if (numberp delay)
                                   (min 30 (max 0.1 delay))
                                 1))))
                           (_ (fail
                               "X returned an invalid media processing state")))
                       (succeed media-id)))
                   (finalize
                     (media-id)
                     (chirp-x--request
                      upload-url 'post
                      (lambda (payload)
                        (handle-processing media-id payload 0))
                      :data (chirp-x--urlencode
                             `(("command" . "FINALIZE")
                               ("media_id" . ,media-id)))
                      :content-type "application/x-www-form-urlencoded"
                      :errback #'fail))
                   (append-segment
                     (media-id remaining segment-index)
                     (if (null remaining)
                         (finalize media-id)
                       (pcase-let* ((`(,body . ,content-type)
                                     (chirp-x--multipart-data
                                      media-id segment-index (car remaining))))
                         (chirp-x--request
                          upload-url 'post
                          (lambda (_payload)
                            (append-segment media-id (cdr remaining)
                                            (1+ segment-index)))
                          :data body
                          :content-type content-type
                          :allow-empty t
                          :errback #'fail)))))
                (chirp-x--request
                 upload-url 'post
                 (lambda (payload)
                   (if-let* ((media-id (chirp-x--upload-media-id payload)))
                       (append-segment media-id segments 0)
                     (fail "X media INIT did not return a media ID")))
                 :data (chirp-x--urlencode
                        (append
                         `(("command" . "INIT")
                           ("total_bytes" . ,(number-to-string file-size))
                           ("media_type" . ,media-type))
                         (when (equal media-type "image/gif")
                           '(("media_category" . "tweet_gif")))))
                 :content-type "application/x-www-form-urlencoded"
                 :errback #'fail)))))
      (error
       (funcall error-fn (error-message-string err))
       nil))))

(provide 'chirp-x)

;;; chirp-x.el ends here
