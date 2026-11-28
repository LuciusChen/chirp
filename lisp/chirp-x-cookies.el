;;; chirp-x-cookies.el --- Import X session cookies from local browsers -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Read X's `auth_token' and `ct0' cookies from local Firefox and Chromium
;; browser profiles using Emacs's built-in SQLite support plus standard OS
;; crypto utilities (`openssl', macOS `security', or Linux `secret-tool').
;; No third-party Emacs packages, Python libraries, or CLI tools are required.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function sqlite-available-p "sqlite.c" ())
(declare-function sqlite-open "sqlite.c" (&optional file))
(declare-function sqlite-select "sqlite.c" (db query &optional values return-type))
(declare-function sqlite-close "sqlite.c" (db))

(defcustom chirp-x-cookie-browser nil
  "Preferred browser symbol for automatic X cookie import, or nil for auto.

Supported values are `chrome', `dia', `arc', `brave', `edge', `chromium',
`vivaldi', `firefox', `zen', and `librewolf'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const :tag "Google Chrome" chrome)
                 (const :tag "Dia" dia)
                 (const :tag "Arc" arc)
                 (const :tag "Brave" brave)
                 (const :tag "Microsoft Edge" edge)
                 (const :tag "Chromium" chromium)
                 (const :tag "Vivaldi" vivaldi)
                 (const :tag "Firefox" firefox)
                 (const :tag "Zen" zen)
                 (const :tag "LibreWolf" librewolf))
  :group 'chirp-x)

(defconst chirp-x-cookies--chromium-epoch-offset 11644473600000000
  "Microseconds between Windows FILETIME (1601) and Unix (1970) epochs.")

(defconst chirp-x-cookies--chromium-iv "20202020202020202020202020202020"
  "Hex-encoded 16-space IV used by Chromium AES-128-CBC cookie encryption.")

(defconst chirp-x-cookies--browsers-darwin
  '((firefox  :label "Firefox"  :engine firefox
              :dirs ("~/Library/Application Support/Firefox/Profiles")
              :process-match "/Firefox\\.app/")
    (zen      :label "Zen"      :engine firefox
              :dirs ("~/Library/Application Support/zen/Profiles")
              :process-match "/Zen\\(?: Browser\\)?\\.app/")
    (dia      :label "Dia"      :engine chromium
              :dirs ("~/Library/Application Support/Dia/User Data"
                     "~/Library/Application Support/Dia")
              :keychain-services ("Dia Safe Storage" "Dia Storage")
              :process-match "/Dia\\.app/")
    (arc      :label "Arc"      :engine chromium
              :dirs ("~/Library/Application Support/Arc/User Data"
                     "~/Library/Application Support/Arc")
              :keychain-services ("Arc Safe Storage" "Arc Storage")
              :process-match "/Arc\\.app/")
    (chrome   :label "Chrome"   :engine chromium
              :dirs ("~/Library/Application Support/Google/Chrome")
              :keychain-services ("Chrome Safe Storage")
              :process-match "/Google Chrome\\.app/")
    (brave    :label "Brave"    :engine chromium
              :dirs ("~/Library/Application Support/BraveSoftware/Brave-Browser")
              :keychain-services ("Brave Safe Storage")
              :process-match "/Brave Browser\\.app/")
    (edge     :label "Edge"     :engine chromium
              :dirs ("~/Library/Application Support/Microsoft Edge")
              :keychain-services ("Microsoft Edge Safe Storage")
              :process-match "/Microsoft Edge\\.app/")
    (chromium :label "Chromium" :engine chromium
              :dirs ("~/Library/Application Support/Chromium")
              :keychain-services ("Chromium Safe Storage")
              :process-match "/Chromium\\.app/")
    (vivaldi  :label "Vivaldi"  :engine chromium
              :dirs ("~/Library/Application Support/Vivaldi")
              :keychain-services ("Vivaldi Safe Storage")
              :process-match "/Vivaldi\\.app/"))
  "Known macOS browser profile roots, process matchers, and Keychain metadata.")

(defconst chirp-x-cookies--browsers-linux
  '((firefox   :label "Firefox"   :engine firefox
               :dirs ("~/.mozilla/firefox")
               :process-match "\\`firefox"
               :desktop-match "firefox")
    (zen       :label "Zen"       :engine firefox
               :dirs ("~/.zen")
               :process-match "\\`zen\\(?:-bin\\|-browser\\)?\\'"
               :desktop-match "zen")
    (librewolf :label "LibreWolf" :engine firefox
               :dirs ("~/.librewolf")
               :process-match "\\`librewolf"
               :desktop-match "librewolf")
    (chrome    :label "Chrome"    :engine chromium
               :dirs ("~/.config/google-chrome")
               :secret-app "chrome"
               :kwallet-folder "Chrome Keys"
               :kwallet-entry "Chrome Safe Storage"
               :process-match "\\`\\(?:google-\\)?chrome"
               :desktop-match "google-chrome\\|\\`chrome\\.")
    (chromium  :label "Chromium"  :engine chromium
               :dirs ("~/.config/chromium")
               :secret-app "chromium"
               :kwallet-folder "Chromium Keys"
               :kwallet-entry "Chromium Safe Storage"
               :process-match "\\`chromium"
               :desktop-match "chromium")
    (brave     :label "Brave"     :engine chromium
               :dirs ("~/.config/BraveSoftware/Brave-Browser")
               :secret-app "brave"
               :kwallet-folder "Brave Keys"
               :kwallet-entry "Brave Safe Storage"
               :process-match "\\`brave"
               :desktop-match "brave")
    (edge      :label "Edge"      :engine chromium
               :dirs ("~/.config/microsoft-edge")
               :secret-app "microsoft-edge"
               :kwallet-folder "Microsoft Edge Keys"
               :kwallet-entry "Microsoft Edge Safe Storage"
               :process-match "\\`\\(?:msedge\\|microsoft-edge\\)"
               :desktop-match "microsoft-edge")
    (vivaldi   :label "Vivaldi"   :engine chromium
               :dirs ("~/.config/vivaldi")
               :secret-app "vivaldi"
               :kwallet-folder "Vivaldi Keys"
               :kwallet-entry "Vivaldi Safe Storage"
               :process-match "\\`vivaldi"
               :desktop-match "vivaldi"))
  "Known Linux browser profile roots, process matchers, and Secret metadata.")

(defvar chirp-x-cookies--last-issues nil
  "Structured diagnostic plist recorded during the last cookie import scan.")

(defun chirp-x-cookies--reset-issues ()
  "Clear recorded diagnostics for a fresh browser cookie scan."
  (setq chirp-x-cookies--last-issues nil))

(defun chirp-x-cookies--record-issue (key value)
  "Record VALUE under diagnostic KEY in `chirp-x-cookies--last-issues'."
  (when value
    (let ((current (plist-get chirp-x-cookies--last-issues key)))
      (unless (member value current)
        (setq chirp-x-cookies--last-issues
              (plist-put chirp-x-cookies--last-issues
                         key (append current (list value))))))))

(defun chirp-x-cookies-last-denied-browsers ()
  "Return browser labels skipped for permission errors during the last scan."
  (copy-sequence (plist-get chirp-x-cookies--last-issues :denied)))

(defun chirp-x-cookies-last-diagnostic ()
  "Return a concise explanation of issues hit on the last scan, or nil."
  (let ((denied (plist-get chirp-x-cookies--last-issues :denied))
        (keychain (plist-get chirp-x-cookies--last-issues :keychain-failed))
        (tools (plist-get chirp-x-cookies--last-issues :missing-tools))
        (no-sqlite (plist-get chirp-x-cookies--last-issues :sqlite-unavailable))
        parts)
    (when no-sqlite
      (push "Emacs was built without SQLite support" parts))
    (when denied
      (push (format "Permission denied reading %s cookies (grant Emacs access in OS privacy settings)"
                    (string-join denied "/"))
            parts))
    (when keychain
      (push (format "Could not unlock OS keychain for %s cookies"
                    (string-join keychain "/"))
            parts))
    (when tools
      (push (format "Missing %s to decrypt Chromium cookies"
                    (string-join tools " and "))
            parts))
    (when parts
      (string-join (nreverse parts) "; "))))

(defun chirp-x-cookies--to-unibyte (string)
  "Return STRING as unibyte UTF-8 bytes."
  (if (multibyte-string-p string)
      (encode-coding-string string 'utf-8)
    string))

(defun chirp-x-cookies--hmac-sha1 (key message)
  "Return the 20-byte unibyte HMAC-SHA1 of MESSAGE under KEY."
  (let* ((raw-key (chirp-x-cookies--to-unibyte key))
         (raw-msg (chirp-x-cookies--to-unibyte message))
         (sized-key (if (> (length raw-key) 64)
                        (secure-hash 'sha1 raw-key nil nil t)
                      raw-key))
         (padded (concat sized-key (make-string (- 64 (length sized-key)) 0)))
         (ipad (make-string 64 0))
         (opad (make-string 64 0)))
    (dotimes (i 64)
      (let ((byte (aref padded i)))
        (aset ipad i (logxor byte #x36))
        (aset opad i (logxor byte #x5c))))
    (secure-hash 'sha1
                 (concat opad (secure-hash 'sha1 (concat ipad raw-msg) nil nil t))
                 nil nil t)))

(defun chirp-x-cookies--pbkdf2-sha1 (password salt iterations dk-len)
  "Derive DK-LEN bytes from PASSWORD and SALT using ITERATIONS of PBKDF2-SHA1."
  (let* ((u (chirp-x-cookies--hmac-sha1
             password (concat (chirp-x-cookies--to-unibyte salt)
                              (unibyte-string 0 0 0 1))))
         (acc (copy-sequence u)))
    (dotimes (_ (1- iterations))
      (setq u (chirp-x-cookies--hmac-sha1 password u))
      (dotimes (i (length acc))
        (aset acc i (logxor (aref acc i) (aref u i)))))
    (substring acc 0 dk-len)))

(defun chirp-x-cookies--bytes-to-hex (bytes)
  "Encode unibyte BYTES as a lowercase hexadecimal string."
  (mapconcat (lambda (byte) (format "%02x" byte)) bytes ""))

(defun chirp-x-cookies--aes-128-cbc-decrypt (ciphertext key-bytes)
  "Decrypt unibyte CIPHERTEXT with 16-byte KEY-BYTES via `openssl'."
  (let ((openssl (or (and (file-executable-p "/usr/bin/openssl") "/usr/bin/openssl")
                     (executable-find "openssl"))))
    (if (not openssl)
        (progn
          (chirp-x-cookies--record-issue :missing-tools "openssl")
          nil)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert ciphertext)
          (let ((status (call-process-region
                         (point-min) (point-max)
                         openssl t t nil
                         "enc" "-aes-128-cbc" "-d"
                         "-K" (chirp-x-cookies--bytes-to-hex key-bytes)
                         "-iv" chirp-x-cookies--chromium-iv)))
            (when (and (numberp status) (zerop status))
              (buffer-string))))))))

(defun chirp-x-cookies--strip-host-prefix (plaintext host-key)
  "Strip Chromium v24+ SHA-256 HOST-KEY prefix from unibyte PLAINTEXT if present."
  (if (and (stringp host-key)
           (>= (length plaintext) 32)
           (equal (substring plaintext 0 32)
                  (secure-hash 'sha256
                               (chirp-x-cookies--to-unibyte host-key)
                               nil nil t)))
      (substring plaintext 32)
    plaintext))

(defun chirp-x-cookies--decrypt-chromium-value (encrypted host-key password iterations)
  "Decrypt Chromium ENCRYPTED blob for HOST-KEY with PASSWORD and ITERATIONS."
  (when (and (stringp encrypted)
             (> (length encrypted) 3)
             (member (substring encrypted 0 3) '("v10" "v11")))
    (let* ((key (chirp-x-cookies--pbkdf2-sha1 password "saltysalt" iterations 16))
           (raw (chirp-x-cookies--aes-128-cbc-decrypt (substring encrypted 3) key)))
      (when raw
        (let ((trimmed (chirp-x-cookies--strip-host-prefix raw host-key)))
          (when (and (> (length trimmed) 0)
                     (not (string-match-p "[^[:ascii:]]\\|[[:cntrl:];]" trimmed)))
            (decode-coding-string trimmed 'us-ascii)))))))

(defun chirp-x-cookies--process-output (program &rest args)
  "Return trimmed stdout of PROGRAM with ARGS, or nil on failure."
  (when program
    (with-temp-buffer
      (let ((status (apply #'call-process program nil (list t nil) nil args)))
        (when (and (numberp status) (zerop status))
          (let ((text (string-trim-right (buffer-string) "[\r\n]+")))
            (unless (string-empty-p text)
              text)))))))

(defun chirp-x-cookies--chromium-secret (spec prefix)
  "Return (PASSWORD . ITERATIONS) for Chromium browser SPEC and cookie PREFIX."
  (let ((label (plist-get spec :label)))
    (cond
     ((eq system-type 'darwin)
      (let* ((services (or (plist-get spec :keychain-services)
                           (when-let* ((service (plist-get spec :keychain-service)))
                             (list service))))
             (password
              (cl-some
               (lambda (service)
                 (chirp-x-cookies--process-output
                  "/usr/bin/security"
                  "find-generic-password" "-w"
                  "-s" service))
               services)))
        (if password
            (cons password 1003)
          (chirp-x-cookies--record-issue :keychain-failed label)
          nil)))
     ((equal prefix "v10")
      (cons "peanuts" 1))
     ((equal prefix "v11")
      (let* ((app (plist-get spec :secret-app))
             (secret-tool (executable-find "secret-tool"))
             (kwallet (executable-find "kwallet-query"))
             (folder (plist-get spec :kwallet-folder))
             (entry (plist-get spec :kwallet-entry))
             (password
              (or (when (and app secret-tool)
                    (chirp-x-cookies--process-output
                     secret-tool "lookup" "application" app))
                  (when (and kwallet folder entry)
                    (chirp-x-cookies--process-output
                     kwallet "kdewallet" "-f" folder "-r" entry)))))
        (cond
         (password (cons password 1))
         ((not (or secret-tool kwallet))
          (chirp-x-cookies--record-issue :missing-tools "secret-tool or kwallet-query")
          nil)
         (t
          (chirp-x-cookies--record-issue :keychain-failed label)
          nil)))))))

(defun chirp-x-cookies--permission-error-p (err)
  "Return non-nil when file error ERR indicates denied filesystem permission."
  (let ((msg (error-message-string err)))
    (and (stringp msg)
         (string-match-p "Operation not permitted\\|Permission denied" msg))))

(defun chirp-x-cookies--record-denied (label)
  "Record browser LABEL as skipped because of denied filesystem permission."
  (chirp-x-cookies--record-issue :denied label))

(defun chirp-x-cookies--directory-entries (dir label)
  "Return non-dot entries under DIR, recording LABEL on permission errors."
  (condition-case err
      (directory-files dir t "\\`[^.]")
    (file-error
     (when (chirp-x-cookies--permission-error-p err)
       (chirp-x-cookies--record-denied label))
     nil)))

(defun chirp-x-cookies--with-db-copy (db-file fn &optional label)
  "Copy SQLite DB-FILE (with WAL/SHM if present) to a temp dir and call FN.

Optional LABEL identifies the browser for permission-error reporting."
  (when (and (fboundp 'sqlite-available-p)
             (sqlite-available-p)
             (file-regular-p db-file)
             (file-readable-p db-file))
    (let* ((tmp-dir (make-temp-file "chirp-cookies-" t))
           (copy (expand-file-name (file-name-nondirectory db-file) tmp-dir))
           db)
      (unless (eq system-type 'windows-nt)
        (set-file-modes tmp-dir #o700))
      (unwind-protect
          (condition-case err
              (progn
                (copy-file db-file copy t)
                (dolist (suffix '("-wal" "-shm"))
                  (let ((sidecar (concat db-file suffix)))
                    (when (file-readable-p sidecar)
                      (copy-file sidecar (concat copy suffix) t))))
                (setq db (sqlite-open copy))
                (funcall fn db))
            (file-error
             (when (chirp-x-cookies--permission-error-p err)
               (chirp-x-cookies--record-denied label))
             nil)
            (error nil))
        (when db
          (ignore-errors (sqlite-close db)))
        (ignore-errors (delete-directory tmp-dir t))))))

(defun chirp-x-cookies--profile-db-files (spec)
  "Return existing cookie SQLite paths for browser SPEC, newest first."
  (let* ((dirs (or (plist-get spec :dirs)
                   (when-let* ((dir (plist-get spec :dir)))
                     (list dir))))
         (engine (plist-get spec :engine))
         (label (plist-get spec :label))
         files)
    (dolist (raw-root dirs)
      (let ((root (expand-file-name raw-root)))
        (when (file-directory-p root)
          (if (eq engine 'firefox)
              (dolist (entry (chirp-x-cookies--directory-entries root label))
                (when (file-directory-p entry)
                  (let ((db (expand-file-name "cookies.sqlite" entry)))
                    (when (and (file-regular-p db) (file-readable-p db))
                      (push db files)))))
            (dolist (rel '("Network/Cookies" "Cookies"))
              (let ((db (expand-file-name rel root)))
                (when (and (file-regular-p db) (file-readable-p db))
                  (push db files))))
            (dolist (entry (chirp-x-cookies--directory-entries root label))
              (let ((name (file-name-nondirectory entry)))
                (when (and (file-directory-p entry)
                           (not (member name '("System Profile" "Guest Profile"))))
                  (dolist (rel '("Network/Cookies" "Cookies"))
                    (let ((db (expand-file-name rel entry)))
                      (when (and (file-regular-p db) (file-readable-p db))
                        (push db files)))))))))))
    (setq files (delete-dups files))
    (sort files
          (lambda (a b)
            (let ((ta (when-let* ((attrs (file-attributes a)))
                        (file-attribute-modification-time attrs)))
                  (tb (when-let* ((attrs (file-attributes b)))
                        (file-attribute-modification-time attrs))))
              (and ta tb (time-less-p tb ta)))))))

(defun chirp-x-cookies--profile-name (db-file engine)
  "Return a human-readable profile name extracted from DB-FILE for ENGINE."
  (let* ((dir (directory-file-name (file-name-directory db-file)))
         (name (file-name-nondirectory dir)))
    (if (and (eq engine 'chromium) (equal name "Network"))
        (file-name-nondirectory (directory-file-name (file-name-directory dir)))
      name)))

(defun chirp-x-cookies--read-firefox-db (db-file &optional label)
  "Read X `(AUTH-TOKEN . CT0)' and timestamp from Firefox DB-FILE, or nil.

Optional LABEL identifies the browser for permission-error reporting."
  (chirp-x-cookies--with-db-copy
   db-file
   (lambda (db)
     (let ((rows (sqlite-select
                  db
                  "SELECT name, value, lastAccessed FROM moz_cookies
                   WHERE name IN ('auth_token', 'ct0')
                     AND host IN ('.x.com', 'x.com')
                     AND (expiry IS NULL OR expiry = 0 OR expiry > ?)
                   ORDER BY lastAccessed DESC"
                  (list (floor (float-time)))))
           auth ct0 latest)
       (dolist (row rows)
         (pcase-let ((`(,name ,value ,accessed) row))
           (when (and (stringp value) (not (string-blank-p value)))
             (cond
              ((and (equal name "auth_token") (not auth))
               (setq auth value))
              ((and (equal name "ct0") (not ct0))
               (setq ct0 value)))
             (when (numberp accessed)
               (setq latest (max (or latest 0) accessed))))))
       (when (and auth ct0)
         (list :auth-token auth :ct0 ct0 :timestamp (or latest 0)))))
   label))

(defun chirp-x-cookies--scan-chromium-db (db-file &optional label)
  "Return raw X cookie rows from Chromium DB-FILE when both cookies exist.

Optional LABEL identifies the browser for permission-error reporting."
  (chirp-x-cookies--with-db-copy
   db-file
   (lambda (db)
     (let* ((now-filetime (+ (* (floor (float-time)) 1000000)
                             chirp-x-cookies--chromium-epoch-offset))
            (rows (sqlite-select
                   db
                   "SELECT host_key, name, value, encrypted_value, last_access_utc
                    FROM cookies
                    WHERE name IN ('auth_token', 'ct0')
                      AND host_key IN ('.x.com', 'x.com')
                      AND (expires_utc IS NULL OR expires_utc = 0 OR expires_utc > ?)
                    ORDER BY last_access_utc DESC"
                   (list now-filetime)))
            auth-row ct0-row)
       (dolist (row rows)
         (let ((name (nth 1 row)))
           (cond
            ((and (equal name "auth_token") (not auth-row))
             (setq auth-row row))
            ((and (equal name "ct0") (not ct0-row))
             (setq ct0-row row)))))
       (when (and auth-row ct0-row)
         (let ((raw-ts (max (or (nth 4 auth-row) 0)
                            (or (nth 4 ct0-row) 0))))
           (list :auth-row auth-row
                 :ct0-row ct0-row
                 :timestamp (if (> raw-ts chirp-x-cookies--chromium-epoch-offset)
                                (- raw-ts chirp-x-cookies--chromium-epoch-offset)
                              raw-ts))))))
   label))

(defun chirp-x-cookies--resolve-chromium-row (row spec secret-cache)
  "Extract plaintext cookie value from Chromium ROW using SPEC and SECRET-CACHE."
  (pcase-let ((`(,host-key _name ,value ,encrypted _accessed) row))
    (cond
     ((and (stringp value) (not (string-blank-p value)))
      value)
     ((and (stringp encrypted) (> (length encrypted) 3))
      (let* ((prefix (substring encrypted 0 3))
             (cached (assoc prefix (cdr secret-cache)))
             (secret (if cached
                         (cdr cached)
                       (let ((resolved (chirp-x-cookies--chromium-secret spec prefix)))
                         (push (cons prefix resolved) (cdr secret-cache))
                         resolved))))
        (when secret
          (chirp-x-cookies--decrypt-chromium-value
           encrypted host-key (car secret) (cdr secret))))))))

(defun chirp-x-cookies--specs ()
  "Return browser specs supported on the current operating system."
  (pcase system-type
    ('darwin chirp-x-cookies--browsers-darwin)
    ('gnu/linux chirp-x-cookies--browsers-linux)
    (_ nil)))

(defun chirp-x-cookies--running-browsers (specs)
  "Return browser symbols from SPECS that currently have a running process."
  (let ((output (pcase system-type
                  ('darwin (chirp-x-cookies--process-output
                            "/bin/ps" "-axo" "comm="))
                  ('gnu/linux (chirp-x-cookies--process-output
                               (or (and (file-executable-p "/bin/ps") "/bin/ps")
                                   (executable-find "ps"))
                               "-eo" "comm="))))
        running)
    (when output
      (let ((case-fold-search t))
        (dolist (line (split-string output "\n" t))
          (let ((proc (string-trim line)))
            (dolist (entry specs)
              (let ((sym (car entry))
                    (pattern (plist-get (cdr entry) :process-match)))
                (when (and pattern
                           (not (memq sym running))
                           (string-match-p pattern proc))
                  (push sym running))))))))
    (nreverse running)))

(defun chirp-x-cookies--default-browser (specs)
  "Return the system default browser symbol from SPECS, or nil."
  (pcase system-type
    ('darwin
     (when-let* ((app-path
                  (chirp-x-cookies--process-output
                   "/usr/bin/osascript" "-l" "JavaScript" "-e"
                   (concat "ObjC.import('AppKit');"
                           "var u = $.NSWorkspace.sharedWorkspace"
                           ".URLForApplicationToOpenURL("
                           "$.NSURL.URLWithString('https://x.com'));"
                           "(u && !u.isNil()) ? ObjC.unwrap(u.path) : '';"))))
       (let ((probe (concat (directory-file-name app-path) "/"))
             (case-fold-search t))
         (car (cl-find-if
               (lambda (entry)
                 (when-let* ((pattern (plist-get (cdr entry) :process-match)))
                   (string-match-p pattern probe)))
               specs)))))
    ('gnu/linux
     (when-let* ((desktop
                  (or (when-let* ((bin (executable-find "xdg-settings")))
                        (chirp-x-cookies--process-output
                         bin "get" "default-web-browser"))
                      (when-let* ((bin (executable-find "xdg-mime")))
                        (chirp-x-cookies--process-output
                         bin "query" "default" "x-scheme-handler/https")))))
       (let ((case-fold-search t))
         (car (cl-find-if
               (lambda (entry)
                 (when-let* ((pattern (plist-get (cdr entry) :desktop-match)))
                   (string-match-p pattern desktop)))
               specs)))))))

(defun chirp-x-cookies--prioritized-tiers (specs)
  "Partition SPECS into priority tiers: running, default, then remaining."
  (let* ((running (chirp-x-cookies--running-browsers specs))
         (default (chirp-x-cookies--default-browser specs))
         (running-ordered (if (and default (memq default running))
                              (cons default (delq default (copy-sequence running)))
                            running))
         (tier1 (delq nil (mapcar (lambda (sym) (assq sym specs)) running-ordered)))
         (tier2 (when (and default (not (memq default running)))
                  (when-let* ((entry (assq default specs)))
                    (list entry))))
         (used (append (mapcar #'car tier1) (mapcar #'car tier2)))
         (tier3 (cl-remove-if (lambda (entry) (memq (car entry) used)) specs)))
    (delq nil (list tier1 tier2 tier3))))

(defun chirp-x-cookies--scan-specs (specs &optional profile-filter)
  "Scan SPECS for unexpired X cookie candidates without touching the OS Keychain.

When PROFILE-FILTER is non-nil, only keep candidates matching that profile name."
  (let ((counts (make-hash-table :test #'eq))
        candidates)
    (dolist (entry specs)
      (let* ((sym (car entry))
             (spec (cdr entry))
             (label (plist-get spec :label))
             (engine (plist-get spec :engine)))
        (dolist (db (chirp-x-cookies--profile-db-files spec))
          (let ((profile (chirp-x-cookies--profile-name db engine)))
            (when (or (null profile-filter) (equal profile profile-filter))
              (if (eq engine 'firefox)
                  (when-let* ((result (chirp-x-cookies--read-firefox-db db label)))
                    (puthash sym (1+ (gethash sym counts 0)) counts)
                    (push (list :symbol sym
                                :profile profile
                                :engine 'firefox
                                :label label
                                :timestamp (plist-get result :timestamp)
                                :auth-token (plist-get result :auth-token)
                                :ct0 (plist-get result :ct0))
                          candidates))
                (when-let* ((scan (chirp-x-cookies--scan-chromium-db db label)))
                  (puthash sym (1+ (gethash sym counts 0)) counts)
                  (push (list :symbol sym
                              :profile profile
                              :engine 'chromium
                              :label label
                              :spec spec
                              :timestamp (plist-get scan :timestamp)
                              :auth-row (plist-get scan :auth-row)
                              :ct0-row (plist-get scan :ct0-row))
                        candidates))))))))
    (dolist (cand candidates)
      (let* ((sym (plist-get cand :symbol))
             (label (plist-get cand :label))
             (profile (plist-get cand :profile))
             (multi (> (gethash sym counts 0) 1))
             (display (if (and (or multi profile-filter)
                               profile (not (string-empty-p profile)))
                          (format "%s (%s)" label profile)
                        label)))
        (plist-put cand :display-label display)))
    (sort candidates
          (lambda (a b)
            (> (plist-get a :timestamp) (plist-get b :timestamp))))))

(defun chirp-x-cookies--decrypt-candidate (cand secret-caches)
  "Decrypt cookie candidate CAND using per-browser SECRET-CACHES, or nil."
  (let ((label (or (plist-get cand :display-label) (plist-get cand :label))))
    (if (eq (plist-get cand :engine) 'firefox)
        (list :auth-token (plist-get cand :auth-token)
              :ct0 (plist-get cand :ct0)
              :browser label
              :symbol (plist-get cand :symbol)
              :profile (plist-get cand :profile))
      (let* ((sym (plist-get cand :symbol))
             (secret-cache (or (gethash sym secret-caches)
                               (puthash sym (list nil) secret-caches)))
             (auth (chirp-x-cookies--resolve-chromium-row
                    (plist-get cand :auth-row)
                    (plist-get cand :spec)
                    secret-cache))
             (ct0 (chirp-x-cookies--resolve-chromium-row
                   (plist-get cand :ct0-row)
                   (plist-get cand :spec)
                   secret-cache)))
        (when (and auth ct0)
          (list :auth-token auth
                :ct0 ct0
                :browser label
                :symbol sym
                :profile (plist-get cand :profile)))))))

(defun chirp-x-cookies-available-sources ()
  "Return readable browser/profile sources containing unexpired X cookies.

Each entry is a plist `(:browser SYMBOL :profile NAME :label DISPLAY)'.
Prioritizes currently running and default browsers, skips unreadable
directories, and does not touch the OS Keychain."
  (chirp-x-cookies--reset-issues)
  (if (not (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
      (progn
        (setq chirp-x-cookies--last-issues
              (plist-put chirp-x-cookies--last-issues :sqlite-unavailable t))
        nil)
    (let* ((ordered (apply #'append (chirp-x-cookies--prioritized-tiers
                                     (chirp-x-cookies--specs))))
           (counts (make-hash-table :test #'eq))
           raw)
      (dolist (entry ordered)
        (let* ((sym (car entry))
               (spec (cdr entry))
               (label (plist-get spec :label))
               (engine (plist-get spec :engine))
               seen-profiles)
          (dolist (db (chirp-x-cookies--profile-db-files spec))
            (let ((profile (chirp-x-cookies--profile-name db engine)))
              (when (and (not (member profile seen-profiles))
                         (if (eq engine 'firefox)
                             (chirp-x-cookies--read-firefox-db db label)
                           (chirp-x-cookies--scan-chromium-db db label)))
                (push profile seen-profiles)
                (puthash sym (1+ (gethash sym counts 0)) counts)
                (push (list :browser sym :profile profile :base-label label) raw))))))
      (mapcar
       (lambda (item)
         (let* ((sym (plist-get item :browser))
                (profile (plist-get item :profile))
                (base (plist-get item :base-label))
                (display (if (> (gethash sym counts 0) 1)
                             (format "%s (%s)" base profile)
                           base)))
           (list :browser sym :profile profile :label display)))
       (nreverse raw)))))

(defun chirp-x-cookies-make-iterator (&optional browser profile)
  "Return a lazy zero-arg iterator yielding local browser X cookie plists.

Optional BROWSER and PROFILE narrow scanning to one browser or profile.
Auto-detection scans currently running browsers first, then the system default
browser, and only touches remaining browsers if earlier candidates fail or are
exhausted.  Duplicate `(auth_token . ct0)' pairs are skipped."
  (chirp-x-cookies--reset-issues)
  (if (not (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
      (progn
        (setq chirp-x-cookies--last-issues
              (plist-put chirp-x-cookies--last-issues :sqlite-unavailable t))
        (lambda () nil))
    (let* ((all-specs (chirp-x-cookies--specs))
           (target (or browser chirp-x-cookie-browser))
           (remaining-tiers
            (if target
                (when-let* ((entry (assq target all-specs)))
                  (list (list entry)))
              (chirp-x-cookies--prioritized-tiers all-specs)))
           (queue nil)
           (secret-caches (make-hash-table :test #'eq))
           (seen (make-hash-table :test #'equal)))
      (lambda ()
        (let (found)
          (while (and (not found) (or queue remaining-tiers))
            (unless queue
              (setq queue (chirp-x-cookies--scan-specs (pop remaining-tiers) profile)))
            (while (and (not found) queue)
              (let ((decrypted (chirp-x-cookies--decrypt-candidate
                                (pop queue) secret-caches)))
                (when decrypted
                  (let ((key (cons (plist-get decrypted :auth-token)
                                   (plist-get decrypted :ct0))))
                    (unless (gethash key seen)
                      (puthash key t seen)
                      (setq found decrypted)))))))
          found)))))

(defun chirp-x-cookies-import (&optional browser profile)
  "Import X `auth_token' and `ct0' from a local browser, or return nil.

Optional BROWSER and PROFILE select a specific browser symbol (defaulting to
`chirp-x-cookie-browser', or auto-detecting when nil) and profile name.
Return a plist `(:auth-token AUTH :ct0 CT0 :browser LABEL ...)' on success."
  (funcall (chirp-x-cookies-make-iterator browser profile)))

(provide 'chirp-x-cookies)

;;; chirp-x-cookies.el ends here
