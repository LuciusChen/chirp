;;; chirp-web.el --- Use an X login session in WebKit -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; X handles sign-in in its own page.  The web view attaches its cookies to
;; requests, including cookies rotated after sign-in.

;;; Code:

(require 'cl-lib)
(require 'json)

(declare-function set-xwidget-query-on-exit-flag "xwidget.c" (xwidget flag))
(declare-function xwidget-live-p "xwidget.c" (object))
(declare-function xwidget-buffer "xwidget.c" (xwidget))
(declare-function xwidget-webkit-goto-uri "xwidget.c" (xwidget uri))
(declare-function xwidget-webkit-execute-script "xwidget.c" (xwidget script &optional fun))
(declare-function xwidget-webkit-new-session "xwidget" (url))
(declare-function xwidget-webkit-current-session "xwidget" ())

(defvar chirp-web--widget nil)
(defvar chirp-web--login-timer nil)
(defvar chirp-web--login-id nil)
(defvar chirp-web--ready nil
  "Identity of the current signed-in browser session, or nil.")
(defvar chirp-web--serial 0)
(defvar chirp-web--callbacks (make-hash-table :test #'eql)
  "Pending WebKit result handlers keyed by operation ID.")

(defun chirp-web--receive (reply)
  "Deliver WebKit REPLY to its pending operation, if still active."
  (pcase reply
    (`[,(and (pred stringp) key) ,value]
     (when-let* ((id (string-to-number key))
                 (callback (gethash id chirp-web--callbacks)))
       (remhash id chirp-web--callbacks)
       (funcall callback value)))))

(defun chirp-web--execute (widget id script callback)
  "Evaluate SCRIPT on WIDGET and deliver its result to CALLBACK for ID."
  ;; Cocoa holds the native completion block outside the Lisp GC roots.
  ;; Give it an interned function and keep the operation's closure in Lisp.
  (puthash id callback chirp-web--callbacks)
  (xwidget-webkit-execute-script
   widget (format "['%d',%s]" id script) #'chirp-web--receive))

(defun chirp-web-ready-p ()
  "Return non-nil when the X browser session is ready for requests."
  (and chirp-web--ready (fboundp 'xwidget-live-p)
       (xwidget-live-p chirp-web--widget)))

(defconst chirp-web--home-url "https://x.com/home"
  "X home page used to restore an existing WebKit session quietly.")

(defconst chirp-web--login-url
  "https://x.com/i/flow/login?redirect_after_login=%2Fhome"
  "X sign-in page opened for interactive authentication.")

(defconst chirp-web--login-script
  "(() => {
  if (document.readyState === 'loading') return '';
  if (location.origin === 'https://x.com') {
    const path = location.pathname;
    if (path.startsWith('/i/flow/') || path === '/login') return 'signin';
    const hasCsrf = /(?:^|;\\s*)ct0=[^;]+/.test(document.cookie);
    const signedIn = /(?:^|;\\s*)twid=[^;]+/.test(document.cookie) ||
      !!document.querySelector('[data-testid=\"AppTabBar_Home_Link\"], [data-testid=\"SideNav_AccountSwitcher_Button\"]');
    if (hasCsrf && signedIn) return 'ready';
    if (document.readyState === 'complete' &&
        document.querySelector('[data-testid=\"loginButton\"], a[href=\"/login\"]')) return 'signin';
    return '';
  }
  return location.href && location.href !== 'about:blank' && document.readyState === 'complete'
    ? 'signin' : '';
})()"
  "Detect whether the X WebKit session is ready or requires sign-in.")

(defun chirp-web--reveal-login (widget reveal-state)
  "Show WIDGET's sign-in buffer once when REVEAL-STATE is still pending."
  (when (and reveal-state (not (cdr reveal-state)) (xwidget-live-p widget))
    (setcdr reveal-state t)
    (let ((buffer (xwidget-buffer widget)))
      (unless (get-buffer-window buffer t)
        (pop-to-buffer buffer)))
    (message "Sign in to X here. Chirp will open when the session is ready.")))

(defun chirp-web--check-login (callback deadline &optional reveal-state)
  "Run CALLBACK once the X home session is ready, or stop at DEADLINE.
Optional REVEAL-STATE is (REVEAL-AT . REVEALED-P) for background restore."
  (let* ((widget chirp-web--widget)
         (timer chirp-web--login-timer)
         (live (xwidget-live-p widget)))
    (cond
     ((or (not live) (> (float-time) deadline))
      (cancel-timer timer)
      (remhash chirp-web--login-id chirp-web--callbacks)
      (setq chirp-web--login-timer nil)
      (when live (message "X sign-in timed out. Run M-x chirp-login to resume.")))
     (t
      (when (and reveal-state (not (cdr reveal-state))
                 (> (float-time) (car reveal-state)))
        (chirp-web--reveal-login widget reveal-state))
      (chirp-web--execute
       widget chirp-web--login-id chirp-web--login-script
       (lambda (state)
         (when (eq timer chirp-web--login-timer)
           (cond
            ((equal state "ready")
             (cancel-timer timer)
             (setq chirp-web--login-timer nil chirp-web--ready (gensym "chirp-web-session-"))
             (let* ((buffer (xwidget-buffer widget))
                    (window (get-buffer-window buffer t)))
               (when (window-live-p window)
                 (quit-windows-on buffer))
               (if (window-live-p window)
                   (with-selected-window window (funcall callback))
                 (funcall callback))))
            ((equal state "signin")
             (chirp-web--reveal-login widget reveal-state))))))))))

(defun chirp-web--confirm-kill ()
  "Ask before closing the browser buffer that holds Chirp's X session."
  (or (not (and (xwidget-live-p chirp-web--widget)
                (eq (xwidget-buffer chirp-web--widget) (current-buffer))))
      (yes-or-no-p "Close Chirp’s X browser session? ")))

(defun chirp-web-login (callback &optional interactive-p)
  "Ensure an X WebKit session is ready and call CALLBACK.
When INTERACTIVE-P is non-nil, show the X sign-in page immediately.
Otherwise try restoring an existing WebKit session quietly in the
background and show the browser buffer only if X requires sign-in."
  (unless (and (display-graphic-p) (fboundp 'xwidget-webkit-execute-script)
               (require 'xwidget nil t))
    (user-error "Open Chirp in a graphical Emacs with WebKit to sign in"))
  (let ((url (if interactive-p chirp-web--login-url chirp-web--home-url))
        (reveal-state (unless interactive-p (cons (+ (float-time) 5.0) nil))))
    (if (xwidget-live-p chirp-web--widget)
        (progn
          (when interactive-p
            (pop-to-buffer (xwidget-buffer chirp-web--widget)))
          (when (or interactive-p (not chirp-web--login-timer))
            (xwidget-webkit-goto-uri chirp-web--widget url)))
      (if interactive-p
          (progn
            (xwidget-webkit-new-session url)
            (setq chirp-web--widget (xwidget-webkit-current-session)))
        (save-window-excursion
          (xwidget-webkit-new-session url)
          (setq chirp-web--widget (xwidget-webkit-current-session))))
      (unless (xwidget-live-p chirp-web--widget)
        (user-error "WebKit could not open the X sign-in page"))
      (set-xwidget-query-on-exit-flag chirp-web--widget nil)
      (with-current-buffer (xwidget-buffer chirp-web--widget)
        (add-hook 'kill-buffer-query-functions #'chirp-web--confirm-kill nil t)))
    (message "%s"
             (if interactive-p
                 "Sign in to X here. Chirp will open when the session is ready."
               "Connecting to X..."))
    (when chirp-web--login-timer
      (cancel-timer chirp-web--login-timer)
      (remhash chirp-web--login-id chirp-web--callbacks))
    (setq chirp-web--ready nil
          chirp-web--login-id (cl-incf chirp-web--serial)
          chirp-web--login-timer (run-at-time 0.25 0.35 #'chirp-web--check-login
                                             callback (+ (float-time) 600) reveal-state))))

(defconst chirp-web--fetch-script
  "((id, url, method, bearer, type, data, limit) => {
  if (location.origin !== 'https://x.com') return 'signin';
  const results = window.__emacsChirpResults ??= Object.create(null);
  results[id] = null;
  setTimeout(() => delete results[id], 75000);
  (async () => {
    try {
      const csrf = document.cookie.match(/(?:^|;\\s*)ct0=([^;]+)/);
      if (!csrf) { results[id] = [401, '']; return; }
      const headers = {'Authorization': 'Bearer ' + bearer,
        'X-Csrf-Token': csrf[1], 'X-Twitter-Active-User': 'yes',
        'X-Twitter-Auth-Type': 'OAuth2Session', 'X-Twitter-Client-Language': 'en'};
      if (type) headers['Content-Type'] = type;
      let body;
      if (data !== null) {
        const raw = atob(data);
        body = new Uint8Array(raw.length);
        for (let i = 0; i < raw.length; i++) body[i] = raw.charCodeAt(i);
      }
      const abort = new AbortController();
      const timeout = setTimeout(() => abort.abort(), 60000);
      try {
        const res = await fetch(url, {method, headers, body, credentials: 'include',
          redirect: 'error', signal: abort.signal});
        const reader = res.body?.getReader();
        const decoder = new TextDecoder();
        let size = 0, text = '';
        if (reader) for (;;) {
          const {done, value} = await reader.read();
          if (done) break;
          size += value.byteLength;
          if (size > limit) { abort.abort(); throw Error('X response exceeds size limit'); }
          text += decoder.decode(value, {stream: true});
        }
        results[id] = [res.status, text + decoder.decode()];
      } finally { clearTimeout(timeout); }
    } catch (err) { results[id] = [0, String(err.message)]; }
  })();
  return 'started';
})"
  "Start a browser request while keeping session cookies inside WebKit.")

(cl-defun chirp-web-request (url method callback &key bearer type data limit)
  "Send URL with METHOD and call CALLBACK with HTTP status and body.
BEARER is the web client token.  TYPE describes the optional binary DATA.
LIMIT bounds the response in bytes.  A zero status indicates a network
or session error.  The caller must validate URL against its API allowlist."
  (unless (chirp-web-ready-p) (user-error "Sign in with M-x chirp"))
  (let* ((widget chirp-web--widget)
         (session chirp-web--ready)
         (id (cl-incf chirp-web--serial))
         (args (vector id url method bearer type
                       (and data (base64-encode-string data t)) limit))
         (poll (format (concat "(() => {const r=window.__emacsChirpResults;"
                               "if (!r || !Object.hasOwn(r,%d)) return 'gone';"
                               "if (r[%d] === null) return 'wait';"
                               "const v=r[%d]; delete r[%d]; return JSON.stringify(v)})()")
                       id id id id))
         timer poll-timer (interval 0.03) (retry-read (equal method "GET")))
    (cl-labels
        ((finish (status body)
           (when timer
             (cancel-timer timer)
             (when poll-timer (cancel-timer poll-timer))
             (remhash id chirp-web--callbacks)
             (setq timer nil)
             (funcall callback status body)))
         (start ()
           (chirp-web--execute
            widget id (concat chirp-web--fetch-script "(..." (json-serialize args :null-object nil) ")")
            (lambda (started)
              (when timer
                (pcase started
                  ("started" (check))
                  ("signin"
                   (when (eq session chirp-web--ready) (setq chirp-web--ready nil))
                   (finish 0 "Sign in with M-x chirp"))
                  (_ (page-changed)))))))
         (page-changed ()
           (if (and retry-read (eq session chirp-web--ready) (xwidget-live-p widget))
               (progn (setq retry-read nil) (start))
             (finish 0 "X browser session changed")))
         (check ()
           (if (xwidget-live-p widget)
               (chirp-web--execute
                widget id poll
                (lambda (value)
                  (when timer
                    (cond
                     ((equal value "wait")
                      (setq poll-timer (run-at-time interval nil #'check)
                            interval (min 0.15 (* 2 interval))))
                     ((or (not (stringp value)) (equal value "gone"))
                      (page-changed))
                     (t
                      (let ((response (condition-case nil
                                          (json-parse-string value)
                                        (json-parse-error nil))))
                        (if (and (vectorp response) (= 2 (length response))
                                 (integerp (aref response 0)) (stringp (aref response 1)))
                            (finish (aref response 0) (aref response 1))
                          (finish 0 "Invalid X browser response"))))))))
             (finish 0 "X browser session closed or timed out"))))
      (setq timer (run-at-time 70 nil #'finish 0 "X browser request timed out"))
      (start))))

(provide 'chirp-web)

;;; chirp-web.el ends here
