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
(defvar chirp-web--ready nil
  "Identity of the current signed-in browser session, or nil.")
(defvar chirp-web--serial 0)

(defun chirp-web-ready-p ()
  "Return non-nil when the X browser session is ready for requests."
  (and chirp-web--ready (fboundp 'xwidget-live-p)
       (xwidget-live-p chirp-web--widget)))

(defconst chirp-web--login-script
  "location.origin === 'https://x.com' && document.readyState !== 'loading' &&
   (/^\\/home\\/?$/.test(location.pathname) ||
    (!location.pathname.startsWith('/i/flow/') &&
     document.querySelector('[data-testid=\"AppTabBar_Home_Link\"]'))) &&
   /(?:^|;\\s*)ct0=[^;]+/.test(document.cookie) ? 'ready' : ''"
  "Detect sign-in inside the current X document after client-side navigation.")

(defun chirp-web--check-login (callback deadline)
  "Run CALLBACK once the X home session is ready, or stop at DEADLINE."
  (let* ((widget chirp-web--widget)
         (timer chirp-web--login-timer)
         (live (xwidget-live-p widget)))
    (cond
     ((or (not live) (> (float-time) deadline))
      (cancel-timer timer)
      (setq chirp-web--login-timer nil)
      (when live (message "X sign-in timed out. Run M-x chirp-login to resume.")))
     (t
      (xwidget-webkit-execute-script
       widget chirp-web--login-script
       (lambda (state)
         (when (and (eq timer chirp-web--login-timer) (equal state "ready"))
           (cancel-timer timer)
           (setq chirp-web--login-timer nil chirp-web--ready (gensym "chirp-web-session-"))
           (let* ((buffer (xwidget-buffer widget))
                  (window (get-buffer-window buffer t)))
             (quit-windows-on buffer)
             (if (window-live-p window)
                 (with-selected-window window (funcall callback))
               (funcall callback))))))))))

(defun chirp-web--confirm-kill ()
  "Ask before closing the browser buffer that holds Chirp's X session."
  (or (not (and (xwidget-live-p chirp-web--widget)
                (eq (xwidget-buffer chirp-web--widget) (current-buffer))))
      (yes-or-no-p "Close Chirp’s X browser session? ")))

(defun chirp-web-login (callback)
  "Open X sign-in and call CALLBACK when the session is ready."
  (unless (and (display-graphic-p) (fboundp 'xwidget-webkit-execute-script)
               (require 'xwidget nil t))
    (user-error "Open Chirp in a graphical Emacs with WebKit to sign in"))
  (let ((login "https://x.com/i/flow/login?redirect_after_login=%2Fhome"))
    (if (xwidget-live-p chirp-web--widget)
        (progn
          (pop-to-buffer (xwidget-buffer chirp-web--widget))
          (unless chirp-web--login-timer
            (xwidget-webkit-goto-uri chirp-web--widget login)))
      (xwidget-webkit-new-session login)
      (setq chirp-web--widget (xwidget-webkit-current-session))
      (unless (xwidget-live-p chirp-web--widget)
        (user-error "WebKit could not open the X sign-in page"))
      (set-xwidget-query-on-exit-flag chirp-web--widget nil)
      (with-current-buffer (xwidget-buffer chirp-web--widget)
        (add-hook 'kill-buffer-query-functions #'chirp-web--confirm-kill nil t)))
    (message "Sign in to X here. Chirp will open when the session is ready.")
    (when chirp-web--login-timer (cancel-timer chirp-web--login-timer))
    (setq chirp-web--ready nil
          chirp-web--login-timer (run-at-time 0.5 0.75 #'chirp-web--check-login
                                             callback (+ (float-time) 600)))))

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
             (setq timer nil)
             (funcall callback status body)))
         (start ()
           (xwidget-webkit-execute-script
            widget (concat chirp-web--fetch-script "(..." (json-serialize args :null-object nil) ")")
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
               (xwidget-webkit-execute-script
                widget poll
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
