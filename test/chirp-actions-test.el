;;; chirp-actions-test.el --- Tests for compose actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'transient)
(require 'chirp-actions)

(defun chirp-test--make-compose-buffer (body)
  "Return a cons of compose and source buffers seeded with BODY."
  (let ((compose (generate-new-buffer " *chirp-compose-test*"))
        (source (generate-new-buffer " *chirp-source-test*")))
    (with-current-buffer compose
      (chirp-compose-mode)
      (setq-local chirp-compose-kind 'post)
      (setq-local chirp-compose-source-buffer source)
      (setq-local chirp-compose-attachments nil)
      (setq-local chirp-compose-temp-attachments nil)
      (setq-local chirp-compose-sending nil)
      (erase-buffer)
      (setq-local chirp-compose-body-start-marker (copy-marker (point-min)))
      (insert body)
      (setq-local chirp-compose-body-end-marker (copy-marker (point-max) t)))
    (cons compose source)))

(defun chirp-test--open-compose-from-foreign-current-buffer (kind &optional tweet)
  "Open compose KIND while `current-buffer' is not the displayed source buffer.

Return a list of (compose source foreign)."
  (let ((source (generate-new-buffer " *chirp-source-window*"))
        (foreign (generate-new-buffer " *chirp-transient*"))
        compose)
    (with-current-buffer source
      (chirp-view-mode)
      (setq-local chirp--view-title "For You"))
    (switch-to-buffer source)
    (with-current-buffer foreign
      (chirp-compose-open kind tweet)
      (setq compose (current-buffer)))
    (list compose source foreign)))

(ert-deftest chirp-compose-send-closes-buffer-immediately ()
  "Sending should close the compose buffer before the backend replies."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello world")))
    (let (captured-args)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-actions--perform)
                       (lambda (args _on-success &optional _on-error)
                         (setq captured-args args))))
              (with-current-buffer compose
                (chirp-compose-send)))
            (should (equal captured-args '("post" "hello world")))
            (should-not (buffer-live-p compose)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-send-cleans-temp-files-after-success ()
  "Temporary attachments should stay alive until the async send finishes."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "photo post")))
    (let* ((temp-file (make-temp-file "chirp-compose-test-" nil ".png"))
           success-callback)
      (with-temp-file temp-file
        (insert "png"))
      (unwind-protect
          (progn
            (with-current-buffer compose
              (setq-local chirp-compose-attachments (list temp-file))
              (setq-local chirp-compose-temp-attachments (list temp-file)))
            (cl-letf (((symbol-function 'chirp-actions--perform)
                       (lambda (_args on-success &optional _on-error)
                         (setq success-callback on-success))))
              (with-current-buffer compose
                (chirp-compose-send)))
            (should (functionp success-callback))
            (should (file-exists-p temp-file))
            (funcall success-callback nil nil)
            (should-not (file-exists-p temp-file)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest chirp-compose-send-rejects-duplicate-submit ()
  "Sending should reject a second submit while a draft is marked in flight."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "duplicate")))
    (let ((perform-count 0))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-actions--perform)
                       (lambda (&rest _args)
                         (setq perform-count (1+ perform-count)))))
              (with-current-buffer compose
                (setq-local chirp-compose-sending t)
                (should-error (chirp-compose-send) :type 'user-error)))
            (should (= perform-count 0)))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-paste-image-uses-chirp-temp-directory ()
  "Clipboard pastes should create temporary files under Chirp's own cache tree."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "")))
    (let* ((temp-root (make-temp-file "chirp-compose-dir-" t))
           (chirp-compose-temporary-directory temp-root)
           pasted-path)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-compose--clipboard-image-backend)
                       (lambda ()
                         '(:kind stdout :program "fake" :args nil :extension ".png")))
                      ((symbol-function 'chirp-compose--paste-image-to-file)
                       (lambda (_backend file)
                         (setq pasted-path file)
                         (with-temp-file file
                           (insert "png"))
                         t)))
              (with-current-buffer compose
                (chirp-compose-paste-image)
                (should (string-prefix-p (file-name-as-directory temp-root)
                                         pasted-path))
                (should (member pasted-path chirp-compose-attachments))
                (should (member pasted-path chirp-compose-temp-attachments)))))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))
        (when (file-directory-p temp-root)
          (delete-directory temp-root t))))))

(ert-deftest chirp-compose-empty-body-allows-insert-and-delete ()
  "An empty compose buffer should keep the body editable."
  (let ((buffer (generate-new-buffer " *chirp-compose-empty*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-compose-mode)
          (setq-local chirp-compose-kind 'post)
          (setq-local chirp-compose-attachments nil)
          (setq-local chirp-compose-temp-attachments nil)
          (chirp-compose--refresh-display)
          (goto-char (marker-position chirp-compose-body-start-marker))
          (insert "abc")
          (should (equal (chirp-compose--current-body) "abc"))
          (delete-char -1)
          (should (equal (chirp-compose--current-body) "ab")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-compose-refresh-keeps-body-editable-at-footer-boundary ()
  "Refreshing compose chrome should not make the body read-only at the end."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello")))
    (unwind-protect
        (with-current-buffer compose
          (chirp-compose--refresh-display)
          (goto-char (marker-position chirp-compose-body-end-marker))
          (insert "!")
          (should (equal (chirp-compose--current-body) "hello!"))
          (delete-char -1)
          (should (equal (chirp-compose--current-body) "hello")))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-open-uses-displayed-source-buffer-for-post-reply-and-quote ()
  "Compose buffers should capture the displayed Chirp view as their source."
  (dolist (case `((post nil)
                  (reply ,(list :kind 'tweet :id "123" :author-handle "alice"))
                  (quote ,(list :kind 'tweet :id "123" :author-handle "alice"))))
    (pcase-let* ((`(,kind ,tweet) case)
                 (`(,compose ,source ,foreign)
                  (chirp-test--open-compose-from-foreign-current-buffer kind tweet)))
      (unwind-protect
          (with-current-buffer compose
            (should (eq chirp-compose-source-buffer source)))
        (dolist (buffer (list compose source foreign))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest chirp-compose-source-buffer-prefers-minibuffer-origin-view ()
  "Source buffer lookup should prefer the minibuffer-origin Chirp view."
  (let ((thread (generate-new-buffer " *chirp-thread-source*"))
        (home (generate-new-buffer " *chirp-home-source*"))
        (foreign (generate-new-buffer " *chirp-transient*")))
    (unwind-protect
        (progn
          (with-current-buffer home
            (chirp-view-mode)
            (setq-local chirp--view-title "For You"))
          (with-current-buffer thread
            (chirp-view-mode)
            (setq-local chirp--view-title "Thread"))
          (with-current-buffer foreign
            (cl-letf (((symbol-function 'active-minibuffer-window)
                       (lambda ()
                         t))
                      ((symbol-function 'selected-window)
                       (lambda ()
                         'selected-win))
                      ((symbol-function 'minibuffer-selected-window)
                       (lambda ()
                         'origin-win))
                      ((symbol-function 'window-live-p)
                       (lambda (window)
                         (memq window '(selected-win origin-win))))
                      ((symbol-function 'window-buffer)
                       (lambda (window)
                         (pcase window
                           ('selected-win home)
                           ('origin-win thread)
                           (_ foreign)))))
              (should (eq (chirp-compose--source-buffer) thread)))))
      (dolist (buffer (list thread home foreign))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-quote-send-closes-buffer-and-restores-source-view ()
  "Quote sending should close the compose buffer and return to the source view."
  (pcase-let* ((`(,compose ,source ,foreign)
                (chirp-test--open-compose-from-foreign-current-buffer
                 'quote
                 (list :kind 'tweet
                       :id "123"
                       :author-handle "alice"
                       :url "https://x.com/alice/status/123"))))
    (let (captured-args)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-actions--perform)
                       (lambda (args _on-success &optional _on-error)
                         (setq captured-args args))))
              (with-current-buffer compose
                (goto-char (marker-position chirp-compose-body-start-marker))
                (insert "hello quote")
                (chirp-compose-send)))
            (should (equal captured-args '("quote" "123" "hello quote")))
            (should-not (buffer-live-p compose))
            (should (eq (window-buffer (selected-window)) source)))
        (dolist (buffer (list compose source foreign))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest chirp-compose-send-deletes-compose-window-when-source-is-still-visible ()
  "Sending should remove the extra compose window instead of duplicating the source view."
  (let ((source (generate-new-buffer " *chirp-source-window*"))
        (compose (generate-new-buffer " *chirp-compose-window*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((source-window (selected-window))
                (compose-window (split-window-below)))
            (with-current-buffer source
              (chirp-view-mode)
              (setq-local chirp--view-title "For You"))
            (set-window-buffer source-window source)
            (set-window-buffer compose-window compose)
            (select-window compose-window)
            (with-current-buffer compose
              (chirp-compose-mode)
              (setq-local chirp-compose-kind 'post)
              (setq-local chirp-compose-source-buffer source)
              (setq-local chirp-compose-attachments nil)
              (setq-local chirp-compose-temp-attachments nil)
              (setq-local chirp-compose-sending nil)
              (erase-buffer)
              (setq-local chirp-compose-body-start-marker (copy-marker (point-min)))
              (insert "hello world")
              (setq-local chirp-compose-body-end-marker (copy-marker (point-max) t)))
            (cl-letf (((symbol-function 'chirp-actions--perform)
                       (lambda (_args _on-success &optional _on-error)
                         nil)))
              (with-current-buffer compose
                (chirp-compose-send)))
            (should-not (buffer-live-p compose))
            (should (= (length (window-list nil 'no-minibuf)) 1))
            (should (eq (window-buffer (selected-window)) source))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(defun chirp-test--with-tweet-buffer (tweet fn)
  "Create a temporary Chirp view buffer with TWEET and call FN inside it."
  (let ((buffer (generate-new-buffer " *chirp-action-tweet*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (let ((inhibit-read-only t))
            (insert "tweet\n")
            (add-text-properties
             (point-min) (point-max)
             `(chirp-entry-item ,tweet))
            (put-text-property (point-min) (1+ (point-min)) 'chirp-entry-start t)
            (goto-char (point-min)))
          (funcall fn buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-toggle-like-at-point-uses-like-and-unlike-commands ()
  "Toggle like should choose the backend command from the current local state."
  (clrhash chirp-tweet-state-overrides)
  (let (captured-args rerendered)
    (unwind-protect
        (progn
          (chirp-test--with-tweet-buffer
           '(:kind tweet :id "123" :liked-p nil :like-count 10)
           (lambda (_buffer)
             (cl-letf (((symbol-function 'chirp-actions--perform)
                        (lambda (args on-success &optional _on-error)
                          (setq captured-args args)
                          (funcall on-success nil nil)))
                       ((symbol-function 'chirp-request-rerender)
                        (lambda (&optional _buffer _delay)
                          (setq rerendered t))))
               (chirp-toggle-like-at-point)
               (should (equal captured-args '("like" "123")))
               (should rerendered)
               (should (plist-get (chirp-entry-at-point) :liked-p))
               (should (= (plist-get (chirp-entry-at-point) :like-count) 11)))))
          (setq captured-args nil
                rerendered nil)
          (chirp-test--with-tweet-buffer
           '(:kind tweet :id "123" :liked-p t :like-count 10)
           (lambda (_buffer)
             (cl-letf (((symbol-function 'chirp-actions--perform)
                        (lambda (args on-success &optional _on-error)
                          (setq captured-args args)
                          (funcall on-success nil nil)))
                       ((symbol-function 'chirp-request-rerender)
                        (lambda (&optional _buffer _delay)
                          (setq rerendered t))))
               (chirp-toggle-like-at-point)
               (should (equal captured-args '("unlike" "123")))
               (should rerendered)
               (should-not (plist-get (chirp-entry-at-point) :liked-p))
               (should (= (plist-get (chirp-entry-at-point) :like-count) 9))))))
      (clrhash chirp-tweet-state-overrides))))

(ert-deftest chirp-dispatch-uses-toggle-actions-for-stateful-tweet-actions ()
  "The Chirp transient should expose only toggle entries for like/RT/bookmark."
  (let ((home-suffix (transient-get-suffix 'chirp-dispatch "h"))
        (following-suffix (transient-get-suffix 'chirp-dispatch "f"))
        (bookmarks-suffix (transient-get-suffix 'chirp-dispatch "b"))
        (liked-suffix (transient-get-suffix 'chirp-dispatch "L"))
        (list-suffix (transient-get-suffix 'chirp-dispatch "s"))
        (retweet-suffix (transient-get-suffix 'chirp-dispatch "R"))
        (like-suffix (transient-get-suffix 'chirp-dispatch "l"))
        (bookmark-suffix (transient-get-suffix 'chirp-dispatch "B"))
        (copy-suffix (transient-get-suffix 'chirp-dispatch "y")))
    (should (eq (plist-get (cdr home-suffix) :command) 'chirp-timeline-open-home))
    (should (eq (plist-get (cdr following-suffix) :command) 'chirp-timeline-open-following))
    (should (eq (plist-get (cdr bookmarks-suffix) :command) 'chirp-timeline-open-bookmarks))
    (should (eq (plist-get (cdr liked-suffix) :command) 'chirp-timeline-open-likes))
    (should (eq (plist-get (cdr list-suffix) :command) 'chirp-timeline-open-list))
    (should (eq (plist-get (cdr retweet-suffix) :command) 'chirp-toggle-retweet-at-point))
    (should (eq (plist-get (cdr like-suffix) :command) 'chirp-toggle-like-at-point))
    (should (eq (plist-get (cdr bookmark-suffix) :command) 'chirp-toggle-bookmark-at-point))
    (should (eq (plist-get (cdr copy-suffix) :command) 'chirp-copy-fixupx-url-at-point))
    (should-error (transient-get-suffix 'chirp-dispatch "T"))
    (should-error (transient-get-suffix 'chirp-dispatch "u"))
    (should-error (transient-get-suffix 'chirp-dispatch "U"))))

(ert-deftest chirp-copy-fixupx-url-at-point-copies-rewritten-url ()
  "Copy action should rewrite tweet URLs from x.com to fixupx.com."
  (let (captured-url last-message)
    (chirp-test--with-tweet-buffer
     '(:kind tweet
       :id "123"
       :author-handle "alice"
       :url "https://x.com/alice/status/123")
     (lambda (_buffer)
       (cl-letf (((symbol-function 'kill-new)
                  (lambda (text &optional _replace)
                    (setq captured-url text)))
                 ((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (setq last-message (apply #'format format-string args)))))
         (chirp-copy-fixupx-url-at-point)
         (should (equal captured-url "https://fixupx.com/alice/status/123"))
         (should (equal last-message
                        "Copied https://fixupx.com/alice/status/123")))))))

(provide 'chirp-actions-test)

;;; chirp-actions-test.el ends here
