;;; chirp-actions-test.el --- Tests for compose actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'transient)
(require 'chirp-actions)
(require 'chirp-render)

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

(ert-deftest chirp-compose-completes-mention-handles-and-caches-query ()
  "Mention completion should replace only the handle and reuse its lookup."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello @em")))
    (let ((request-count 0))
      (unwind-protect
          (cl-letf (((symbol-function 'run-at-time) (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'chirp-backend-search-users)
                     (lambda (query ok _fail &optional max-results)
                       (setq request-count (1+ request-count))
                       (should (equal query "em"))
                       (should-not max-results)
                       (funcall ok '((:handle "emacs") (:handle "emacslife")) nil))))
            (with-current-buffer compose
              (goto-char (marker-position chirp-compose-body-end-marker))
              (let ((point-before (point))
                    (completion (chirp-compose-mention-completion-at-point)))
                (should (equal (buffer-substring-no-properties
                                (nth 0 completion) (nth 1 completion))
                               "em"))
                (should (equal (all-completions "em" (nth 2 completion))
                               '("emacs" "emacslife")))
                (should (= (point) point-before)))
              (chirp-compose-mention-completion-at-point)
              (should (= request-count 1))))
        (when (buffer-live-p compose)
          (kill-buffer compose))
        (when (buffer-live-p source)
          (kill-buffer source))))))

(ert-deftest chirp-compose-mention-delivers-to-the-existing-completion-table ()
  (pcase-let ((`(,draft . ,source) (chirp-test--make-compose-buffer "@em")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer draft)
          (goto-char chirp-compose-body-end-marker)
          (let (timer callback)
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_delay _repeat fn &rest args) (setq timer (lambda () (apply fn args)))))
                      ((symbol-function 'chirp-backend-search-users)
                       (lambda (_query ok _err) (setq callback ok)))
                      ((symbol-function 'completion-help-at-point)
                       (lambda () (ert-fail "Background lookup opened a completion window"))))
              (let* ((capf (run-hook-with-args-until-success 'completion-at-point-functions))
                     (table (nth 2 capf)))
                (should-not (all-completions "em" table))
                (funcall timer)
                (funcall callback '((:handle "emacs" :name "GNU Emacs")) nil)
                (should (equal (all-completions "em" table) '("emacs")))
                (should (equal (funcall (plist-get (nthcdr 3 capf) :annotation-function) "emacs")
                               "  GNU Emacs"))
                (should (equal (chirp-compose--current-body) "@em"))
                (insert "a")
                (let ((next (run-hook-with-args-until-success 'completion-at-point-functions)))
                  (should (equal (all-completions "ema" (nth 2 next)) '("emacs"))))
                (call-interactively (key-binding (kbd "M-TAB")))
                (should (equal (chirp-compose--current-body) "@emacs"))))))
      (kill-buffer draft)
      (kill-buffer source))))

(ert-deftest chirp-compose-does-not-complete-email-addresses ()
  "An @ inside an email address should not start user completion."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "mail emacs@example")))
    (unwind-protect
        (with-current-buffer compose
          (goto-char (marker-position chirp-compose-body-end-marker))
          (should-not (chirp-compose-mention-completion-at-point)))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-send-retains-draft-until-confirmed ()
  "Keep the pending draft until the backend replies."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "hello world")))
    (let (captured-args)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest args)
                         (setq captured-args (list (plist-get args :kind) (plist-get args :target-id) (plist-get args :text))))))
              (with-current-buffer compose
                (chirp-compose-send)))
            (should (equal captured-args '(post nil "hello world")))
            (should (buffer-live-p compose))
            (should (buffer-local-value 'chirp-compose-sending compose)))
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
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest args)
                         (setq success-callback (plist-get args :callback)))))
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

(ert-deftest chirp-compose-failure-restores-body-and-attachment ()
  "A failed submission leaves the same editable draft and its temporary file."
  (pcase-let* ((`(,draft . ,source) (chirp-test--make-compose-buffer "Keep this"))
               (file (make-temp-file "chirp-draft-"))
               (fail nil))
    (unwind-protect
        (with-current-buffer draft
          (setq chirp-compose-attachments (list file)
                chirp-compose-temp-attachments (list file))
          (cl-letf (((symbol-function 'chirp-backend-compose)
                     (lambda (&rest args) (setq fail (plist-get args :errback)))))
            (chirp-compose-send))
          (funcall fail "Connection interrupted")
          (should-not chirp-compose-sending)
          (should-not buffer-read-only)
          (should (equal (chirp-compose--body-text) "Keep this"))
          (should (file-exists-p file)))
      (kill-buffer draft)
      (kill-buffer source)
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest chirp-compose-send-rejects-duplicate-submit ()
  "Sending should reject a second submit while a draft is marked in flight."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "duplicate")))
    (let ((perform-count 0))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'chirp-backend-compose)
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

(ert-deftest chirp-compose-q-self-inserts ()
  "Compose buffers should leave `q' available for normal text insertion."
  (let ((buffer (generate-new-buffer " *chirp-compose-q*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-compose-mode)
          (should (eq (key-binding (kbd "q")) #'self-insert-command)))
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
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest args)
                         (setq captured-args
                               (list (plist-get args :kind)
                                     (plist-get args :target-id)
                                     (plist-get args :target-handle)
                                     (plist-get args :text))))))
              (with-current-buffer compose
                (goto-char (marker-position chirp-compose-body-start-marker))
                (insert "hello quote")
                (chirp-compose-send)))
            (should (equal captured-args '(quote "123" "alice" "hello quote")))
            (should (buffer-live-p compose))
            (should (buffer-local-value 'chirp-compose-sending compose))
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
            (cl-letf (((symbol-function 'chirp-backend-compose)
                       (lambda (&rest _args)
                         nil)))
              (with-current-buffer compose
                (chirp-compose-send)))
            (should (buffer-live-p compose))
            (should (buffer-local-value 'chirp-compose-sending compose))
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

(defun chirp-test--with-user-buffer (user fn)
  "Create a temporary Chirp view buffer with USER and call FN inside it."
  (let ((buffer (generate-new-buffer " *chirp-action-user*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-view-mode)
          (let ((inhibit-read-only t))
            (insert "user\n")
            (add-text-properties
             (point-min) (point-max)
             `(chirp-entry-item ,user))
            (put-text-property (point-min) (1+ (point-min)) 'chirp-entry-start t)
            (goto-char (point-min)))
          (funcall fn buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-actions-mouse-action-targets-clicked-tweet ()
  "Mouse tweet metrics should dispatch their action for the clicked tweet."
  (let ((buffer (generate-new-buffer " *chirp-mouse-action-test*"))
        (first-tweet '(:kind tweet
                       :id "first"
                       :text "First tweet"
                       :author-name "Alice"
                       :author-handle "alice"
                       :reply-count 1
                       :retweet-count 2
                       :like-count 3
                       :quote-count 4
                       :bookmark-count 5
                       :view-count 6))
        (clicked-tweet '(:kind tweet
                         :id "clicked"
                         :text "Clicked tweet"
                         :author-name "Bob"
                         :author-handle "bob"
                         :reply-count 7
                         :retweet-count 8
                         :like-count 9
                         :quote-count 10
                         :bookmark-count 11
                         :view-count 12))
        dispatched
        event-position)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (with-current-buffer buffer
            (chirp-view-mode)
            (cl-letf (((symbol-function 'chirp-media-avatar-image)
                       (lambda (&rest _args) nil)))
              (let ((inhibit-read-only t))
                (chirp-render-insert-tweet first-tweet)
                (chirp-render-insert-tweet clicked-tweet)))
            (let (positions)
              (dolist (action '(reply retweet like bookmark))
                (goto-char (point-min))
                (let (position)
                  (while (< (point) (point-max))
                    (when (eq (get-text-property (point) 'chirp-tweet-action)
                              action)
                      (setq position (point)))
                    (goto-char
                     (or (next-single-property-change
                          (point) 'chirp-tweet-action nil (point-max))
                         (point-max))))
                  (push (cons action position) positions)))
              (cl-letf (((symbol-function 'chirp-reply-at-point)
                         (lambda ()
                           (setq dispatched
                                 (list 'reply (chirp-entry-id-at-point)))))
                        ((symbol-function 'chirp-toggle-retweet-at-point)
                         (lambda ()
                           (setq dispatched
                                 (list 'retweet (chirp-entry-id-at-point)))))
                        ((symbol-function 'chirp-toggle-like-at-point)
                         (lambda ()
                           (setq dispatched
                                 (list 'like (chirp-entry-id-at-point)))))
                        ((symbol-function 'chirp-toggle-bookmark-at-point)
                         (lambda ()
                           (setq dispatched
                                 (list 'bookmark (chirp-entry-id-at-point))))))
                (goto-char (point-min))
                (search-forward "Clicked tweet")
                (let ((body (1- (point))))
                  (setq event-position (list (selected-window) body '(0 . 0) nil))
                  (goto-char (point-min))
                  (cl-letf (((symbol-function 'chirp-open-at-point)
                             (lambda () (ert-fail "Plain text opened a view"))))
                    (funcall (key-binding [mouse-1]) (list 'mouse-1 event-position)))
                  (should (= (point) body)))
                (dolist (action '(reply retweet like bookmark))
                  (setq event-position
                        (list (selected-window)
                              nil
                              (cons 0 0)
                              nil
                              nil
                              (cdr (assq action positions))
                              nil
                              nil
                              nil
                              nil))
                  (funcall (key-binding [mouse-1]) (list 'mouse-1 event-position))
                  (should (equal dispatched (list action "clicked"))))))))
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

(ert-deftest chirp-follow-and-unfollow-user-at-point-use-explicit-commands ()
  "User follow actions should dispatch follow and unfollow commands."
  (let (captured-args refreshed)
    (chirp-test--with-user-buffer
     '(:kind user :handle "alice")
     (lambda (_buffer)
       (cl-letf (((symbol-function 'chirp-actions--perform)
                  (lambda (args on-success &optional _on-error)
                    (setq captured-args args)
                    (funcall on-success nil nil)))
                 ((symbol-function 'chirp-actions--refresh-buffer)
                  (lambda (_target)
                    (setq refreshed t))))
         (chirp-follow-user-at-point)
         (should (equal captured-args '("follow" "alice")))
         (should refreshed)
         (setq captured-args nil
               refreshed nil)
         (chirp-unfollow-user-at-point)
         (should (equal captured-args '("unfollow" "alice")))
         (should refreshed))))))

(ert-deftest chirp-translate-at-point-caches-and-renders-result ()
  "Translation should be stored on the current tweet and trigger a rerender."
  (clrhash chirp-tweet-state-overrides)
  (let ((chirp-translation-language "zh")
        rerendered)
    (unwind-protect
        (chirp-test--with-tweet-buffer
         '(:kind tweet :id "123" :translation nil :translation-language nil)
         (lambda (_buffer)
           (cl-letf (((symbol-function 'chirp-backend-translate)
                      (lambda (tweet-id language callback &optional _errback)
                        (should (equal tweet-id "123"))
                        (should (equal language "zh"))
                        (funcall callback
                                 '(("translation" . "你好")
                                   ("destinationLanguage" . "zh"))
                                 nil)))
                     ((symbol-function 'chirp-request-rerender)
                      (lambda (&optional _buffer _delay)
                        (setq rerendered t))))
             (chirp-translate-at-point)
             (should rerendered)
             (should (equal (plist-get (chirp-entry-at-point) :translation)
                            "你好"))
             (should (equal
                      (plist-get (gethash "123" chirp-tweet-state-overrides)
                                 :translation-language)
                      "zh")))))
      (clrhash chirp-tweet-state-overrides))))

(ert-deftest chirp-dispatch-uses-toggle-actions-for-stateful-tweet-actions ()
  "The Chirp transient should expose only toggle entries for like/RT/bookmark."
  (dolist (binding '(("h" . chirp)
                     ("f" . chirp-following)
                     ("u" . chirp-me)
                     ("b" . chirp-bookmarks)
                     ("L" . chirp-likes)
                     ("s" . chirp-list)
                     ("+" . chirp-follow-user-at-point)
                     ("-" . chirp-unfollow-user-at-point)
                     ("R" . chirp-toggle-retweet-at-point)
                     ("l" . chirp-toggle-like-at-point)
                     ("B" . chirp-toggle-bookmark-at-point)
                     ("T" . chirp-translate-at-point)
                     ("y" . chirp-copy-url-at-point)))
    (let* ((suffix (transient-get-suffix 'chirp-dispatch (car binding)))
           (properties (if (integerp (car suffix)) (nth 2 suffix) (cdr suffix))))
      (should (eq (plist-get properties :command) (cdr binding)))))
  (should-error (transient-get-suffix 'chirp-dispatch "U")))

(ert-deftest chirp-copy-url-at-point-copies-canonical-url ()
  "Copy action should copy the canonical x.com URL of the tweet at point."
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
         (chirp-copy-url-at-point)
         (should (equal captured-url "https://x.com/alice/status/123"))
         (should (equal last-message
                        "Copied https://x.com/alice/status/123")))))))

(ert-deftest chirp-compose-reuses-count-until-draft-changes ()
  (pcase-let ((`(,draft . ,source) (chirp-test--make-compose-buffer "Hi")))
    (unwind-protect
        (with-current-buffer draft
          (let ((calls 0) (chirp-compose--count-cache nil))
            (cl-letf (((symbol-function 'chirp-backend-tweet-weighted-length)
                       (lambda (text) (cl-incf calls) (length text))))
              (should (equal (chirp-compose--count-label) " 2/280"))
              (goto-char chirp-compose-body-end-marker)
              (chirp-compose--count-label)
              (should (= calls 1))
              (insert "!")
              (should (equal (chirp-compose--count-label) " 3/280"))
              (should (= calls 2)))))
      (kill-buffer draft)
      (kill-buffer source))))

(ert-deftest chirp-toggle-like-previews-and-settles-across-views ()
  (dolist (success '(nil t))
    (let ((chirp-pending-tweet-states (make-hash-table :test #'equal))
          (chirp-tweet-state-overrides (make-hash-table :test #'equal))
          (raw '((id . "321") (text . "Example") (metrics (likes . 10))))
          done fail)
      (cl-labels ((show (buffer)
                    (let ((tweet (chirp-normalize-tweet raw)))
                      (with-current-buffer buffer
                        (setq-local chirp--rerender-function
                                    (lambda ()
                                      (let ((inhibit-read-only t))
                                        (erase-buffer)
                                        (chirp-render-insert-tweet tweet)
                                        (goto-char (point-min)))))
                        (funcall chirp--rerender-function))))
                  (likes (buffer)
                    (with-current-buffer buffer
                      (let ((start (text-property-any (point-min) (point-max) 'chirp-tweet-action 'like)))
                        (buffer-substring-no-properties
                         start (next-single-property-change start 'chirp-tweet-action))))))
        (cl-letf (((symbol-function 'chirp-actions--perform)
                   (lambda (_args ok err) (setq done ok fail err)))
                  ((symbol-function 'chirp-request-rerender)
                   (lambda (buf) (with-current-buffer buf (funcall chirp--rerender-function)))))
          (with-temp-buffer
            (chirp-view-mode)
            (let ((first (current-buffer)))
              (show first)
              (chirp-toggle-like-at-point)
              (should (string-suffix-p " 11" (likes first)))
              (with-temp-buffer
                (chirp-view-mode)
                (show (current-buffer))
                (should (string-suffix-p " 11" (likes (current-buffer))))
                (if success (funcall done nil nil) (funcall fail "Rejected"))
                (dolist (buf (list first (current-buffer)))
                  (should (string-suffix-p (if success " 11" " 10") (likes buf)))
                  (with-current-buffer buf
                    (should (eq (plist-get (chirp-entry-at-point) :liked-p) success))))))))))))

(ert-deftest chirp-view-mode-binds-direct-action-shortcuts ()
  "High-frequency actions should be directly bound in `chirp-view-mode-map'."
  (dolist (binding '(("c" . chirp-compose-post)
                     ("r" . chirp-reply-at-point)
                     ("Q" . chirp-quote-at-point)
                     ("R" . chirp-toggle-retweet-at-point)
                     ("l" . chirp-toggle-like-at-point)
                     ("B" . chirp-toggle-bookmark-at-point)
                     ("T" . chirp-translate-at-point)
                     ("y" . chirp-copy-url-at-point)
                     ("i" . chirp-notifications)))
    (should (eq (lookup-key chirp-view-mode-map (kbd (car binding)))
                (cdr binding)))))

(ert-deftest chirp-compose-quote-places-card-below-body ()
  "Quote buffers should leave one editable blank line above the quoted card."
  (pcase-let* ((`(,compose ,source ,foreign)
                (chirp-test--open-compose-from-foreign-current-buffer
                 'quote
                 (list :kind 'tweet
                       :id "123"
                       :author-name "Alice"
                       :author-handle "alice"
                       :text "First line\nSecond line"
                       :url "https://x.com/alice/status/123"
                       :media '((:type "photo") (:type "photo"))))))
    (unwind-protect
        (with-current-buffer compose
          (should (= (point) (point-min)))
          (should (= (marker-position chirp-compose-body-start-marker) (point-min)))
          (should (string-prefix-p "\n\n│ Alice @alice · 2 photos\n" (buffer-string)))
          (should (string-match-p "C-c C-c" (chirp-compose--header-line)))
          (should (string-match-p "│ First line\n│ Second line" (buffer-string)))
          (should-not (string-match-p "https://x.com/alice/status/123" (buffer-string)))
          (should-not (string-match-p "No images attached" (buffer-string)))
          ;; Leaving line 1 blank and typing on the blank line above the card should work.
          (forward-line 1)
          (insert "On line 2")
          (should (equal (chirp-compose--body-text) "On line 2"))
          (should (string-prefix-p " 9/280" (chirp-compose--count-label)))
          (goto-char (marker-position chirp-compose-body-end-marker))
          (insert "!")
          (should (equal (chirp-compose--body-text) "On line 2!")))
      (dolist (buffer (list compose source foreign))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-compose-reply-shows-parent-card-above-editable-body ()
  "Reply buffers should render the parent tweet card above the editable body."
  (pcase-let* ((`(,compose ,source ,foreign)
                (chirp-test--open-compose-from-foreign-current-buffer
                 'reply
                 (list :kind 'tweet
                       :id "456"
                       :author-name "Bob"
                       :author-handle "bob"
                       :text "Hello thread"
                       :url "https://x.com/bob/status/456"))))
    (unwind-protect
        (with-current-buffer compose
          (let ((body-start (marker-position chirp-compose-body-start-marker)))
            (should (> body-start (point-min)))
            (let ((header (buffer-substring-no-properties (point-min) body-start)))
              (should (string-match-p "│ Bob @bob" header))
              (should (string-match-p "↳ Replying to @bob" header)))
            (insert "Agreed")
            (should (equal (chirp-compose--current-body) "Agreed"))))
      (dolist (buffer (list compose source foreign))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-compose-attachments-section-is-hidden-when-empty ()
  "Compose buffers should show the Images section only while images are attached."
  (pcase-let ((`(,compose . ,source) (chirp-test--make-compose-buffer ""))
              (file (make-temp-file "chirp-attach-" nil ".png")))
    (with-temp-file file (insert "png"))
    (unwind-protect
        (with-current-buffer compose
          (chirp-compose--refresh-display)
          (should-not (string-match-p "Images" (buffer-string)))
          (chirp-compose--add-attachment file t)
          (should (string-match-p "Images (1/4)" (buffer-string)))
          (should (string-match-p "Clipboard image (PNG)" (buffer-string)))
          (chirp-compose-remove-image)
          (should-not (string-match-p "Images" (buffer-string))))
      (when (buffer-live-p compose) (kill-buffer compose))
      (when (buffer-live-p source) (kill-buffer source))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest chirp-compose-cancel-confirms-only-for-nonempty-drafts ()
  "Cancelling an empty draft should not prompt, while non-empty drafts should confirm."
  (pcase-let ((`(,empty . ,source-a) (chirp-test--make-compose-buffer "   "))
              (`(,filled . ,source-b) (chirp-test--make-compose-buffer "Unsent thought")))
    (let ((prompted nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (_prompt) (setq prompted t) nil)))
              (with-current-buffer empty
                (chirp-compose-cancel))
              (should-not prompted)
              (should-not (buffer-live-p empty))
              (with-current-buffer filled
                (chirp-compose-cancel))
              (should prompted)
              (should (buffer-live-p filled)))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (with-current-buffer filled
                (chirp-compose-cancel))
              (should-not (buffer-live-p filled))))
        (dolist (buf (list empty source-a filled source-b))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest chirp-compose-clipboard-backend-falls-back-to-osascript-on-macos ()
  "macOS image paste should fall back to built-in osascript when pngpaste is missing."
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
      (let ((backend (chirp-compose--clipboard-image-backend)))
        (should (eq (plist-get backend :kind) 'filearg))
        (should (equal (plist-get backend :program) "/usr/bin/osascript"))
        (should (equal (plist-get backend :extension) ".png"))))))

(provide 'chirp-actions-test)

;;; chirp-actions-test.el ends here
