;;; chirp-actions-test.el --- Tests for compose actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
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

(provide 'chirp-actions-test)

;;; chirp-actions-test.el ends here
