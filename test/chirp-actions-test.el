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

(provide 'chirp-actions-test)

;;; chirp-actions-test.el ends here
