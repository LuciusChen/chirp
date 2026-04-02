;;; chirp-thread-test.el --- Tests for Chirp thread loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-thread)

(ert-deftest chirp-thread-open-prefetched-article-is-applied-before-first-render ()
  "Article enrichment should overlap thread loading and feed the first render."
  (let ((buffer (generate-new-buffer " *chirp-thread-test*"))
        article-callback
        thread-callback
        rendered)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'thread-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'thread-token)))
                  ((symbol-function 'chirp-backend-article)
                   (lambda (_tweet-id callback &optional _errback)
                     (setq article-callback callback)))
                  ((symbol-function 'chirp-backend-thread)
                   (lambda (_target callback &optional _errback)
                     (setq thread-callback callback)))
                  ((symbol-function 'chirp-thread--render-view)
                   (lambda (_buffer _title _refresh ordered &optional _anchor-id _display-p)
                     (setq rendered ordered)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (chirp-thread-open
           '(:kind tweet
             :id "123"
             :article-title "Article"
             :text ""
             :urls ("https://example.com/article"))
           "123"
           buffer)
          (should (functionp article-callback))
          (should (functionp thread-callback))
          (funcall article-callback
                   '(:kind tweet
                     :id "123"
                     :article-title "Article"
                     :article-text "Full body")
                   nil)
          (funcall thread-callback
                   (list '(:kind tweet
                           :id "123"
                           :article-title "Article"
                           :text ""
                           :urls ("https://example.com/article"))
                         '(:kind tweet
                           :id "456"
                           :text "Reply"))
                   nil)
          (should (equal (plist-get (car rendered) :article-text) "Full body")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'chirp-thread-test)

;;; chirp-thread-test.el ends here
