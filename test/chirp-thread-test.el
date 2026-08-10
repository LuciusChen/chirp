;;; chirp-thread-test.el --- Tests for Chirp thread loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-thread)

(ert-deftest chirp-thread-open-renders-seed-focus-tweet-before-network-thread-load ()
  "Opening a thread from a visible tweet should render that tweet immediately."
  (let ((buffer (generate-new-buffer " *chirp-thread-seed-test*"))
        thread-callback
        renders)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'thread-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'thread-token)))
                  ((symbol-function 'chirp-backend-thread)
                   (lambda (_target callback &optional _errback)
                     (setq thread-callback callback)))
                  ((symbol-function 'chirp-backend-article) #'ignore)
                  ((symbol-function 'chirp-thread--render-view)
                   (lambda (_buffer _title _refresh ordered &optional _anchor-id _display-p)
                     (push ordered renders)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (chirp-thread-open
           '(:kind tweet
             :id "123"
             :text "Focus tweet")
           "123"
           buffer)
          (should (functionp thread-callback))
          (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                 (car (last renders)))
                         '("123")))
          (funcall thread-callback
                   (list '(:kind tweet :id "123" :text "Focus tweet")
                         '(:kind tweet :id "456" :text "Reply"))
                   nil)
          (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                 (car renders))
                         '("123" "456"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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

(ert-deftest chirp-thread-open-filters-keyword-spam-replies ()
  "Thread loading should hide matching replies without hiding the focus tweet."
  (let ((buffer (generate-new-buffer " *chirp-thread-spam-test*"))
        (chirp-thread-spam-keywords '("dm me" "t.me/" "推广昵称" "  "))
        thread-callback
        rendered)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'thread-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'thread-token)))
                  ((symbol-function 'chirp-backend-thread)
                   (lambda (_target callback &optional _errback)
                     (setq thread-callback callback)))
                  ((symbol-function 'chirp-backend-article) #'ignore)
                  ((symbol-function 'chirp-thread--render-view)
                   (lambda (_buffer _title _refresh ordered
                            &optional _anchor-id _display-p)
                     (setq rendered ordered)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (chirp-thread-open
           '(:kind tweet :id "123" :text "DM me is quoted in the focus")
           "123"
           buffer)
          (should (functionp thread-callback))
          (funcall thread-callback
                   (list
                    '(:kind tweet :id "123" :text "DM me is quoted in the focus")
                    '(:kind tweet :id "spam-text" :text "Please DM ME for support")
                    '(:kind tweet :id "spam-url" :text "More details"
                      :urls ("https://t.me/example"))
                    '(:kind tweet :id "spam-author" :text "Ordinary reply"
                      :author-name "这是推广昵称")
                    '(:kind tweet :id "related" :text "DM me for context"
                      :timeline-context related)
                    '(:kind tweet :id "legit" :text "Useful reply"))
                   nil)
          (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) rendered)
                         '("123" "related" "legit"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-thread-spam-keywords-default-to-collected-templates ()
  "Spam defaults should contain collected templates but omit broad terms."
  (dolist (template '("三网优化专线"
                      "刚放个人主页上了"
                      "比她好看的没她骚"
                      "我福不黑不信你看"
                      "应该没人比我玩的开"
                      "應該沒人比我玩得開"
                      "线下sao货"
                      "比我好看的没我骚"
                      "有人想锐评一下我的福嘛"
                      "check my bio asappp"
                      "dm me or follow back"
                      "no upfront payment is required until after a successful recovery"))
    (should (member template chirp-thread-spam-keywords)))
  (should (member '("体制内幼师" "sao的很")
                  chirp-thread-spam-keywords))
  (dolist (template '(("Deepcoin" "大户返佣")
                      ("找炮友" "点主页")
                      ("同城上门" "线下选妃")
                      ("Gate" "Visa卡")
                      ("催情" "听话")
                      ("只入身体" "不入生活")))
    (should (member template chirp-thread-spam-keywords)))
  (dolist (broad-term '("主页" "私信" "微信" "带单" "稳赚" "空投" "DM me"))
    (should-not (member broad-term chirp-thread-spam-keywords)))
  (dolist (legitimate-template '("空投详情现已公布" "更多详情请查看公告"))
    (should-not (member legitimate-template chirp-thread-spam-keywords))))

(ert-deftest chirp-thread-default-spam-rules-have-no-redundant-overlap ()
  "Default spam rules should not duplicate or subsume one another."
  (let ((rules chirp-spam-rules-default))
    (cl-labels
        ((normalized (fragment)
           (and (stringp fragment)
                (let ((trimmed (string-trim fragment)))
                  (unless (string-empty-p trimmed)
                    trimmed))))
         (fragments (rule)
           (if (listp rule) rule (list rule)))
         (fragment-subsumes-p (broad narrow)
           (when-let* ((broad (normalized broad))
                       (narrow (normalized narrow)))
             (let ((case-fold-search t))
               (string-match-p (regexp-quote broad) narrow))))
         (rule-subsumes-p (broad narrow)
           (let ((broad-fragments (fragments broad))
                 (narrow-fragments (fragments narrow)))
             (or (null narrow-fragments)
                 (and broad-fragments
                      (cl-every
                       (lambda (broad-fragment)
                         (cl-some
                          (lambda (narrow-fragment)
                            (fragment-subsumes-p
                             broad-fragment narrow-fragment))
                          narrow-fragments))
                       broad-fragments))))))
      (should-not
       (cl-loop for tail on rules
                thereis
                (cl-some
                 (lambda (other)
                   (or (rule-subsumes-p (car tail) other)
                       (rule-subsumes-p other (car tail))))
                 (cdr tail)))))))

(ert-deftest chirp-thread-default-spam-rules-match-collected-variants ()
  "Default spam rules should match collected evasive variants."
  (dolist (text '("线下sao货没人pK比她sao😂👌 y7N"
                  "线下sao货pK比她sao🎍🎼比我骚的没我好看"
                  "比我好看的没我骚🎍🎼比我骚的没我好看"
                  "我果然太涩了🌜🤲有人想锐评一下我的福嘛"
                  "体制内幼师🌻📣sao的很Q1"))
    (should (chirp-thread--spam-reply-p (list :text text))))
  (dolist (text '("我是一名体制内幼师"
                  "空投详情现已公布"
                  "更多详情请查看公告"))
    (should-not (chirp-thread--spam-reply-p (list :text text)))))

(ert-deftest chirp-thread-default-spam-rules-match-collected-author-variants ()
  "Default spam rules should match collected author-name templates."
  (dolist (name '("深币Deepcoin93%大户返佣"
                  "草莓熊🍑找炮友🍑点主页🍑"
                  "方露🌸同城上门♥线下选妃"
                  "返85 Gate·Visa卡可领"
                  "返佣85·Gate｜Visa卡免费"
                  "👈催情💊春💊男用💊听话 🧳 🎌 🍀"))
    (should
     (chirp-thread--spam-reply-p
      (list :text "普通回复" :author-name name))))
  (dolist (text '("只入身体🌱🪐不入生活。"
                  "只入身体🍁🍁不入生活。"))
    (should (chirp-thread--spam-reply-p (list :text text))))
  (dolist (name '("Deepcoin 使用体验"
                  "Gate 平台"
                  "Visa卡可领"
                  "同城生活"
                  "请勿轻信催情药"))
    (should-not
     (chirp-thread--spam-reply-p
      (list :text "普通回复" :author-name name)))))

(ert-deftest chirp-thread-spam-keyword-groups-require-every-fragment ()
  "Grouped spam rules should require every configured fragment."
  (let ((chirp-thread-spam-keywords '(("体制内幼师" "sao的很"))))
    (should
     (chirp-thread--spam-reply-p
      '(:text "体制内幼师🌻📣sao的很Q1")))
    (should-not
     (chirp-thread--spam-reply-p '(:text "体制内幼师的日常")))
    (should-not
     (chirp-thread--spam-reply-p '(:text "这个说法 sao的很")))))

(ert-deftest chirp-thread-spam-keywords-match-author-nickname-and-handle ()
  "Spam rules should inspect reply author display names and handles."
  (let ((chirp-thread-spam-keywords '("推广昵称" "spam_handle")))
    (should
     (chirp-thread--spam-reply-p
      '(:text "普通回复" :author-name "这是推广昵称" :author-handle "alice")))
    (should
     (chirp-thread--spam-reply-p
      '(:text "普通回复" :author-name "Alice" :author-handle "Spam_Handle_01")))
    (should-not
     (chirp-thread--spam-reply-p
      '(:text "普通回复" :author-name "Alice" :author-handle "alice")))
    (should-not
     (chirp-thread--spam-reply-p
      '(:text "推广昵称" :author-name "推广昵称" :author-handle "spam_handle"
        :timeline-context related)))))

(provide 'chirp-thread-test)

;;; chirp-thread-test.el ends here
