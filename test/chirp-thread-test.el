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
        (chirp-thread-spam-rules-file nil)
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

(ert-deftest chirp-thread-user-spam-rules-load-and-deduplicate ()
  "Persistent rules should ignore comments, blanks, and case duplicates."
  (let ((file (make-temp-file "chirp-spam-rules-")))
    (unwind-protect
        (let ((chirp-thread-spam-rules-file file))
          (with-temp-file file
            (insert "# Local rules\n\n  Promo Name  \nspam_handle\nSPAM_HANDLE\n"))
          (should (equal (chirp-thread--read-user-spam-rules)
                         '("Promo Name" "spam_handle"))))
      (delete-file file))))

(ert-deftest chirp-thread-user-spam-rules-share-reply-match-scope ()
  "One persistent rule set should inspect reply content and author identity."
  (let ((file (make-temp-file "chirp-spam-rules-"))
        (chirp-thread-spam-keywords '("built-in phrase")))
    (unwind-protect
        (let ((chirp-thread-spam-rules-file file))
          (with-temp-file file
            (insert "local phrase\nPromo Name\nspam_handle\n"))
          (should
           (equal
            (mapcar
             (lambda (tweet) (plist-get tweet :id))
             (chirp-thread--filter-spam-replies
              (list '(:id "focus" :text "built-in phrase")
                    '(:id "built-in" :text "contains BUILT-IN PHRASE")
                    '(:id "text" :text "contains LOCAL PHRASE")
                    '(:id "name" :text "ordinary" :author-name "Promo Name 01")
                    '(:id "handle" :text "ordinary" :author-handle "Spam_Handle_01")
                    '(:id "related" :text "local phrase"
                      :timeline-context related)
                    '(:id "legit" :text "ordinary" :author-name "Alice"))))
            '("focus" "related" "legit"))))
      (delete-file file))))

(ert-deftest chirp-thread-user-spam-rules-respect-filter-disable ()
  "Setting the keyword option to nil should also disable persistent rules."
  (let ((file (make-temp-file "chirp-spam-rules-"))
        (chirp-thread-spam-keywords nil))
    (unwind-protect
        (let ((chirp-thread-spam-rules-file file)
              (tweets (list '(:id "focus" :text "ordinary")
                            '(:id "reply" :text "local phrase"))))
          (with-temp-file file
            (insert "local phrase\n"))
          (should (eq (chirp-thread--filter-spam-replies tweets) tweets)))
      (delete-file file))))

(ert-deftest chirp-thread-add-spam-rule-persists-and-avoids-duplicates ()
  "Adding a rule should persist once and refresh only for a new rule."
  (let ((file (make-temp-file "chirp-spam-rules-"))
        (chirp-thread-spam-keywords '("built-in"))
        (refresh-count 0)
        initial-input)
    (unwind-protect
        (let ((chirp-thread-spam-rules-file file))
          (with-temp-buffer
            (chirp-view-mode)
            (setq-local chirp--refresh-function #'ignore)
            (let ((inhibit-read-only t))
              (insert "Selected phrase"))
            (set-mark (point-min))
            (activate-mark)
            (let ((transient-mark-mode t))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (_prompt &optional initial-input-arg &rest _args)
                           (setq initial-input initial-input-arg)
                           "Selected phrase"))
                        ((symbol-function 'chirp-refresh)
                         (lambda ()
                           (cl-incf refresh-count))))
                (chirp-thread-add-spam-rule)
                (should (equal initial-input "Selected phrase"))
                (should (= refresh-count 1))
                (should (equal (chirp-thread--read-user-spam-rules)
                               '("Selected phrase")))
                (chirp-thread-add-spam-rule)
                (should (= refresh-count 1))
                (should (equal (chirp-thread--read-user-spam-rules)
                               '("Selected phrase")))))))
      (delete-file file))))

(ert-deftest chirp-thread-spam-rule-suggestion-can-use-author ()
  "A prefix request should suggest the current author display name."
  (with-temp-buffer
    (cl-letf (((symbol-function 'chirp-entry-at-point)
               (lambda ()
                 '(:kind tweet :text "Reply text" :author-name "Promo Author"))))
      (should (equal (chirp-thread--spam-rule-suggestion nil) "Reply text"))
      (should (equal (chirp-thread--spam-rule-suggestion t) "Promo Author")))))

(ert-deftest chirp-thread-add-spam-rule-rejects-comments ()
  "Interactive additions should reject values parsed as file comments."
  (let ((chirp-thread-spam-rules-file (make-temp-file "chirp-spam-rules-")))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _args) "# ignored")))
          (should-error (chirp-thread-add-spam-rule) :type 'user-error)
          (should (string-empty-p
                   (with-temp-buffer
                     (insert-file-contents chirp-thread-spam-rules-file)
                     (buffer-string)))))
      (delete-file chirp-thread-spam-rules-file))))

(ert-deftest chirp-view-mode-binds-spam-rule-capture ()
  "The documented spam capture key should invoke its command."
  (should (eq (lookup-key chirp-view-mode-map (kbd "S"))
              #'chirp-thread-add-spam-rule)))

(ert-deftest chirp-thread-edit-spam-rules-opens-configured-file ()
  "The edit command should create the parent directory and open the rule file."
  (let* ((directory (make-temp-file "chirp-spam-directory-" t))
         (nested-directory (expand-file-name "nested" directory))
         (file (expand-file-name "rules.txt" nested-directory))
         opened)
    (unwind-protect
        (let ((chirp-thread-spam-rules-file file))
          (cl-letf (((symbol-function 'find-file)
                     (lambda (path)
                       (setq opened path))))
            (chirp-thread-edit-spam-rules)
            (should (file-directory-p nested-directory))
            (should (equal opened file))))
      (delete-directory directory t))))

(ert-deftest chirp-thread-spam-keywords-default-to-collected-templates ()
  "Spam defaults should contain accepted templates and omit unsafe terms."
  (dolist (template '("三网优化专线"
                      "刚放个人主页上了"
                      "比她好看的没她骚"
                      "我福不黑不信你看"
                      "应该没人比我玩的开"
                      "應該沒人比我玩得開"
                      "线下sao货"
                      "返佣"
                      "比我好看的没我骚"
                      "有人想锐评一下我的福嘛"
                      "check my bio asappp"
                      "dm me or follow back"
                      "no upfront payment is required until after a successful recovery"))
    (should (member template chirp-thread-spam-keywords)))
  (should (member '("体制内幼师" "sao的很")
                  chirp-thread-spam-keywords))
  (dolist (template '(("FoxLink" "银狐")
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
                  "FoxLink🚀银狐全球高速连接"
                  "体制内幼师🌻📣sao的很Q1"))
    (should (chirp-thread--spam-reply-p (list :text text))))
  (dolist (text '("我是一名体制内幼师"
                  "空投详情现已公布"
                  "更多详情请查看公告"))
    (should-not (chirp-thread--spam-reply-p (list :text text)))))

(ert-deftest chirp-thread-default-spam-rules-match-collected-author-variants ()
  "Default spam rules should match collected author-name templates."
  (dolist (name '("深币Deepcoin93%大户返佣"
                  "FoxLink银狐全球高速连接"
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
  (dolist (name '("FoxLink 服务"
                  "银狐读书会"
                  "Deepcoin 使用体验"
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
