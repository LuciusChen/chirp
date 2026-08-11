;;; chirp-spam-rules.el --- Built-in spam rules for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Data matched against thread replies and their author identities.

;;; Code:

(defconst chirp-spam-rules-default
  '("三网优化专线"
    "刚放个人主页上了"
    "dd看主页"
    "比她好看的没她骚"
    "比我好看的没我骚"
    "我福不黑不信你看"
    "应该没人比我玩的开"
    "應該沒人比我玩得開"
    "应该没人比她玩的更开"
    "线下sao货"
    "有人想锐评一下我的福嘛"
    "返佣"
    ("体制内幼师" "sao的很")
    ("FoxLink" "银狐")
    ("找炮友" "点主页")
    ("同城上门" "线下选妃")
    ("Gate" "Visa卡")
    ("催情" "听话")
    ("只入身体" "不入生活")
    "check my bio asappp"
    "be brave and check my bio"
    "talk to me pleaseee check my bio"
    "dm me or follow back"
    "I offer FREE help, just DM me"
    "no upfront payment is required until after a successful recovery"
    "we don't collect upfront payment until after a successful recovery")
  "Built-in literal rules for filtering likely spam replies.

Each string is matched case-insensitively, and a nested list matches only when
all of its strings occur.")

(provide 'chirp-spam-rules)

;;; chirp-spam-rules.el ends here
