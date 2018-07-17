
;; 设置 orgmode 的 TODO 状态
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITTING(w!)" "SOMEDAY(s!)" "|" "DONE(d!)" "ABORT(a@/!)")))

;; 预设 tags
(setq org-tag-alist '(
                      (:newline . nil)
                      ("@Anywhere" . ?a) ("@Home" . ?h) ("@Office" . ?o) ("@Shiren" . ?s)
                      (:newline . nil)
                      (:newline . nil)
                      ("#Mac OS" . ?m) ("#iPhone" . ?i) ("#Win" . ?w) ("#Arch Linux" . ?l) ("#LinodeServer" . ?L)
                      ))   

;; --------------------------------------------------------------------------

;; 在 orgmode 中实现语法高亮
(require 'org)
(setq org-src-fontify-natively t)

;; 改变各级标题大小
(defun ArchCST/org-heading-font ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook 'ArchCST/org-heading-font)

;; 标签对齐
(setq org-tags-column -90)

;; 开启indent-mode
(setq org-startup-indented t)

;; org 文件自动换行
(add-hook 'org-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

;; org 回车打开链接
(setq org-return-follows-link t)

;; 设置org-agenda-files的目录
(setq org-agenda-files '("~/Dropbox/OrgCST/"))

;; 自动完成父级任务
(defun org-summary-todo (n-done n-not-done)
  (let (org-log-done org-log-states)
                     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; 添加 orgmode 常用快捷键
(global-set-key (kbd "C-c w") 'org-refile)
(global-set-key (kbd "C-c b") 'org-switchb)

;; 设置refile target
(setq org-refile-targets '((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3)))
;; refile时显示完整的大纲
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
;; refile时排除 DONE 的标题
(defun ArchCST/verify-refile-target()
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'ArchCST/verify-refile-target)

;; 设置 org-capture 的目标文件
(setq org-default-notes-file "~/Dropbox/OrgCST/inbox.org")

;; 在当前buffer打开 indirect buffer
;; (setq org-indirect-buffer-display 'current-buffer)
