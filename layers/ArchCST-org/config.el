
;; 设置 orgmode 的 TODO 状态
(setq org-todo-keywords
      '((sequence "TODO(t)" "WIP(w!)" "|" "DONE(d!)" "ABORT(a@/!)")))

;; 预设 tags
(setq org-tag-alist '(
                      (:newline . nil)
                      ("Anywhere" . ?A) ("Home" . ?H) ("Office" . ?O) ("Shiren" . ?S)
                      (:newline . nil)
                      (:newline . nil)
                      ("Mac" . ?m) ("Cellhone" . ?c) ("Win" . ?w) ("Linux" . ?l) ("LinodeServer" . ?L)
                      (:newline . nil)
                      (:newline . nil)
                      ("idea" . ?i) ("someday" . ?s) 
                      ))   

;; --------------------------------------------------------------------------

;; 打开org的config.el
(defun ArchCST/org-open-config-file()
  (interactive)
  (find-file "~/.spacemacs.d/layers/ArchCST-org/config.el"))
(spacemacs/set-leader-keys "ooc" 'ArchCST/org-open-config-file)

;; 打开inbox.org
(defun ArchCST/org-open-inbox()
  (interactive)
  (find-file "~/Dropbox/OrgCST/inbox.org"))
(spacemacs/set-leader-keys "ooi" 'ArchCST/org-open-inbox)

;; 以datetree方式refile文件
(defun ArchCST/org-read-datetree-date (d)
;; "Parse a time string D and return a date to pass to the datetree functions."
  (let ((dtmp (nthcdr 3 (parse-time-string d))))
    (list (cadr dtmp) (car dtmp) (caddr dtmp))))
(defun ArchCST/org-refile-to-archive-datetree (&optional bfn)
;; "Refile an entry to a datetree under an archive."
  (interactive)
  (require 'org-datetree)
  (let* ((bfn (or bfn (find-file-noselect (expand-file-name "~/Dropbox/OrgCST/archive.org"))))
         (datetree-date (ArchCST/org-read-datetree-date (org-read-date t nil))))
    (org-refile nil nil (list nil (buffer-file-name bfn) nil
                              (with-current-buffer bfn
                                (save-excursion
                                  (org-datetree-find-date-create datetree-date)
                                  (point)))))))
(spacemacs/set-leader-keys "ooa" 'ArchCST/org-refile-to-archive-datetree)

;; 在 orgmode 中实现编程语言语法高亮
(require 'org)
(setq org-src-fontify-natively t)

;; 改变各级标题大小
(defun ArchCST/org-heading-font ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook 'ArchCST/org-heading-font)

;; 标签对齐
(setq org-tags-column -70)

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
(defun ArchCST/org-verify-refile-target()
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'ArchCST/org-verify-refile-target)

;; 设置 org-capture 的目标文件
(setq org-default-notes-file "~/Dropbox/OrgCST/inbox.org")

;; org-capture 模板
(setq org-capture-templates
      '(("i" "inbox" entry (file+headline "~/Dropbox/OrgCST/inbox.org" "Org-Capture")
         "* %?")
         ("j" "journal" entry (file+datetree "~/Dropbox/OrgCST/journal.org")
          "* %?\nDATE: %U\n %i\n %a")
        ))

;; 在当前buffer打开 indirect buffer
;; (setq org-indirect-buffer-display 'current-buffer)
