;; agenda-files 文件位置
(setq org-agenda-files '("~/Dropbox/OrgCST"))
;; 设置org-agenda-files的目录
(setq ArchCST/org-files "~/Dropbox/OrgCST/")
(setq ArchCST/org-inbox (concat ArchCST/org-files "inbox" ".org"))
(setq ArchCST/org-archive (concat ArchCST/org-files "archive" ".org"))
(setq ArchCST/org-fulfillment (concat ArchCST/org-files "fulfilment" ".org"))
(setq ArchCST/org-journal (concat ArchCST/org-files "journal" ".org"))
(setq ArchCST/org-personal (concat ArchCST/org-files "personal" ".org"))
(setq ArchCST/org-someday (concat ArchCST/org-files "someday" ".org"))
(setq ArchCST/org-work (concat ArchCST/org-files "work" ".org"))
;; org-capture 文件位置
(setq org-default-notes-file ArchCST/org-inbox)
;; Archive 文件位置，和 headline 的命名（使用 datetree） 
(setq org-archive-location (concat ArchCST/org-archive "::datetree/* From %s"))

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
                      ("idea" . ?i) ("someday" . ?s) ("notes" . ?n)
                      ))

;; org-capture 模板
(setq org-capture-templates
      '(("i" "inbox" entry (file+headline ArchCST/org-inbox "Org-Capture")
         "* TODO %?\n%U")
        ("u" "url" entry (file+headline ArchCST/org-inbox "Org-Capture")
         "* TODO [#C] %?\n%U\n%(ArchCST/retrieve-safari-current-tab-url)")
        ("j" "journal" entry (file+datetree ArchCST/org-journal)
         "* %?\nDATE: %U\n%a")
        ))
;; --------------------------------------------------------------------------

;; 设置默认 clocktable 产生的方法
(setq org-clock-clocktable-default-properties '(:maxlevel 3 :scope file))

;; 持续时间的显示格式改为小时分钟（默认是天小时分钟）
(setq org-duration-format 'h:mm)

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

;; 自动完成父级任务
(defun org-summary-todo (n-done n-not-done)
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

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

;; 在当前buffer打开 indirect buffer
;; (setq org-indirect-buffer-display 'current-buffer)

;; ------------------------------ keybindings ------------------------------ 

;; 设置 org-clock 快捷键
(spacemacs/set-leader-keys "oi" 'org-clock-in)
(spacemacs/set-leader-keys "oo" 'org-clock-out)
(spacemacs/set-leader-keys "oc" 'org-clock-cancel)
(spacemacs/set-leader-keys "og" 'org-clock-goto)
(spacemacs/set-leader-keys "oe" 'org-evaluate-time-range)
(spacemacs/set-leader-keys "ol" 'org-clock-in-last)
(spacemacs/set-leader-keys "op" 'org-priority)

;; SPC o + 首字母打开 org-files
(defun ArchCST/org-open-file (filename)
  (interactive)
  (find-file (concat ArchCST/org-files filename ".org")))
(spacemacs/set-leader-keys "ofi" '(lambda() (interactive) (ArchCST/org-open-file "inbox")))
(spacemacs/set-leader-keys "ofa" '(lambda() (interactive) (ArchCST/org-open-file "archive")))
(spacemacs/set-leader-keys "off" '(lambda() (interactive) (ArchCST/org-open-file "fulfilment")))
(spacemacs/set-leader-keys "ofj" '(lambda() (interactive) (ArchCST/org-open-file "journal")))
(spacemacs/set-leader-keys "ofp" '(lambda() (interactive) (ArchCST/org-open-file "personal")))
(spacemacs/set-leader-keys "ofs" '(lambda() (interactive) (ArchCST/org-open-file "someday")))
(spacemacs/set-leader-keys "ofw" '(lambda() (interactive) (ArchCST/org-open-file "work")))

;; 添加 orgmode 常用快捷键
(global-set-key (kbd "C-c w") 'org-refile)
(global-set-key (kbd "C-c a") 'org-agenda)

;; ;; 以 datetree 方式 archive 文件
;; (defun ArchCST/org-read-datetree-date (d)
;;   ;; "Parse a time string D and return a date to pass to the datetree functions."
;;   (let ((dtmp (nthcdr 3 (parse-time-string d))))
;;     (list (cadr dtmp) (car dtmp) (caddr dtmp))))
;; (defun ArchCST/org-refile-to-archive-datetree (&optional bfn)
;;   ;; "Refile an entry to a datetree under an archive."
;;   (interactive)
;;   (require 'org-datetree)
;;   (let* ((bfn (or bfn (find-file-noselect (expand-file-name ArchCST/org-archive))))
;;          (datetree-date (ArchCST/org-read-datetree-date (org-read-date t nil))))
;;     (org-refile nil nil (list nil (buffer-file-name bfn) nil
;;                               (with-current-buffer bfn
;;                                 (save-excursion
;;                                   (org-datetree-find-date-create datetree-date)
;;                                   (point)))))))
;; (spacemacs/set-leader-keys "oa" 'ArchCST/org-refile-to-archive-datetree)
