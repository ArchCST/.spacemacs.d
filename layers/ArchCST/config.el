;; 全局开启行号
(global-linum-mode t)

;; 关闭启动画面
(setq inhibit-splash-screen t)

;; 在 orgmode 中实现语法高亮
(require 'org)
(setq org-src-fontify-natively t)

;; 设置org-agenda-files的目录
(setq org-agenda-files '("~/Dropbox/OrgCST/"))

;; 开启indent-mode
(setq org-startup-indented t)

;; 删除多个空格
(setq global-hungry-delete-mode t)
