;; 在 orgmode 中实现语法高亮
(require 'org)
(setq org-src-fontify-natively t)

;; org 文件自动换行
(add-hook 'org-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

;; org 回车打开链接
(setq org-return-follows-link t)

;; 全局开启行号
(global-linum-mode t)

;; 关闭启动画面
(setq inhibit-splash-screen t)

;; 设置org-agenda-files的目录
(setq org-agenda-files '("~/Dropbox/OrgCST/"))

;; 开启indent-mode
(setq org-startup-indented t)

;; 为编程语言类的 major mode 默认启用删除多个空格
(add-hook 'prog-mode-hook 'spacemacs/toggle-hungry-delete-on)

;; 设置默认浏览器
(setq browse-url-browser-function 'browse-url-chromium)
