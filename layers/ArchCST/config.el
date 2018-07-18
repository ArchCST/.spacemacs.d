;; 全局开启行号
(global-linum-mode t)

;; 关闭启动画面
;; (setq inhibit-splash-screen nil)

;; 为编程语言类的 major mode 默认启用删除多个空格
(add-hook 'prog-mode-hook 'spacemacs/toggle-hungry-delete-on)

;; 设置默认浏览器
(when (and (spacemacs/system-is-linux) window-system)
  (setq browse-url-browser-function 'browse-url-chromium))

;; 自动加载外部更改的文件
(auto-revert-mode t)
