;; 自动加载外部更改的文件
(global-auto-revert-mode t)

;; ;; 关闭启动画面
;; (setq inhibit-splash-screen nil)

;;;; 为编程语言类的 major mode 默认启用删除多个空格
;;(add-hook 'prog-mode-hook 'spacemacs/toggle-hungry-delete-on)

;; 设置 Linux 默认浏览器
(when (and (spacemacs/system-is-linux) window-system)
  (setq browse-url-browser-function 'browse-url-chromium))

;; ;; flycheck
;; (add-hook 'prog-mode-hook 'spaceline-toggle-flycheck-info-on)
;; (add-hook 'prog-mode-hook 'flycheck-mode)

;; 关闭自动备份
(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

;; 解决远程服务器响应缓慢
(setq projectile-mode-line "Projectile")

;; set hl-color
;; set todo keyword color
(setf hl-todo-keyword-faces '(("TODO" . (:foreground "#247ba0" :weight bold))
                              ("NEXT" . (:foreground "#ff3864" :weight bold))
                              ("PNDG" . (:foreground "#bebbbb" :weight bold))
                              ("WATG" . (:foreground "#ffe066" :weight bold))
                              ("SMDY" . (:foreground "#9593d9" :weight bold))
                              ("DONE" . (:foreground "#70c1b3" :weight bold))
                              ("ABRT" . (:foreground "#afd8af" :weight bold))
                              ("FAIL" . (:foreground "#ff9b71" :weight bold))

                              ("FXME" . (:foreground "#f25c54" :weight bold))
                              ))
