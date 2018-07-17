;; 全局开启行号
(global-linum-mode t)

;; 关闭启动画面
;; (setq inhibit-splash-screen nil)

;; 为编程语言类的 major mode 默认启用删除多个空格
(add-hook 'prog-mode-hook 'spacemacs/toggle-hungry-delete-on)

;; 设置默认浏览器
(setq browse-url-browser-function 'browse-url-chromium)

;; ;; 设置模式下的字体
;; (set-face-attribute
;;  'default nil :font "WenQuanYi Micro Hei Mono 14")
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset
;;                     (font-spec :family "WenQuanYi Micro Hei Mono" :size 16)))
