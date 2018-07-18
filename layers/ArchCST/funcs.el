;; 自定义 layout 文件路径，并加载/保存 layout 信息
(defun ArchCST/load-LayoutCST()
  (interactive)
  (persp-load-state-from-file "~/.spacemacs.d/layouts/LayoutCST"))
(defun ArchCST/save-LayoutCST()
  (interactive)
  (persp-save-state-to-file "~/.spacemacs.d/layouts/LayoutCST"))

;; Setting English Font
;; (set-face-attribute 'default nil :font "M+ 1mn")

;; Chinese Font
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset (font-spec :family "WenQuanYi Mirco Hei Mono"
;;                                        :size 12)))
