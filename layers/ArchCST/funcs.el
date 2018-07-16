;; 自定义 layout 文件路径，并加载/保存 layout 信息
(defun ArchCST/load-LayoutCST()
  (interactive)
  (persp-load-state-from-file "~/.spacemacs.d/layouts/LayoutCST"))
(defun ArchCST/save-LayoutCST()
  (interactive)
  (persp-save-state-to-file "~/.spacemacs.d/layouts/LayoutCST"))
