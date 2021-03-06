;; ;; 设置 保存/加载 layouts 的快捷键
;; (spacemacs/set-leader-keys "oll" 'ArchCST/load-LayoutCST)
;; (spacemacs/set-leader-keys "ols" 'ArchCST/save-LayoutCST)

;; 打开最近使用文件的快捷键设置
(global-set-key "\C-x\ \C-r" 'lazy-helm/helm-recentf)

;; 定义 helm-swoop 的快捷键到 C-s
(global-set-key "\C-s" 'helm-swoop)

;; 绑定查找函数、变量、和绑定键的定义位置
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; 获取当前 URL
(spacemacs/set-leader-keys "d;" 'ArchCST/insert-safari-current-tab-url)
(global-set-key (kbd "C-c C-;") 'ArchCST/insert-safari-current-tab-url)

;; 绑定有道快捷键
(spacemacs/set-leader-keys "df" 'youdao-dictionary-search-at-point+)

;; 自动更正错误词
(spacemacs/set-leader-keys "dc" 'flyspell-correct-at-point)

;; fix better-defaults layer C-a C-e unexpected behavior
(define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key evil-motion-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-motion-state-map (kbd "C-e") 'mwim-end-of-code-or-line)

;; snippet
;; (spacemacs/set-leader-keys "de" 'hippie-expand)
(spacemacs/set-leader-keys "dr" 'helm-yas-create-snippet-on-region)
;; (global-set-key (kbd "<tab>") 'yas-expand)
