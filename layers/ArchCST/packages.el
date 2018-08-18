(defconst ArchCST-packages
  '(
    ;;    youdao-dictionary
    company
    spaceline
    fcitx
    org-page
    ))

;; (defun ArchCST/init-youdao-dictionary()
;;   (use-package youdao-dictionary
;;     :defer t
;;     :init
;;     (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
;;     )
;;   )

(defun ArchCST/post-init-company()
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.01)
  )

(defun ArchCST/post-init-spaceline()
  :init
  (setq powerline-default-separator 'brace)
  (display-time-mode)
  (fancy-battery-mode)
  )

(defun ArchCST/post-init-fcitx()
  :init
  (fcitx-aggressive-setup))

(defun ArchCST/init-org-page ()
  (spacemacs/declare-prefix "ab" "blog")
  (use-package org-page;
    :config (progn (setq op/repository-directory "~/blog"
                         op/site-main-title "ArchCST's blog"
                         op/site-sub-title "=============> 修行"
                         op/site-domain "https://ArchCST.github.io"
                         op/personal-github-link "http://github.com/ArchCST"
                         ;; op/category-ignore-list '()
                         )
                   (spacemacs/set-leader-keys
                     "abP" 'op/do-publication-and-preview-site
                     "abp" 'op/do-publication
                     "abn" 'op/new-post
                     "abt" 'op/insert-options-template))))

