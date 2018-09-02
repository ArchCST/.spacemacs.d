(defconst ArchCST-orgpage-packages
  '(org-page))

(defun ArchCST-orgpage/init-org-page ()
  (spacemacs/declare-prefix "ab" "blog")
  (use-package org-page;
    :config (progn (setq op/repository-directory "~/Dropbox/Blog"
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
