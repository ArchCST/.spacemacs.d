(defconst ArchCST-packages
  '(youdao-dictionary
    company
    )
  )

(defun ArchCST/init-youdao-dictionary()
  (use-package youdao-dictionary
    :defer t
    :init
    (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
    )
  )

(defun ArchCST/post-init-company()
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.01)
  )
