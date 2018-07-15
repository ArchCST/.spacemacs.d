(defconst ArchCST-packages
  '(youdao-dictionary)
  )

(defun ArchCST/init-youdao-dictionary()
  (use-package youdao-dictionary
    :defer t
    :init
    (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
    )
  )
