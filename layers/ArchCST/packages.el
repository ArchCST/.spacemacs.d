(defconst ArchCST-packages
  '(
    company
;;    (company-anaconda :toggle (configuration-layer/package-usedp 'company))
    fcitx
    ;;    youdao-dictionary
    ;; spaceline
    ;; (lpy :location (recipe :fetcher github :repo "abo-abo/lpy"))
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
  (display-time-mode)
  (fancy-battery-mode))

(defun ArchCST/post-init-fcitx()
  :init
  (fcitx-aggressive-setup))
