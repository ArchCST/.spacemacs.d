(defconst ArchCST-packages
  '(
    company
    fcitx
    ))

(defun ArchCST/post-init-company()
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.01)
  (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]"))

(defun ArchCST/post-init-fcitx()
  :init
  (setq fcitx-active-evil-states '(insert emacs hybrid))
  (fcitx-aggressive-setup)
  (fcitx-prefix-keys-add "M-m")
  (fcitx-prefix-keys-add "SPC"))

;; (defun ArchCST/init-all-the-icons()
;;   (use-package all-the-icons))
