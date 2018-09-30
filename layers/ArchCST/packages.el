(defconst ArchCST-packages
  '(
    company
    fcitx
    ;; (lpy :location (recipe :fetcher github :repo "abo-abo/lpy"))
    ))


(defun ArchCST/post-init-company()
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.01)
  (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]"))

(defun ArchCST/post-init-spaceline()
  :init
  (display-time-mode)
  (fancy-battery-mode))

(defun ArchCST/post-init-fcitx()
  :init
  (setq fcitx-active-evil-states '(insert emacs hybrid))
  (fcitx-aggressive-setup)
  (fcitx-prefix-keys-add "M-m")
  (fcitx-prefix-keys-add "SPC"))
