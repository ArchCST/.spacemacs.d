(defun ArchCST/insert-safari-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (ArchCST/retrieve-safari-current-tab-url)))

;; 获取 Chrome 浏览器当前标签页
(defun ArchCST/retrieve-safari-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  "set theUrl to get URL of active tab of first window\n"
                  "set theTitle to get name of active tab of first window\n"
                  "set theResult to {get \"[[\", theUrl, \"][\", theTitle, \"]]\"}\n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

;; 获取 Safari 浏览器当前标签页
;; (defun ArchCST/retrieve-safari-current-tab-url()
;;   "Get the URL of the active tab of the first window"
;;   (interactive)
;;   (let ((result (do-applescript
;;                  (concat
;;                   "set frontmostApplication to path to frontmost application\n"
;;                   "tell application \"Safari\"\n"
;;                   "set theUrl to get URL of current tab of first window\n"
;;                   "set theTitle to get name of current tab of first window\n"
;;                   "set theResult to {get \"[[\", theUrl, \"][\", theTitle, \"]]\"}\n"
;;                   "end tell\n"
;;                   "activate application (frontmostApplication as text)\n"
;;                   "set links to {}\n"
;;                   "copy theResult to the end of links\n"
;;                   "return links as string"))))
;;     (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

;; 解决远程服务器响应缓慢
(setq projectile-mode-line "Projectile")

;; org-mode company
(defun my-org-mode-hook ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
(add-hook 'org-mode-hook #'my-org-mode-hook)
