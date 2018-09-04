(defun ArchCST/insert-safari-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (ArchCST/retrieve-safari-current-tab-url)))

;; ;; 获取 Chrome 浏览器当前标签页
;; (defun ArchCST/retrieve-safari-current-tab-url()
;;   "Get the URL of the active tab of the first window"
;;   (interactive)
;;   (let ((result (do-applescript
;;                  (concat
;;                   "set frontmostApplication to path to frontmost application\n"
;;                   "tell application \"Google Chrome\"\n"
;;                   "set theUrl to get URL of active tab of first window\n"
;;                   "set theTitle to get name of active tab of first window\n"
;;                   "set theResult to {get \"[[\", theUrl, \"][\", theTitle, \"]]\"}\n"
;;                   "end tell\n"
;;                   "activate application (frontmostApplication as text)\n"
;;                   "set links to {}\n"
;;                   "copy theResult to the end of links\n"
;;                   "return links as string"))))
;;     (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

;; 获取 Safari 浏览器当前标签页
(defun ArchCST/retrieve-safari-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Safari\"\n"
                  "set theUrl to get URL of current tab of first window\n"
                  "set theTitle to get name of current tab of first window\n"
                  "set theResult to {get \"[[\", theUrl, \"][\", theTitle, \"]]\"}\n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

;; 解决远程服务器响应缓慢
(setq projectile-mode-line "Projectile")

;; transfer .org file to markdown file for Hexo
(defun ArchCST/hexo-ox-gfm (&optional async subtreep visible-only)
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep "~/git/CSTHexo/source/_posts")))
    (org-export-to-file 'gfm outfile async subtreep visible-only)))
(spacemacs/set-leader-keys "dhe" 'ArchCST/hexo-ox-gfm)

;; insert metadata at the top of file for Hexo
(defun ArchCST/hexo-insert-metadata ()
  (interactive)
  (evil-goto-first-line)
  (insert-before-markers "#+OPTIONS: toc:nil \\n:t
title: 
date: 
updated: 
comments: true
tags:
  - 
categories: 
layout: post
permalink: 
------
")
  (evil-goto-line 2)
  (evil-append-line 0)
  )
(spacemacs/set-leader-keys "dhi" 'ArchCST/hexo-insert-metadata)
