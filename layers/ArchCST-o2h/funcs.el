;; export current file to /source/_posts/
(defun ArchCST-o2h/export-this-file (&optional async subtreep visible-only)
  (interactive)
  (save-buffer)
  (let ((outfile (org-export-output-file-name ".md" subtreep "~/git/CSTHexo/source/_posts")))
    (org-export-to-file 'gfm outfile async subtreep visible-only)))
(spacemacs/set-leader-keys "dhe" 'ArchCST-o2h/export-this-file)

;; insert metadata at the top of file for Hexo
(defun ArchCST-o2h/insert-frontmater ()
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
(spacemacs/set-leader-keys "dhi" 'ArchCST-o2h/insert-frontmater)
