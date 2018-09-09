;;; funcs.el --- o2h Layer functions File
;;
;; Copyright (c) 2018 ArchCST
;;
;; Author: ArchCST <cst@crystl.cc>
;; URL: https://archcst.me
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;; code:

;; Set your RAW-DIR and SOURCE-DIR here
(setq hexo-raw-dir (expand-file-name "~/git/CSTHexo/raw"))
(setq hexo-source-dir (expand-file-name "~/git/CSTHexo/source"))

(defun ArchCST-o2h/set-raw-files-path ()
  "This function will return a var named hexo-raw-files-path contains all
files absolute path in raw directory, including all subdirectories. please
make sure hexo-raw-dir existed.
Only files with extension .org can be returned."
  (interactive)
  (setq hexo-raw-files-path
        (split-string (shell-command-to-string
                       (concat "find " hexo-raw-dir " -name \"*.org\"")) "\n" t))
  )

;; https://emacs.stackexchange.com/questions/44622/how-to-use-org-export-to-export-from-a-file-in-emacs
(defun ArchCST-o2h/hexo-org-to-md (org-file md-file)
  "Export ORG-FILE to MD-FILE.
This function will export .org file to .md file, and close buffers which
were not opened in emacs before called this function."
  (interactive "f\nf")
  (unless (string= "org" (file-name-extension org-file))
    (error "INFILE must be an org file."))
  (unless (string= "md" (file-name-extension md-file))
    (error "OUTFILE must have an .md extension."))
  (let ((current-buffers (buffer-list))
        (open (find-buffer-visiting org-file))
        (org-file-buffer (find-file-noselect org-file)))
    (with-current-buffer org-file-buffer
        (org-export-to-file 'gfm md-file))
    (unless open (kill-buffer org-file-buffer))))

(defun ArchCST-o2h/export-all-files ()
  "This function will use ox-gfm.el to export all fills into your SOURCE
directory, will automatically find the same directories name and export
to it.
Recommendation: only use _posts and _drafts directories in your RAW directory
to store your .org files, to prevent more unexpected behaviors for now.
I'll update this function when it became a emacs package.
Key binding: SPC d h E , can be modified in keybindings.el"
  (interactive)
  (dolist (i hexo-raw-files-path)
    (let* ((o (replace-regexp-in-string "\.org$" "\.md" (replace-regexp-in-string "/raw/" "/source/" i))))
      (ArchCST-o2h/hexo-org-to-md i o)
      ))
  (message "Org to Hexo: export all files DONE!")
  )

(defun ArchCST-o2h/export-this-file ()
  "This function wiil export current buffer to a directory from your choice. Notice
that if you use `ArchCST-org/export-all-files' after this you might lose this file's
exported due to this one is not in your RAW-DIR. Make sure this file is in the
RAW-DIR to prevent this happen.

Recommendation: use this function when only added one file to your RAW directory.

If \"Output file not writable\" returned please make sure you have that directory existed.

Binding key: SPC d h e , can be modified in keybindings.el"
  (interactive)
  (save-buffer)
  (let* ((path (read-file-name "New Directory: "))
         (outfile (org-export-output-file-name ".md" path)))
    (org-export-to-file 'gfm outfile )))

;; TODO insert current date as default date
;; insert metadata at the top of file for Hexo
(defun ArchCST-o2h/insert-frontmater ()
  "This function will insert frontmater in the beginning of current buffer, after
inserted your cursor will be located at the end of TITLE line and automatically
switch into insert mode so you could white your title immediately.

o2h layer use ox-gfm.el to export files, so you need to be sure there's no html
lable in the frontmater (.e.g time lable) otherwise ox-gfm will transcode it.

Binding key: SPC d h i RET
"
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
------
")
  (evil-goto-line 2)
  (evil-append-line 0)
  )
