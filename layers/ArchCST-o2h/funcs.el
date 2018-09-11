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
(defvar hexo-raw-dir (expand-file-name "~/git/CSTHexo/raw"))
(defvar hexo-source-dir (expand-file-name "~/git/CSTHexo/source"))

(defun ArchCST-o2h/get-org-files-paths ()
  "Return a var named hexo-org-files-path contains all .org files absolute path in raw directory,
including all subdirectories. Make sure `hexo-raw-dir' existed."
  (interactive)
  (setq hexo-org-files-path
        (split-string (shell-command-to-string
                       (concat "find " hexo-raw-dir " -name \"*.org\"")) "\n" t))
  )

(defun ArchCST-o2h/get-md-files-paths ()
  "Return a var named hexo-md-files-path contains all .md files absolute path in source directory,
including all subdirectories. Make sure `hexo-source-dir' existed."
  (interactive)
  (setq hexo-md-files-path
        (split-string (shell-command-to-string
                       (concat "find " hexo-source-dir " -name \"*.md\"")) "\n" t))
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

(defun ArchCST-o2h/clean-none-exists ()
  "Find raw files of filse in `hexo-source-dir', if not exist, ask user if
move it to trash."
  (interactive)
  (ArchCST-o2h/get-md-files-paths)
  (dolist (element hexo-md-files-path)
    (if (not (f-exists-p (replace-regexp-in-string "\.md$" "\.org" (replace-regexp-in-string hexo-source-dir hexo-raw-dir element))))
        (if (y-or-n-p (concat "Raw file of: [ " (replace-regexp-in-string hexo-source-dir "" element) " ] doesn't exist, delete it? "))
            (delete-file element t))))
  (recentf-cleanup)
  (message "Org to Hexo: clean none exists DONE!"))

(defun ArchCST-o2h/export-all-files ()
  "This function will use ox-gfm.el to export all .org fills in `hexo-raw-dir'
into your `hexo-source-dir', will automatically find the same directories name
and exportto it.
Will only export when files in `hexo-raw-dir' is newer than `hexo-source-dir'
to make this function acts more faster.
Recommendation: only use _posts and _drafts directories in your RAW directory
to store your .org files, to prevent more unexpected behaviors for now.
I'll update this function when it became a emacs package.
Key binding: SPC d h E , can be modified in keybindings.el"
  (interactive)
  (ArchCST-o2h/get-org-files-paths)
  (dolist (i hexo-org-files-path)
    (let* ((o (replace-regexp-in-string "\.org$" "\.md" (replace-regexp-in-string hexo-raw-dir hexo-source-dir i))))
      (if (file-newer-than-file-p i o)
          (ArchCST-o2h/hexo-org-to-md i o))
      ))
  (message "Org to Hexo: export all files DONE!")
  )

;; TODO choose directory
(defun ArchCST-o2h/export-this-file ()
  "This function wiil export current buffer to a directory from your choice. Notice
that if you use `ArchCST-org/clean-none-exists' after this you might lose this file's
exported if this one is not in your RAW-DIR. Make sure this file is in the RAW-DIR to
prevent this happening.

Recommendation: use this function when only modified one file in your RAW directory.

If \"Output file not writable\" returned please make sure you have that directory existed.

Binding key: SPC d h e , can be modified in keybindings.el"
  (interactive)
  (save-buffer)
  (let* ((outfile (org-export-output-file-name ".md" nil (read-file-name "Destination: "))))
    (org-export-to-file 'gfm outfile))
  (message "Org to Hexo: export this file DONE!"))

 ;; insert

(defun ArchCST-o2h/insert-frontmater ()
  "This function will insert front-mater at the beginning of current buffer, after
inserted, your cursor will be located at TITLE so you could white your title immediately
by C W (vim keybindings).

o2h layer use ox-gfm.el to export files, so you need to be sure there's no html
lable in the front-mater (.e.g time label) otherwise ox-gfm will transcode it.

Binding key: SPC d h i RET
"
  (interactive)
  (evil-goto-first-line)
  (insert-before-markers (concat "#+OPTIONS: toc:nil \\n:t
title: untitled
date: " (format-time-string "%Y-%m-%d" (current-time))
"\nupdated:
comments: true
tags:
  - tag1
categories: uncategorized
layout: post
------
"))
  (evil-goto-line 2)
  (evil-forward-word-begin 2)
  )

(defun ArchCST-o2h/insert-more ()
  "Insert html lable: more"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "#+HTML: <!-- more -->")
  )

(defun ArchCST-o2h/insert-quote ()
  "Insert lable: quote"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% cq %}\n\n{% endcq %}")
  (evil-previous-line)
  )

(defun ArchCST-o2h/insert-blockquote ()
  "Insert lable: blockquote"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% blockquote %}\n\n{% endblockquote %}")
  (evil-previous-line)
  )

;; bootstrap callout
(defun ArchCST-o2h/insert-bc-default ()
  "Insert lable: bootstrap-callout default"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note default %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun ArchCST-o2h/insert-bc-primary ()
  "Insert lable: bootstrap-callout primary"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note primary %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun ArchCST-o2h/insert-bc-success ()
  "Insert lable: bootstrap-callout success"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note success %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun ArchCST-o2h/insert-bc-info ()
  "Insert lable: bootstrap-callout info"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note info %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun ArchCST-o2h/insert-bc-warning ()
  "Insert lable: bootstrap-callout warning"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note warning %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun ArchCST-o2h/insert-bc-danger ()
  "Insert lable: bootstrap-callout danger"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note danger %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun ArchCST-o2h/insert-img-url ()
  "Insert url of an image form the kill-ring, if it's not a picture, you
might need to input it manually"
  (interactive)
  ;; if clipboard is an img
  (let ((img-url (replace-regexp-in-string "\n$" "" (car kill-ring))))
    (unless (string-match "\\.\\(jpg\\|png\\|jpeg\\|gif\\|bmp\\|ico\\|tif\\)$" img-url)
      (setq img-url (read-string "url: ")))

    (evil-insert-newline-below)
    (insert-before-markers (concat "#+HTML: <img src=\"" img-url "\" alt=\"img\" width=\"\" />"))
    ))
