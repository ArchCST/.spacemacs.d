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

;; (defvar hexo-raw-dir)
;; (defvar hexo-source-dir)

;; (defvar hexo-raw-dir-ab (expand-file-name hexo-raw-dir))
;; (defvar hexo-source-dir-ab (expand-file-name hexo-source-dir))

(defvar hexo-raw-dir-ab (expand-file-name "~/git/archcst.github.io/raw"))
(defvar hexo-source-dir-ab (expand-file-name "~/git/archcst.github.io/source"))

(defun o2h/check-validation ()
  "Check if `hexo-raw-dir' and `hexo-source-dir' exists, and set
`hexo-raw-dir-ab' and `hexo-source-dir-ab'"
  ;; TODO
  )

(defun o2h/org-to-md (org-file md-file)
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

(defun o2h/get-raw-list ()
  "Return contains all files absolute path in `hexo-raw-dir-ab',
including all subdirectories. Make sure `hexo-raw-dir-ab' existed."
  (interactive)
  (setq hexo-raw-list (directory-files-recursively hexo-raw-dir-ab "")))

(defun o2h/get-source-list ()
  "Return contains all files absolute path in `hexo-source-dir-ab',
including all subdirectories. Make sure `hexo-source-dir-ab' existed."
  (interactive)
  (setq hexo-source-list (reverse (directory-files-recursively hexo-source-dir-ab "" t))))

(defun o2h/find-source-for-raw (file)
  "Get corresponding file path in `hexo-source-dir-ab' for `hexo-raw-dir-ab' files.
If FILE with extension .org will return a string contains .md extension, with
other extensions will return with the same extension."
  (interactive)
  (if (string-match-p (concat "^" hexo-raw-dir-ab) file)
      (if (string= "org" (file-name-extension file))
          (replace-regexp-in-string "\.org$" "\.md" (replace-regexp-in-string hexo-raw-dir-ab hexo-source-dir-ab file))
        (replace-regexp-in-string hexo-raw-dir-ab hexo-source-dir-ab file))))

(defun o2h/find-raw-for-source (file)
  "If raw file of source file FILE exists, return corresponding file name.
If raw file of source file FILE none-exists, return nil.

When FILE extension is \".md\", will try to find the raw file extension
\".org\" or \".md\", else just find the same named file in `hexo-raw-dir-ab' of FILE."
  (if (f-exists-p (replace-regexp-in-string hexo-source-dir-ab hexo-raw-dir-ab file))
      (replace-regexp-in-string hexo-source-dir-ab hexo-raw-dir-ab file)
    (if (and (string= "md" (file-name-extension file))
             (f-exists-p (replace-regexp-in-string "\.md$" "\.org" (replace-regexp-in-string hexo-source-dir-ab hexo-raw-dir-ab file))))
        (replace-regexp-in-string "\.md$" "\.org" (replace-regexp-in-string hexo-source-dir-ab hexo-raw-dir-ab file))
      nil)))

(defun o2h/rename-conflict-org-file (org-file &optional path)
  "If ORG-FILE file has same named \".md\" file in PATH, ask user to rename it.
Optional 2nd argument PATH non-nil will find conflict in PATH, default it refers to ORG-FILE's directory.
Return the renamed file's absolute path."
  (setq path (f-slash (file-truename (or path (file-name-directory org-file)))))
  (let* ((new-name (file-name-nondirectory org-file))
         conflict-file)
    (while (or (if (file-exists-p (concat path (replace-regexp-in-string "\.org$" "\.md" new-name)))
                   (setq conflict-file (replace-regexp-in-string "\.org$" "\.md" new-name)))
               (if (file-exists-p (concat path new-name))
                   (setq conflict-file new-name)))
      (setq new-name (read-string (concat "File [ " conflict-file " ] found in " path "\nRename to: ") new-name))
      (unless (string= "org" (file-name-extension new-name))
        (setq new-name (concat new-name ".org"))))
    (o2h/check-directory (concat path new-name))
    (rename-file org-file (concat path new-name))
    (concat path new-name)))

(defun o2h/check-directory (file)
  "Check if directory of a file existed, if not make it"
  (let ((file-dir (file-name-directory file)))
    (unless (file-exists-p file-dir)
      (make-directory file-dir t))))

(defun o2h/export-raw-to-source ()
  "This function will use ox-gfm.el to export all .org fills in `hexo-raw-dir-ab'
into your `hexo-source-dir-ab', will automatically find the same directories name
and export to it.
Will only export when files in `hexo-raw-dir-ab' is newer than in `hexo-source-dir-ab'
to make this function acts more faster.
Recommendation: only use _posts and _drafts directories in your RAW directory
to store your .org files, to prevent more unexpected behaviors for now.
I'll update this function when it became a emacs package."
  (interactive)
  (dolist (i (o2h/get-raw-list))
    (when (and (string= "org" (file-name-extension i))
               (file-exists-p (replace-regexp-in-string "\.org$" "\.md" i)))
      (o2h/rename-conflict-org-file i)))
  (dolist (i (o2h/get-raw-list))
    (let* ((o (o2h/find-source-for-raw i)))
      (if (file-newer-than-file-p i o)
          (progn
            (o2h/check-directory o)
            (if (string= "org" (file-name-extension i))
                (o2h/org-to-md i o)
              (copy-file i o t))))))
  (message "Org to Hexo: export all files DONE!"))

(defun o2h/clean-source-dir ()
  "This function is designed for cleanup `hexo-source-dir-ab', try to find
same named directories and files in `hexo-raw-dir-ab', if none-exists, ask
user if delete it or copy it form `hexo-source-dir-ab' to `hexo-raw-dir-ab'.

You can also use this function to generate `hexo-source-dir-ab' when first
use org-to-hexo.el"
  (interactive)
  (let* (user-choice)
    (dolist (element (o2h/get-source-list))
      (while (and (f-directory-p element)
                  (not (o2h/find-raw-for-source element)))
        (setq user-choice (read-key (concat "Missing raw directory of \n" element " (d)elete it or (c)opy it?")))
        (when (equal user-choice 100) ;; delete it
          (delete-directory element t t))
        (when (equal user-choice 99) ;; copy it
          (copy-directory element (replace-regexp-in-string hexo-source-dir-ab hexo-raw-dir-ab element) t t t))))
    (dolist (element (o2h/get-source-list))
      (while (and (f-file-p element)
                  (not (o2h/find-raw-for-source element)))
        (setq user-choice (read-key (concat "Missing raw file of \n" element " (d)elete it or (c)opy it?")))
        (when (equal user-choice 100) ;; delete it
          (delete-file element t))
        (when (equal user-choice 99) ;; copy it
          (copy-file element (replace-regexp-in-string hexo-source-dir-ab hexo-raw-dir-ab element) nil t )))))
  (recentf-cleanup)
  (message "Org to Hexo: clean source directory DONE!"))

(defun o2h/hyper ()
  "Call `o2h/clean-source-dir' and `o2h/export-raw-to-source' together.
This is the main function of org-to-hexo, you can always use this to
prevent unwanted behaviors.
When first use, it might take some time to create raw directory."
  (interactive)
  (o2h/clean-source-dir)
  (o2h/export-raw-to-source))

(defun o2h/new-draft ()
  "This function will create a new .org file in your `hexo-raw-dir-ab' directory and
automatically insert front-matter.
If the file already exited, will switch to it.
Will create extension \".org\" automatically, you don't have to type it manually."
  (interactive)
  (let* ((new-draft-file (concat hexo-raw-dir-ab "/_drafts/" (read-string "New draft name: ") ".org")))
    (if (file-exists-p new-draft-file)
        (progn (find-file new-draft-file)
               (message "Draft [ %s ] exists, switched to it!" buffer-file-truename))
      (progn (find-file new-draft-file)
             (o2h/insert-front-matter)
             (save-buffer)
             (message "New draft [ %s ] created and saved!" buffer-file-truename)))))

(defun o2h/publish-draft ()
  "Move raw-dir's drafts to posts and export it to source-dir. Works when
  current buffer file in `hexo-raw-dir-ab' /_drafts directory only.

Details:
If current buffer is not in _drafts directory, will fail. Then do several steps
below:

1. Save current buffer
2. Move this file from `hexo-raw-dir-ab' /_drafts to /_posts, If /_posts has a
same named file with \".org\" or \".md\" extension, ask user to rename it.
3. Move corresponding \".md\" file in `hexo-source-dir-ab' to trash
4. Export this file to `hexo-source-dir-ab' /_posts"
  (interactive)
  (if (not (file-in-directory-p buffer-file-name (concat hexo-raw-dir-ab "/_drafts")))
      (message "Org to Hexo: can publish files in [ raw/_drafts ] only, FAIL!")
    (save-buffer)
    (let* ((before-pub buffer-file-name))
      (find-alternate-file
       (o2h/rename-conflict-org-file buffer-file-name (concat hexo-raw-dir-ab "/_posts")))
      (when (file-exists-p (o2h/find-source-for-raw before-pub))
        (delete-file (o2h/find-source-for-raw before-pub) t))
      (o2h/org-to-md buffer-file-name (o2h/find-source-for-raw buffer-file-name)))))

 ;; insert
;; TODO fix all evil requied funcs
(defun o2h/insert-front-matter ()
  "This function will insert front-mater at the beginning of current buffer,
after inserted, your cursor will be located at TITLE so you could white your
title immediately with \"c w\" (vim keybindings).

o2h layer use ox-gfm.el to export files, so you need to be sure there's no html
lable in the front-matter (.e.g time label) otherwise ox-gfm will transcode it."
  (interactive)
  (goto-char (point-min))
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
  (goto-char (point-min))
  (forward-line 1)
  (forward-char 7)
  (scroll-up 0))

(defun o2h/insert-more ()
  "Insert html lable: more"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "#+HTML: <!-- more -->"))

(defun o2h/insert-quote ()
  "Insert lable: quote"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% cq %}\n\n{% endcq %}")
  (evil-previous-line))

(defun o2h/insert-blockquote ()
  "Insert lable: blockquote"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% blockquote %}\n\n{% endblockquote %}")
  (evil-previous-line))

;; bootstrap callout
(defun o2h/insert-bc-default ()
  "Insert lable: bootstrap-callout default"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note default %}\n\n{% endnote %}")
  (evil-previous-line))

(defun o2h/insert-bc-primary ()
  "Insert lable: bootstrap-callout primary"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note primary %}\n\n{% endnote %}")
  (evil-previous-line))

(defun o2h/insert-bc-success ()
  "Insert lable: bootstrap-callout success"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note success %}\n\n{% endnote %}")
  (evil-previous-line))

(defun o2h/insert-bc-info ()
  "Insert lable: bootstrap-callout info"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note info %}\n\n{% endnote %}")
  (evil-previous-line))

(defun o2h/insert-bc-warning ()
  "Insert lable: bootstrap-callout warning"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note warning %}\n\n{% endnote %}")
  (evil-previous-line))

(defun o2h/insert-bc-danger ()
  "Insert lable: bootstrap-callout danger"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note danger %}\n\n{% endnote %}")
  (evil-previous-line))

(defun o2h/insert-img-url ()
  "Insert url of an image form the kill-ring, if it's not a picture, you
might need to input it manually"
  (interactive)
  ;; if clipboard is an img
  (let ((img-url (replace-regexp-in-string "\n$" "" (current-kill 0 t))))
    (unless (string-match "\\.\\(jpg\\|png\\|jpeg\\|gif\\|bmp\\|ico\\|tif\\)$" img-url)
      (setq img-url (read-string "url: ")))
    (evil-insert-newline-below)
    (insert-before-markers (concat "#+HTML: <img src=\"" img-url "\" alt=\"img\" width=\"\" />"))))
