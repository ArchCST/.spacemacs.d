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

;;(require 'ox)
;;(require 'ox-gfm)

;; Set your RAW-DIR and SOURCE-DIR here

(defvar hexo-raw-dir (expand-file-name "~/git/CSTHexo/raw"))
(defvar hexo-source-dir (expand-file-name "~/git/CSTHexo/source"))

(defun o2h/get-raw-files-list ()
  "Return a var named hexo-raw-files-list contains all .org files absolute path in raw directory,
including all subdirectories. Make sure `hexo-raw-dir' existed."
  (interactive)
  (setq hexo-raw-files-list (directory-files-recursively hexo-raw-dir "")
  ))

;; https://emacs.stackexchange.com/questions/44622/how-to-use-org-export-to-export-from-a-file-in-emacs
;; TODO raw-to-source, add copy .md files in raw to source.
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

(defun o2h/md-to-md (raw-md source-md)
  "Export .md file form `hexo-raw-dir' to `hexo-source-dir'"
  (interactive)
  (unless (string= "md" (file-name-extension raw-md))
    (error "raw-md must have an .md extension."))
  (unless (string= "md" (file-name-extension source-md))
    (error "source-md must have an .md extension."))
  (copy-file raw-md source-md t))

(defun o2h/get-corresponding-path (file)
  "Get corresponding file path in `hexo-source-dir' for `hexo-raw-dir' files.
If FILE with extension .org will return a string contains .md extension, with
other extensions will return with the same extensions"
  (interactive)
  (if (string-match-p (concat "^" hexo-raw-dir) file)
      (if (string= "org" (file-name-extension file))
          (replace-regexp-in-string "\.org$" "\.md" (replace-regexp-in-string hexo-raw-dir hexo-source-dir file))
        (replace-regexp-in-string hexo-raw-dir hexo-source-dir file))
    ))

(defun o2h/find-duplicate ()
  "If an .org file has a same named .md file, return the .md file's name."
  (o2h/get-raw-files-list)
  (catch 'loop
    (dolist (org-file hexo-raw-files-list)
      (let ((md-file (replace-regexp-in-string "\.org$" "\.md" org-file)))
        (when (and (string= "org" (file-name-extension org-file))
                 (f-exists-p md-file))
          (setq o2h-duplicate-md md-file)
          (setq o2h-duplicate-org org-file)
          (throw 'loop o2h-duplicate-md))))))

(defun o2h/rename-org-file (file &optional reason)
  "Rename org-file"
  (let* ((rename-prompt (concat reason "\n"
                                "Rename " file "\n"
                                "To:    " (file-name-directory file)))
         (new-org-name (concat (file-name-directory file)
                               (read-string rename-prompt))))
    (while (file-exists-p new-org-name)
        (progn
          (setq rename-prompt (concat new-org-name " existed\n" rename-prompt))
          (setq new-org-name (concat (file-name-directory file)
                                     (read-string rename-prompt))))
        )
    (rename-file file new-org-name)
    ))

(defun o2h/fix-duplicate ()
  "Check if files in `hexo-raw-dir' have same file with .md extensions, if yes ask user to fix it."
  (interactive)
  (while (o2h/find-duplicate)
    (o2h/rename-org-file o2h-duplicate-org (concat "File:  "
                                                   o2h-duplicate-md
                                                   " existed, rename to prevent overwrite it."))))

(defun o2h/export-raw-to-source ()
  "This function will use ox-gfm.el to export all .org fills in `hexo-raw-dir'
into your `hexo-source-dir', will automatically find the same directories name
and export to it.
Will only export when files in `hexo-raw-dir' is newer than in `hexo-source-dir'
to make this function acts more faster.
Recommendation: only use _posts and _drafts directories in your RAW directory
to store your .org files, to prevent more unexpected behaviors for now.
I'll update this function when it became a emacs package."
  ;; TODO check directory existed
  (interactive)
  (o2h/fix-duplicate)
  (o2h/get-raw-files-list)
  (dolist (i hexo-raw-files-list)
    (let* ((o (o2h/get-corresponding-path i)))
      (if (file-newer-than-file-p i o)
          ;;(o2h/org-to-md i o)
          (if (string= "org" (file-name-extension i))
              (o2h/org-to-md i o)
            (copy-file i o t)))))
  (message "Org to Hexo: export all files DONE!"))


;; (defun o2h/clean-none-exists ()
;;   "Find raw files of filse in `hexo-source-dir', if not exist, ask user if
;; move it to trash."
;;   (interactive)
;;   (o2h/get-md-files-paths)
;;   (o2h/get-o2hignore)
;;   (let* (user-choise)
;;     (dolist (element hexo-md-files-path)
;;       (if (and (not (f-exists-p (o2h/get-corresponding-path element)))
;;                (not (member element o2hignore-list)))
;;           (progn
;;             (setq user-choise (read-key (concat "Raw file of: [ " (replace-regexp-in-string hexo-source-dir "" element) " ] doesn't exist, How to proceed?\n
;; [1]. Add to ignore list;
;; [2]. Delete .md file;
;; [3]. Any other key to ignore temporally: \n")))
;;             (when (equal user-choise 49)
;;               (o2h/append-to-ignore-file element))
;;             (when (equal user-choise 50)
;;               (delete-file element t))))))
;;   (recentf-cleanup)
;;   (message "Org to Hexo: clean none exists DONE!"))


(defun o2h/export-to-file ()
  "This function wiil export current buffer to a directory from your choice. Notice
that if you use `ArchCST-org/clean-none-exists' after this you might lose this file's
exported if this one is not in your RAW-DIR. Make sure this file is in the RAW-DIR to
prevent this happening.

Recommendation: use this function when only modified one file in your RAW directory.

If \"Output file not writable\" returned please make sure you have that directory existed.

Binding key: SPC d h E , can be modified in keybindings.el"
  (interactive)
  (save-buffer)
  (let* ((outfile (org-export-output-file-name ".md" nil (read-file-name "Destination: "))))
    (org-export-to-file 'gfm outfile))
  (message "Org to Hexo: export this file DONE!"))

;; (defun o2h/new-draft ()
;;   "This function will create a new .org file in your `hexo-raw-dir' directory and
;; automatically insert front-matter for it.
;; If the file already exited, will switch to it."
;;   (interactive)
;;   (let* ((new-draft-file (concat hexo-raw-dir "/_drafts/" (read-string "New draft name: ") ".org")))
;;     (if (file-exists-p new-draft-file)
;;         (progn (find-file new-draft-file)
;;                (message "Draft [ %s ] exists, switched to it!" buffer-file-truename))
;;       (progn (find-file new-draft-file)
;;              (o2h/insert-front-matter)
;;              (save-buffer)
;;              (message "New draft [ %s ] created and saved!" buffer-file-truename)
;;              ))))

 ;; insert

(defun o2h/insert-front-matter ()
  "This function will insert front-mater at the beginning of current buffer, after
inserted, your cursor will be located at TITLE so you could white your title immediately
by C W (vim keybindings).

o2h layer use ox-gfm.el to export files, so you need to be sure there's no html
lable in the front-matter (.e.g time label) otherwise ox-gfm will transcode it.

Binding key: SPC d h i RET
"
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
  (scroll-up 0)
  )

(defun o2h/insert-more ()
  "Insert html lable: more"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "#+HTML: <!-- more -->")
  )

(defun o2h/insert-quote ()
  "Insert lable: quote"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% cq %}\n\n{% endcq %}")
  (evil-previous-line)
  )

(defun o2h/insert-blockquote ()
  "Insert lable: blockquote"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% blockquote %}\n\n{% endblockquote %}")
  (evil-previous-line)
  )

;; bootstrap callout
(defun o2h/insert-bc-default ()
  "Insert lable: bootstrap-callout default"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note default %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun o2h/insert-bc-primary ()
  "Insert lable: bootstrap-callout primary"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note primary %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun o2h/insert-bc-success ()
  "Insert lable: bootstrap-callout success"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note success %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun o2h/insert-bc-info ()
  "Insert lable: bootstrap-callout info"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note info %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun o2h/insert-bc-warning ()
  "Insert lable: bootstrap-callout warning"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note warning %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun o2h/insert-bc-danger ()
  "Insert lable: bootstrap-callout danger"
  (interactive)
  (evil-insert-newline-below)
  (insert-before-markers "{% note danger %}\n\n{% endnote %}")
  (evil-previous-line)
  )

(defun o2h/insert-img-url ()
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
