;;; keybindings.el --- o2h Layer key bindings File
;;
;; Copyright (c) 2018 ArchCST
;;
;; Author: ArchCST <cst@crystl.cc>
;; URL: https://archcst.me
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(spacemacs/set-leader-keys "dhe" 'o2h/export-raw-to-source)
(spacemacs/set-leader-keys "dhE" 'o2h/export-to-file)

 ;; insert

(spacemacs/set-leader-keys "dhiF" 'o2h/insert-front-matter)
(spacemacs/set-leader-keys "dhim" 'o2h/insert-more)
(spacemacs/set-leader-keys "dhiq" 'o2h/insert-quote)
(spacemacs/set-leader-keys "dhib" 'o2h/insert-blockquote)
;; bootstrap callout
(spacemacs/set-leader-keys "dhiD" 'o2h/insert-bc-default)
(spacemacs/set-leader-keys "dhiP" 'o2h/insert-bc-primary)
(spacemacs/set-leader-keys "dhiS" 'o2h/insert-bc-success)
(spacemacs/set-leader-keys "dhiI" 'o2h/insert-bc-info)
(spacemacs/set-leader-keys "dhiW" 'o2h/insert-bc-warning)
(spacemacs/set-leader-keys "dhi-" 'o2h/insert-bc-danger)

(spacemacs/set-leader-keys "dhii" 'o2h/insert-img-url)

 ;; declare prefix keys

(spacemacs/declare-prefix "dh" "org to hexo")
(spacemacs/declare-prefix "dhi" "insert labels")
