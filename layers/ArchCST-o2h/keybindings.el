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


(spacemacs/set-leader-keys "dhe" 'ArchCST-o2h/export-this-file)
(spacemacs/set-leader-keys "dhE" 'ArchCST-o2h/export-all-files)

 ;; insert

(spacemacs/set-leader-keys "dhiF" 'ArchCST-o2h/insert-frontmater)
(spacemacs/set-leader-keys "dhim" 'ArchCST-o2h/insert-more)
(spacemacs/set-leader-keys "dhiq" 'ArchCST-o2h/insert-quote)
(spacemacs/set-leader-keys "dhib" 'ArchCST-o2h/insert-blockquote)
;; bootstrap callout
(spacemacs/set-leader-keys "dhiD" 'ArchCST-o2h/insert-bc-default)
(spacemacs/set-leader-keys "dhiP" 'ArchCST-o2h/insert-bc-primary)
(spacemacs/set-leader-keys "dhiS" 'ArchCST-o2h/insert-bc-success)
(spacemacs/set-leader-keys "dhiI" 'ArchCST-o2h/insert-bc-info)
(spacemacs/set-leader-keys "dhiW" 'ArchCST-o2h/insert-bc-warning)
(spacemacs/set-leader-keys "dhi-" 'ArchCST-o2h/insert-bc-danger)

(spacemacs/set-leader-keys "dhii" 'ArchCST-o2h/insert-img-url)

