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
(spacemacs/set-leader-keys "dhi\n" 'ArchCST-o2h/insert-frontmater)
(spacemacs/set-leader-keys "dhE" 'ArchCST-o2h/export-all-files)

