;; AGENDA FILES
;; Files location
;; Both ~/OrgCST and ~/Dropbox/OrgCST are simlink to ~/Documents/OrgCST
(setq org-agenda-files '("~/Documents/OrgCST"))
(setq org-directory "~/Documents/OrgCST")

;; Capture
;; Org capture files location
(setq org-default-notes-file (concat org-directory "/inbox.org"))

;; org-capture templates
(setq org-capture-templates
      '(("i" "inbox" entry (file+headline "inbox.org" "Org-Capture")
         "* TODO %? :RAW:\n%U" :unnarrowed t)
        ("u" "url" entry (file+headline  "inbox.org" "Org-Capture")
         "* TODO %? :RAW:\n%U\n%(ArchCST/retrieve-safari-current-tab-url)")
        ("j" "journal" entry (file+datetree "Datetree.org")
         "* %?\nDATE: %U\n%a")
        ))



;; TAGS & PROPERTIES & TODOs & PRIORITIES

;; (setq org-tag-alist '())

;; Global persistent tags
(setq org-tag-persistent-alist '(("FLAGGED" . ?1) ("RAW" . ?0) ("ROUTINE" . ?8) ("REVIEW" . ?9)
                                 (:newline)
                                 ("FULFILMENT" . ?f) ("JOB" .?j) ("EARNED" . ?e) ("WASTED" . ?w)
                                 (:newline)
                                 ("IMPORTANT" . ?i) ("URGENT" . ?u) ("REFERENCE" . ?r)
                                 (:newline)
                                 (:newline)))

;; Set inheritance tags
;; (setq org-tags-exclude-from-inheritance '("EARNED"))

;; preset properties
(setq org-global-properties '(("VALUE_ALL" . "0 1 2 3 4 5")))

;; TODOs
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PNDG(p!)" "WATG(w@/!)" "SMDY(s)" "HOLD(h)" "|" "DONE(d!)" "ABRT(a@/!)" "FAIL(f@/!)")))

;; Set default priorities 'Z'
(setq org-default-priority 90)


;; ARCHIVE & CLOCKS
;; Default way of generate clocktable
(setq org-clock-clocktable-default-properties '(:scope subtree))

;; 持续时间的显示格式改为小时分钟（默认是天小时分钟）
(setq org-duration-format 'h:mm)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Do not prompt to resume an active clock, just resume it
(setq org-clock-persist-query-resume nil)

;; Archive to Datetree.org with datetree
(setq org-archive-location "Datetree.org::datetree/")

;; Archive all done tasks in current buffer
(defun ArchCST/archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE"))

;; AGENDA VIEW

;; Deadline warning n days before
;; If want to show before n days in a single entry, use timestamp like
;; DEADLINE: <2004-02-29 Sun -5d>
;; this will show in 5 days before deadline in agenda view
(setq org-deadline-warning-days 3)

;; Don't show future scheduled entries
(setq org-agenda-todo-ignore-scheduled 'future)

;; Don't show scheduled when deadline is shown except shceduled today
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)

;; Cleanup search temp
(defun ArchCST/cleanup-org-tags-history()
  "This function will reset `org-tags-history' value to nil."
  (interactive)
  (setq org-tags-history nil))


;; APPEARANCE
;; Log into drawers
(setq org-log-into-drawer t)

;; Change font-size for headings
(defun ArchCST/org-heading-font ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
(add-hook 'org-mode-hook 'ArchCST/org-heading-font)

;; set todo keyword color
(setf org-todo-keyword-faces '(("TODO" . (:foreground "#247ba0" :weight bold))
                               ("NEXT" . (:foreground "#ff3864" :weight bold))
                               ("PNDG" . (:foreground "#bebbbb" :weight bold))
                               ("WATG" . (:foreground "#ffe066" :weight bold))
                               ("SMDY" . (:foreground "#9593d9" :weight bold))
                               ("HOLD" . (:foreground "#f25c54" :weight bold))
                               ("DONE" . (:foreground "#70c1b3" :weight bold))
                               ("ABRT" . (:foreground "#afd8af" :weight bold))
                               ("FAIL" . (:foreground "#ff9b71" :weight bold))
                               ))

;; FXME tag keyword color
;; (setq org-tag-faces '(())


;; Highlight syntax in orgmode
(setq org-src-fontify-natively t)

;; Indent tags
;; (setq org-tags-column -70)

;; Indent headings
(setq org-startup-indented t)

;; Set org-bullet
(setq org-bullets-bullet-list '("▶" "♬" "♫" "♪" "♩"))

;; Auto wrap long lines
(add-hook 'org-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

;; Let org transcode markups in current buffer
;; (setq org-pretty-entities nil)

;; SETTINGS & CONFIGS
;; Return to open links
(setq org-return-follows-link t)

;; 设置refile target
(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 3)))
;; refile时显示完整的大纲
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; refile时排除 DONE 的标题
(defun ArchCST/org-verify-refile-target()
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'ArchCST/org-verify-refile-target)

;; 在当前buffer打开 indirect buffer
;; (setq org-indirect-buffer-display 'current-buffer)

;; ;; auto done father level
;; (defun org-summary-todo (n-done n-not-done)
;;   (let (org-log-done org-log-states)
;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;; KEYBINDINGS
;; Refile
(global-set-key (kbd "C-c w") 'org-refile)
;; (global-set-key (kbd "C-c a") 'org-agenda)

;; Open files
(spacemacs/set-leader-keys "ofi" '(lambda() (interactive) (find-file (concat org-directory "/inbox.org"))))
(spacemacs/set-leader-keys "ofa" '(lambda() (interactive) (find-file (concat org-directory "/archive.org"))))
(spacemacs/set-leader-keys "off" '(lambda() (interactive) (find-file (concat org-directory "/fulfilment.org"))))
(spacemacs/set-leader-keys "ofj" '(lambda() (interactive) (find-file (concat org-directory "/journal.org"))))
(spacemacs/set-leader-keys "ofp" '(lambda() (interactive) (find-file (concat org-directory "/personal.org"))))
(spacemacs/set-leader-keys "ofc" '(lambda() (interactive) (find-file (concat org-directory "/collections.org"))))
(spacemacs/set-leader-keys "ofw" '(lambda() (interactive) (find-file (concat org-directory "/work.org"))))

;; NEXT quickly open org files function
;; (spacemacs/declare-prefix "of" "open-org-files")

;; Clock keybindings
(spacemacs/set-leader-keys "oi" 'org-clock-in)
(spacemacs/set-leader-keys "oo" 'org-clock-out)
(spacemacs/set-leader-keys "oc" 'org-clock-cancel)
(spacemacs/set-leader-keys "og" 'org-clock-goto)
(spacemacs/set-leader-keys "oe" 'org-evaluate-time-range)
(spacemacs/set-leader-keys "ol" 'org-clock-in-last)
(spacemacs/set-leader-keys "op" 'org-priority)

;; Archive
(spacemacs/set-leader-keys "oa" 'org-archive-subtree)
(spacemacs/set-leader-keys "oA" 'ArchCST/archive-done-tasks)
