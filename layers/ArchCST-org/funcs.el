(defun ArchCST/create-datetree ()
  (interactive)
  ;; (let (i (gen-date (org-read-date nil nil "++1" nil (org-time-string-to-time "2019-01-01"))))
  ;;   (message "%s" (format-time-string "%Y-%m-%d %A" (org-time-string-to-time gen-date))))
  (let (i gen-date)
    (setq i 0)
    (while (< i 10)
      (setq gen-date (org-read-date nil nil (format "++%s" i) nil (org-time-string-to-time "2019-01-01")))
      (message "** %s" (format-time-string "%Y-%m-%d %A" (org-time-string-to-time gen-date)))
      (setq i (+ i 1))
      )
    )
  )

;; http://blog.lujun9972.win/emacs-document/blog/2016/10/16/org-mode中的日期计算方式/


