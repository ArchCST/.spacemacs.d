(defun ArchCST/insert-safari-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (ArchCST/retrieve-safari-current-tab-url)))

(defun ArchCST/retrieve-safari-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Safari\"\n"
                  "set theUrl to get URL of current tab of first window\n"
                  "set theTitle to get name of current tab of first window\n"
                  "set theResult to {get \"[[\", theUrl, \"][\", theTitle, \"]]\"}\n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))
