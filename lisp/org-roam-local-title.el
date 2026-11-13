;;; org-roam-local-title.el --- Index titles from note files only -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-roam-db)

(defun rvf/org-roam-db-local-file-title ()
  "Return the current note's title without titles from setup files."
  (let (titles)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search t))
       (while (re-search-forward (org-make-options-regexp '("TITLE")) nil t)
         (let ((element (org-element-at-point)))
           (when (and (eq (org-element-type element) 'keyword)
                      (equal (org-element-property :key element) "TITLE"))
             (let ((value (org-element-property :value element)))
               (when (org-string-nw-p value)
                 (push value titles))))))))
    (org-link-display-format
     (if titles
         (string-join (nreverse titles) " ")
       (file-name-sans-extension
        (file-relative-name (buffer-file-name (buffer-base-buffer))
                            org-roam-directory))))))

(provide 'org-roam-local-title)
;;; org-roam-local-title.el ends here
