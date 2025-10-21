;;; cross-refs.el --- Custom helper functions for cross referencing in Org mode

(defvar my/org-smart-ref-map
  '(("theorem" . "thm:")
    ("definition" . "def:")
    ("lemma" . "lem:")
    ("equation" . "eq:")
    ("figure" . "fig:")
    ("table" . "tbl:")
    ("proof" . "prf:"))
  "Alist mapping context words to label prefixes for smart referencing.")

(defun my/org-insert-label-ref (prefix)
  "Insert a link to a #+label: label, filtered by PREFIX."
  (let ((labels '())
        (regex (concat "^[ \t]*#\\+label:[ \t]+\\(\\(" (regexp-quote prefix) "[^ \t\n\r]+\\)\\)")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((label (match-string 1)))
          (let ((preview ""))
            (save-excursion
              (goto-char (match-end 0))
              (when (re-search-forward "^[ \t]*#\\+begin_" nil t)
                (when (re-search-forward "^[ \t]*\\S-"
                                         (save-excursion (re-search-forward "^[ \t]*#\\+end_") (point))
                                         t)
                  (setq preview (string-trim (buffer-substring-no-properties
                                              (line-beginning-position)
                                              (line-end-position)))))))
            (push (cons (format "%-20s  %s" label preview) label) labels)))))

    (if (null labels)
        (message "No labels found matching '%s'." prefix)
      (let* ((stable-label-list (nreverse labels))
             (choice (completing-read (format "Ref (%s): " prefix) stable-label-list nil t)))
        (when choice
          (insert (format "[[%s]]" (cdr (assoc choice stable-label-list)))))))))

(defun my/org-insert-smart-ref ()
  "Intelligently insert a cross-reference to a #+label:.
Guesses the label prefix (e.g., 'thm:') based on the word
preceding point. If no guess is found, prompts for a prefix."
  (interactive)
  (let* ((text-before (buffer-substring-no-properties (line-beginning-position) (point)))
         (word-list (split-string text-before "[^[:word:]_]+" t))
         (last-word (downcase (car (last word-list))))
         (prefix-guess (cdr (assoc last-word my/org-smart-ref-map))))

    (if prefix-guess
        (my/org-insert-label-ref prefix-guess)
      (call-interactively 'my/org-insert-label-ref))))

;; This final line is essential for 'require' to work
(provide 'cross-refs)


