;;; cross-refs.el --- Smart cross-reference helpers for Org mode

(defvar my/org-smart-ref-map
  '(("theorem" . "thm:")
    ("definition" . "def:")
    ("lemma" . "lem:")
    ("equation" . "eq:")
    ("proof" . "prf:")
    ("corollary" . "cor:")
    ("proposition" . "pro:")
    ("figure" . "fig:")
    ("table" . "tbl:"))
  "Alist mapping context words to label prefixes for smart referencing.")

(defun my/org-insert-label-ref (prefix)
  "Insert a link to a #+label: or \\label{} label, filtered by PREFIX."
  (let ((labels '())
        ;; Match both #+label: and \label{} formats
        (regex-orgmode (concat "^[ \t]*#\\+label:[ \t]+\\(\\(" (regexp-quote prefix) "[^ \t\n\r]+\\)\\)"))
        (regex-latex (concat "\\\\label{\\(\\(" (regexp-quote prefix) "[^}]+\\)\\)}")))
    (save-excursion
      (goto-char (point-min))
      ;; Search for #+label: style
      (while (re-search-forward regex-orgmode nil t)
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
            (push (cons (format "%-20s  %s" label preview) label) labels))))
      ;; Search for \label{} style
      (goto-char (point-min))
      (while (re-search-forward regex-latex nil t)
        (let ((label (match-string 1)))
          (let ((preview ""))
            (save-excursion
              ;; Try to get some context from the same line or next line
              (goto-char (match-beginning 0))
              (let ((line-content (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
                ;; Extract some math content as preview
                (when (string-match "[^\\\\]+$" line-content)
                  (setq preview (string-trim (match-string 0 line-content))))))
            (push (cons (format "%-20s  %s" label preview) label) labels)))))

    (if (null labels)
        (message "No labels found matching '%s'." prefix)
      (let* ((stable-label-list (nreverse labels))
             (choice (completing-read (format "Ref (%s): " prefix) stable-label-list nil t)))
        (when choice
          (insert (format "[[%s]]" (cdr (assoc choice stable-label-list)))))))))

(defun my/org-insert-smart-ref ()
  "Intelligently insert a cross-reference to a #+label:."
  (interactive)
  (let* ((text-before (buffer-substring-no-properties (line-beginning-position) (point)))
         (word-list (split-string text-before "[^[:word:]_]+" t))
         (last-word (downcase (car (last word-list))))
         (prefix-guess (cdr (assoc last-word my/org-smart-ref-map))))
    (if prefix-guess
        (my/org-insert-label-ref prefix-guess)
      (call-interactively 'my/org-insert-label-ref))))

(defun my/org-snippet-get-unique-label (prefix)
  "Prompt for a new snippet label with PREFIX, ensuring it is unique.
IntDended to be called from a Yasnippet."
  (let ((new-label nil))
    (progn
      (while (progn
               (setq new-label (read-string (format "Label (%s): " prefix)))
               (cond
                ((string-empty-p new-label)
                 (message "Label cannot be empty. Try again.")
                 t)
                ;; Check both #+label: and \label{} formats
                ((or (save-excursion
                       (goto-char (point-min))
                       (search-forward-regexp (concat "#\\+label:[ \t]+" (regexp-quote prefix) (regexp-quote new-label)) nil t))
                     (save-excursion
                       (goto-char (point-min))
                       (search-forward-regexp (concat "\\\\label{" (regexp-quote prefix) (regexp-quote new-label) "}") nil t)))
                 (message "Label '%s%s' already exists! Try again." prefix new-label)
                 t)
                (t nil))))
        nil)
      new-label))

(provide 'cross-refs)

;;; cross-refs.el ends here
