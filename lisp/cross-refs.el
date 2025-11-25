;; -*- lexical-binding: t; -*-

;;; cross-refs.el --- Smart cross-reference helpers for Org mode

;; Label conventions:
;; - Special blocks (theorem, definition, etc.): Use both #+name: and #+label:
;;   #+name: is for org internal linking, #+label: exports to LaTeX \label{}
;; - Equations: Use only #+name:
;;   Org-mode auto-generates \label{} from #+name: during export
;;   An export filter converts \ref{eq:*} to \eqref{eq:*} in LaTeX output

;; Export filter to convert \ref{eq:*} to \eqref{eq:*} in LaTeX
(with-eval-after-load 'ox-latex
  (defun my/org-latex-filter-eqref (text backend info)
    "Convert \\ref{eq:...} to \\eqref{eq:...} in LaTeX export."
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string
       "\\\\ref{\\(eq:[^}]+\\)}"
       "\\\\eqref{\\1}"
       text)))

  (add-to-list 'org-export-filter-final-output-functions
               #'my/org-latex-filter-eqref))

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
  "Insert a link to a #+name: or #+label: label, filtered by PREFIX."
  (let ((labels '())
        (regex-label (concat "^[ \t]*#\\+label:[ \t]+\\(\\(" (regexp-quote prefix) "[^ \t\n\r]+\\)\\)"))
        (regex-name (concat "^[ \t]*#\\+name:[ \t]+\\(\\(" (regexp-quote prefix) "[^ \t\n\r]+\\)\\)")))
    (save-excursion
      ;; Search for #+name: (used by both special blocks and equations)
      (goto-char (point-min))
      (while (re-search-forward regex-name nil t)
        (let ((label (match-string 1))
              (preview ""))
          (save-excursion
            (goto-char (match-end 0))
            ;; Try to get preview from special block content
            (when (re-search-forward "^[ \t]*\\(#\\+begin_\\|\\\\begin{\\)" nil t)
              (when (re-search-forward "^[ \t]*\\S-"
                                       (save-excursion 
                                         (or (re-search-forward "^[ \t]*\\(#\\+end_\\|\\\\end{\\)" nil t)
                                             (point-max))
                                         (point))
                                       t)
                (setq preview (string-trim (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position)))))))
          (push (cons (format "%-20s  %s" label preview) label) labels)))
      ;; Also search for #+label: (special blocks only, for compatibility with existing docs)
      (goto-char (point-min))
      (while (re-search-forward regex-label nil t)
        (let ((label (match-string 1)))
          (unless (assoc label labels) ; Avoid duplicates if both exist
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
              (push (cons (format "%-20s  %s" label preview) label) labels))))))

    (if (null labels)
        (message "No labels found matching '%s'." prefix)
      (let* ((stable-label-list (nreverse labels))
             (choice (completing-read (format "Ref (%s): " prefix) stable-label-list nil t)))
        (when choice
          (insert (format "[[%s]]" (cdr (assoc choice stable-label-list)))))))))

(defun my/org-insert-smart-ref ()
  "Intelligently insert a cross-reference based on context word."
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
Intended to be called from a Yasnippet."
  (let ((new-label nil))
    (while (progn
             (setq new-label (read-string (format "Label (%s): " prefix)))
             (cond
              ((string-empty-p new-label)
               (message "Label cannot be empty. Try again.")
               t)
              ;; Check both #+label: and #+name: formats
              ((save-excursion
                     (goto-char (point-min))
                     (re-search-forward (concat "^[ \t]*#\\+\\(label\\|name\\):[ \t]+" 
                                                (regexp-quote prefix) 
                                                (regexp-quote new-label) 
                                                "[ \t]*$") nil t))
               (message "Label '%s%s' already exists! Try again." prefix new-label)
               t)
              (t nil))))
    new-label))

(provide 'cross-refs)

;;; cross-refs.el ends here
