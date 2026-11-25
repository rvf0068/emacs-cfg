;; -*- lexical-binding: t; -*-

;;; cross-refs.el --- Smart cross-reference helpers for Org mode

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-element)

;; Label conventions:
;; - Special blocks retain both #+name: and #+label: for compatibility.
;;   Org treats #+label: as an older spelling of #+name:.
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
    ("table" . "tbl:")
    ("example" . "exa:")
    ("remark" . "rem:"))
  "Alist mapping context words to label prefixes for smart referencing.")

(defvar my/org-smart-ref-prefix-aliases
  '(("prf:" . ("pf:"))
    ("pro:" . ("prop:")))
  "Historical label prefixes accepted by context-sensitive references.
New snippets continue to use their existing canonical prefixes.")

(defconst my/org-ref-label-regexp
  "^[ \t]*#\\+\\(?:name\\|label\\):[ \t]+\\([^ \t\n\r]+\\)[ \t]*$"
  "Match a single-token Org name or label declaration.")

(defun my/org-ref-element-preview (element)
  "Return the first nonblank content line of ELEMENT, or an empty string.
Never search outside ELEMENT or use an opening or closing block marker."
  (let* ((type (org-element-type element))
         (start (org-element-property :post-affiliated element))
         (end (org-element-property :end element))
         (contents-begin (org-element-property :contents-begin element))
         (contents-end (org-element-property :contents-end element))
         (block-p (memq type '(special-block src-block example-block
                               export-block center-block quote-block
                               verse-block comment-block latex-environment))))
    (save-excursion
      (cond
       ((and contents-begin contents-end)
        (goto-char contents-begin)
        (setq end contents-end))
       ((and start end block-p)
        (goto-char start)
        (forward-line 1))
       ((and start end) (goto-char start))
       (t (setq end nil)))
      (let ((case-fold-search t))
        (if (and end (< (point) end)
                 (re-search-forward "^[ \t]*[^ \t\r\n]" end t))
            (let ((line (string-trim
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (min end (line-end-position))))))
              (if (or (string-prefix-p "#+begin_" line t)
                      (string-prefix-p "#+end_" line t)
                      (string-prefix-p "#+name:" line t)
                      (string-prefix-p "#+label:" line t)
                      (string-prefix-p "\\begin{" line)
                      (string-prefix-p "\\end{" line))
                  ""
                line))
          "")))))

(defun my/org-ref-labels ()
  "Return document labels and previews as (LABEL . PREVIEW) pairs.
Read real Org affiliated keywords, not text inside source/example blocks.
Preserve legacy standalone NAME/LABEL keywords with an empty preview.
Deduplicate exact label strings, preserving their document order."
  (save-restriction
    (widen)
    (save-excursion
      (save-match-data
        (let ((seen (make-hash-table :test 'equal))
              (case-fold-search t)
              labels)
          (cl-labels
              ((record (label preview)
                 (let ((entry (gethash label seen)))
                   (if entry
                       (when (and (string-empty-p (cdr entry))
                                  (not (string-empty-p preview)))
                         (setcdr entry preview))
                     (let ((new (cons label preview)))
                       (puthash label new seen)
                       (push new labels))))))
            (org-element-map (org-element-parse-buffer) org-element-all-elements
              (lambda (element)
                (let ((begin (org-element-property :begin element))
                      (post (org-element-property :post-affiliated element)))
                  ;; Org translates LABEL to NAME and may retain only one
                  ;; property; read each actual affiliated declaration.
                  (when (and begin post (< begin post))
                    (goto-char begin)
                    (let ((preview (my/org-ref-element-preview element)))
                      (while (re-search-forward my/org-ref-label-regexp post t)
                        (record (match-string-no-properties 1) preview))))
                  ;; Standalone keywords remain usable while editing.
                  (when (and (eq (org-element-type element) 'keyword)
                             (member (org-element-property :key element)
                                     '("NAME" "LABEL")))
                    (let ((label (string-trim
                                  (org-element-property :value element))))
                      (when (and (not (string-empty-p label))
                                 (not (string-match-p "[[:space:]]" label)))
                        (record label ""))))))))
          (nreverse labels))))))

(defun my/org-insert-label-ref (&optional prefix)
  "Insert a link to a document label, filtered by PREFIX.
An empty or omitted PREFIX offers all labels.  Recognize historical
aliases for the canonical prefixes in my/org-smart-ref-prefix-aliases."
  (interactive "sLabel prefix (empty for all): ")
  (let* ((prefix (or prefix ""))
         (prefixes (cons prefix (cdr (assoc prefix my/org-smart-ref-prefix-aliases))))
         (labels
          (cl-loop for (label . preview) in (my/org-ref-labels)
                   when (cl-some (lambda (p) (string-prefix-p p label t))
                                 prefixes)
                   collect (cons (format "%-20s  %s" label preview) label))))
    (if (null labels)
        (message "No labels found matching '%s'." prefix)
      (let* ((choice (completing-read (format "Ref (%s): " prefix)
                                      labels nil t))
             (label (cdr (assoc choice labels))))
        (when label
          (insert (format "[[%s]]" label)))))))

(defun my/org-insert-smart-ref ()
  "Insert a cross-reference based on the preceding context word.
When the context is unknown or empty, offer all document labels."
  (interactive)
  (let* ((text-before (buffer-substring-no-properties
                       (line-beginning-position) (point)))
         (words (split-string text-before "[^[:word:]_]+" t))
         (last-word (car (last words)))
         (prefix (and last-word
                      (cdr (assoc (downcase last-word) my/org-smart-ref-map)))))
    (my/org-insert-label-ref prefix)))

(defun my/org-snippet-get-unique-label (prefix)
  "Prompt for a new snippet label suffix with PREFIX, ensuring uniqueness.
Intended to be called from a Yasnippet.  Check the whole document,
even when narrowed, and preserve the existing case-insensitive check."
  (let ((existing (mapcar (lambda (entry) (downcase (car entry)))
                          (my/org-ref-labels)))
        new-label)
    (while
        (progn
          (setq new-label (read-string (format "Label (%s): " prefix)))
          (cond
           ((or (string-empty-p new-label)
                (string-match-p "[[:space:][:cntrl:]]" new-label)
                (string-match-p (regexp-opt '("[" "]" "{" "}" "\\")) new-label))
            (message "Use a nonempty label suffix without whitespace or delimiters. Try again.")
            t)
           ((member (downcase (concat prefix new-label)) existing)
            (message "Label '%s%s' already exists! Try again." prefix new-label)
            t)
           (t nil))))
    new-label))

(provide 'cross-refs)

;;; cross-refs.el ends here
