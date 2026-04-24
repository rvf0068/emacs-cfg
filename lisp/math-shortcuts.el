;;; math-shortcuts.el --- Math shortcuts for LaTeX and Org mode -*- lexical-binding: t; -*-

(require 'texmathp nil t) ; Try to load texmathp if available
(require 'org nil t)      ; Try to load org if available

(defun math-shortcuts--in-math-p ()
  "Check if point is in a math environment."
  (cond
   ((derived-mode-p 'org-mode)
    (and (fboundp 'org-inside-LaTeX-fragment-p)
         (org-inside-LaTeX-fragment-p)))
   ((derived-mode-p 'TeX-mode)
    (and (fboundp 'texmathp)
         (texmathp)))
   (t nil)))

(defun math-shortcuts--insert-mathbb (char)
  "Insert \\mathbb{CHAR} if in math mode, otherwise insert CHAR.
If \\mathbb{CHAR} is already the text immediately before point,
delete it and insert the plain CHAR instead."
  (if (math-shortcuts--in-math-p)
      (let* ((mathbb-str (format "\\mathbb{%c}" char))
             (len (length mathbb-str))
             (start (- (point) len)))
        (if (and (>= start (point-min))
                 (string= (buffer-substring-no-properties start (point))
                          mathbb-str))
            (progn
              (delete-char (- len))
              (insert (char-to-string char)))
          (insert mathbb-str)))
    (call-interactively #'self-insert-command)))

(defvar math-shortcuts-mode-map (make-sparse-keymap)
  "Keymap for math-shortcuts-mode.")

(defmacro math-shortcuts-define-key (key)
  "Define a command to insert \\mathbb{KEY} and bind it."
  (let ((cmd-name (intern (format "math-shortcuts-insert-%c" key))))
    `(progn
       (defun ,cmd-name ()
         (interactive)
         (math-shortcuts--insert-mathbb ,key))
       (define-key math-shortcuts-mode-map (char-to-string ,key) #',cmd-name))))

;; Define keys for standard number sets
(math-shortcuts-define-key ?R)
(math-shortcuts-define-key ?Q)
(math-shortcuts-define-key ?C)
(math-shortcuts-define-key ?Z)
(math-shortcuts-define-key ?N)

;;;###autoload
(define-minor-mode math-shortcuts-mode
  "Minor mode to quickly insert \\mathbb characters in math mode.
When enabled, pressing R, Q, C, Z, N inside a math environment
inserts \\mathbb{R}, \\mathbb{Q}, etc. Otherwise, it inserts the
character as normal."
  :lighter " BB"
  :keymap math-shortcuts-mode-map)

(provide 'math-shortcuts)
