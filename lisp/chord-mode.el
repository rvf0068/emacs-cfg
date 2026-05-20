;;; -*- lexical-binding: t; -*-
;; chord-mode.el --- Major mode for editing Chord Pro files

;; Copyright (C) 2001 by Rafael Villarroel
;; My first Emacs major mode file!  (March 9, 2001)

;; For now, we only make the character "[" 'active', it inserts the closing
;; bracket and capitalizes the next character (so that we don't need to press
;; Caps Lock to type the chord

;;; History:

;; (august 13, 2001): Added chord-up-a-semitone

;; (november 24, 2001): Sort of rewritten using generic-mode, based
;; on chordpro-mode.el, by Daniel Polansky

;; (december 6, 2011): chordpack can be found at
;; http://sites.google.com/site/danpolansky/chordpack

;;; Code:

(require 'generic)

;;;###autoload
(define-generic-mode 'chord-mode
   nil  ; comments
   (list "{soc}" "{eoc}")  ; keywords
   '(("\\(\\[[^]]*\\]\\)"       1 'font-lock-string-face)
     ("^#.*" 1 'font-lock-comment-face)
     ("\\({subtitle[^}]*}\\|{st:[^}]*}\\)"         1 'font-lock-type-face)
     ("\\({title[^}]*}\\|{t:[^}]*}\\)"         1 'font-lock-keyword-face)
     ("\\({[^}]*}\\)"         1 'font-lock-variable-name-face)
     )
   (list "\\.crd\\'")
   (list 'generic-chord-mode-setup-function))

(defvar chord-mode-map (make-sparse-keymap)
  "Keymap for Chord Mode.")

(defun chord-insert-title ()
  "Insert curly parentheses for title and subtitle."
  (interactive)
  (insert "{t: }\n")
  (insert "{st: }")
  (backward-char 1))

(defun chord-insert-columnbreak ()
  "Insert \\columnbreak."
  (interactive)
  (insert "\\columnbreak"))

(defun chord-insert-smallskip ()
  "Insert \\smallskip."
  (interactive)
  (insert "\\smallskip"))


(defun chord-up-a-semitone ()
  "Transports a Chordpro file half a tone up"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "[" nil t)
    (cond
     ((looking-at "A#") (progn (insert "B")  (delete-char 2)))
     ((looking-at "Bb") (progn (insert "B")  (delete-char 2)))
     ((looking-at "A")  (progn (insert "A#") (delete-char 1)))
     ((looking-at "B")  (progn (insert "C")  (delete-char 1)))
     ((looking-at "C#") (progn (insert "D")  (delete-char 2)))
     ((looking-at "C")  (progn (insert "C#") (delete-char 1)))
     ((looking-at "D#") (progn (insert "E")  (delete-char 2)))
     ((looking-at "D")  (progn (insert "D#") (delete-char 1)))
     ((looking-at "E")  (progn (insert "F")  (delete-char 1)))
     ((looking-at "F#") (progn (insert "G")  (delete-char 2)))
     ((looking-at "F")  (progn (insert "F#") (delete-char 1)))
     ((looking-at "G#") (progn (insert "A")  (delete-char 2)))
     ((looking-at "G")  (progn (insert "G#") (delete-char 1)))
     ))
  (goto-char (point-min))
  )

(defun chord-up (semitones)
  "Transpose the current ChordPro buffer up by SEMITONES half steps."
  (interactive "p")
  (dotimes (_ semitones)
    (chord-up-a-semitone)))

(defun chord-down-a-semitone ()
  "Transports a Chordpro file half a tone down"
  (interactive)
  (chord-up 11))

(defun chord-create-ascii ()
  "Creates ascii chords"
  (interactive)
  (let* ((input-file (buffer-file-name))
         (chord-new-txt-file (concat (file-name-sans-extension input-file) ".txt")))
    (shell-command
     (concat "chordpack ascii "
             (shell-quote-argument input-file)
             " > "
             (shell-quote-argument chord-new-txt-file)))
    (find-file chord-new-txt-file)))

(defun chord-create-latex ()
  "Creates LaTeX source of chords"
  (interactive)
  (let* ((input-file (buffer-file-name))
         (chord-new-tex-file (concat (file-name-sans-extension input-file) ".tex")))
    (shell-command
     (concat "chordpack tex "
             (shell-quote-argument input-file)
             " > "
             (shell-quote-argument chord-new-tex-file)))
    (find-file chord-new-tex-file)))

(defun chord-create-html ()
  "Creates HTML source of chords"
  (interactive)
  (let* ((input-file (buffer-file-name))
         (chord-new-html-file (concat (file-name-sans-extension input-file) ".html")))
    (shell-command
     (concat "chordpack html "
             (shell-quote-argument input-file)
             " > "
             (shell-quote-argument chord-new-html-file)))
    (find-file chord-new-html-file)))

(defun chord-create-nochords ()
  "Creates a text file without chords"
  (interactive)
  (let* ((input-file (buffer-file-name))
         (chord-new-txtnc-file (concat (file-name-sans-extension input-file) ".txt")))
    (shell-command
     (concat "chordpack nochord "
             (shell-quote-argument input-file)
             " > "
             (shell-quote-argument chord-new-txtnc-file)))
    (find-file chord-new-txtnc-file)))

(defun chord-insert-chord ()
  "Inserts a chord with capitalization"
  (interactive)
  (insert "[]")
  (forward-char -1)
  ;; read-char receives the next character, but transforms it
  ;; into its ascii character, so we need to use char-to-string to
  ;; return the actual char. Then we capitalize it, but we need
  ;; insert to actually put it in the buffer
  (insert (capitalize (char-to-string (read-char)))))

(define-key chord-mode-map (kbd "<") #'chord-down-a-semitone)
(define-key chord-mode-map (kbd ">") #'chord-up-a-semitone)
(define-key chord-mode-map (kbd "[") #'chord-insert-chord)
(define-key chord-mode-map "\C-c\C-a" #'chord-create-ascii)
(define-key chord-mode-map "\C-c\C-b" #'chord-insert-columnbreak)
(define-key chord-mode-map "\C-c\C-h" #'chord-create-html)
(define-key chord-mode-map "\C-c\C-l" #'chord-create-latex)
(define-key chord-mode-map "\C-c\C-s" #'chord-insert-smallskip)
(define-key chord-mode-map "\C-c\C-t" #'chord-insert-title)
(define-key chord-mode-map "\C-c\C-n" #'chord-create-nochords)

(defun generic-chord-mode-setup-function ()
  (use-local-map chord-mode-map)
  (run-hooks 'chord-mode-hook))

(provide 'chord-mode)

;;; chord-mode.el ends here
