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

;; (april 26, 2026): modernized with the help of Claude AI

;;; Code:

(require 'generic)
(require 'subr-x)

;;;###autoload
(define-generic-mode 'chord-mode
   nil  ; comments
   (list "{soc}" "{eoc}")  ; keywords
   '(("\\(\\[[^]]*\\]\\)"       1 'font-lock-string-face)
     ("^#.*" 0 'font-lock-comment-face)
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


(defconst chord--pitch-classes
  '(("C" . 0) ("B#" . 0) ("C#" . 1) ("Db" . 1)
    ("D" . 2) ("D#" . 3) ("Eb" . 3) ("E" . 4) ("Fb" . 4)
    ("F" . 5) ("E#" . 5) ("F#" . 6) ("Gb" . 6)
    ("G" . 7) ("G#" . 8) ("Ab" . 8) ("A" . 9)
    ("A#" . 10) ("Bb" . 10) ("B" . 11) ("Cb" . 11))
  "Pitch classes of conventional single-accidental chord roots.")

(defconst chord--sharp-names
  ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"]
  "Output spelling, preserving the historical preference for sharps.")

(defconst chord--quality-regexp
  (concat "^\\(?:"
          (regexp-opt '("maj" "min" "dim" "aug" "sus" "add" "omit"
                        "no" "alt" "dom" "m" "M" "Δ" "ø" "°"))
          "\\|[-0-9#b+()/,]\\)*$")
  "Recognize conventional chord-quality notation.")

(defun chord--transpose-note (note semitones)
  "Transpose NOTE by SEMITONES, using the historical sharp spelling."
  (aref chord--sharp-names
        (mod (+ (cdr (assoc note chord--pitch-classes)) semitones) 12)))

(defun chord--transpose-token (token semitones)
  "Transpose a bracketed chord TOKEN, leaving its quality unchanged.
Recognize a final slash bass but not a numeric extension such as 6/9.
Return TOKEN unchanged for unrecognized chord symbols or annotations."
  (save-match-data
    (let ((case-fold-search nil))
      (if (not (string-match "^\\([A-G][#b]?\\)\\(.*\\)$" token))
          token
        (let* ((root (match-string 1 token))
               (quality (match-string 2 token))
               bass)
          (when (string-match "/\\([A-G][#b]?\\)$" quality)
            (setq bass (match-string 1 quality)
                  quality (substring quality 0 (match-beginning 0))))
          (if (not (string-match-p chord--quality-regexp quality))
              token
            (concat (chord--transpose-note root semitones)
                    quality
                    (when bass
                      (concat "/" (chord--transpose-note bass semitones))))))))))

(defun chord-up (semitones)
  "Transpose bracketed chords by SEMITONES, which may be negative.
Preserve chord qualities, transpose slash basses, and leave comments
and unrecognized bracketed annotations alone.  A whole octave is a
no-op.  Operate on the accessible part of the buffer in one pass."
  (interactive "p")
  (unless (zerop (mod semitones 12))
    (save-excursion
      (goto-char (point-min))
      (atomic-change-group
        (while (re-search-forward "\\[\\([^][\n]+\\)\\]" nil t)
          (unless (save-excursion
                    (beginning-of-line)
                    (looking-at-p "[ \t]*#"))
            (let* ((token (match-string-no-properties 1))
                   (transposed (chord--transpose-token token semitones)))
              (unless (equal token transposed)
                (replace-match transposed t t nil 1)))))))))

(defun chord-up-a-semitone ()
  "Transpose the accessible ChordPro buffer up by one semitone."
  (interactive)
  (chord-up 1))

(defun chord-down-a-semitone ()
  "Transpose the accessible ChordPro buffer down by one semitone."
  (interactive)
  (chord-up -1))

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
