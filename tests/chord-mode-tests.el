;;; chord-mode-tests.el --- ChordPro regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'chord-mode)

(ert-deftest rvf/chord-transpose-flats-and-slash-bass ()
  (with-temp-buffer
    (insert "[Dbmaj7/F] [Ebmin7/Bb] [Gb] [Ab] [Cb] [E#] [C6/9]")
    (chord-up-a-semitone)
    (should (equal (buffer-string)
                   "[Dmaj7/F#] [Emin7/B] [G] [A] [C] [F#] [C#6/9]"))))

(ert-deftest rvf/chord-transpose-negative-and-large-intervals ()
  (with-temp-buffer
    (insert "[C] [G/B] [Am] [F]")
    (chord-down-a-semitone)
    (should (equal (buffer-string) "[B] [F#/A#] [G#m] [E]"))
    (chord-up 25)
    (should (equal (buffer-string) "[C] [G/B] [Am] [F]"))
    (chord-up -2)
    (should (equal (buffer-string) "[A#] [F/A] [Gm] [D#]"))))

(ert-deftest rvf/chord-transpose-preserves-annotations-and-comments ()
  (with-temp-buffer
    (insert "# Example [C]\n  # [D]\n[Chorus] [Bridge] [Coda] [Am7] [G7(b9)]")
    (chord-up 2)
    (should (equal (buffer-string)
                   "# Example [C]\n  # [D]\n[Chorus] [Bridge] [Coda] [Bm7] [A7(b9)]"))))

(ert-deftest rvf/chord-transpose-respects-narrowing-and-octaves ()
  (with-temp-buffer
    (insert "[Db]\n[Eb] [G/B]\n[Ab]")
    (goto-char (point-min))
    (forward-line 1)
    (narrow-to-region (point) (line-end-position))
    (let ((before (buffer-string)))
      (chord-up 12)
      (chord-up 0)
      (should (equal before (buffer-string))))
    (chord-up 1)
    (should (equal (buffer-string) "[E] [G#/C]"))
    (should (buffer-narrowed-p))
    (widen)
    (should (equal (buffer-string) "[Db]\n[E] [G#/C]\n[Ab]"))))

(ert-deftest rvf/chord-mode-keybindings-and-comment-fontification ()
  (with-temp-buffer
    (chord-mode)
    (insert "# Comment\n[C] text\n")
    (font-lock-ensure)
    (should (eq (get-text-property (point-min) 'face) 'font-lock-comment-face))
    (should (eq (local-key-binding (kbd ">")) #'chord-up-a-semitone))
    (should (eq (local-key-binding (kbd "<")) #'chord-down-a-semitone))
    (should (eq (local-key-binding (kbd "[")) #'chord-insert-chord))))

;;; chord-mode-tests.el ends here
