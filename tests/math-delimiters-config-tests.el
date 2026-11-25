;;; math-delimiters-config-tests.el --- Tests for math delimiter bindings -*- lexical-binding: t; -*-

(require 'ert)
(require 'use-package)

(defconst rvf/math-delimiters-test-config-file
  (expand-file-name "../emacs-config.org" (file-name-directory load-file-name)))

(ert-deftest rvf/math-delimiters-bindings-work-before-package-load ()
  (should-not (featurep 'math-delimiters))
  (let ((form
         (with-temp-buffer
           (insert-file-contents rvf/math-delimiters-test-config-file)
           (goto-char (point-min))
           (search-forward "(use-package math-delimiters\n")
           (goto-char (match-beginning 0))
           (read (current-buffer)))))
    ;; Straight handles installation in the real configuration.
    (should (equal (nth 2 form) :straight))
    (eval (append (seq-take form 2) (nthcdr 4 form))))
  (require 'latex)
  (require 'cdlatex)
  (should-not (featurep 'math-delimiters))
  (with-temp-buffer
    (LaTeX-mode)
    (cdlatex-mode 1)
    (should (eq (key-binding (kbd "$")) 'math-delimiters-insert))
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro "$"))
    (should (equal (buffer-string) "\\(\\)"))
    (should (= (point) 3)))
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (org-cdlatex-mode 1)
    (should (eq (key-binding (kbd "$")) 'math-delimiters-insert))
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (execute-kbd-macro "$"))
    (should (equal (buffer-string) "\\(\\)"))
    (should (= (point) 3))))

;;; math-delimiters-config-tests.el ends here
