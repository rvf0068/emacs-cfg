;;; smartparens-math-config-tests.el --- Tests for math pairs -*- lexical-binding: t; -*-

(require 'ert)
(require 'use-package)

(defconst rvf/smartparens-math-test-config-file
  (expand-file-name "../emacs-config.org" (file-name-directory load-file-name)))

(defun rvf/smartparens-math-test-load-package-config (package)
  (let ((form
         (with-temp-buffer
           (insert-file-contents rvf/smartparens-math-test-config-file)
           (goto-char (point-min))
           (search-forward (format "(use-package %s\n" package))
           (goto-char (match-beginning 0))
           (read (current-buffer)))))
    ;; Straight handles installation in the real configuration.
    (should (eq (nth 2 form) :straight))
    (eval (append (seq-take form 2) (nthcdr 4 form)))))

(ert-deftest rvf/smartparens-pairs-bars-in-latex-math ()
  (rvf/smartparens-math-test-load-package-config 'cdlatex)
  (rvf/smartparens-math-test-load-package-config 'smartparens)
  (require 'latex)
  (require 'org)
  (dolist (case '((LaTeX-mode "\\(\\)" 3 "\\(||\\)" 4)
                  (LaTeX-mode "\\[\\]" 3 "\\[||\\]" 4)
                  (LaTeX-mode "$x$" 3 "$x||$" 4)
                  (LaTeX-mode "text" 5 "text|" 6)
                  (org-mode "\\(\\)" 3 "\\(||\\)" 4)
                  (org-mode "text" 5 "text|" 6)))
    (with-temp-buffer
      (funcall (nth 0 case))
      (if (eq major-mode 'LaTeX-mode)
          (cdlatex-mode 1)
        (org-cdlatex-mode 1))
      (insert (nth 1 case))
      (goto-char (nth 2 case))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (execute-kbd-macro "|"))
      (should (equal (buffer-string) (nth 3 case)))
      (should (= (point) (nth 4 case))))))

;;; smartparens-math-config-tests.el ends here
