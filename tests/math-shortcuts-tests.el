;;; math-shortcuts-tests.el --- Math shortcut typing tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'latex)
(require 'math-shortcuts)

(defun rvf/math-shortcuts-test-type (keys)
  "Type KEYS in the current test buffer through actual key bindings."
  (save-window-excursion
    (switch-to-buffer (current-buffer))
    (execute-kbd-macro (kbd keys))))

(ert-deftest rvf/math-shortcuts-expand-and-toggle-in-real-math ()
  (dolist (mode '(org-mode LaTeX-mode))
    (dolist (letter '("R" "Q" "C" "Z" "N"))
      (ert-info ((format "%S, letter %s" mode letter))
        (with-temp-buffer
          (funcall mode)
          (math-shortcuts-mode 1)
          (insert "\\(x+y\\)")
          (goto-char 4)
          (should (math-shortcuts--in-math-p))
          (rvf/math-shortcuts-test-type letter)
          (should (equal (buffer-string)
                         (format "\\(x\\mathbb{%s}+y\\)" letter)))
          (rvf/math-shortcuts-test-type letter)
          (should (equal (buffer-string)
                         (format "\\(x%s+y\\)" letter))))))))

(ert-deftest rvf/math-shortcuts-delegate-to-underlying-key-binding ()
  (with-temp-buffer
    (org-mode)
    (let ((called 0))
      (use-local-map (copy-keymap (current-local-map)))
      (local-set-key
       (kbd "R")
       (lambda ()
         (interactive)
         (should (= last-command-event ?R))
         (cl-incf called)
         (insert "native")))
      (math-shortcuts-mode 1)
      (rvf/math-shortcuts-test-type "R")
      (should (equal (buffer-string) "native"))
      (should (= called 1))
      (should math-shortcuts-mode))))

(ert-deftest rvf/math-shortcuts-preserve-ordinary-text-and-prefix ()
  (dolist (mode '(org-mode LaTeX-mode))
    (with-temp-buffer
      (funcall mode)
      (math-shortcuts-mode 1)
      (rvf/math-shortcuts-test-type "R Q C Z N")
      (rvf/math-shortcuts-test-type "C-u 3 R")
      (should (equal (buffer-string) "RQCZNRRR")))))

(ert-deftest rvf/math-shortcuts-preserve-org-table-insertion ()
  ;; Compare the actual Org behavior with the shortcut disabled and enabled.
  (let (results)
    (dolist (enabled '(nil t))
      (with-temp-buffer
        (org-mode)
        (insert "| A   | B |\n")
        (goto-char (point-min))
        (search-forward "A")
        (math-shortcuts-mode (if enabled 1 -1))
        (rvf/math-shortcuts-test-type "R")
        (push (list (buffer-string) (point) org-table-may-need-update)
              results)))
    (should (equal (car results) (cadr results)))))

;;; math-shortcuts-tests.el ends here
