;;; jupyter-config-tests.el --- Optional Jupyter configuration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'jupyter-config)

;; Declare the upstream variable special without loading Jupyter.
(defvar jupyter-executable "jupyter")

(ert-deftest rvf/jupyter-command-resolves-without-path ()
  (let* ((dir (make-temp-file "rvf-jupyter-command-" t))
         (program (expand-file-name "jupyter" dir))
         (my-jupyter-executable program)
         (exec-path nil))
    (unwind-protect
        (progn
          (with-temp-file program (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes program #o755)
          (should (equal (rvf/jupyter-command-name) program))
          (should (equal (rvf/jupyter-available-p) program))
          (should-not exec-path)
          (set-file-modes program #o644)
          (should-not (rvf/jupyter-available-p))
          (delete-file program)
          (should-not (rvf/jupyter-available-p)))
      (delete-directory dir t))))

(ert-deftest rvf/jupyter-command-preserves-upstream-selection ()
  (let ((my-jupyter-executable nil)
        (jupyter-executable "my-existing-jupyter"))
    (should (equal (rvf/jupyter-command-name) "my-existing-jupyter"))
    (let ((my-jupyter-executable "jupyter"))
      (should (equal (rvf/jupyter-command-name) "jupyter")))
    (should (equal jupyter-executable "my-existing-jupyter"))))

(ert-deftest rvf/jupyter-babel-language-settings-are-preserved ()
  (let ((org-babel-load-languages '((emacs-lisp . t) (python . nil) (shell . t)))
        loaded)
    (cl-letf (((symbol-function 'org-babel-do-load-languages)
               (lambda (symbol value)
                 (should (eq symbol 'org-babel-load-languages))
                 (setq loaded value))))
      (rvf/enable-babel-languages 'python 'latex))
    (should (alist-get 'python loaded))
    (should (alist-get 'latex loaded))
    (should (alist-get 'shell loaded))
    (should (alist-get 'emacs-lisp loaded))
    (should-not (alist-get 'jupyter loaded))
    (should-not (alist-get 'python org-babel-load-languages))))

;;; jupyter-config-tests.el ends here
