;;; notes-search-tests.el --- Consult notes search regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'notes-search)

(ert-deftest rvf/notes-search-real-ripgrep ()
  ;; This setting must be special across lexical-binding files; a bare
  ;; defvar without an initializer is only a local declaration.
  (should (special-variable-p 'my-notes-directory))
  ;; Avoid opening a minibuffer in batch Emacs.  Exercise the actual
  ;; Consult builder and ripgrep subprocess with the options the command
  ;; supplies, including the null-separated output Consult expects.
  (let* ((dir (make-temp-file "rvf-notes-search-" t))
         (my-notes-directory dir)
         (original consult-ripgrep-args))
    (unwind-protect
        (progn
          (dolist (name '("a.org" "b.org_archive" "c.txt" "d.py"))
            (with-temp-file (expand-file-name name dir)
              (insert "NEEDLE in a file\n")))
          (cl-letf (((symbol-function 'consult-ripgrep)
                     (lambda (directory &optional _initial)
                       (should (equal directory (file-name-as-directory dir)))
                       (let* ((default-directory directory)
                              (builder (consult--ripgrep-make-builder '(".")))
                              (command (car (funcall builder "needle"))))
                         (should (member "--null" command))
                         (should (member "--color=never" command))
                         (should (member "--ignore-case" command))
                         (should (member "--type" command))
                         (with-temp-buffer
                           (should (zerop (apply #'process-file (car command)
                                                 nil t nil (cdr command))))
                           (let ((output (buffer-string)))
                             (should (string-search (concat "a.org" (string 0)) output))
                             (should (string-search (concat "b.org_archive" (string 0)) output))
                             (should-not (string-search "c.txt" output))
                             (should-not (string-search "d.py" output))))))))
            (bms/org-roam-rg-search))
          (should (equal consult-ripgrep-args original)))
      (delete-directory dir t))))

(ert-deftest rvf/notes-search-preserves-custom-list-arguments ()
  (let* ((my-notes-directory temporary-file-directory)
         (consult-ripgrep-args '("rg --null --color=never" "--hidden"))
         (original (copy-tree consult-ripgrep-args))
         called)
    (cl-letf (((symbol-function 'consult-ripgrep)
               (lambda (&rest _)
                 (setq called t)
                 (should (equal (seq-take consult-ripgrep-args 2) original))
                 (should (member "--type org --ignore-case --max-columns=500"
                                 consult-ripgrep-args)))))
      (bms/org-roam-rg-search))
    (should called)
    (should (equal consult-ripgrep-args original))))

(ert-deftest rvf/notes-search-invalid-directory ()
  ;; Ensure these dynamic bindings really reach the command, so an
  ;; accidentally unbound variable cannot make all these cases pass.
  (should (special-variable-p 'my-notes-directory))
  (let ((my-notes-directory nil))
    (should-error (bms/org-roam-rg-search) :type 'user-error))
  (let ((my-notes-directory ""))
    (should-error (bms/org-roam-rg-search) :type 'user-error))
  (let ((my-notes-directory (make-temp-name
                             (expand-file-name "missing-notes-" temporary-file-directory))))
    (should-error (bms/org-roam-rg-search) :type 'user-error)))

;;; notes-search-tests.el ends here
