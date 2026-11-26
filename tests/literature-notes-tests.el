;;; literature-notes-tests.el --- Literature capture tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'personal-settings)
(require 'literature-notes)

(ert-deftest rvf/literature-note-pdf-selection ()
  (let* ((dir (make-temp-file "rvf-literature-" t))
         (my-library-path dir)
         (canonical (expand-file-name "paper.pdf" dir))
         (alternate (expand-file-name "Other title.PDF" dir))
         (second (expand-file-name "second.pdf" dir))
         (text-file (expand-file-name "paper.txt" dir))
         (files (make-hash-table :test 'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'citar-get-files)
                   (lambda (key) (should (equal key "paper")) files)))
          (should (equal (rvf/literature-note-pdf "paper") canonical))
          (dolist (file (list alternate second text-file))
            (with-temp-file file (insert "fixture")))
          (puthash "paper" (list alternate alternate text-file) files)
          (should (equal (rvf/literature-note-pdf "paper") alternate))
          (puthash "paper" (list alternate second) files)
          (should (equal (rvf/literature-note-pdf "paper") ""))
          (with-temp-file canonical (insert "fixture"))
          (should (equal (rvf/literature-note-pdf "paper") canonical)))
      (delete-directory dir t))))

(ert-deftest rvf/literature-note-optional-setup-and-footer ()
  (let* ((dir (make-temp-file "rvf-literature-" t))
         (default-directory (file-name-as-directory dir))
         (my-literature-note-setupfile "options.org")
         (my-literature-note-extra "* References\n"))
    (unwind-protect
        (progn
          (should (equal (rvf/literature-note-setup nil) ""))
          (with-temp-file (expand-file-name "options.org" dir)
            (insert "#+options: toc:nil\n"))
          (should (equal (rvf/literature-note-setup nil)
                         "#+setupfile: options.org\n"))
          (let ((my-literature-note-setupfile nil))
            (should (equal (rvf/literature-note-setup nil) "")))
          (should (equal (rvf/literature-note-footer nil) "* References\n")))
      (delete-directory dir t))))

;;; literature-notes-tests.el ends here
