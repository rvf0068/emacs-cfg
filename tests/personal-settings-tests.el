;;; personal-settings-tests.el --- Portable private settings -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'personal-settings)

(ert-deftest rvf/personal-settings-fresh-install ()
  (let* ((user-emacs-directory (make-temp-file "rvf-personal-" t))
         (my-notes-directory nil)
         (my-library-path nil)
         (my-bibliography nil))
    (unwind-protect
        (progn
          (rvf/prepare-personal-settings)
          (should (equal my-notes-directory (rvf/local-data-path "notes/")))
          (should (equal my-library-path (rvf/local-data-path "library/")))
          (should (rvf/notes-directory-ready-p))
          (should (file-directory-p my-library-path))
          (should (rvf/default-notes-directory-p))
          (should-not (rvf/bibliography-files))
          (should-not (file-exists-p (rvf/local-data-path "org-roam.db"))))
      (delete-directory user-emacs-directory t))))

(ert-deftest rvf/personal-settings-existing-and-missing-paths ()
  (let* ((user-emacs-directory (make-temp-file "rvf-personal-" t))
         (my-notes-directory (expand-file-name "existing" user-emacs-directory))
         (my-library-path (expand-file-name "missing" user-emacs-directory))
         warnings)
    (unwind-protect
        (progn
          (make-directory my-notes-directory)
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings))))
            (rvf/prepare-personal-settings))
          (should (equal my-notes-directory
                         (expand-file-name "existing/" user-emacs-directory)))
          (should (equal my-library-path
                         (expand-file-name "missing/" user-emacs-directory)))
          (should (rvf/notes-directory-ready-p))
          (should-not (rvf/default-notes-directory-p))
          (should-not (file-exists-p my-library-path))
          (should (= (length warnings) 1))
          (let ((my-notes-directory my-library-path))
            (should-not (rvf/notes-directory-ready-p))))
      (delete-directory user-emacs-directory t))))

(ert-deftest rvf/personal-settings-bibliography-shapes ()
  (let* ((user-emacs-directory temporary-file-directory)
         (a (expand-file-name "a.bib" user-emacs-directory))
         (b (expand-file-name "b.bib" user-emacs-directory)))
    (let ((my-bibliography "a.bib"))
      (should (equal (rvf/bibliography-files) (list a)))
      (should (equal my-bibliography "a.bib")))
    (let ((my-bibliography (list "a.bib" b a)))
      (should (equal (rvf/bibliography-files) (list a b)))
      (should (equal my-bibliography (list "a.bib" b a))))
    (let ((my-bibliography nil))
      (should-not (rvf/bibliography-files)))
    (let ((my-bibliography '("a.bib" nil)))
      (should-error (rvf/bibliography-files) :type 'user-error))))

;;; personal-settings-tests.el ends here
