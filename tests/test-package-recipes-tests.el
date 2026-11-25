;;; test-package-recipes-tests.el --- Regression tests for dependency setup -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-package-recipes)

(ert-deftest rvf/test-math-packages-registered ()
  ;; Check actual library availability without requiring packages.
  (dolist (library '("latex" "cdlatex" "smartparens" "math-delimiters"))
    (should (locate-library library))))

(ert-deftest rvf/test-straight-recipe-extraction ()
  (let* ((config (make-temp-file "rvf-test-recipes-" nil ".org"))
         (forms '((use-package tex :straight auctex)
                  (use-package cdlatex :straight t)
                  (use-package math-delimiters
                    :straight (:host github :repo "oantolin/math-delimiters"))
                  (use-package explicit :straight (other :host github :repo "a/b"))
                  (use-package missing :straight nil))))
    (unwind-protect
        (progn
          (with-temp-file config
            (insert "#+begin_src emacs-lisp\n")
            (dolist (form forms)
              (prin1 form (current-buffer))
              (insert "\n"))
            (insert "(error \"This form must not be evaluated\")\n#+end_src\n"))
          (let ((declarations (rvf/test-package-declarations config)))
            (should (equal forms declarations))
            (should (eq (rvf/test-package-recipe 'tex declarations) 'auctex))
            (should (eq (rvf/test-package-recipe 'cdlatex declarations) 'cdlatex))
            (should (equal (rvf/test-package-recipe 'math-delimiters declarations)
                           '(math-delimiters :host github :repo "oantolin/math-delimiters")))
            (should (equal (rvf/test-package-recipe 'explicit declarations)
                           '(other :host github :repo "a/b")))
            (should-error (rvf/test-package-recipe 'missing declarations))
            (should-error (rvf/test-package-recipe 'unknown declarations))))
      (delete-file config))))
