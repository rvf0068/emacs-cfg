;;; run-unit.el --- Isolated ERT runner -*- lexical-binding: t; -*-
(setq user-emacs-directory (file-name-as-directory (getenv "RVF_TEST_DIR"))
      package-enable-at-startup nil)
(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)
      nil 'nomessage)
;; Match init.el's bootstrap order, without loading the configuration.
(straight-use-package 'org)
(straight-use-package 'transient)
(load (expand-file-name "tests/test-package-recipes.el" user-emacs-directory)
      nil 'nomessage)
;; Keep dependency names explicit, but obtain recipes from the actual
;; configuration. Register autoloads without eagerly requiring packages.
(let ((declarations
       (rvf/test-package-declarations
        (expand-file-name "emacs-config.org" user-emacs-directory))))
  (dolist (name '(org-roam elfeed embark citar citar-embark
                  tex cdlatex smartparens math-delimiters yasnippet))
    (straight-use-package (rvf/test-package-recipe name declarations))))
(require 'use-package)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(dolist (file (directory-files (expand-file-name "tests" user-emacs-directory)
                              t "-tests\\.el\\'"))
  (unless (string-suffix-p "/workflow-tests.el" file)
    (load file nil 'nomessage)))
(ert-run-tests-batch-and-exit)
