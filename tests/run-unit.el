;;; run-unit.el --- Isolated ERT runner -*- lexical-binding: t; -*-
(setq user-emacs-directory (file-name-as-directory (getenv "RVF_TEST_DIR"))
      package-enable-at-startup nil)
(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)
      nil 'nomessage)
;; Match the runtime bootstrap order, without loading the user's configuration.
;; Citar Embark has a separate straight recipe and is not built by citar.
(dolist (package '(org transient org-roam elfeed embark citar citar-embark))
  (straight-use-package package))
(require 'use-package)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(dolist (file (directory-files (expand-file-name "tests" user-emacs-directory)
                              t "-tests\\.el\\'"))
  (unless (string-suffix-p "/workflow-tests.el" file)
    (load file nil 'nomessage)))
(ert-run-tests-batch-and-exit)
