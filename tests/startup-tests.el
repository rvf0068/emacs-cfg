;;; startup-tests.el --- Early startup policy -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest rvf/startup-early-init-disables-package-el ()
  ;; Simulate Emacs's default startup setting independently of the runner.
  (let ((package-enable-at-startup t))
    (load (expand-file-name "early-init.el" user-emacs-directory)
          nil 'nomessage)
    (should-not package-enable-at-startup)))

;;; startup-tests.el ends here
