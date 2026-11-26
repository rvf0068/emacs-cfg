;;; startup-tests.el --- Early startup policy -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest rvf/startup-early-init-disables-package-el ()
  ;; Simulate Emacs's default startup setting independently of the runner.
  (let ((package-enable-at-startup t))
    (load (expand-file-name "early-init.el" user-emacs-directory)
          nil 'nomessage)
    (should-not package-enable-at-startup)))

(load (expand-file-name "tests/startup-benchmark.el" user-emacs-directory)
      nil 'nomessage)

(ert-deftest rvf/startup-gc-policy-is-scoped ()
  (let ((gc-cons-threshold (* 2 1024 1024)))
    (should (= (rvf/benchmark-with-gc-policy "baseline"
                                           (lambda () gc-cons-threshold))
               (* 2 1024 1024)))
    (should (= (rvf/benchmark-with-gc-policy "tuned"
                                           (lambda () gc-cons-threshold))
               (* 32 1024 1024)))
    (should (= gc-cons-threshold (* 2 1024 1024)))
    (should-error (rvf/benchmark-with-gc-policy "tuned"
                                               (lambda () (error "test"))))
    (should (= gc-cons-threshold (* 2 1024 1024)))
    (should-error (rvf/benchmark-with-gc-policy "unknown" #'ignore))))

(ert-deftest rvf/startup-gc-policy-does-not-lower-existing-threshold ()
  (let ((gc-cons-threshold (* 64 1024 1024)))
    (should (= (rvf/benchmark-with-gc-policy "tuned"
                                           (lambda () gc-cons-threshold))
               (* 64 1024 1024)))))

;;; startup-tests.el ends here
