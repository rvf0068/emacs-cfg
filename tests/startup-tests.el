;;; startup-tests.el --- Early startup policy -*- lexical-binding: t; -*-

(require 'ert)

(defvar rvf/startup-gc-enabled)
(defvar rvf/startup-gc-original)

(ert-deftest rvf/startup-early-init-disables-package-el ()
  ;; Simulate Emacs's default startup setting independently of the runner.
  (let ((package-enable-at-startup t)
        (rvf/startup-gc-enabled nil))
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

(defmacro rvf/test-with-startup-gc (&rest body)
  "Evaluate BODY with isolated startup policy and hook state."
  (declare (indent 0))
  `(let ((gc-cons-threshold 800000)
         (rvf/startup-gc-enabled t)
         (rvf/startup-gc-original nil)
         (after-init-hook nil)
         (process-environment (copy-sequence process-environment)))
     (setenv "RVF_STARTUP_GC" nil)
     ,@body))

(defun rvf/test-load-early-init ()
  (load (expand-file-name "early-init.el" user-emacs-directory)
        nil 'nomessage))

(ert-deftest rvf/startup-production-gc-restores-original ()
  (rvf/test-with-startup-gc
    (rvf/test-load-early-init)
    (should (= gc-cons-threshold (* 32 1024 1024)))
    ;; Reloading early-init must not overwrite the saved threshold.
    (rvf/test-load-early-init)
    (run-hooks 'after-init-hook)
    (should (= gc-cons-threshold 800000))
    (should-not rvf/startup-gc-original)
    (should-not (memq #'rvf/restore-startup-gc after-init-hook))))

(ert-deftest rvf/startup-production-gc-preserves-private-override ()
  (rvf/test-with-startup-gc
    (rvf/test-load-early-init)
    (setq gc-cons-threshold (* 8 1024 1024))
    (run-hooks 'after-init-hook)
    (should (= gc-cons-threshold (* 8 1024 1024)))))

(ert-deftest rvf/startup-production-gc-opt-out-and-higher-threshold ()
  (rvf/test-with-startup-gc
    (setenv "RVF_STARTUP_GC" "baseline")
    (rvf/test-load-early-init)
    (should (= gc-cons-threshold 800000))
    (should-not rvf/startup-gc-original)
    (setenv "RVF_STARTUP_GC" nil)
    (setq gc-cons-threshold (* 64 1024 1024))
    (rvf/test-load-early-init)
    (run-hooks 'after-init-hook)
    (should (= gc-cons-threshold (* 64 1024 1024)))))

;;; startup-tests.el ends here
