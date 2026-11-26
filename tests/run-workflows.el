;;; run-workflows.el --- Isolated graphical integration runner -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'ert)
(require 'subr-x)

;; The benchmark measures a stable byte-compiled installation, not the
;; CPU contention and temporary files of an asynchronous native build.
;; Do not change native compilation for normal regression runs.
(when (getenv "RVF_BENCHMARK_PHASE")
  (setq straight-disable-native-compile t
        native-comp-jit-compilation nil
        native-comp-deferred-compilation nil))
(when-let* ((program (getenv "RVF_EPDFINFO")))
  (setq pdf-info-epdfinfo-program program))
(setq user-emacs-directory (file-name-as-directory (getenv "RVF_TEST_DIR"))
      user-init-file (expand-file-name "init.el" user-emacs-directory)
      message-log-max t
      package-enable-at-startup nil)

(defun rvf/test-install-answer (prompt)
  "Answer only known installation prompts."
  (cond ((string-match-p "compatible module binary" prompt) nil)
        ((string-match-p "ZMQ module not found" prompt) t)
        ((string-match-p "rebuild.*epdfinfo" prompt) t)
        (t (error "Unexpected installation prompt: %s" prompt))))

(defun rvf/test-load-configuration ()
  "Load the public configuration and persist straight's build metadata."
  (let ((early (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p early) (load early nil 'nomessage)))
  (load user-init-file nil 'nomessage)
  (straight--transaction-finalize)
  (unless (file-regular-p (straight--build-cache-file))
    (error "Straight did not persist its build cache")))

(defun rvf/test-prepare-zmq ()
  "Ensure the actual native ZeroMQ module is loaded before kernel tests.
The pinned zmq-load can return after an unsuccessful source build
without signaling an error.  Detect that condition at its source."
  (when (featurep 'ob-jupyter)
    (require 'zmq)
    (zmq-load)
    (unless (and (featurep 'zmq-core) (boundp 'zmq-REQ))
      (error "ZeroMQ native module did not load; inspect compilation diagnostics"))))

(defvar rvf/benchmark-gc-policy
  (let ((policy (getenv "RVF_BENCHMARK_GC_POLICY")))
    (unless (or (null policy) (string-empty-p policy)) policy)))

(when rvf/benchmark-gc-policy
  (load (expand-file-name "tests/startup-benchmark.el" user-emacs-directory)
        nil 'nomessage))

(let ((status 1))
  (unwind-protect
      (cl-letf (((symbol-function 'y-or-n-p) #'rvf/test-install-answer)
                ((symbol-function 'yes-or-no-p) #'rvf/test-install-answer))
        ;; Keep the startup error handler separate from ERT: an outer
        ;; condition-case catches test failures before ERT's debugger can
        ;; record them, aborting the entire suite.
        (when (condition-case err
                  (progn
                    ;; These graphical processes load init.el outside the command
                    ;; loop.  Unlike a normal interactive startup, they may
                    ;; exit without ever running straight's post-command hook.
                    (let ((start (float-time))
                          (gc-start gcs-done)
                          (gc-seconds-start gc-elapsed))
                      (if-let* ((policy rvf/benchmark-gc-policy))
                          (rvf/benchmark-with-gc-policy
                           policy #'rvf/test-load-configuration)
                        (rvf/test-load-configuration))
                      (message "RVF STARTUP %s %.3f s (GCs %d, GC %.3f s)"
                               (or (getenv "RVF_BENCHMARK_PHASE")
                                   (getenv "RVF_TEST_PERSONAL") "configured")
                               (- (float-time) start)
                               (- gcs-done gc-start)
                               (- gc-elapsed gc-seconds-start)))
                    (message "RVF STARTUP loaded features: %S"
                             (cl-remove-if-not
                              #'featurep '(org latex org-roam citar embark magit
                                           gptel pdf-tools org-noter ob-jupyter
                                           elfeed bibtex smartparens)))
                    t)
                (error (message "Integration startup failed: %S" err) nil))
          (cond
           ((equal (getenv "RVF_BENCHMARK_PHASE") "warmup")
            (rvf/test-prepare-zmq)
            (setq status 0))
           ((equal (getenv "RVF_BENCHMARK_PHASE") "measure")
            (unless (featurep 'startup-benchmark)
              (load (expand-file-name "tests/startup-benchmark.el" user-emacs-directory)))
            (when-let* ((policy rvf/benchmark-gc-policy))
              ;; Measure the deferred collection separately.  Both policies
              ;; receive the same collection before the first-use probes.
              (message "RVF GC POLICY %s (restored threshold %d bytes)"
                       policy gc-cons-threshold)
              (rvf/benchmark-measure "post-startup-collection" #'garbage-collect))
            (rvf/benchmark-first-use)
            (setq status 0))
           (t
            (rvf/test-prepare-zmq)
            (load (expand-file-name "tests/workflow-tests.el" user-emacs-directory))
            (let* ((mode (getenv "RVF_TEST_MODE"))
                   (selector (if (equal (getenv "RVF_TEST_PERSONAL") "missing")
                               "^rvf/workflow-\\(personal-settings\\|babel-initialization\\|jupyter-uv-workspace-python\\|latex-real-previews\\)$"
                             (pcase mode
                               ("latex" "^rvf/workflow-latex-")
                               ("jupyter" "^rvf/workflow-jupyter-")
                               (_ "^rvf/workflow-"))))
                   (stats (ert-run-tests-batch selector)))
              (setq status
                    (if (and (> (ert-stats-total stats) 0)
                             (= (ert-stats-completed-expected stats)
                                (ert-stats-total stats))
                             (zerop (ert-stats-skipped stats))
                             (zerop (ert-stats-completed-unexpected stats)))
                        0 1)))))))
    ;; First-use probes may register additional packages.  Save their
    ;; metadata as well, but never conceal an unsuccessful regression.
    (when (and (zerop status) (fboundp 'straight--transaction-finalize))
      (condition-case err
          (straight--transaction-finalize)
        (error
         (message "Straight transaction finalization failed: %S" err)
         (setq status 1))))
    ;; Preserve subprocess diagnostics when a preview fails.  Messages alone
    ;; only report an exit code and cannot identify missing TeX dependencies.
    (unless (zerop status)
      (when-let* ((compilation (get-buffer "*compilation*")))
        (message "Native module compilation output:\n%s"
                 (with-current-buffer compilation
                   (buffer-substring-no-properties (point-min) (point-max)))))
      (when-let* ((output (get-buffer "*Org Preview LaTeX Output*")))
        (message "Org preview subprocess output:\n%s"
                 (with-current-buffer output
                   (buffer-substring-no-properties (point-min) (point-max))))))
    (when (get-buffer "*Messages*")
      (with-current-buffer "*Messages*"
        (write-region (point-min) (point-max) (getenv "RVF_TEST_LOG") nil 'silent)))
    (kill-emacs status)))
