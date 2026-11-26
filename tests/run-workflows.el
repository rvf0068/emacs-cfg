;;; run-workflows.el --- Isolated graphical integration runner -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'ert)
(require 'subr-x)
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

(let ((status 1))
  (unwind-protect
      (cl-letf (((symbol-function 'y-or-n-p) #'rvf/test-install-answer)
                ((symbol-function 'yes-or-no-p) #'rvf/test-install-answer))
        ;; Keep the startup error handler separate from ERT: an outer
        ;; condition-case catches test failures before ERT's debugger can
        ;; record them, aborting the entire suite.
        (when (condition-case err
                  (progn
                    (let ((early (expand-file-name "early-init.el" user-emacs-directory)))
                      (when (file-exists-p early) (load early nil 'nomessage)))
                    (load user-init-file nil 'nomessage)
                    (load (expand-file-name "tests/workflow-tests.el" user-emacs-directory))
                    t)
                (error (message "Integration startup failed: %S" err) nil))
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
                      0 1)))))
    ;; Preserve subprocess diagnostics when a preview fails.  Messages alone
    ;; only report an exit code and cannot identify missing TeX dependencies.
    (unless (zerop status)
      (when-let* ((output (get-buffer "*Org Preview LaTeX Output*")))
        (message "Org preview subprocess output:\n%s"
                 (with-current-buffer output
                   (buffer-substring-no-properties (point-min) (point-max))))))
    (when (get-buffer "*Messages*")
      (with-current-buffer "*Messages*"
        (write-region (point-min) (point-max) (getenv "RVF_TEST_LOG") nil 'silent)))
    (kill-emacs status)))
