;;; run-workflows.el --- Isolated graphical integration runner -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'ert)
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
      (condition-case err
          (cl-letf (((symbol-function 'y-or-n-p) #'rvf/test-install-answer)
                    ((symbol-function 'yes-or-no-p) #'rvf/test-install-answer))
            (let ((early (expand-file-name "early-init.el" user-emacs-directory)))
              (when (file-exists-p early) (load early nil 'nomessage)))
            (load user-init-file nil 'nomessage)
            (load (expand-file-name "tests/workflow-tests.el" user-emacs-directory))
            (let* ((mode (getenv "RVF_TEST_MODE"))
                   (selector (pcase mode
                               ("latex" "^rvf/workflow-latex-")
                               ("jupyter" "^rvf/workflow-jupyter-")
                               (_ "^rvf/workflow-")))
                   (stats (ert-run-tests-batch selector)))
              (setq status (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))
        (error (message "Integration runner failed: %S" err)))
    (when (get-buffer "*Messages*")
      (with-current-buffer "*Messages*"
        (write-region (point-min) (point-max) (getenv "RVF_TEST_LOG") nil 'silent)))
    (kill-emacs status)))
