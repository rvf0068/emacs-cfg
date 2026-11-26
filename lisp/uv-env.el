;;; uv-env.el --- Buffer-local Python project environments -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defvar-local rvf/uv-environment nil
  "Absolute path of the .venv selected by this helper, or nil.")
(defvar-local rvf/uv--saved-settings nil
  "Original buffer-local settings before this helper activated an environment.")

(defun rvf/uv-find-environment (&optional directory)
  "Find the nearest usable project .venv above DIRECTORY.
A candidate must have a pyproject.toml and executable .venv/bin/python.
Continue past nested package manifests without an environment, but do
not cross the nearest Git repository boundary.  Do not probe TRAMP paths.
This is filesystem discovery, not a full parser of uv workspace membership."
  (let ((dir (file-name-as-directory (expand-file-name
                                      (or directory default-directory))))
        found)
    (unless (file-remote-p dir)
      (while (and dir (not found))
        (let* ((venv (expand-file-name ".venv" dir))
               (python (expand-file-name "bin/python" venv))
               (parent (file-name-directory (directory-file-name dir))))
          (when (and (file-regular-p (expand-file-name "pyproject.toml" dir))
                     (file-regular-p python)
                     (file-executable-p python))
            (setq found venv))
          (setq dir (unless (or (file-exists-p (expand-file-name ".git" dir))
                                (equal dir parent))
                      parent)))))
    found))

(defun rvf/uv--save-settings (variables)
  "Remember the original local bindings of VARIABLES in this buffer."
  (setq rvf/uv--saved-settings
        (mapcar (lambda (var)
                  (list var (local-variable-p var)
                        (boundp var)
                        (and (boundp var) (symbol-value var))))
                variables)))

(defun rvf/uv-clear-environment ()
  "Undo this helper's buffer-local activation without changing global values."
  (interactive)
  (dolist (entry rvf/uv--saved-settings)
    (pcase-let ((list var local bound value) entry)
      (if local
          (if bound (set (make-local-variable var) value)
            (makunbound (make-local-variable var)))
        (kill-local-variable var))))
  (setq rvf/uv--saved-settings nil
        rvf/uv-environment nil))

(defun my/find-uv-venv ()
  "Activate the nearest usable Python environment for the current buffer.
Retain the historical command name.  The Python interpreter, Babel,
executable lookup, and subprocess environment are all buffer-local.
Do not install packages, run uv, or change an existing Jupyter session."
  (interactive)
  (let ((venv (rvf/uv-find-environment)))
    ;; Restore first so repeated calls cannot accumulate PATH entries or
    ;; retain a previous project's environment.
    (rvf/uv-clear-environment)
    (when venv
      (let* ((bin (expand-file-name "bin" venv))
             (python (expand-file-name "python" bin)))
        (rvf/uv--save-settings
         '(python-shell-interpreter python-shell-virtualenv-root
           org-babel-python-command exec-path process-environment))
        (setq-local rvf/uv-environment venv
                    python-shell-interpreter python
                    python-shell-virtualenv-root venv
                    org-babel-python-command python
                    exec-path (cons bin (remove bin exec-path))
                    process-environment (copy-sequence process-environment))
        (setenv "VIRTUAL_ENV" venv)
        (setenv "PYTHONHOME" nil)
        (setenv "PATH"
                (mapconcat #'identity
                           (cons bin (remove bin (split-string (or (getenv "PATH") "")
                                                                path-separator t)))
                           path-separator))))
    (when (called-interactively-p 'interactive)
      (message "%s" (if venv (format "Activated uv environment: %s" venv)
                      "No usable project .venv; using original settings")))
    venv))

(defun rvf/describe-python-environment ()
  "Display Python executable and environment resolution in this buffer.
The Jupyter executable is not necessarily the interpreter of a kernel:
an explicit kernelspec or an existing session may select another Python."
  (interactive)
  (let ((report
         (format
          "Directory: %s\nDetected .venv: %s\nActivated .venv: %s\nPython shell: %s\nBabel Python: %s\nVIRTUAL_ENV: %s\npython on exec-path: %s\npylsp on exec-path: %s\njupyter on exec-path: %s\n\nJupyter kernels are selected by kernelspec/session, not PATH alone.\n"
          default-directory (or (rvf/uv-find-environment) "none")
          (or rvf/uv-environment "none")
          (and (boundp 'python-shell-interpreter) python-shell-interpreter)
          (and (boundp 'org-babel-python-command) org-babel-python-command)
          (getenv "VIRTUAL_ENV") (executable-find "python")
          (executable-find "pylsp") (executable-find "jupyter"))))
    (with-help-window "*Python environment*"
      (princ report))))

(provide 'uv-env)
;;; uv-env.el ends here
