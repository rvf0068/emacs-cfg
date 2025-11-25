;;; uv-env.el --- Helper to activate uv environments -*- lexical-binding: t; -*-

(defun my/find-uv-venv ()
  "Search for a .venv directory in the project root and set it as the python environment."
  (interactive)
  (let* ((project-root (locate-dominating-file default-directory "pyproject.toml"))
         (venv-path (when project-root (expand-file-name ".venv" project-root))))
    (when (and venv-path (file-directory-p venv-path))
      (let ((python-bin (expand-file-name "bin/python" venv-path)))
        ;; Tell python-mode and generic comint to use this binary
        (setq-local python-shell-interpreter python-bin)
        ;; Tell Org-babel to use this binary (essential for your math notebooks)
        (setq-local org-babel-python-command python-bin)
        (message "Activated uv environment: %s" venv-path)))))

(provide 'uv-env)
