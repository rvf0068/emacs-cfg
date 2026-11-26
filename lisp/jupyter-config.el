;;; jupyter-config.el --- Optional Jupyter executable and kernel settings -*- lexical-binding: t; -*-

(require 'org)
(require 'subr-x)
(require 'personal-settings)

(defun rvf/jupyter-command-name ()
  "Return the Jupyter command to use, without changing PATH.
Honor an existing upstream jupyter-executable setting when the
optional private override is nil. Preserve bare command names for
buffer-local executable discovery."
  (let ((command (or my-jupyter-executable
                     (and (boundp 'jupyter-executable) jupyter-executable)
                     "jupyter")))
    (when (and (stringp command) (not (string-empty-p command)))
      (if (file-name-directory command)
          (expand-file-name command user-emacs-directory)
        command))))

(defun rvf/jupyter-available-p ()
  "Return the executable path when the configured Jupyter command is usable."
  (when-let* ((command (rvf/jupyter-command-name)))
    (if (file-name-absolute-p command)
        (and (file-regular-p command) (file-executable-p command) command)
      (executable-find command))))

(defun rvf/enable-babel-languages (&rest languages)
  "Enable LANGUAGES while retaining other configured Babel languages."
  (let ((settings (copy-tree org-babel-load-languages)))
    (dolist (language languages)
      (setf (alist-get language settings) t))
    (org-babel-do-load-languages 'org-babel-load-languages settings)))

(defun rvf/configure-jupyter-python-kernel ()
  "Select an optional default Python kernelspec without changing explicit headers.
Call only after ob-jupyter loads. Use its alias creation routine to
preserve all default header arguments and the language integration."
  (when my-jupyter-python-kernel
    (unless (and (stringp my-jupyter-python-kernel)
                 (not (string-empty-p my-jupyter-python-kernel)))
      (user-error "my-jupyter-python-kernel must be a nonempty kernelspec name"))
    (org-babel-jupyter-make-language-alias my-jupyter-python-kernel "python")
    (setq org-babel-default-header-args:jupyter-python
          (cons (cons :kernel my-jupyter-python-kernel)
                (assq-delete-all :kernel
                                 (copy-tree org-babel-default-header-args:jupyter-python))))))

(provide 'jupyter-config)
;;; jupyter-config.el ends here
