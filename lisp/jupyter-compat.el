;;; jupyter-compat.el --- Workarounds for pinned emacs-jupyter -*- lexical-binding: t; -*-

(require 'jupyter-monads)

;; Issue #613: the locked revision 3b9caed predates upstream commit
;; 77a983425f95ce361b5799c76706bb92d662bad0, which moves jupyter-mlet*
;; before its uses. Keep the previously working definition, but do not
;; install it before Jupyter has loaded its own implementation.
;; https://github.com/emacs-jupyter/jupyter/issues/613
(defun rvf/jupyter-get-client-compat ()
  "Return a monadic value that returns the client."
  (jupyter-mlet* ((state (jupyter-get-state))
                  (client (jupyter-return (if (listp state) (car state) state))))
    (cl-check-type client jupyter-kernel-client)
    (jupyter-return client)))

(advice-add 'jupyter-get-client :override #'rvf/jupyter-get-client-compat)

;; Issue #500 is still open. Preserve the established login behavior;
;; real local-kernel tests do not cover remote/server authentication.
;; https://github.com/emacs-jupyter/jupyter/issues/500#issuecomment-2323985951
(defun my-jupyter-api-http-request--ignore-login-error-a
    (func url endpoint method &rest data)
  (if (equal endpoint "login")
      (condition-case nil
          (apply func url endpoint method data)
        (jupyter-api-http-error nil))
    (apply func url endpoint method data)))

(with-eval-after-load 'jupyter-rest-api
  (advice-add 'jupyter-api-http-request :around
              #'my-jupyter-api-http-request--ignore-login-error-a))

;; Preserve the Corfu/incomplete-block compatibility behavior. These
;; guards affect only completion and the source-block cache, not kernel
;; execution. Revisit the broad catches with a dedicated completion test.
(defun my-safe-jupyter-org-completion-at-point (orig-fun &rest args)
  "Catch and suppress errors in Jupyter's Org completion function."
  (condition-case nil
      (apply orig-fun args)
    (error nil)))

(defun my-safe-jupyter-org-set-src-block-cache (orig-fun &rest args)
  "Run ORIG-FUN only in Org, catching incomplete-block errors."
  (when (derived-mode-p 'org-mode)
    (condition-case nil
        (apply orig-fun args)
      (error nil))))

(with-eval-after-load 'jupyter-org-client
  (advice-add 'jupyter-org-completion-at-point :around
              #'my-safe-jupyter-org-completion-at-point)
  (advice-add 'jupyter-org--set-src-block-cache :around
              #'my-safe-jupyter-org-set-src-block-cache))

(provide 'jupyter-compat)
;;; jupyter-compat.el ends here
