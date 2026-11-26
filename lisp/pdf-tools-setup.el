;;; pdf-tools-setup.el --- Non-interactive PDF Tools startup -*- lexical-binding: t; -*-

(defvar my-pdf-tools-offer-install nil
  "Whether to offer to build a missing PDF Tools server at startup.
Nil leaves the normal PDF viewer available and reports how to install
the server explicitly.  Non-nil restores the previous startup behavior.")

(declare-function pdf-tools-install "pdf-tools")
(declare-function pdf-tools-install-noverify "pdf-tools")
(declare-function pdf-info-check-epdfinfo "pdf-info")
(declare-function pdf-info-running-p "pdf-info")

(defun rvf/pdf-tools-initialize ()
  "Activate PDF Tools if its server works; do not build it by default."
  (if my-pdf-tools-offer-install
      (pdf-tools-install)
    (let ((failure
           (condition-case err
               (progn
                 (unless (pdf-info-running-p)
                   (pdf-info-check-epdfinfo))
                 nil)
             (error (error-message-string err)))))
      (if failure
          (display-warning
           'rvf-pdf-tools
           (format "PDF Tools not activated: %s. Run M-x pdf-tools-install when ready."
                   failure)
           :warning)
        (pdf-tools-install-noverify)))))

(provide 'pdf-tools-setup)
;;; pdf-tools-setup.el ends here
