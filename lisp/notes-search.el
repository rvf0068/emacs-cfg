;;; notes-search.el --- Search mathematical Org notes -*- lexical-binding: t; -*-

(require 'consult)
(require 'subr-x)

(defvar my-notes-directory nil
  "Directory containing Org notes, normally set in personal.org.")

;;;###autoload
(defun bms/org-roam-rg-search ()
  "Search Org files in the configured notes directory using Consult.
Use case-insensitive matching and preserve Consult's normal output protocol
and live preview.  Leave ordinary `consult-ripgrep' searches unchanged."
  (interactive)
  (unless (and (boundp 'my-notes-directory)
               (stringp my-notes-directory)
               (not (string-empty-p my-notes-directory))
               (file-directory-p my-notes-directory))
    (user-error "Set my-notes-directory to an existing directory in personal.org"))
  (let ((consult-ripgrep-args
         (append (if (listp consult-ripgrep-args)
                     consult-ripgrep-args
                   (list consult-ripgrep-args))
                 '("--type org --ignore-case --max-columns=500"))))
    (consult-ripgrep (file-name-as-directory
                      (expand-file-name my-notes-directory)))))

(provide 'notes-search)
;;; notes-search.el ends here
