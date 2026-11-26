;;; literature-notes.el --- Portable Citar literature captures -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)

(defvar my-library-path)
(defvar org-roam-capture--info)
(declare-function citar-get-files "citar")

(defvar my-literature-note-setupfile "options.org"
  "Optional setup file, relative to the new note or absolute.
Include the directive only when the file exists.  Nil disables it.")

(defvar my-literature-note-extra ""
  "Optional trusted Org capture text appended to new literature notes.
Set in personal.org to retain machine-specific bibliography or note sections.")

(defun rvf/literature-note-pdf (citekey)
  "Choose a PDF for CITEKEY using the conventional name and Citar.
Prefer an existing citekey.pdf in the configured library. Otherwise use
a unique existing PDF associated by Citar. Leave the property empty if
multiple alternatives exist; when no PDF exists, retain the conventional
path so it can be downloaded there later."
  (let ((canonical (expand-file-name (concat citekey ".pdf") my-library-path)))
    (if (and (file-regular-p canonical) (file-readable-p canonical))
        canonical
      (let* ((files (gethash citekey (citar-get-files citekey)))
             (pdfs (delete-dups
                    (mapcar #'expand-file-name
                            (seq-filter
                             (lambda (file)
                               (and (stringp file)
                                    (string-equal
                                     (downcase (or (file-name-extension file) "")) "pdf")
                                    (file-regular-p file)
                                    (file-readable-p file)))
                             files)))))
        (cond ((null pdfs) canonical)
              ((null (cdr pdfs)) (car pdfs))
              (t (message "Several PDFs for %s; set NOTER_DOCUMENT explicitly" citekey)
                 ""))))))

(defun rvf/literature-note-document (_node)
  "Return the Org Noter document path for the current Citar capture."
  (let ((key (plist-get org-roam-capture--info :citar-citekey)))
    (unless (and (stringp key) (not (string-empty-p key)))
      (user-error "Create literature notes through Citar"))
    (rvf/literature-note-pdf key)))

(defun rvf/literature-note-setup (_node)
  "Return a setup directive only for a readable file beside the new note."
  (if (and my-literature-note-setupfile
           (let ((path (expand-file-name my-literature-note-setupfile)))
             (and (file-regular-p path) (file-readable-p path))))
      (concat "#+setupfile: " my-literature-note-setupfile "\n")
    ""))

(defun rvf/literature-note-footer (_node)
  "Return the explicitly configured private capture footer."
  my-literature-note-extra)

(provide 'literature-notes)
;;; literature-notes.el ends here
