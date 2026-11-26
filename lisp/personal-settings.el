;;; personal-settings.el --- Portable private configuration defaults -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)

(defvar my-notes-directory nil
  "Directory containing Org notes, normally set in personal.org.")
(defvar my-library-path nil
  "Directory containing PDF and other research files.")
(defvar my-bibliography nil
  "Bibliography file, list of files, or nil.")
(defvar my-latex-preview-scale 1
  "Scale factor for Org LaTeX previews.")

(defvar my-jupyter-executable nil
  "Optional Jupyter command or absolute executable path; nil uses upstream discovery.")
(defvar my-jupyter-python-kernel nil
  "Optional default named Jupyter Python kernel; nil preserves upstream defaults.")

(defun rvf/local-data-path (name)
  "Return NAME inside this installation's private local-data directory."
  (expand-file-name name (expand-file-name "local-data/" user-emacs-directory)))

(defun rvf/personal-directory (value name)
  "Resolve VALUE as a directory, or create the local default NAME.
Never create a missing explicitly configured external directory."
  (let ((path (file-name-as-directory
               (if value
                   (progn
                     (unless (and (stringp value) (not (string-empty-p value)))
                       (user-error "Invalid personal directory for %s: %S" name value))
                     (expand-file-name value user-emacs-directory))
                 (rvf/local-data-path name)))))
    (when (not value)
      (condition-case err
          (make-directory path t)
        (file-error (display-warning 'rvf-personal
                                     (error-message-string err) :warning))))
    (unless (file-directory-p path)
      (display-warning 'rvf-personal
                       (format "Directory unavailable: %s. Check personal.org." path)
                       :warning))
    path))

(defun rvf/prepare-personal-settings ()
  "Initialize portable defaults after reading optional personal.org."
  (setq my-notes-directory (rvf/personal-directory my-notes-directory "notes/")
        my-library-path (rvf/personal-directory my-library-path "library/")))

(defun rvf/notes-directory-ready-p ()
  "Return non-nil when the configured notes directory can be used."
  (and (stringp my-notes-directory)
       (file-directory-p my-notes-directory)
       (file-readable-p my-notes-directory)
       (file-writable-p my-notes-directory)))

(defun rvf/default-notes-directory-p ()
  "Whether the configured notes directory is the isolated local default."
  (equal (file-name-as-directory (expand-file-name my-notes-directory))
         (file-name-as-directory (rvf/local-data-path "notes/"))))

(defun rvf/bibliography-files ()
  "Return personal bibliography settings as a flat list of absolute paths.
Accept the historical single filename as well as a list or nil.
Do not create bibliography files or modify the original setting."
  (let ((files (if (stringp my-bibliography)
                   (list my-bibliography)
                 my-bibliography)))
    (unless (and (listp files)
                 (seq-every-p (lambda (file)
                                (and (stringp file) (not (string-empty-p file))))
                              files))
      (user-error "my-bibliography must be a filename, a list of filenames, or nil"))
    (delete-dups
     (mapcar (lambda (file) (expand-file-name file user-emacs-directory))
             files))))

(provide 'personal-settings)
;;; personal-settings.el ends here
