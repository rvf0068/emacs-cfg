;;; academic-editing.el --- BibTeX and Org file-link helpers -*- lexical-binding: t; -*-

(require 'bibtex)
(require 'ol)

(defun rvf/bibtex-year-first-autokey ()
  "Generate a year-author-title key using BibTeX's native components.
Keep the historical ordering and separators.  Honor the native prefix
and presentation function rather than replacing BibTeX's public defun."
  (let* ((names (bibtex-autokey-get-names))
         (year (bibtex-autokey-get-year))
         (title (bibtex-autokey-get-title))
         (key (concat bibtex-autokey-prefix-string
                      year
                      (unless (or (equal names "") (equal year ""))
                        bibtex-autokey-name-year-separator)
                      names
                      (unless (or (and (equal names "") (equal year ""))
                                  (equal title ""))
                        bibtex-autokey-year-title-separator)
                      title)))
    (if bibtex-autokey-before-presentation-function
        (funcall bibtex-autokey-before-presentation-function key)
      key)))

(defun rvf/bibtex-enable-year-first-autokey ()
  "Install the named year-first key generator once."
  (unless (advice-member-p #'rvf/bibtex-year-first-autokey
                           'bibtex-generate-autokey)
    (advice-add 'bibtex-generate-autokey :override
                #'rvf/bibtex-year-first-autokey)))

(defun my-embark-org-insert-link (path)
  "Insert an Org link to PATH, using its basename without extension.
Embark supplies PATH.  Use Org's own escaping of link targets and
descriptions, including filenames containing square brackets."
  (insert (org-link-make-string
           (concat "file:" path)
           (file-name-sans-extension (file-name-nondirectory path)))))

(provide 'academic-editing)
;;; academic-editing.el ends here
