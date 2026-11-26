;;; latex-preview-config.el --- Org native preview settings -*- lexical-binding: t; -*-

(require 'org-latex-preview)
(require 'personal-settings)

(defconst rvf/latex-preview-extra-preamble
  (concat
   "\n\\usepackage{lxfonts}\n\\usepackage{amsmath}\n\\usepackage{amsfonts}"
   "\n\\makeatletter"
   "\n\\providecommand{\\usefonttheme}[2][]{}"
   "\n\\providecommand{\\usebeamerfont}{\\@ifstar\\@gobble\\@gobble}"
   "\n\\providecommand{\\usebeamercolor}[2][]{}"
   "\n\\providecommand{\\usebeamertemplate}{\\@ifstar\\@gobble\\@gobble}"
   "\n\\providecommand{\\setbeamertemplate}[2]{}"
   "\n\\providecommand{\\setbeamerfont}[2]{}"
   "\n\\providecommand{\\setbeamercolor}[2]{}"
   "\n\\makeatother")
  "Mathematical fonts and harmless Beamer commands for article previews.
Keep this block identical to the previous inline configuration so
reevaluation also removes duplicates left by that configuration.")

(defun rvf/latex-preview-extend-preamble (preamble)
  "Return PREAMBLE with exactly one copy of the configured additions.
Preserve other private or upstream header text."
  (let ((block rvf/latex-preview-extra-preamble))
    (concat (replace-regexp-in-string (regexp-quote block) "" preamble t t)
            block)))

(defun rvf/configure-latex-previews ()
  "Configure native Org previews without accumulating preamble additions."
  (setq org-latex-preview-process-default 'dvisvgm
        org-latex-preview-process-precompile t)
  (setf (plist-get org-latex-preview-appearance-options :scale)
        my-latex-preview-scale)
  (let ((new-preamble (rvf/latex-preview-extend-preamble
                       org-latex-preview-preamble)))
    (unless (equal new-preamble org-latex-preview-preamble)
      (setq org-latex-preview-preamble new-preamble)
      ;; Existing Org buffers cache the expanded header. Invalidate it,
      ;; but leave their existing images and persistent cache alone.
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'org-mode)
            (org-latex-preview--clear-preamble-cache))))))
  (add-hook 'org-mode-hook #'rvf/enable-latex-previews))

(defun rvf/enable-latex-previews ()
  "Enable native previews explicitly, rather than toggling the mode."
  (unless org-latex-preview-mode
    (org-latex-preview-mode 1)))

(provide 'latex-preview-config)
;;; latex-preview-config.el ends here
