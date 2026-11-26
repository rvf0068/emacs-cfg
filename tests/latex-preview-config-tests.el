;;; latex-preview-config-tests.el --- Idempotent preview configuration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'latex-preview-config)

(ert-deftest rvf/latex-preview-preamble-idempotent ()
  (let* ((base "\\documentclass{article}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\newcommand{\\PrivateMath}{x}")
         (extra rvf/latex-preview-extra-preamble)
         (expected (concat base extra)))
    (should (equal (rvf/latex-preview-extend-preamble base) expected))
    (should (equal (rvf/latex-preview-extend-preamble expected) expected))
    (should (equal (rvf/latex-preview-extend-preamble
                    (concat base extra extra extra)) expected))))

(ert-deftest rvf/latex-preview-configuration-reload ()
  (let ((org-latex-preview-preamble "\\documentclass{article}")
        (org-latex-preview-process-default 'dvipng)
        (org-latex-preview-process-precompile nil)
        (org-latex-preview-appearance-options (copy-tree org-latex-preview-appearance-options))
        (org-latex-preview-cache 'temp)
        (my-latex-preview-scale 1.25)
        (org-mode-hook nil))
    (rvf/configure-latex-previews)
    (let ((preamble org-latex-preview-preamble))
      (rvf/configure-latex-previews)
      (should (equal org-latex-preview-preamble preamble))
      (should (eq org-latex-preview-process-default 'dvisvgm))
      (should org-latex-preview-process-precompile)
      (should (= (plist-get org-latex-preview-appearance-options :scale) 1.25))
      (should (eq org-latex-preview-cache 'temp))
      (should (= (cl-count #'rvf/enable-latex-previews org-mode-hook) 1)))))

;;; latex-preview-config-tests.el ends here
