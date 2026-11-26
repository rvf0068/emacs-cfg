;;; startup-benchmark.el --- Optional warm startup and first-use timings -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun rvf/benchmark-measure (label function)
  "Run FUNCTION, return its value and report wall time and GC cost."
  (let ((start (float-time))
        (gc-start gcs-done)
        (gc-seconds-start gc-elapsed))
    (prog1 (funcall function)
      (message "RVF BENCHMARK %-25s %.3f s (GCs %d, GC %.3f s)"
               label (- (float-time) start)
               (- gcs-done gc-start) (- gc-elapsed gc-seconds-start)))))

(defun rvf/benchmark-first-use ()
  "Measure first research operations in the isolated graphical test home.
The fixture is created before timing.  No user files, external feeds,
remote Git operations or authenticated services are used."
  (let* ((dir (make-temp-file "rvf-benchmark-" t))
         (org-file (expand-file-name "research.org" dir))
         (tex-file (expand-file-name "research.tex" dir))
         (bib-file (expand-file-name "fixture.bib" dir))
         (repo (expand-file-name "repo/" dir))
         org-buffer tex-buffer)
    (unwind-protect
        (progn
          (with-temp-file org-file
            (insert "#+title: Research fixture\n\n* Notes\n"
                    "A graph with \\(n\\) vertices.\n\n"
                    "#+begin_src jupyter-python :session rvf-benchmark "
                    ":kernel rvf-regression :results replace\n1+1\n#+end_src\n"))
          (with-temp-file tex-file
            (insert "\\documentclass{article}\n\\begin{document}\n"
                    "A graph with \\(n\\) vertices.\n\\end{document}\n"))
          (with-temp-file bib-file
            (insert "@article{rvf-benchmark, author={Example, A.}, "
                    "title={Clique graphs}, year={2026}}\n"))
          (make-directory repo)
          (let ((default-directory repo))
            (unless (zerop (call-process "git" nil nil nil "init" "-q"))
              (error "Could not create benchmark repository")))
          (setq org-buffer
                (rvf/benchmark-measure "first-org-file"
                                       (lambda () (find-file-noselect org-file))))
          (with-current-buffer org-buffer
            (unless (derived-mode-p 'org-mode) (error "Not an Org buffer")))
          (setq tex-buffer
                (rvf/benchmark-measure "first-latex-file"
                                       (lambda () (find-file-noselect tex-file))))
          (with-current-buffer tex-buffer
            (unless (derived-mode-p 'LaTeX-mode) (error "Not an AUCTeX buffer")))
          (rvf/benchmark-measure
           "first-magit-status"
           (lambda ()
             (require 'magit)
             (save-window-excursion
               (magit-status repo)
               (unless (derived-mode-p 'magit-status-mode)
                 (error "Magit status did not open")))))
          (rvf/benchmark-measure
           "first-citar-query"
           (lambda ()
             (require 'citar)
             (let ((citar-bibliography (list bib-file)))
               (unless (gethash "rvf-benchmark" (citar-get-entries))
                 (error "Citar did not read benchmark reference")))))
          (rvf/benchmark-measure
           "first-jupyter-evaluation"
           (lambda ()
             (with-current-buffer org-buffer
               (goto-char (point-min))
               (search-forward "#+begin_src jupyter-python")
               (beginning-of-line)
               (let ((org-confirm-babel-evaluate nil))
                 (org-babel-execute-src-block))
               (let ((result (org-babel-where-is-src-block-result)))
                 (unless (and result
                              (save-excursion
                                (goto-char result)
                                (forward-line 1)
                                (looking-at-p ": +2[ \t]*$")))
                   (error "Unexpected Jupyter benchmark result")))))))
      (dolist (buffer (list org-buffer tex-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory dir t))))

(provide 'startup-benchmark)
;;; startup-benchmark.el ends here
