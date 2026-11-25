;;; workflow-tests.el --- Real Org LaTeX and Jupyter tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'ob-core)
(require 'org-element)

(defun rvf/test-wait-for (predicate seconds)
  "Wait up to SECONDS for PREDICATE, servicing real process output."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (funcall predicate)))

(defun rvf/test-image-file-p (spec)
  "Check that SPEC describes an existing nonempty image file."
  (and (consp spec) (eq (car spec) 'image)
       (let ((file (plist-get (cdr spec) :file)))
         (and (stringp file) (file-regular-p file)
              (> (file-attribute-size (file-attributes file)) 0)))))

(defun rvf/test-preview-images ()
  (cl-loop for ov in (overlays-in (point-min) (point-max))
           for spec = (or (overlay-get ov 'org-preview-image)
                          (overlay-get ov 'display))
           when (and (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
                     (rvf/test-image-file-p spec))
           collect spec))

(ert-deftest rvf/workflow-latex-real-previews ()
  (should (display-graphic-p))
  (should (image-type-available-p 'svg))
  (should (executable-find "pdflatex"))
  (should (executable-find "dvisvgm"))
  (let ((file (make-temp-file "rvf-preview-" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (insert "#+LATEX_HEADER: \\usepackage{amssymb}\n\n"
                  "Inline \\(\\mathbb{Q}\\) and display:\n\n"
                  "\\[\\widetilde H_i(\\Delta(G);\\mathbb{Q})\\]\n\n"
                  "\\begin{equation}\nA\\cap B=\\varnothing\n\\end{equation}\n")
          (save-buffer)
          (should (bound-and-true-p org-latex-preview-mode))
          (should (eq org-latex-preview-process-default 'dvisvgm))
          (goto-char (point-min))
          (org-latex-preview '(16))
          (should (rvf/test-wait-for
                   (lambda () (= 3 (length (rvf/test-preview-images)))) 120))
          (dolist (spec (rvf/test-preview-images))
            (should (eq (plist-get (cdr spec) :type) 'svg))))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-file file))))

(defun rvf/test-execute-named (name)
  (goto-char (point-min))
  (org-babel-goto-named-src-block name)
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-execute-src-block)))

(defun rvf/test-result-matches (name regexp)
  (save-excursion
    (goto-char (point-min))
    (org-babel-goto-named-src-block name)
    (let ((result (org-babel-where-is-src-block-result)))
      (and result
           (progn (goto-char result)
                  (forward-line 1)
                  (looking-at-p regexp))))))

(ert-deftest rvf/workflow-jupyter-real-kernel ()
  (should (executable-find "jupyter"))
  (let* ((dir (make-temp-file "rvf-jupyter-" t))
         (file (expand-file-name "workflow.org" dir)))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (insert "#+PROPERTY: header-args:jupyter-python :session rvf-test :kernel rvf-regression :results replace\n\n"
                  "#+name: graph\n#+begin_src jupyter-python\n"
                  "import networkx as nx\nimport numpy as np\n"
                  "G = nx.octahedral_graph()\nG.number_of_nodes()\n#+end_src\n\n"
                  "#+name: reuse\n#+begin_src jupyter-python\n"
                  "int(np.sum([G.number_of_nodes(), G.number_of_edges()]))\n#+end_src\n\n"
                  "#+name: figure\n#+begin_src jupyter-python :exports results\n"
                  "%matplotlib inline\nimport matplotlib.pyplot as plt\n"
                  "nx.draw(G, with_labels=True)\nplt.show()\n#+end_src\n")
          (save-buffer)
          (org-mode)
          ;; ob-jupyter creates language aliases in org-mode-hook, not at load time.
          (should (featurep 'ob-jupyter))
          (should (fboundp 'org-babel-execute:jupyter-python))
          (rvf/test-execute-named "graph")
          (should (rvf/test-result-matches "graph" "^: +6[ \t]*$"))
          (rvf/test-execute-named "reuse")
          (should (rvf/test-result-matches "reuse" "^: +18[ \t]*$"))
          (rvf/test-execute-named "figure")
          (let ((images
                 (org-element-map (org-element-parse-buffer) 'link
                   (lambda (link)
                     (when (equal (org-element-property :type link) "file")
                       (let ((path (expand-file-name (org-element-property :path link) dir)))
                         (when (and (file-regular-p path)
                                    (string-match-p "\\.\\(png\\|svg\\)\\'" path)
                                    (> (file-attribute-size (file-attributes path)) 0))
                           path)))))))
            (should images))
          (org-display-inline-images)
          (should (cl-some (lambda (ov)
                            (rvf/test-image-file-p (overlay-get ov 'display)))
                          (overlays-in (point-min) (point-max)))))
      (when-let* ((buffer (get-file-buffer file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory dir t))))
