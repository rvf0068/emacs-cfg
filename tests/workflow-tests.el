;;; workflow-tests.el --- Real Org LaTeX and Jupyter tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'ob-core)
(require 'org-element)

(ert-deftest rvf/workflow-bootstrap-straight ()
  ;; Inspect the configuration actually loaded by the graphical runner.
  (should-not package-enable-at-startup)
  (should-not straight-use-package-by-default)
  (should (featurep 'use-package))
  (should (memq :straight use-package-keywords))
  (should (featurep 'org))
  (let ((straight-dir (expand-file-name "straight/" user-emacs-directory)))
    (dolist (library (list (symbol-file 'org-mode 'defun)
                           (locate-library "transient")))
      (should library)
      (should (file-in-directory-p library straight-dir)))))

(ert-deftest rvf/workflow-personal-settings ()
  (require 'citar)
  (require 'org-roam)
  (require 'citar-org-roam)
  (let* ((scenario (getenv "RVF_TEST_PERSONAL"))
         (absent (equal scenario "absent"))
         (missing (equal scenario "missing"))
         (notes (cond (absent (rvf/local-data-path "notes/"))
                      (missing (expand-file-name "unavailable-notes/" "~"))
                      (t (expand-file-name "notes/" "~"))))
         (library (cond (absent (rvf/local-data-path "library/"))
                        (missing (expand-file-name "unavailable-library/" "~"))
                        (t (expand-file-name "library/" "~"))))
         (bibliography (unless (or absent missing)
                         (list (expand-file-name "library/test.bib" "~"))))
         (inbox (expand-file-name "Inbox.org" notes)))
    (should (eq absent (not (file-exists-p
                             (expand-file-name "personal.org" user-emacs-directory)))))
    (should (equal my-notes-directory notes))
    (should (equal my-library-path library))
    (should (equal org-roam-directory notes))
    (should (equal org-cite-global-bibliography bibliography))
    (should (equal citar-bibliography bibliography))
    (should (equal citar-library-paths (list library)))
    (should (equal citar-notes-paths (list notes)))
    (if missing
        (progn
          (should-not (file-exists-p notes))
          (should-not (file-exists-p library))
          (should-not org-roam-db-autosync-mode)
          (should-not citar-org-roam-mode)
          (should-not (member inbox org-agenda-files))
          (should-not (assoc "i" org-capture-templates)))
      (should (file-directory-p library))
      (should (file-exists-p inbox))
      (should org-roam-db-autosync-mode)
      (should citar-org-roam-mode)
      (should (member inbox org-agenda-files)))
    (when absent
      (should (equal org-roam-db-location (rvf/local-data-path "org-roam.db"))))))

(ert-deftest rvf/workflow-babel-initialization ()
  (should (featurep 'ob-python))
  (should (featurep 'ob-latex))
  (should (fboundp 'org-babel-execute:python))
  (should (fboundp 'org-babel-execute:latex))
  (should (alist-get 'python org-babel-load-languages))
  (should (alist-get 'latex org-babel-load-languages))
  (if (equal (getenv "RVF_TEST_PERSONAL") "missing")
      (progn
        (should-not (rvf/jupyter-available-p))
        (should-not (featurep 'ob-jupyter))
        (should-not (featurep 'jupyter-compat)))
    (should (rvf/jupyter-available-p))
    (should (featurep 'ob-jupyter))
    (should (featurep 'jupyter-compat))))

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


(defun rvf/test-preview-overlay ()
  "Return the single preview overlay in the current fixture."
  (cl-find-if (lambda (ov)
                (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))
              (overlays-in (point-min) (point-max))))

(defun rvf/test-preview-command (action)
  "Run ACTION with native preview mode's registered command hooks.
ERT runs Lisp without the interactive command loop, so explicitly
invoke the two hooks which normally surround each editing command."
  (should (memq #'org-latex-preview-mode--handle-pre-cursor pre-command-hook))
  (should (memq #'org-latex-preview-mode--handle-post-cursor post-command-hook))
  (org-latex-preview-mode--handle-pre-cursor)
  (funcall action)
  (org-latex-preview-mode--handle-post-cursor))

(defun rvf/test-image-digest (spec)
  "Hash the actual image bytes described by SPEC."
  (when (rvf/test-image-file-p spec)
    (with-temp-buffer
      (insert-file-contents-literally (plist-get (cdr spec) :file))
      (secure-hash 'sha256 (current-buffer)))))

(ert-deftest rvf/workflow-latex-edit-regenerates-preview ()
  (let* ((file (make-temp-file "rvf-preview-edit-" nil ".org"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (should org-latex-preview-mode)
          ;; Inline editing, not live side-by-side block rendering.
          (insert "Before\n\nInline \\(x+1\\) after.\n")
          (save-buffer)
          (goto-char (point-min))
          (org-latex-preview '(16))
          (should (rvf/test-wait-for
                   (lambda () (= 1 (length (rvf/test-preview-images)))) 120))
          (let* ((ov (rvf/test-preview-overlay))
                 (original (rvf/test-image-digest (overlay-get ov 'display))))
            (should original)
            ;; Enter the rendered fragment through the native cursor hooks.
            (rvf/test-preview-command
             (lambda () (goto-char (+ 3 (overlay-start ov)))))
            (should (overlay-get ov 'org-view-text))
            (should-not (overlay-get ov 'display))
            ;; Edit the fragment in place, and then leave it. No explicit
            ;; preview command is used after the initial rendering.
            (rvf/test-preview-command
             (lambda ()
               (goto-char (+ 2 (overlay-start ov)))
               (delete-char 1)
               (insert "\\mathbb{Q}")))
            (should (eq (overlay-get ov 'org-preview-state) 'modified))
            (rvf/test-preview-command
             (lambda () (goto-char (point-max))))
            (should
             (rvf/test-wait-for
              (lambda ()
                (let* ((current (rvf/test-preview-overlay))
                       (image (and current (overlay-get current 'display)))
                       (digest (rvf/test-image-digest image)))
                  (and digest (not (equal digest original))
                       (not (overlay-get current 'org-view-text))
                       (not (overlay-get current 'help-echo)))))
              120))
            (should (string-search "\\(\\mathbb{Q}+1\\)" (buffer-string)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest rvf/workflow-latex-beamer-setupfile-preview ()
  (let* ((dir (make-temp-file "rvf-preview-beamer-" t))
         (setup (expand-file-name "options.org" dir))
         (file (expand-file-name "slides.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file setup
            (insert "#+LATEX_CLASS: beamer\n"
                    "#+LATEX_HEADER: \\usefonttheme{professionalfonts}\n"
                    "#+LATEX_HEADER: \\setbeamertemplate{navigation symbols}{}\n"
                    "#+LATEX_HEADER: \\setbeamerfont{title}{series=\\bfseries}\n"
                    "#+LATEX_HEADER: \\setbeamercolor{title}{fg=blue}\n"
                    "#+LATEX_HEADER: \\newcommand{\\PreviewFixture}{\\mathbb{Q}}\n"))
          (with-temp-file file
            (insert "#+setupfile: options.org\n\n"
                    "A Beamer formula: \\(\\PreviewFixture\\).\n"))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (org-latex-preview '(16))
            (should (rvf/test-wait-for
                     (lambda () (= 1 (length (rvf/test-preview-images)))) 120))
            (let ((ov (rvf/test-preview-overlay)))
              (should (eq (plist-get (cdr (overlay-get ov 'org-preview-image)) :type)
                          'svg))
              (should-not (overlay-get ov 'help-echo)))))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-directory dir t))))

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


(ert-deftest rvf/workflow-jupyter-uv-workspace-python ()
  ;; A real Python virtual environment, without installing any packages.
  ;; The regular Jupyter test below still exercises a real kernel separately.
  (let* ((root (make-temp-file "rvf-uv-workflow-" t))
         (package (expand-file-name "packages/demo/" root))
         (file (expand-file-name "research.org" package))
         (venv (expand-file-name ".venv" root))
         (python (expand-file-name "bin/python" venv)))
    (unwind-protect
        (progn
          (make-directory package t)
          (dolist (dir (list root package))
            (with-temp-file (expand-file-name "pyproject.toml" dir)
              (insert "[project]\nname = \"demo\"\nversion = \"0.1.0\"\n")))
          (make-directory (expand-file-name ".git" root))
          (let ((default-directory root))
            (with-temp-buffer
              (should (zerop (process-file "python" nil t nil
                                           "-m" "venv" "--without-pip" venv)))))
          (with-temp-file file
            (insert "#+name: interpreter\n#+begin_src python :results output\n"
                    "import sys, os\nprint(sys.executable)\nprint(os.environ.get('VIRTUAL_ENV'))\n"
                    "#+end_src\n"))
          (with-current-buffer (find-file-noselect file)
            (should (equal rvf/uv-environment venv))
            (should (equal org-babel-python-command python))
            (should (equal (executable-find "python") python))
            (rvf/test-execute-named "interpreter")
            (should (rvf/test-result-matches
                     "interpreter" (concat "^: +" (regexp-quote python) "$")))
            (should (save-excursion
                      (goto-char (point-min))
                      (re-search-forward (concat "^: +" (regexp-quote venv) "$") nil t)))))
      (when-let* ((buffer (get-file-buffer file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest rvf/workflow-jupyter-dedicated-kernel ()
  (let* ((configured (not (equal (getenv "RVF_TEST_PERSONAL") "absent")))
         (dir (make-temp-file "rvf-dedicated-notes-" t))
         (file (expand-file-name "research.org" dir))
         (python (getenv "RVF_TEST_DEDICATED_PYTHON")))
    (unwind-protect
        (progn
          (should (file-executable-p python))
          (when configured
            (should (equal jupyter-executable (getenv "RVF_TEST_JUPYTER")))
            ;; Verify the configured CLI can be invoked without exec-path.
            (let ((exec-path nil))
              (should (stringp (jupyter-command "--version"))))
            (should (equal (alist-get :kernel org-babel-default-header-args:jupyter-python)
                           "rvf-dedicated")))
          (with-temp-file file
            (insert "#+PROPERTY: header-args:jupyter-python :session rvf-dedicated-test :results replace"
                    (if configured "" " :kernel rvf-dedicated") "\n\n"
                    "#+name: interpreter\n#+begin_src jupyter-python\n"
                    "import sys\nsys.executable\n#+end_src\n\n"
                    "#+name: package\n#+begin_src jupyter-python\n"
                    "import rvf_kernel_fixture\nrvf_kernel_fixture.VALUE\n#+end_src\n"))
          (with-current-buffer (find-file-noselect file)
            ;; This is an ordinary notes directory, not a uv workspace.
            (should-not rvf/uv-environment)
            (rvf/test-execute-named "interpreter")
            (should (rvf/test-result-matches
                     "interpreter" (concat "^: +" (regexp-quote python) "[ \t]*$")))
            (rvf/test-execute-named "package")
            (should (rvf/test-result-matches "package" "^: +12345[ \t]*$"))))
      (when-let* ((buffer (get-file-buffer file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest rvf/workflow-jupyter-real-kernel ()
  (should (executable-find "jupyter"))
  (let* ((dir (make-temp-file "rvf-jupyter-" t))
         (file (expand-file-name "workflow.org" dir)))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (insert "#+PROPERTY: header-args:jupyter-python :session rvf-test :kernel rvf-regression :results replace\n\n"
                  "#+name: base-interpreter\n#+begin_src jupyter-python\n"
                  "import sys\nsys.executable\n#+end_src\n\n"
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
          ;; Explicit kernel header must override a configured default.
          (rvf/test-execute-named "base-interpreter")
          (should (rvf/test-result-matches
                   "base-interpreter"
                   (concat "^: +" (regexp-quote (getenv "RVF_TEST_BASE_PYTHON")) "[ \t]*$")))
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
