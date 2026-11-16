;;; elfeed-latex-tests.el --- Tests for Elfeed LaTeX previews -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'org)
(require 'ob-core)
(require 'org-latex-preview)
(require 'elfeed)

(let ((config (expand-file-name "../emacs-config.org"
                                (file-name-directory load-file-name))))
  (with-temp-buffer
    (insert-file-contents config)
    (org-mode)
    (org-babel-goto-named-src-block "elfeed-latex-previews")
    (let ((body (nth 1 (org-babel-get-src-block-info 'light))))
      (eval (read (concat "(progn\n" body "\n)")) t))))

(defmacro my/elfeed-test-capture-previews (&rest body)
  "Run BODY with Org preview requests captured in `previews'."
  (declare (indent 0))
  `(let (previews)
     (cl-letf (((symbol-function 'org-latex-preview-place)
                (lambda (backend entries &rest _)
                  (push (cons backend
                              (mapcar (lambda (entry)
                                        (buffer-substring-no-properties
                                         (car entry) (cadr entry)))
                                      entries))
                        previews)
                  nil)))
       ,@body)))

(defmacro my/elfeed-test-preview-queue (&rest body)
  "Run BODY with an isolated, manually driven preview queue."
  (declare (indent 0))
  `(let (my/elfeed-latex-preview-pending
         my/elfeed-latex-preview-active
         my/elfeed-latex-preview-timer)
     (cl-letf (((symbol-function 'my/elfeed-latex-preview-schedule) #'ignore))
       ,@body)))

(ert-deftest my/elfeed-latex-finds-fragments ()
  (with-temp-buffer
    (insert "Title: $x$-y and \\(a+b\\)\n\nDisplay: \\[c=d\\]\n")
    (goto-char 4)
    (let ((org-latex-preview-process-default 'dvisvgm))
      (my/elfeed-test-capture-previews
        (my/elfeed-latex-preview)
        (should (= (point) 4))
        (should (equal (caar previews) 'dvisvgm))
        (should (equal (sort (cdar previews) #'string<)
                       (sort '("$x$" "\\(a+b\\)" "\\[c=d\\]") #'string<)))))))

(ert-deftest my/elfeed-latex-ignores-empty-buffers ()
  (with-temp-buffer
    (insert "No math here.")
    (my/elfeed-test-capture-previews
      (my/elfeed-latex-preview)
      (should-not previews))))

(ert-deftest my/elfeed-latex-previews-search-updates ()
  (with-temp-buffer
    (elfeed-search-mode)
    (let ((search-buffer (current-buffer))
          (elfeed-search-entries '(first second))
          (elfeed-search-print-entry-function
           (lambda (entry) (insert (if (eq entry 'first) "$x$-y" "$z$")))))
      (cl-letf (((symbol-function 'elfeed-search-buffer) (lambda () search-buffer))
                ((symbol-function 'elfeed-search--update-list) #'ignore))
        (my/elfeed-test-preview-queue
          (my/elfeed-test-capture-previews
            (elfeed-search-update t)
            (should-not previews)
            (my/elfeed-latex-preview-run)
            (should (equal (cdar previews) '("$x$" "$z$")))
            (setq previews nil)
            (elfeed-search-update-entry 'second)
            (my/elfeed-latex-preview-run)
            (should (equal (cdar previews) '("$x$" "$z$")))
            (setq previews nil)
            (elfeed-search-update-entry 'missing)
            (should-not my/elfeed-latex-preview-pending)
            (should-not previews)))))))

(ert-deftest my/elfeed-latex-previews-refreshed-posts ()
  (with-temp-buffer
    (elfeed-show-mode)
    (let ((elfeed-show-refresh-function
           (lambda ()
             (interactive)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "A post: \\(x+y\\)")))))
      (my/elfeed-test-preview-queue
        (my/elfeed-test-capture-previews
          (elfeed-show-refresh)
          (should-not previews)
          (my/elfeed-latex-preview-run)
          (should (equal (cdar previews) '("\\(x+y\\)"))))))))

(ert-deftest my/elfeed-latex-coalesces-and-serializes-jobs ()
  (my/elfeed-test-preview-queue
    (let ((first (generate-new-buffer " *elfeed-preview-test-1*"))
          (second (generate-new-buffer " *elfeed-preview-test-2*"))
          calls)
      (unwind-protect
          (cl-letf (((symbol-function 'my/elfeed-latex-preview)
                     (lambda (&rest _)
                       (push (current-buffer) calls)
                       '(job))))
            (with-current-buffer first
              (my/elfeed-latex-preview-queue)
              (my/elfeed-latex-preview-queue))
            (should (equal my/elfeed-latex-preview-pending (list first)))
            (my/elfeed-latex-preview-run)
            (should (eq my/elfeed-latex-preview-active first))
            (with-current-buffer second (my/elfeed-latex-preview-queue))
            (with-current-buffer first (my/elfeed-latex-preview-queue))
            (my/elfeed-latex-preview-run)
            (should (equal calls (list first)))
            (my/elfeed-latex-preview-finished 0 nil (list :org-buffer second))
            (should (eq my/elfeed-latex-preview-active first))
            (my/elfeed-latex-preview-finished 252 nil (list :org-buffer first))
            (my/elfeed-latex-preview-run)
            (should (equal calls (list second first)))
            (my/elfeed-latex-preview-finished 0 nil (list :org-buffer second))
            (my/elfeed-latex-preview-run)
            (should (equal calls (list first second first)))
            (my/elfeed-latex-preview-finished 0 nil (list :org-buffer first))
            (should-not my/elfeed-latex-preview-active)
            (should-not my/elfeed-latex-preview-pending))
        (kill-buffer first)
        (kill-buffer second)))))

(ert-deftest my/elfeed-latex-releases-queue-on-error ()
  (my/elfeed-test-preview-queue
    (with-temp-buffer
      (my/elfeed-latex-preview-queue)
      (cl-letf (((symbol-function 'my/elfeed-latex-preview)
                 (lambda (&rest _) (error "Preview failed"))))
        (should-error (my/elfeed-latex-preview-run)))
      (should-not my/elfeed-latex-preview-active))))

;;; elfeed-latex-tests.el ends here
