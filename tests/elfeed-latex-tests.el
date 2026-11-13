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
                        previews))))
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
        (my/elfeed-test-capture-previews
          (elfeed-search-update t)
          (should (equal (cdar previews) '("$x$" "$z$")))
          (setq previews nil)
          (elfeed-search-update-entry 'second)
          (should (equal (cdar previews) '("$z$")))
          (setq previews nil)
          (elfeed-search-update-entry 'missing)
          (should-not previews))))))

(ert-deftest my/elfeed-latex-previews-refreshed-posts ()
  (with-temp-buffer
    (elfeed-show-mode)
    (let ((elfeed-show-refresh-function
           (lambda ()
             (interactive)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "A post: \\(x+y\\)")))))
      (my/elfeed-test-capture-previews
        (elfeed-show-refresh)
        (should (equal (cdar previews) '("\\(x+y\\)")))))))

;;; elfeed-latex-tests.el ends here
