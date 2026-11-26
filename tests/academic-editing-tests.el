;;; academic-editing-tests.el --- Academic helper tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'academic-editing)

(ert-deftest rvf/bibtex-year-first-autokey-real-entry ()
  (with-temp-buffer
    (bibtex-mode)
    (insert "@article{old,\n"
            " author = {Ada Smith and Ben Jones},\n"
            " year = {2024},\n"
            " title = {On Clique Graphs}\n}\n")
    (goto-char (point-min))
    (let* ((bibtex-autokey-names 'infty)
           (bibtex-autokey-name-separator "-")
           (bibtex-autokey-name-year-separator "-")
           (bibtex-autokey-year-title-separator "-")
           (bibtex-autokey-year-length 4)
           (bibtex-autokey-titlewords 'infty)
           (bibtex-autokey-titleword-length 'infty)
           (bibtex-autokey-titleword-separator "-")
           (bibtex-autokey-titleword-ignore nil)
           (bibtex-autokey-prefix-string "")
           (bibtex-autokey-before-presentation-function #'identity)
           (expected (concat "2024-" (bibtex-autokey-get-names)
                             "-on-clique-graphs"))
           (before (buffer-string)))
      (should (equal (rvf/bibtex-year-first-autokey) expected))
      (should (string-match-p "smith-jones" expected))
      (should (equal (buffer-string) before))
      (let ((bibtex-autokey-prefix-string "ref-")
            (bibtex-autokey-before-presentation-function
             (lambda (key) (concat key "-checked"))))
        (should (equal (rvf/bibtex-year-first-autokey)
                       (concat "ref-" expected "-checked")))))))

(ert-deftest rvf/bibtex-year-first-autokey-missing-fields ()
  (with-temp-buffer
    (bibtex-mode)
    (insert "@misc{old, title = {Clique Graphs}}\n")
    (goto-char (point-min))
    (let ((bibtex-autokey-prefix-string "")
          (bibtex-autokey-before-presentation-function nil)
          (bibtex-autokey-titlewords 'infty)
          (bibtex-autokey-titleword-length 'infty)
          (bibtex-autokey-titleword-separator "-")
          (bibtex-autokey-titleword-ignore nil))
      (should (equal (rvf/bibtex-year-first-autokey) "clique-graphs")))))

(ert-deftest rvf/bibtex-year-first-advice-idempotent ()
  (let ((already (advice-member-p #'rvf/bibtex-year-first-autokey
                                  'bibtex-generate-autokey)))
    (unwind-protect
        (progn
          (rvf/bibtex-enable-year-first-autokey)
          (let ((installed (symbol-function 'bibtex-generate-autokey)))
            (rvf/bibtex-enable-year-first-autokey)
            (should (eq installed (symbol-function 'bibtex-generate-autokey)))))
      (unless already
        (advice-remove 'bibtex-generate-autokey
                       #'rvf/bibtex-year-first-autokey)))))

(ert-deftest rvf/embark-file-link-with-special-characters ()
  (dolist (path '("/tmp/Ordinary paper.pdf" "/tmp/Paper [draft].pdf"
                  "/tmp/Paper ]].pdf"))
    (with-temp-buffer
      (org-mode)
      (my-embark-org-insert-link path)
      (goto-char (point-min))
      (forward-char 2)
      (let ((link (org-element-context)))
        (should (eq (org-element-type link) 'link))
        (should (equal (org-element-property :type link) "file"))
        (should (equal (org-element-property :path link) path))
        (should (org-element-property :contents-begin link))))))

;;; academic-editing-tests.el ends here
