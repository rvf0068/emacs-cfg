;;; org-roam-local-title-tests.el --- Tests for local Org Roam titles -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-roam-local-title)

(defmacro rvf/with-org-roam-title-fixture (&rest body)
  "Run BODY with a temporary Org Roam directory and setup file."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "org-roam-title-test-" t))
          (org-roam-directory dir)
          (org-roam-db-location (expand-file-name "org-roam.db" dir))
          (note (expand-file-name "note.org" dir)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "options.org" dir)
             (insert "#+title: Options\n"))
           ,@body)
       (org-roam-db--close-all)
       (delete-directory dir t))))

(ert-deftest rvf/org-roam-title-excludes-setup-file ()
  (rvf/with-org-roam-title-fixture
    (with-temp-file note
      (insert "#+setupfile: options.org\n#+title: Simplicial Complexes\n"
              "#+title: [[https://example.com][More]]\n"
              "#+begin_src org\n#+title: Not a title\n#+end_src\n"))
    (with-temp-buffer
      (insert-file-contents note)
      (setq buffer-file-name note)
      (let ((default-directory dir))
        (org-mode)
        (narrow-to-region (point-min) (line-end-position))
        (should (equal (rvf/org-roam-db-local-file-title)
                       "Simplicial Complexes More"))))))

(ert-deftest rvf/org-roam-title-falls-back-to-relative-file-name ()
  (rvf/with-org-roam-title-fixture
    (make-directory (expand-file-name "nested" dir))
    (setq note (expand-file-name "nested/note.org" dir))
    (with-temp-file note
      (insert "#+setupfile: ../options.org\n"))
    (with-temp-buffer
      (insert-file-contents note)
      (setq buffer-file-name note)
      (let ((default-directory (file-name-directory note)))
        (org-mode)
        (should (equal (rvf/org-roam-db-local-file-title) "nested/note"))))))

(ert-deftest rvf/org-roam-title-indexed-in-database ()
  (rvf/with-org-roam-title-fixture
    (with-temp-file note
      (insert ":PROPERTIES:\n:ID: test-node\n:END:\n"
              "#+title: Simplicial Complexes\n#+setupfile: options.org\n"))
    (org-roam-db-update-file note)
    (should (equal (org-roam-db-query [:select title :from nodes :where (= id $s1)] "test-node")
                   '(("Simplicial Complexes Options"))))
    (advice-add 'org-roam-db--file-title :override #'rvf/org-roam-db-local-file-title)
    (unwind-protect
        (progn
          (org-roam-db-sync t)
          (should (equal (org-roam-db-query [:select title :from files :where (= file $s1)] note)
                         '(("Simplicial Complexes"))))
          (should (equal (org-roam-db-query [:select title :from nodes :where (= id $s1)] "test-node")
                         '(("Simplicial Complexes")))))
      (advice-remove 'org-roam-db--file-title #'rvf/org-roam-db-local-file-title))))

;;; org-roam-local-title-tests.el ends here
