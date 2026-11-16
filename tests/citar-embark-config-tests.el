;;; citar-embark-config-tests.el --- Tests for Citar Embark setup -*- lexical-binding: t; -*-

(require 'ert)
(require 'use-package)
(require 'org)

(defconst rvf/citar-embark-test-config-file
  (expand-file-name "../emacs-config.org" (file-name-directory load-file-name)))

(ert-deftest rvf/citar-embark-activates-on-first-embark-use ()
  (should-not (featurep 'citar))
  (should-not (featurep 'embark))
  (load "citar-autoloads" nil t)
  (let ((form
         (with-temp-buffer
           (insert-file-contents rvf/citar-embark-test-config-file)
           (goto-char (point-min))
           (search-forward "(use-package citar-embark\n")
           (goto-char (match-beginning 0))
           (read (current-buffer)))))
    ;; Straight handles installation in the real configuration.
    (should (equal (nth 2 form) :straight))
    (eval (append (seq-take form 2) (nthcdr 4 form))))
  (should-not (featurep 'citar))
  (require 'embark)
  (should (featurep 'citar))
  (should citar-embark-mode)
  (should (eq citar-at-point-function 'embark-act))
  (unwind-protect
      (progn
        (with-temp-buffer
          (org-mode)
          (insert "[cite:@2008-kozlov-combinatorial-algebraic-topology; @other]")
          (search-backward "kozlov")
          (let* ((targets (embark--targets))
                 (first (car targets))
                 (citation (seq-find (lambda (target)
                                       (eq (plist-get target :type) 'citar-citation))
                                     targets)))
            (should (eq (plist-get first :type) 'citar-key))
            (should (equal (plist-get first :target)
                           "2008-kozlov-combinatorial-algebraic-topology"))
            (should citation)
            (should (equal (plist-get citation :target)
                           "2008-kozlov-combinatorial-algebraic-topology & other"))
            (should (eq (alist-get 'citar-key embark-keymap-alist)
                        'citar-embark-citation-map))
            (should (eq (lookup-key citar-embark-citation-map (kbd "e"))
                        'citar-open-entry))
            (let (invoked)
              (cl-letf (((symbol-function 'embark--prompt)
                         (lambda (_indicators _keymap _targets) 'citar-open-entry))
                        ((symbol-function 'embark--act)
                         (lambda (action target &optional _quit)
                           (setq invoked (list action target)))))
                (embark-act))
              (should (eq (car invoked) 'citar-open-entry))
              (should (equal (plist-get (cadr invoked) :target)
                             "2008-kozlov-combinatorial-algebraic-topology")))))
        (with-temp-buffer
          (org-mode)
          (insert "contact@example.com")
          (goto-char (point-min))
          (should (eq (plist-get (car (embark--targets)) :type) 'email))))
    (citar-embark-mode -1)))

;;; citar-embark-config-tests.el ends here
