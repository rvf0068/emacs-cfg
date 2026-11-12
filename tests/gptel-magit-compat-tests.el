;;; gptel-magit-compat-tests.el --- Tests for gptel-magit compatibility -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-magit-compat)

(ert-deftest rvf/gptel-magit-fix-build-copy ()
  (let* ((dir (make-temp-file "gptel-magit-test-" t))
         (source (expand-file-name "source.el" dir))
         (build (expand-file-name "gptel-magit.el" dir))
         (compiled (expand-file-name "gptel-magit.elc" dir))
         (original "(when-let ((existing-buffer (get-buffer buffer-name)))\n  (kill-buffer existing-buffer))\n"))
    (unwind-protect
        (progn
          (with-temp-file source (insert original))
          (make-symbolic-link source build)
          (with-temp-file compiled (insert "stale"))
          (should (rvf/gptel-magit-fix-when-let build))
          (should-not (file-symlink-p build))
          (should-not (file-exists-p compiled))
          (should (equal (with-temp-buffer
                           (insert-file-contents source)
                           (buffer-string))
                         original))
          (should (equal (with-temp-buffer
                           (insert-file-contents build)
                           (buffer-string))
                         (replace-regexp-in-string "when-let" "when-let*" original t t)))
          (should-not (rvf/gptel-magit-fix-when-let build)))
      (delete-directory dir t))))

(ert-deftest rvf/gptel-magit-fix-leaves-updated-source-alone ()
  (let ((file (make-temp-file "gptel-magit-test-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(message \"already updated\")\n"))
          (should-not (rvf/gptel-magit-fix-when-let file))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "(message \"already updated\")\n")))
      (delete-file file))))

(ert-deftest rvf/gptel-magit-fix-uses-straight-build-directory ()
  (let* ((dir (make-temp-file "gptel-magit-test-" t))
         (file (expand-file-name "gptel-magit.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(when-let ((existing-buffer (get-buffer buffer-name))) t)\n"))
          (cl-letf (((symbol-function 'straight--build-dir)
                     (lambda (package)
                       (should (equal package "gptel-magit"))
                       (file-name-as-directory dir))))
            (should (rvf/gptel-magit-fix-when-let)))
          (should (string-match-p
                   "(when-let\\*"
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))))
      (delete-directory dir t))))

;;; gptel-magit-compat-tests.el ends here
