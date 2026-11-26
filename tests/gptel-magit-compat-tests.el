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

;; GPTel is deliberately not loaded in this offline unit suite.  Declare
;; its two dynamic switches here so the test exercises actual let binding.
(defvar gptel-stream nil)
(defvar gptel-use-curl nil)

(ert-deftest rvf/gptel-magit-stream-completes-once ()
  (let ((state (cons nil nil))
        (results nil)
        (info '(:stream t)))
    (let ((callback (lambda (response metadata)
                      (push (list response metadata) results))))
      (dolist (response '("Fix " (reasoning . "private reasoning")
                          "the bug" (reasoning . t)))
        (rvf/gptel-magit-stream-callback callback state response info))
      (rvf/gptel-magit-stream-callback callback state "steering" info 'raw)
      (should-not results)
      (rvf/gptel-magit-stream-callback callback state t info)
      (rvf/gptel-magit-stream-callback callback state t info)
      (rvf/gptel-magit-stream-callback callback state "late chunk" info)
      (should (equal results (list (list "Fix the bug" info))))
      (should-not (car state)))))

(ert-deftest rvf/gptel-magit-nonstreaming-fallback ()
  (let ((state (cons nil nil))
        (results nil)
        (info '(:status "HTTP 200")))
    (let ((callback (lambda (response metadata)
                      (push (list response metadata) results))))
      (rvf/gptel-magit-stream-callback callback state "Complete message" info)
      (should (equal results (list (list "Complete message" info))))
      (rvf/gptel-magit-stream-callback callback state t info)
      (should (= (length results) 1)))))

(ert-deftest rvf/gptel-magit-failed-or-empty-requests-never-insert ()
  (dolist (terminal '(nil abort t))
    (let ((state (cons nil nil))
          (info '(:stream t :status "test failure"))
          (callback (lambda (&rest _) (ert-fail "Unexpected insertion"))))
      (unless (eq terminal t)
        (rvf/gptel-magit-stream-callback callback state "Partial" info))
      (rvf/gptel-magit-stream-callback callback state terminal info)
      (rvf/gptel-magit-stream-callback callback state "Late chunk" info)
      (rvf/gptel-magit-stream-callback callback state t info)
      (should (cdr state))
      (should-not (car state))))
  (rvf/gptel-magit-stream-callback
   (lambda (&rest _) (ert-fail "Unexpected empty insertion"))
   (cons nil nil) "" nil))

(ert-deftest rvf/gptel-magit-request-is-scoped-and-independent ()
  (let ((gptel-stream nil)
        (gptel-use-curl nil)
        (callbacks nil)
        (results nil))
    (let* ((callback (lambda (response _) (push response results)))
           (options (list :callback callback :stream nil :context 'fixture))
           (request
            (lambda (prompt &rest args)
              (should (equal prompt "diff"))
              (should gptel-stream)
              (should gptel-use-curl)
              (should (plist-get args :stream))
              (should (eq (plist-get args :context) 'fixture))
              (push (plist-get args :callback) callbacks)
              'request-result)))
      (dotimes (_ 2)
        (should (eq (apply #'rvf/gptel-magit-stream-request
                           request "diff" options)
                    'request-result)))
      (should (eq (plist-get options :callback) callback))
      (should-not (plist-get options :stream))
      (should-not gptel-stream)
      (should-not gptel-use-curl)
      (let ((first (car callbacks))
            (second (cadr callbacks))
            (info '(:stream t)))
        (funcall first "A" info)
        (funcall second "B" info)
        (funcall first "1" info)
        (funcall second "2" info)
        (funcall first t info)
        (funcall second t info)
        (should (equal results '("B2" "A1")))))))

(ert-deftest rvf/gptel-magit-request-without-callback-unchanged ()
  (let ((gptel-stream nil)
        (gptel-use-curl nil))
    (should
     (equal (rvf/gptel-magit-stream-request
             (lambda (prompt &rest args)
               (should-not gptel-stream)
               (should-not gptel-use-curl)
               (cons prompt args))
             "diff" :stream nil :context 'fixture)
            '("diff" :stream nil :context fixture)))))

;;; gptel-magit-compat-tests.el ends here
