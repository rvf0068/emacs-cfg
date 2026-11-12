;;; gptel-magit-compat.el --- Local gptel-magit compatibility fix -*- lexical-binding: t; -*-

(defun rvf/gptel-magit-fix-when-let (&optional file)
  "Replace the obsolete macro in the built gptel-magit FILE.
When FILE is nil, use the straight.el build directory."
  (let ((file (or file (expand-file-name "gptel-magit.el"
                                         (straight--build-dir "gptel-magit")))))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (search-forward
             "(when-let ((existing-buffer (get-buffer buffer-name)))" nil t)
        (replace-match
         "(when-let* ((existing-buffer (get-buffer buffer-name)))" t t)
        ;; Rename a temporary file over the build symlink, not its source.
        (let ((temporary-file (make-temp-file
                               (expand-file-name ".gptel-magit-"
                                                 (file-name-directory file)))))
          (unwind-protect
              (progn
                (write-region (point-min) (point-max) temporary-file nil 'silent)
                (set-file-modes temporary-file (file-modes file))
                (rename-file temporary-file file t))
            (when (file-exists-p temporary-file)
              (delete-file temporary-file))))
        (let ((compiled-file (concat (file-name-sans-extension file) ".elc")))
          (when (file-exists-p compiled-file)
            (delete-file compiled-file)))
        t))))

(provide 'gptel-magit-compat)
;;; gptel-magit-compat.el ends here
