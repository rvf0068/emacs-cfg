;;; gptel-magit-compat.el --- Local gptel-magit compatibility fixes -*- lexical-binding: t; -*-

(require 'subr-x)

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

;; Declared by GPTel before gptel-magit--request can be invoked.
(defvar gptel-stream)
(defvar gptel-use-curl)

(defun rvf/gptel-magit-stream-callback (original state response info &optional raw)
  "Adapt GPTel's callback protocol to gptel-magit.
STATE is a cons of reversed text chunks and a terminal-event flag.
Ignore reasoning, tool notifications and raw steering text.  Forward
exactly one complete response on success; never forward partial output
after a failure or cancellation."
  (unless (or (cdr state) raw)
    (cond
     ((stringp response)
      ;; GPTel removes :stream from INFO if the actual backend/model
      ;; does not stream, even when streaming was requested.
      (if (plist-get info :stream)
          (push response (car state))
        (setcdr state t)
        (if (string-empty-p response)
            (message "gptel-magit: No response text received")
          (funcall original response info))))
     ((eq response t)
      (setcdr state t)
      (let ((text (mapconcat #'identity (nreverse (car state)) "")))
        (setcar state nil)
        (if (string-empty-p text)
            (message "gptel-magit: No response text received")
          (funcall original text info))))
     ((or (null response) (eq response 'abort))
      (setcdr state t)
      (setcar state nil)
      (if (eq response 'abort)
          (message "gptel-magit: Request aborted")
        (message "gptel-magit request failed: %s"
                 (or (plist-get info :status) "unknown status")))))))

(defun rvf/gptel-magit-stream-request (orig prompt &rest options)
  "Request curl streaming for gptel-magit and adapt its callback.
Keep ordinary GPTel requests and requests without callbacks unchanged.
Each invocation has its own accumulator; do not mutate caller options."
  (let ((callback (plist-get options :callback)))
    (if (not callback)
        (apply orig prompt options)
      (setq options (copy-sequence options))
      (setq options
            (plist-put options :callback
                       (apply-partially #'rvf/gptel-magit-stream-callback
                                        callback (cons nil nil))))
      (setq options (plist-put options :stream t))
      (let ((gptel-stream t)
            (gptel-use-curl t))
        (apply orig prompt options)))))

(with-eval-after-load 'gptel-magit
  (unless (advice-member-p #'rvf/gptel-magit-stream-request
                           'gptel-magit--request)
    (advice-add 'gptel-magit--request :around
                #'rvf/gptel-magit-stream-request)))

(provide 'gptel-magit-compat)
;;; gptel-magit-compat.el ends here
