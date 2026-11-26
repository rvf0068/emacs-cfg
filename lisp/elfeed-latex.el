;;; elfeed-latex.el --- Mathematical previews in Elfeed -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-latex-preview)

(defvar elfeed-search-entries)

(defvar my-elfeed-latex-auto-preview t
  "Whether to automatically queue LaTeX previews after Elfeed redraws.
Nil leaves feed text unchanged unless `my/elfeed-latex-preview' is
called explicitly.")

;; Adapted from https://tony-zorman.com/posts/elfeed-latex.html
;; Preview all feeds, and use refresh advice for the installed Elfeed version.
(defun my/elfeed-latex-preview (&optional beg end)
  "Preview LaTeX fragments between BEG and END in the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (when (and beg end)
        (narrow-to-region beg end))
      (with-syntax-table (copy-syntax-table)
        (modify-syntax-entry ?- ".")
        (let ((entries
               (cl-loop for (_ regexp group _) in org-latex-regexps
                        append (cl-loop initially (goto-char (point-min))
                                        while (re-search-forward regexp nil t)
                                        collect (list (match-beginning group)
                                                      (match-end group))))))
          (when entries
            (org-latex-preview-place org-latex-preview-process-default entries)))))))

(defvar my/elfeed-latex-preview-pending nil
  "Buffers awaiting an Elfeed LaTeX preview.")

(defvar my/elfeed-latex-preview-active nil
  "Buffer whose Elfeed LaTeX preview job is running.")

(defvar my/elfeed-latex-preview-timer nil
  "Idle timer for pending Elfeed LaTeX previews.")

(defun my/elfeed-latex-preview-schedule ()
  "Schedule pending Elfeed previews when no job is running."
  (when (and my/elfeed-latex-preview-pending
             (not my/elfeed-latex-preview-active)
             (not my/elfeed-latex-preview-timer))
    (setq my/elfeed-latex-preview-timer
          (run-with-idle-timer 0.5 nil #'my/elfeed-latex-preview-run))))

(defun my/elfeed-latex-preview-forget-buffer ()
  "Remove the current buffer from the queue when it is killed.
A callback from a closed buffer must not release a newer active job."
  (setq my/elfeed-latex-preview-pending
        (delq (current-buffer) my/elfeed-latex-preview-pending))
  (when (eq (current-buffer) my/elfeed-latex-preview-active)
    (setq my/elfeed-latex-preview-active nil))
  (my/elfeed-latex-preview-schedule))

(defun my/elfeed-latex-preview-queue (&rest _)
  "Queue the current Elfeed buffer when automatic previews are enabled."
  (when my-elfeed-latex-auto-preview
    (add-hook 'kill-buffer-hook #'my/elfeed-latex-preview-forget-buffer nil t)
    (unless (memq (current-buffer) my/elfeed-latex-preview-pending)
      (setq my/elfeed-latex-preview-pending
            (nconc my/elfeed-latex-preview-pending (list (current-buffer)))))
    (my/elfeed-latex-preview-schedule)))

(defun my/elfeed-latex-preview-entry (entry)
  "Queue a preview when ENTRY is visible in the search buffer."
  (when (cl-position entry elfeed-search-entries)
    (my/elfeed-latex-preview-queue)))

(defun my/elfeed-latex-preview-run ()
  "Start one pending Elfeed LaTeX preview job."
  (setq my/elfeed-latex-preview-timer nil)
  (unless my-elfeed-latex-auto-preview
    (setq my/elfeed-latex-preview-pending nil))
  (unless my/elfeed-latex-preview-active
    (let ((buffer (pop my/elfeed-latex-preview-pending)))
      (when (buffer-live-p buffer)
        (setq my/elfeed-latex-preview-active buffer)
        (let (started)
          (unwind-protect
              (with-current-buffer buffer
                (setq started (my/elfeed-latex-preview)))
            (unless started
              (setq my/elfeed-latex-preview-active nil)
              (my/elfeed-latex-preview-schedule))))))
    (my/elfeed-latex-preview-schedule)))

(defun my/elfeed-latex-preview-finished (_exit-code _process-buffer info)
  "Release the Elfeed preview job described by INFO."
  (when (and my/elfeed-latex-preview-active
             (eq my/elfeed-latex-preview-active (plist-get info :org-buffer)))
    (setq my/elfeed-latex-preview-active nil)
    (my/elfeed-latex-preview-schedule)))

(add-hook 'org-latex-preview-process-finish-functions
          #'my/elfeed-latex-preview-finished)

(with-eval-after-load 'elfeed-search
  (remove-hook 'elfeed-search-update-hook #'my/elfeed-latex-preview)
  (advice-remove 'elfeed-search-update-entry #'my/elfeed-latex-preview-entry)
  (add-hook 'elfeed-search-update-hook #'my/elfeed-latex-preview-queue)
  (advice-add 'elfeed-search-update-entry :after #'my/elfeed-latex-preview-entry))

(with-eval-after-load 'elfeed-show
  (advice-remove 'elfeed-show-refresh #'my/elfeed-latex-preview-show)
  (advice-add 'elfeed-show-refresh :after #'my/elfeed-latex-preview-queue))

(provide 'elfeed-latex)
;;; elfeed-latex.el ends here
