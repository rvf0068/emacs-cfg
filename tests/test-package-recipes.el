;;; test-package-recipes.el --- Unit test package registration -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'seq)

(defun rvf/test-package-declarations (config)
  "Read top-level use-package forms from Emacs Lisp source blocks in CONFIG.
Do not evaluate any forms or load the user's configuration."
  (with-temp-buffer
    (insert-file-contents config)
    (org-mode)
    (let (declarations)
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (block)
          (when (equal (org-element-property :language block) "emacs-lisp")
            (let ((body (org-element-property :value block)))
              (with-temp-buffer
                (insert body)
                (emacs-lisp-mode)
                (goto-char (point-min))
                (while (progn
                         (forward-comment (point-max))
                         (not (eobp)))
                  (let ((form (read (current-buffer))))
                    (when (and (consp form) (eq (car form) 'use-package))
                      (push form declarations)))))))))
      (nreverse declarations))))

(defun rvf/test-package-recipe (name declarations)
  "Return the literal straight recipe for use-package NAME in DECLARATIONS."
  (let ((matches (seq-filter (lambda (form) (eq (cadr form) name))
                             declarations)))
    (unless (= (length matches) 1)
      (error "Expected exactly one use-package declaration for %S" name))
    (let ((recipe (cadr (memq :straight (cddr (car matches))))))
      (cond
       ((eq recipe t) name)
       ((and recipe (symbolp recipe) (not (keywordp recipe))) recipe)
       ((and (consp recipe) (keywordp (car recipe)))
        (cons name recipe))
       ((and (consp recipe) (symbolp (car recipe))) recipe)
       (t (error "No supported external straight recipe for %S: %S"
                 name recipe))))))

(provide 'test-package-recipes)
