;;; cross-refs-tests.el --- Org mathematical cross-references -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'cross-refs)
(require 'ox-latex)

(defmacro rvf/test-cross-refs-buffer (source &rest body)
  "Run BODY in an Org buffer containing SOURCE."
  (declare (indent 1))
  (list 'with-temp-buffer '(org-mode) (list 'insert source)
        '(goto-char (point-min)) (cons 'progn body)))

(ert-deftest rvf/cross-refs-deduplicate-and-preview ()
  (rvf/test-cross-refs-buffer
      (concat "#+name: thm:a\n#+label: thm:a\n#+begin_theorem\n\nFirst theorem.\n#+end_theorem\n\n"
              "#+LABEL: lem:b\n#+begin_lemma\nSecond lemma.\n#+end_lemma\n\n"
              "#+name: eq:c\n\\begin{equation}\nx=y\n\\end{equation}\n\n"
              "#+name: tbl:d\n| A | B |\n")
    (should (equal (my/org-ref-labels)
                   '(("thm:a" . "First theorem.")
                     ("lem:b" . "Second lemma.")
                     ("eq:c" . "x=y")
                     ("tbl:d" . "| A | B |"))))))

(ert-deftest rvf/cross-refs-empty-and-malformed-blocks ()
  (rvf/test-cross-refs-buffer
      (concat "#+name: thm:empty\n#+begin_theorem\n#+end_theorem\n\n"
              "#+label: thm:orphan\n\n"
              "#+name: thm:next\n#+begin_theorem\nUnrelated text.\n#+end_theorem\n\n"
              "#+name: thm:unfinished\n#+begin_theorem\n")
    (let ((labels (my/org-ref-labels)))
      (should (equal (assoc "thm:empty" labels) '("thm:empty" . "")))
      (should (equal (assoc "thm:orphan" labels) '("thm:orphan" . "")))
      (should (equal (assoc "thm:unfinished" labels) '("thm:unfinished" . "")))
      (should (equal (assoc "thm:next" labels) '("thm:next" . "Unrelated text."))))))

(ert-deftest rvf/cross-refs-ignore-literal-labels ()
  (rvf/test-cross-refs-buffer
      (concat "#+begin_src text\n#+name: thm:fake\n#+end_src\n\n"
              "#+begin_example\n#+label: thm:fake2\n#+end_example\n\n"
              "#+name: thm:real\n#+begin_theorem\nReal.\n#+end_theorem\n")
    (should (equal (my/org-ref-labels) '(("thm:real" . "Real."))))))

(ert-deftest rvf/cross-refs-context-and-legacy-prefixes ()
  (rvf/test-cross-refs-buffer
      (concat "#+name: prf:a\n#+begin_proof\nA\n#+end_proof\n\n"
              "#+label: pf:b\n#+begin_proof\nB\n#+end_proof\n\n"
              "#+name: pro:c\n#+begin_proposition\nC\n#+end_proposition\n\n"
              "#+name: prop:d\n#+begin_proposition\nD\n#+end_proposition\n\n"
              "Proof ")
    (goto-char (point-max))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (equal (mapcar #'cdr collection) '("prf:a" "pf:b")))
                 (car (rassoc "pf:b" collection)))))
      (my/org-insert-smart-ref))
    (should (string-suffix-p "Proof [[pf:b]]" (buffer-string)))
    (insert "\nProposition ")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (equal (mapcar #'cdr collection) '("pro:c" "prop:d")))
                 (car (rassoc "prop:d" collection)))))
      (my/org-insert-smart-ref))
    (should (string-suffix-p "Proposition [[prop:d]]" (buffer-string)))))

(ert-deftest rvf/cross-refs-unknown-and-empty-context ()
  (rvf/test-cross-refs-buffer
      "#+name: lem:a\n#+begin_lemma\nA\n#+end_lemma\n\n"
    (goto-char (point-max))
    (dolist (context '("" "See "))
      (insert context)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (should (equal (mapcar #'cdr collection) '("lem:a")))
                   (caar collection))))
        (my/org-insert-smart-ref))
      (should (string-suffix-p (concat context "[[lem:a]]") (buffer-string)))
      (insert "\n"))))

(ert-deftest rvf/cross-refs-no-match-does-not-insert ()
  (rvf/test-cross-refs-buffer
      "#+name: lem:a\n#+begin_lemma\nA\n#+end_lemma\n\nTheorem "
    (goto-char (point-max))
    (let ((before (buffer-string)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "Unexpected completion"))))
        (my/org-insert-smart-ref))
      (should (equal before (buffer-string))))))

(ert-deftest rvf/cross-refs-whole-document-unique-label ()
  (rvf/test-cross-refs-buffer
      "#+name: thm:old\n#+begin_theorem\nA\n#+end_theorem\n\n* New\n"
    (search-forward "* New")
    (narrow-to-region (line-beginning-position) (point-max))
    (let ((answers '("" "two words" "OLD" "new")))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (pop answers))))
        (should (equal (my/org-snippet-get-unique-label "thm:") "new"))
        (should-not answers)))
    (should (buffer-narrowed-p))))

(ert-deftest rvf/cross-refs-real-latex-export ()
  (rvf/test-cross-refs-buffer
      (concat "#+name: thm:a\n#+label: thm:a\n#+begin_theorem\nA.\n#+end_theorem\n\n"
              "#+name: eq:b\n\\begin{equation}\nx=y\n\\end{equation}\n\n"
              "Theorem [[thm:a]], Equation [[eq:b]].\n")
    (let* ((org-latex-prefer-user-labels t)
           (org-export-use-babel nil)
           (latex (org-export-as 'latex nil nil t)))
      (dolist (fragment '("\\label{thm:a}" "\\ref{thm:a}"
                          "\\label{eq:b}" "\\eqref{eq:b}"))
        (should (string-match-p (regexp-quote fragment) latex)))
      (should-not (string-match-p (regexp-quote "\\ref{eq:b}") latex)))))


(ert-deftest rvf/cross-refs-name-versus-label-navigation-and-export ()
  ;; Compare the three declaration forms using Org itself, not our collector.
  (dolist (case '(("name" "#+name: thm:target\n" t)
                  ("label" "#+label: thm:target\n" nil)
                  ("both" "#+name: thm:target\n#+label: thm:target\n" t)))
    (ert-info ((format "Declaration form: %s" (car case)))
      (rvf/test-cross-refs-buffer
          (concat (nth 1 case)
                  "#+begin_theorem\nA theorem.\n#+end_theorem\n\n"
                  "See [[thm:target]].\n")
        (let ((org-link-search-must-match-exact-headline t)
              (org-export-use-babel nil)
              (org-latex-prefer-user-labels t))
          ;; Test the actual command used for opening internal links.
          ;; Strict matching excludes incidental fuzzy text matches.
          (goto-char (point-max))
          (search-backward "[[thm:target]]")
          (forward-char 3)
          (if (nth 2 case)
              (progn
                (org-open-at-point)
                (should (looking-at-p "[ \t]*#\\+name: thm:target")))
            (should-error (org-open-at-point)))
          (let ((latex (org-export-as 'latex nil nil t)))
            (should (string-match-p
                     (regexp-quote "\\label{thm:target}") latex))
            (should (string-match-p
                     (regexp-quote "\\ref{thm:target}") latex))))))))

;;; cross-refs-tests.el ends here
