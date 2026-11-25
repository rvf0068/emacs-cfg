;; -*- lexical-binding: t; -*-

;; Async native compilation of packages emits many harmless "function not
;; known to be defined" warnings (cross-file references); keep them quiet.
(setq native-comp-async-report-warnings-errors 'silent)

;; Require explicit :straight declarations for external packages.
;; Set this before bootstrap, and let private settings override it if needed.
(setq straight-use-package-by-default nil)

;; Bootstrap straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Register Org before loading any Org libraries or tangling the configuration.
;; Its revision is recorded in straight/versions/default.el.
(straight-use-package 'org)

;; Ensure straight manages transient so Magit gets the required version,
;; overriding the potentially outdated built-in.
(straight-use-package 'transient)

;; Emacs 29+ includes use-package; straight supplies its :straight keyword.
(require 'use-package)

;; Add our personal lisp directory to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set custom-file to avoid cluttering this file with M-x customize changes
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load configuration from emacs-config.org
(org-babel-load-file  (expand-file-name "emacs-config.org" user-emacs-directory))
