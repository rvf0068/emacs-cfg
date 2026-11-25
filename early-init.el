;;; early-init.el --- Startup package activation policy -*- lexical-binding: t; -*-

;; straight.el manages external packages.  Disable package.el activation
;; before Emacs can load packages (especially Org) from another source.
(setq package-enable-at-startup nil)
