;;; early-init.el --- Startup package activation and GC policy -*- lexical-binding: t; -*-

;; straight.el manages external packages.  Disable package.el activation
;; before Emacs can load packages (especially Org) from another source.
(setq package-enable-at-startup nil)

;; The paired Dell benchmark found a substantial improvement with a 32 MiB
;; threshold during initialization.  Keep normal interactive GC unchanged.
(defvar rvf/startup-gc-enabled t
  "Non-nil enables temporary startup garbage collection tuning.")
(defvar rvf/startup-gc-original nil)
(defconst rvf/startup-gc-threshold (* 32 1024 1024))

(defun rvf/restore-startup-gc ()
  "Restore the pre-startup GC threshold unless configuration changed it."
  (when rvf/startup-gc-original
    (when (= gc-cons-threshold rvf/startup-gc-threshold)
      (setq gc-cons-threshold rvf/startup-gc-original))
    (setq rvf/startup-gc-original nil))
  (remove-hook 'after-init-hook #'rvf/restore-startup-gc))

(when (and rvf/startup-gc-enabled
           (not (equal (getenv "RVF_STARTUP_GC") "baseline"))
           (not rvf/startup-gc-original)
           (< gc-cons-threshold rvf/startup-gc-threshold))
  (setq rvf/startup-gc-original gc-cons-threshold
        gc-cons-threshold rvf/startup-gc-threshold)
  (add-hook 'after-init-hook #'rvf/restore-startup-gc 90))
