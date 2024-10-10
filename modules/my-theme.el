;;; -*- lexical-binding: t -*-
(provide my-theme)

(use-package doom-themes
  :custom
  ;; Must be available in font; enable anyways
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)

  :config
  ;; Flash modeline on errors
  (doom-themes-visual-bell-config)

  ;; Correct native fontification
  (doom-themes-org-config))

(defconst dark-theme 'doom-tomorrow-day)
(defconst light-theme 'doom-one)

(defun light ()
  (interactive)
  (load-theme 'light-theme t))

(defun dark ()
  (interactive)
  (load-theme 'dark-theme t))

;; Set dark by default
(dark)
