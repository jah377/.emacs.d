;;; early-init.el --- Early Init -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; The 'early-init.el' file, introduced with Emacs27, is the first file that
;; Emacs reads when starting up. In principal, the early initialization file
;; should set-up a few basic things before Emacs produces the initial frame,
;; and should not depend on any packages.

;;; Code:

;; https://github.com/doomemacs/doomemacs/blob/master/early-init.el#L29
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.9)

;; https://github.com/jamescherti/minimal-emacs.d/blob/main/init.el#L114
(setopt read-process-output-max (* 512 1024))

(setopt frame-inhibit-implied-resize t)

(setopt frame-resize-pixelwise t)

(menu-bar-mode   -1) ;; Menu bar at top of framen
(scroll-bar-mode -1) ;; Visible scroll-bar that appears when scrolling
(scroll-all-mode -1) ;; Visible scroll-bar during synchronized scrolling
(tool-bar-mode   -1) ;; Icons like "save" button below menu
(tooltip-mode    -1) ;; Hoving over (some) elements triggers pop-up boxes

(setq-default inhibit-startup-screen t
              inhibit-startup-message t
              initial-scratch-message nil
              iniital-buffer-choice t
              initial-major-mode 'text-mode)

;; https://github.com/Thaodan/emacs.d
(setopt warning-suppress-types '((lexical-binding)))

;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; 'init.el' serves as the primary configuration file. Most settings and
;; configurations are organized in separate 'modules/init-*.el' files and
;; loaded at the end of this file.

;;; IMPORTANT:
;; Changes to this file should be done in 'README.org' and re-tangled.

;;; Code:
