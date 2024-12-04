;;; early-init.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2024-2024 Jonathan A. Harris

;; Author: Jonathan A. Harris, MSc.
;; Keywords: configuration
;; Homepage: https://github.com/jah377/.emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from 'README.org'. DO NOT EDIT.

;; Changes to the configuration should be done in 'README.org' and then
;; re-tangled by calling 'C-c C-v C-t'.

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
