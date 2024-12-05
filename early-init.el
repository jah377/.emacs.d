;;; early-init.el --- Emacs configuration file  -*- lexical-binding: t; no-byte-compile: t -*-
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

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (* 16 1024 1024))))

;; Perform GC after startup
(add-hook 'emacs-startup-hook (lambda () (garbage-collect)))

(setopt frame-inhibit-implied-resize t)

(setopt frame-resize-pixelwise t)

(menu-bar-mode   -1) ; Menu bar at top of framen
(scroll-bar-mode -1) ; Visible scroll-bar that appears when scrolling
(scroll-all-mode -1) ; Visible scroll-bar during synchronized scrolling
(tool-bar-mode   -1) ; Icons like "save" button below menu
(tooltip-mode    -1) ; Hoving over (some) elements triggers pop-up boxes

(setq-default initial-major-mode 'text-mode
              inhibit-startup-echo-area-message t ; Disable initial echo message
              inhibit-startup-message t    ; Disable startup message
              inhibit-startup-screen t     ; Disable start-up screen
              inhibit-splash-screen t      ; Disable startup screens/messages
              initial-scratch-message nil  ; Empty '*scratch*' buffer
              initial-buffer-choice t)     ; Open '*scratch*' buffer

(setopt warning-suppress-types '((defvaralias) (lexical-binding)))

;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's
;;   docs say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or
;; regions in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)
