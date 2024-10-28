;;; -*- lexical-binding: t -*-

;; Temporarily increase garbage collect for fast startup
(setq gc-cons-threshold most-positive-fixnum)

;; Inhibit frame resizing due to visual settings
(setq frame-inhibit-implied-resize t)

;; Prevent gimpse of un-styled Emacs
(menu-bar-mode   -1)
(scroll-bar-mode -1) ; Visible scrollbar
(scroll-all-mode -1) ; Synchronized scrolling of buffers
(tool-bar-mode   -1)
(tooltip-mode    -1)

;; No need for splash screen and echo area message
(setq-default inhibit-startup-screen t     ; disable start-up screen
              inhibit-startup-message t    ; disable start-up message
              initial-scratch-message nil  ; Empty initial *scratch* buffer
              initial-buffer-choice t      ; Open *scratch* buffer at init
              initial-major-mode 'org-mode)

;; Suppress warnings about lexical bindings.
;; https://github.com/Thaodan/emacs.d
(setopt warning-suppress-types '((lexical-binding)))
