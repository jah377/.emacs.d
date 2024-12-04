;;; init-ui.el --- Emacs configuration file  -*- lexical-binding: t; -*-
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

;; https://www.unwoundstack.com/blog/switching-emacs-themes.html
(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  (mapc #'disable-theme custom-enabled-themes))

;; Effectively copied from https://github.com/doomemacs/themes
(use-package doom-themes
  :ensure t
  :config
  (setopt doom-themes-enable-bold t
          doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(defun light ()
  (interactive)
  (load-theme 'doom-tomorrow-day t))

(defun dark ()
  (interactive)
  (load-theme 'doom-one t))

(dark)

;; https://github.com/daviwil/dotfiles/blob/guix-home/.emacs.d/modules/dw-core.el#L124
(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 100
                    :weight 'medium)

(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :height 100
                    :weight 'medium)

(set-face-attribute 'variable-pitch nil
                    :font "JetBrains Mono"
                    :height 100
                    :weight 'medium)

;; Modified from https://stackoverflow.com/a/50052751
(defun font-size (fontsize)
  "Set the font-pt size."
  (interactive "nFont size: ")
  (let* ((font-height (* 10 fontsize)))
    (set-face-attribute 'default nil :height font-height)
    (set-face-attribute 'fixed-pitch nil :height font-height)
    (set-face-attribute 'variable-pitch nil :height font-height)))

;; Font size at startup
(font-size 10)

(use-package nerd-icons
  :config
  ;; Download nerd-icons if directory not found
  (unless (car (file-expand-wildcards
                (concat user-emacs-directory "elpa/nerd-icons-*")))
    (nerd-icons-install-fonts t)))

(use-package doom-modeline
  :config (doom-modeline-mode 1)
  :custom
  ;; Display project_name/../file_name
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-encoding nil "Dont care about UTF-8 badge")
  (doom-modeline-vcs-max-length 30   "Limit branch name length")
  (doom-modeline-enable-word-count t "Turn on wordcount"))

(column-number-mode t)

(line-number-mode t)

(use-package anzu
  :hook (emacs-startup . global-anzu-mode)
  :custom
  (anzu-search-threshold 1000 "Limit n words searched to reduce lag")
  (anzu-replace-threshold 50 "Limit n replacement overlay to reduce lag")
  (anzu-minimum-input-length 2 "Increase activation threshold to reduce lag")

  ;; Cleanup mode-line information
  (anzu-mode-lighter "" "Remove mode-name from results")
  (anzu-replace-to-string-separator "")

  :bind (;; Keybindings M-% and C-M-% do not change
         ([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)

         :map isearch-mode-map
         ;; Use Anzu-mode for replacing from isearch results (C-s or C-f)
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(provide 'init-ui)
;;; init-ui.el ends here
