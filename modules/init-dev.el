;;; init-dev.el --- Emacs configuration file  -*- lexical-binding: t; no-byte-compile: t -*-
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

(use-package rainbow-delimiters
  :hook (prog-mode))

(use-package hl-todo
  :hook (prog-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "#FFBF00")
     ("FIXME"  . "#DE3163"))))

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; To provide project management + navigation features
(use-package projectile
  :init (projectile-mode 1)
  :custom
  ;; Cache to prevent slow 'projectile-find-file' on larger projects
  (projectile-enable-caching t)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :diminish magit-minor-mode
  :hook (git-commit-mode . (lambda () (setq fill-column 72)))
  :mode ("/\\.gitmodules\\'" . conf-mode)
  :custom
  ;; hide ^M chars at the end of the line when viewing diffs
  (magit-diff-hide-trailing-cr-characters t)

  ;; Limit legth of commit message summary
  (git-commit-summary-max-length 50)

  ;; Open status buffer in same buffer
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :config
  ;; Must define here to ensure underlying function defined in
  ;; 'init-emacs' is loaded before 'magit'.
  (defun my/magit-kill-all-buffers ()
    "Kill all buffers derived from 'magit-mode'."
    (interactive)
    (my/kill-buffers-by-mode 'magit-mode)))

(use-package with-editor
  :after (vterm magit)
  :commands vterm
  :config
  ;; To use current Emacs instance as "the editor" in 'vterm'
  (add-hook 'vterm-mode-hook 'with-editor-export-editor)

  ;; Activate 'with-editor' for several git message buffers
  (add-to-list 'auto-mode-alist
               '("/\\(?:COMMIT\\|NOTES\\|TAG\\|PULLREQ\\)_EDITMSG\\'"
                 . with-editor-mode))

  ;; To use Emacs bindings in the EDITMSG buffer
  (shell-command "git config --global core.editor emacsclient"))

(use-package git-gutter
  :hook ((prog-mode org-mode) . git-gutter-mode)
  :custom
  (git-gutter:modified-sign "=")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :bind (("C-x P" . git-gutter:previous-hunk)
         ("C-x N" . git-gutter:next-hunk)
         ("C-x G" . git-gutter:popup-hunk))
  :config
  (use-package git-gutter-fringe
    :commands git-gutter-mode
    :config (global-git-gutter-mode)))

(use-package vterm
  :defer t
  :commands vterm
  ;; Requires compilation, which may not work without installing dependencies
  :init (setopt vterm-always-compile-module t)
  :config
  (defun my/vterm-new ()
  "Prompt the user for a new vterm buffer name and open it."
  (interactive)
  (let ((vterm-buffer-name (read-string "Enter new vterm buffer name: ")))
    (vterm (generate-new-buffer-name (concat "*" vterm-buffer-name "*"))))))

(use-package aggressive-indent
  :hook (emacs-lisp-mode))

(use-package helpful
  :bind
  (("C-h k" . helpful-kill-buffers)
    ("C-h j" . helpful-at-point)
    ("C-h f" . helpful-callable)
    ("C-h F" . helpful-function)
    ("C-h v" . helpful-variable)
    ("C-h k" . helpful-key)
    ("C-h c" . helpful-command)
    ("C-h m" . helpful-mode)
    ("C-h M" . describe-macro))
  :config
  ;; Must define here to ensure underlying function defined in
  ;; 'init-emacs' is loaded before 'helpful'.
  (defun my/helpful-kill-all-buffers ()
    (interactive)
    (my/kill-buffers-by-mode 'help-mode 'helpful-mode)))

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(provide 'init-dev)
;;; init-dev.el ends here
