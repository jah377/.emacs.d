;;; init-dev.el --- Emacs configuration file  -*- lexical-binding: t; no-byte-compile: t -*-
;; Copyright (C) 2024-2025 Jonathan A. Harris

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
  :after nerd-icons
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

  ;; Enable file icons in 'magit-status'
  (magit-format-file-function #'magit-format-file-nerd-icons)
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

(use-package devdocs-browser
  :bind ("C-h D" . devdocs-browser-open-in)
  :custom
  (devdocs-browser-data-directory (concat my-persist-dir "no-littering/devdocs"))
  :config
  ;; Programmatically install documentation
  (dolist (doc '("python~3.11"
                 "pytorch~2"
                 "pandas~2"
                 "matplotlib~3.8"
                 "numpy~2.0"
                 "scikit_learn"))
    (devdocs-browser-install-doc doc)))

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

(use-package code-cells
  :hook ((python-mode emacs-lisp-mode)
         (prog-mode . outline-minor-mode))
  :bind (("C-c C-c" . code-cells-eval)
         ("M-p" . code-cells-backward-cell)
         ("M-n" . code-cells-forward-cell)
         ("M-D" . code-cells-kill)
         ("M-W" . code-cells-copy)
         ("M-I" . jh/code-cells-insert)
         ("C-<tab>" . outline-cycle))
  :custom
  (code-cells-eval-region-commands
   '((emacs-lisp-mode . eval-region)
     (lisp-interaction-mode . eval-region)
     (python-base-mode . jh/jupyter-eval-region-maybe)))
  :config
  (defun jh/jupyter-eval-region-maybe ()
    "Call 'jupyter-eval-region' if available, otherwise use shell."
    (if jupyter-current-client
        'jupyter-eval-region
      'python-shell-send-region))

  (defun jh/code-cells-insert ()
    (interactive)
    (when (not (bolp))
      (newline 2))
    (insert (substring code-cells-boundary-regexp 1))
    (newline 2))

  ;; TODO: Reduce redundant code; previously tried 'dolist'
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (setq-local code-cells-boundary-regexp "^;;; ++++++++++"
                                                outline-regexp "^;;; ++++++++++")))
  (add-hook 'python-mode-hook (lambda ()
                                (setq-local code-cells-boundary-regexp "^### ++++++++++"
                                            outline-regexp "^### ++++++++++"))))

(use-package aggressive-indent
  :hook (emacs-lisp-mode))

(use-package helpful
  :custom
  (helpful-max-buffers 1)
  :bind
  (("C-h k" . helpful-key)
   ("C-h f" . helpful-function)
   ("C-h c" . helpful-callable)
   ("C-h p" . helpful-at-point)
   ("C-h v" . helpful-variable)
   ("C-h m" . helpful-macro)))

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config (make-variable-buffer-local 'python-shell-virtualenv-root)
  :custom
  (python-shell-interprter "ipython")
  ;; At minimum, must include '--simple-prompt' if using ipython
  (python-shell-interpreter-args "--simple-prompt --classic")
  ;; 3rd party py-files may have different indentation; disable if guess fails
  (python-indent-guess-indent-offest t)
  (python-indent-guess-indent-offset-verbose nil)
  ;; Modified pep-257 removes new-line at end of docstring
  (python-fill-docstring-style 'pep-257-nn))

(provide 'init-dev)
;;; init-dev.el ends here
