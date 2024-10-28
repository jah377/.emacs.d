;;; -*- lexical-binding: t -*-

;; Terminal emulator inside Emacs
(use-package vterm
  ;; Requires compilation, which may not work without installing dependencies
  :init (setopt vterm-always-compile-module t)
  :config
  (defun jh/vterm-new ()
  "Prompt the user for a new vterm buffer name and open it."
  (interactive)
  (let ((vterm-buffer-name (read-string "Enter new vterm buffer name: ")))
    (vterm (generate-new-buffer-name (concat "*" vterm-buffer-name "*"))))))

;; To provide project management + navigation features
(use-package projectile
  :init (projectile-mode 1)
  :custom
  ;; Cache to prevent slow 'projectile-find-file' on larger projects
  (projectile-enable-caching t)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; Update indentation in response to changes to code
(use-package aggressive-indent
  :hook (emacs-lisp-mode))

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(provide 'my-dev)

;;; my-dev.el ends here
