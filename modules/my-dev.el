;;; -*- lexical-binding: t -*-

;; Terminal emulator inside Emacs
(use-package vterm
  ;; Requires compilation, which may not work without installing dependencies
  :init (setq vterm-always-compile-module t)
  :config
  (defun jh/vterm-new ()
  "Prompt the user for a new vterm buffer name and open it."
  (interactive)
  (let ((vterm-buffer-name (read-string "Enter new vterm buffer name: ")))
    (vterm (generate-new-buffer-name (concat "*" vterm-buffer-name "*"))))))

;; Update indentation in response to changes to code
(use-package aggressive-indent
  :hook (emacs-lisp-mode))

(provide 'my-dev)

;;; my-dev.el ends here
