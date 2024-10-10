;;; -*- lexical-binding: t -*-

;; Terminal emulator inside Emacs
(use-package vterm
  :config
  (defun jh/vterm-new ()
  "Prompt the user for a new vterm buffer name and open it."
  (interactive)
  (let ((vterm-buffer-name (read-string "Enter new vterm buffer name: ")))
    (vterm (generate-new-buffer-name (concat "*" vterm-buffer-name "*"))))))

(use-package aggressive-indent
  :hook (emacs-lisp-mode))

(provide 'my-dev)

;;; my-dev.el ends here
