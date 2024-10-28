;;; -*- lexical-binding: t -*-

(use-package magit
  :bind ("C-x g" . magit-status)
  :diminish magit-minor-mode
  :hook (git-commit-mode . (lambda () (setopt fill-column 72)))
  :mode ("/\\.gitmodules\\'" . conf-mode)
  :custom
  ;; hide ^M chars at the end of the line when viewing diffs
  (magit-diff-hide-trailing-cr-characters t)

  ;; Limit legth of commit message summary
  (git-commit-summary-max-length 50)

  ;; Open status buffer in same buffer
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :config
  (defun jh/magit-kill-all-buffers ()
    "Kill all buffers derived from 'magit-mode'."
    (interactive)
    (jh/kill-buffers-by-mode 'magit-mode)))

;; Switching branchs may change file on disk; if so, refresh buffers
(global-auto-revert-mode)

(use-package git-gutter
  :hook (prog-mode org-mode)
  :bind (("C-x P" . git-gutter:previous-hunk)
         ("C-x N" . git-gutter:next-hunk)
         ("C-x G" . git-gutter:popup-hunk))
  :config
  ;; Must include if 'linum-mode' activated (common in 'prog-mode')
  ;; because 'git-gutter' does not work with 'linum-mode'.
  (use-package git-gutter-fringe
    :commands git-gutter-mode
    :config (global-git-gutter-mode)))

(provide 'my-vc)

;;; my-vc.el ends here
