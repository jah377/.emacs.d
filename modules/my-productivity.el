;;; -*- lexical-binding: t -*-

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

(use-package avy
  :bind (("M-j" . avy-goto-char-timer)  ;; orig. 'default-indent-new-line'
         :map isearch-mode-map
         ("M-j" . avy-isearch))
  :custom
  (avy-timeout-seconds 0.3 "Seconds before overlay appears")
  (avy-style 'pre "Overyly single char at beginning of word")
  :custom-face
  ;; Change colors to improve readability
  (avy-lead-face ((t (:background "#000000" :foreground "#33A4FF" :weight bold)))))

(use-package type-break
  :hook (after-init)
  :custom
  (type-break-interval (* 50 60) "Work session duration")
  (type-break-good-break-interval (* 5 60) "Break duration")
  (type-break-good-rest-interval nil "Start break immediately")
  (type-break-keystroke-threshold '(nil . nil) "Break due to time, not keystroke")
  (type-break-file-name nil "Donot save break info")
  (type-break-query-mode t "Remind later, if break declined")
  (type-break-query-function 'y-or-n-p)
  (type-break-query-interval (* 10 60))
  (type-break-mode-line-message-mode t))

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; Alternative to built-in Emacs help
(use-package helpful
  :after my-functions
  :bind (("C-h k" . helpful-kill-buffers)
         ("C-h j" . helpful-at-point)
         ("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h c" . helpful-command)
         ("C-h m" . helpful-macro)
         ("C-h M" . describe-mode))
  :config
  (defun jh/helpful-kill-all-buffers ()
    "Kill all buffers derived from 'help-mode' or 'helpful-mode'."
    (interactive)
    (jh/kill-buffers-by-mode 'help-mode 'helpful-mode)))

(provide 'my-productivity)

;;; my-productivity.el ends here
