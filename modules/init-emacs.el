;;; init-emacs.el --- Emacs configuration file  -*- lexical-binding: t; -*-
;; Copyright (C) 2024-2024 Jonathan A. Harris

;; Author: Jonathan A. Harris, MSc.
;; Created: 2024-12-03
;; Keywords: configuration
;; Homepage: https://github.com/jah377/.emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from 'README.org'. DO NOT EDIT.

;; Changes to the configuration should be done in 'README.org' and then
;; re-tangled by calling 'C-c C-v C-t'.

;;; Code:

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setopt frame-title-format
        '("emacs: "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name)) "%b"))))

(setopt uniquify-buffer-name-style 'forward)

(setopt enable-recursive-minibuffers t)

(setopt minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(advice-add 'keyboard-quit :before (lambda ()
                                     (when (active-minibuffer-window)
                                       (abort-recursive-edit))))

(defun my/jump-to-minibuffer ()
  "Switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(setq-default cursor-type 'bar)

(pixel-scroll-precision-mode 1)

(setopt mouse-yank-at-point t
        mouse-wheel-follow-mouth 't
        mouse-wheel-progressive-speed nil)

(use-package display-line-numbers
  :hook (prog-mode org-mode)
  :custom
  (display-line-numbers-width  4 "Prevent uneven gutter due to length of linum"))

(setopt fill-column 79)

(global-visual-line-mode 1)

;; Break at whitespace, not middle of word
(setopt word-wrap t)

;; Replace active region when typing text
(delete-selection-mode 1)

(global-auto-revert-mode 1)

(defun my/kill-buffer-name ()
  "Copy file-name of current buffer"
  (interactive)
  (kill-new (buffer-name)))

(defun my/kill-relative-buffer-path ()
  "Copy relative buffer path to kill ring.

Replace '/home/<username>' prefix with '~' if applicable."
  (interactive)
  (if-let* ((full-path (buffer-file-name))
            (abbrev-path (abbreviate-file-name full-path)))
      (progn
        (kill-new abbrev-path)
        (message "Buffer path copied to kill ring: %s" full-path))
    (message "Buffer is not visiting a file.")))

(defun my/kill-buffers-except (buffer-name)
  "Kill all buffers except for BUFFER-NAME."
  (dolist (buffer (buffer-list))
    (unless (or (string-equal (buffer-name buffer) buffer-name)
                (string-equal (buffer-name buffer) (concat " " buffer-name)))
      (kill-buffer buffer))))

(defun my/kill-all-buffers-except-scratch ()
  "Kill all buffers except for *scratch*."
  (interactive)
  (my/kill-buffers-except "*scratch*"))

(defun my/kill-buffers-by-mode (&rest modes)
  "Kill all buffers derived from any of MODES.

Ex: (my/kill-buffers-by-mode 'help-mode 'helpful-mode)"
  (let ((killed 0))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (when (apply #'derived-mode-p modes)
                (kill-buffer buffer)
                (setq killed (1+ killed)))))
          (buffer-list))
    (message "Killed %d buffer(s) derived from %s" killed modes)))

(setopt indicate-empty-lines t)

(use-package whitespace
  :hook ((prog-mode . whitespace-mode)
         (before-save . whitespace-cleanup)
         ;; Disable in 'org'; always flags long org-links
         (org-mode . (lambda () (whitespace-mode 0)))
         ;; Makefiles rely heavily on indentation and whitespace
         (makefile-mode . (lambda ()
                            (setq indent-tabs-mode t
                                  whitespace-mode nil)
                            (add-hook 'before-save-hook
                                      #'delete-trailing-whitespace))))
  :custom
  (whitespace-line-column fill-column "Highlight text beyond column")
  (whitespace-style '(face
                      trailing
                      lines-tail
                      empty
                      indentation::space
                      space-before-tab::tab))
  :config
  (global-whitespace-mode 0))

(setq my-indent-width 4)
(setq-default tab-width my-indent-width
              standard-indent my-indent-width)

(setq-default tab-always-indent 'complete)

(setq-default indent-tabs-mode nil)

(winner-mode 1)

(savehist-mode 1)

(save-place-mode 1)

(setopt use-short-answers t)
(fset 'yes-or-no-p 'y-or-n-p)

(setopt confirm-nonexistent-file-or-buffer nil)

(defun my/crm-indicator (args)
  "Add indicator to completion promp when using 'completing-read-multiple'"
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'my/crm-indicator)

(setopt kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

(use-package beacon
  :hook (after-init . beacon-mode))

(use-package paren
  :custom
  (show-paren-style 'parenthesis "Only highlight ()")
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  ;; If cursor on ), show overlay for (
  (show-paren-context-when-offscreen 'overlay)
  :config
  (show-paren-mode 1)
  (electric-pair-mode 1))

(use-package highlight-thing
  :demand t
  :hook ((prog-mode . highlight-thing-mode)
         (org-mode . highlight-thing-mode))
  :custom
  (highlight-thing-exclude-thing-under-point t)
  (highlight-thing-case-sensitive-p t)
  (highlight-thing-ignore-list
   '("False" "True", "return", "None", "if", "else", "self",
     "import", "from", "in", "def", "class")))

(global-hl-line-mode 1)

(defun gopar/pulse-current-region (&rest _)
  "Pulse the current implicit or active region"
  (if mark-active
      (pulse-momentary-highlight-region (region-beginning) (region-end))
    (pulse-momentary-highlight-region (mark) (point))))

(advice-add #'kill-ring-save :before #'gopar/pulse-current-region)

(global-auto-revert-mode)

;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
(set-default-coding-systems 'utf-8)

;; https://github.com/Thaodan/emacs.d
(define-coding-system-alias 'UTF-8 'utf-8)

(use-package bookmark
  :custom
  (bookmark-save-flag t "Save bookmarks when Emacs killed")
  (bookmark-fringe-mark t "Non-nil to show icon in fringe"))

(auto-image-file-mode 1)

(setopt ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)

(defun my/find-config-file ()
  "Open 'README.org' in other window."
  (interactive)
  (find-file-other-window (concat user-emacs-directory "README.org")))

(defun my/eval-init ()
  "To quickly reload the 'init.el' file."
  (interactive)
  (load-file user-init-file))

(provide 'init-emacs)
;;; init-emacs.el ends here
