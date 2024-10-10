;;; -*- lexical-binding: t -*-

;; Discard all themes before loading next
(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  (mapc #'disable-theme custom-enabled-themes))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun light ()
  (interactive)
  (load-theme 'doom-tomorrow-day t))

(defun dark ()
  (interactive)
  (load-theme 'doom-one t))

(dark)

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
  (doom-modeline-vcs-max-length 30 "Limit branch name length")
  (doom-modeline-enable-word-count t "Turn on wordcount"))

(provide 'my-visuals)