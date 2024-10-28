;;; -*- lexical-binding: t -*-

;; Discard all themes before loading next
(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  (mapc #'disable-theme custom-enabled-themes))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setopt doom-themes-enable-bold t    ; if nil, bold is universally disabled
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

;; Set fonts
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
(defun set-font-size ()
  "Set the font-pt size."
  (interactive)
  (let* ((pt-size (string-to-number (read-string "Font size: ")))
         (font-height (* 10 pt-size)))
    (set-face-attribute 'default nil :height font-height)))

(use-package doom-modeline
  :config (doom-modeline-mode 1)
  :custom
  ;; Display project_name/../file_name
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-encoding nil "Dont care about UTF-8 badge")
  (doom-modeline-vcs-max-length 30   "Limit branch name length")
  (doom-modeline-enable-word-count t "Turn on wordcount"))

;; Highlight occurrences of the same text in buffer
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

(provide 'my-visuals)
