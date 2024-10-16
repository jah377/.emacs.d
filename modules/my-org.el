  ;;; -*- lexical-binding: t -*-

;; The essentials
(use-package org
  :demand t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook (org-src-mode . whitespace-cleanup)
  :custom
  (org-ellipsis " ")
  (org-startup-folded t    "Always fold headers")
  (org-startup-indented t  "Visually indent at startup")
  (org-adapt-indentation t "Align contents with heading")
  (org-element-use-cache nil "Avoid 'org-element--cache' error")

;; "Modernizes" UI experience of 'org-mode'
(use-package org-modern
  :after org
  :init (global-org-modern-mode)
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :commands (org-modern-mode org-modern-agenda))

(setq org-hide-emphasis-markers t)

;; Interactively toggle visability if cursor between markers
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom (org-appear-inside-latex t))

;; 'C-a/e' jump to start-end of headline text
(setq org-special-ctrl-a/e t)

;; 'C-k' behave different on headline text
(setq org-special-ctrl-k t)

;; Do not delete hidden subtree with 'C-k'
(setq org-ctrl-k-protect-subtree t)

;; ? speed-key opens Speed Keys help.
(setq org-use-speed-commands
      ;; If non-nil, 'org-use-speed-commands' allows efficient
      ;; navigation of headline text when cursor is on leading
      ;; star. Custom function allows use of Speed keys if on ANY
      ;; stars.
      (lambda ()
        (and (looking-at org-outline-regexp)
             (looking-back "^\**"))))

(setq org-startup-with-inline-images t
      ;; Allow for inline display of remote images
      org-display-remote-inline-images 'cache)

;; Refresh inline images after executing an 'src-block'
(add-hook 'org-babel-after-execute-hook
          (lambda () (org-display-inline-images nil t)))

(setq org-confirm-babel-evaluate nil
      org-src-window-setup 'current-window
      org-src-ask-before-returning-to-edit-buffer t)

;; Remove code indentation in org-src blocks
(setq org-src-preserve-indentation t)

;; https://github.com/emacs-jupyter/jupyter/issues/366
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq org-structure-template-alist
      '(("x" . "example")
        ("q" . "quote")
        ("e" . "src emacs-lisp")
        ("m" . "src emacs-lisp :tangle modules/my-")
        ("s" . "src sh")
        ("p" . "src python")))

(provide 'my-org)
