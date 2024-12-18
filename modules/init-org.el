;;; init-org.el --- Emacs configuration file  -*- lexical-binding: t; no-byte-compile: t -*-
;; Copyright (C) 2024-2024 Jonathan A. Harris

;; Author: Jonathan A. Harris, MSc.
;; Keywords: configuration
;; Homepage: https://github.com/jah377/.emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has been generated from 'README.org'. DO NOT EDIT.

;; Changes to the configuration should be done in 'README.org' and then
;; re-tangled by calling 'C-c C-v C-t'.

;;; Code:

(use-package org
  :demand t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook ((org-src-mode . whitespace-cleanup)
         ;; Automatic break line at 'current-fill-column' (line wrapping)
         (org-mode . turn-on-auto-fill))
  :custom
  (org-ellipsis " "          "Configured by 'org-modern'")
  (org-startup-folded t      "Always fold headers")
  (org-startup-indented t    "Visually indent headers/blocks at startup")
  (org-adapt-indentation t   "Align contents with heading")
  (org-element-use-cache nil "Avoid 'org-element--cache' error")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package org-make-toc
  :after org
  :hook ((org-mode . org-make-toc-mode)
         (org-mode . (lambda ()
                       ;; 'nil' specifies that this is not a "local" addition
                       ;; 't' ensures the hook is buffer-local
                       (add-hook 'before-save-hook #'org-make-toc nil t)))))

(use-package org-modern
  :after org
  :init (global-org-modern-mode)
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :commands (org-modern-mode org-modern-agenda))

(setopt org-hide-emphasis-markers t)

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom (org-appear-inside-latex t))

;; ? speed-key opens Speed Keys help.
(setopt org-use-speed-commands
      (lambda ()
        (and (looking-at org-outline-regexp)
             (looking-back "^\**"))))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (shell . t)))

(setopt org-structure-template-alist
        '(("x" . "example")
          ("q" . "quote")
          ("e" . "src emacs-lisp")
          ("m" . "src emacs-lisp :tangle modules/init-XXX.el")
          ("s" . "src sh")
          ("p" . "src python")))

(setopt org-confirm-babel-evaluate nil)

(setopt org-src-preserve-indentation t)

(setopt org-src-window-setup 'current-window)

(setopt org-src-ask-before-returning-to-edit-buffer t)

;; https://github.com/emacs-jupyter/jupyter/issues/366
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(setopt org-startup-with-inline-images t)

(setopt org-display-remote-inline-images 'cache)

(add-hook 'org-babel-after-execute-hook
          (lambda () (org-display-inline-images nil t)))

(defconst my-agenda-dir (concat my-persist-dir "agendas/"))
(setopt org-agenda-files (list my-agenda-dir))

(setopt org-agenda-window-setup 'only-window
        org-agenda-restore-windows-after-quit t)

(setopt org-todo-keywords '((sequence "TODO(t!)"
                                      "ACTIVE(a!)"
                                      "VERIFY(v@)"
                                      "HOLD/WAIT(h@)"
                                      "REVIEW(r@)"
                                      "RESPOND(R@)"
                                      "|" "DONE(d!)"
                                      "DELEGATED(o@)"
                                      "DROPPED(D@)")))

(setopt org-use-fast-todo-selection 'auto)

(setopt org-log-into-drawer t
        org-log-states-order-reversed nil)

(setopt org-use-fast-tag-selection 'auto)

(setopt org-tag-alist
        '(;; Setting Context
          ("@home" . ?H)
          ("@personal" . ?W)

          ;; Subject Context
          ("bug" . ?b)
          ("note" . ?n)
          ("emacs" . ?e)
          ("tools" . ?t)
          ("project" . ?p)))

(setopt org-auto-align-tags t)

;; Must specify file for each template
(defconst work-agenda-file (concat my-agenda-dir "agenda_work.org"))
(defconst personal-agenda-file (concat my-agenda-dir "agenda_personal.org"))

(setopt org-capture-templates
        '(("w" "Work Task Template" entry (file work-agenda-file)
           "* TODO %^{Task} %(org-set-tags \"@work\")%^G
:PROPERTIES:
:project: %^{Project}
:git_issue: #%^{Git Issue|None}
:repo: %^{Repository}
:branch: %^{Branch}
:END:
:LOGBOOK:
- State \"TODO\"       from              %U

  %?
:END:"
           :empty-lines 1
           :kill-buffer t)

          ("p" "Personal Task Template" entry (file personal-agenda-file)
           "* TODO %^{Task} %(org-set-tags \"@personal\")%^G
:PROPERTIES:
:project: %^{Git Issue|None}
:END:
:LOGBOOK:
- State \"TODO\"       from              %U

  %?
:END:"
           :empty-lines 1
           :kill-buffer t)

          ("r" "Merge Request Task Template" entry (file work-agenda-file)
           "* REVIEW %^{Task} %(org-set-tags \"@work\")%^G
:PROPERTIES:
:project: %^{Project}
:repo: %^{Repository}
:branch: %^{Branch}
:merge-review: !%^{Git Issue|None}
:END:
:LOGBOOK:
- State \"REVIEW\"       from              %U

  %?
:END:"
           :empty-lines 1
           :kill-buffer t)

          ("r" "Review Task Template" entry (file agenda-file)
           "* TODO %^{Task} %^g
:PROPERTIES:
:repo: %^{Repository}
:branch: %^{Branch}
:merge-request: !%^{MR Number}
:END:
:LOGBOOK:
- State \"TODO\"       from              %U

  %?
:END:"
           :empty-lines 1
           :kill-buffer t)))

(setopt org-agenda-remove-tags t)

(use-package org-super-agenda
  :defer t
  :after org
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom (org-super-agenda-header-prefix "❯ ")
  :config
  (set-face-attribute 'org-super-agenda-header nil :weight 'bold))

(setq org-agenda-custom-commands
      '(("e" "Personal Emacs Tasks"
         ((alltodo "" ((org-agenda-overriding-header "Emacs TODOs")
                       (org-super-agenda-groups '((:discard (:not (:tag ("personal" "emacs"))))
                                                  (:discard (:tag "work"))
                                                  (:auto-property "project")))))))))

(provide 'init-org)
;;; init-org.el ends here
