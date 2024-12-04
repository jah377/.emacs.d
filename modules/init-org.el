;;; init-org.el --- Emacs configuration file  -*- lexical-binding: t; -*-
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
  (org-ellipsis " " "Default to 'org-modern'")
  (org-startup-folded t "Always fold headers")
  (org-startup-indented t "Visually indent at startup")
  (org-adapt-indentation t "Align contents with heading")
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
(defconst my-emacs-agenda (concat my-agenda-dir "agenda_emacs.org"))

(setopt org-agenda-files (list my-agenda-dir))

(defun org-capture--get-project-entry-from-file (agenda-file-name)
  "Compile list of project names from org-headers in FILE that
contain the :project: property"
  (let (project-names)
    (with-current-buffer (find-file-noselect agenda-file-name)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (when (and (org-entry-get nil "project")
                   (not (org-entry-is-done-p)))
          (add-to-list 'project-names (org-entry-get nil "project")))))
    project-names))

(defun org-capture--select-project-entry (agenda-file-name)
  "Prompt user to select :project: property from FILE"
  (let ((projects (org-capture--get-project-entry-from-file agenda-file-name)))
    (completing-read "Select project:" projects nil nil)))

(setq org-capture-templates
      '(("e" "Emacs Config Task" entry (file my-emacs-agenda)
         "* TODO %^{Task} %^g
:PROPERTIES:
:project: %(org-capture--select-project-entry my-emacs-agenda)
:END:
:LOGBOOK:
- State \"TODO\"       from              %U

  %?
:END:"
         :empty-lines 1
         :kill-buffer t)))

(setopt org-agenda-window-setup 'only-window
        org-agenda-restore-windows-after-quit t)

(setopt org-log-into-drawer t
        org-log-states-order-reversed nil)

(setopt org-use-fast-todo-selection 'auto)

(setopt org-use-fast-tag-selection 'auto)

(setopt org-auto-align-tags t)

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
