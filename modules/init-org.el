;;; init-org.el --- Emacs configuration file  -*- lexical-binding: t; no-byte-compile: t -*-
;; Copyright (C) 2024-2025 Jonathan A. Harris

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

(use-package org-auto-tangle
  :after org-mode
  :hook (org-mode . org-auto-tangle-mode))

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
                                      "COMMENT(c@)"
                                      "MESSAGE(m@)"
                                      "|" "DONE(d!)"
                                      "MERGED(M!)"
                                      "DELEGATED(o@)"
                                      "DROPPED(D@)")))

(setopt org-use-fast-todo-selection 'auto)

(setopt org-log-into-drawer t
        org-log-states-order-reversed nil)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all sub-entries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(setopt org-use-fast-tag-selection 'auto)

(setopt org-tag-alist
        '(("bug" . ?b)
          ("note" . ?n)
          ("emacs" . ?e)
          ("tools" . ?t)
          ("reviews" . ?r)
          ("project" . ?p)))

(setopt org-auto-align-tags t)

(setopt org-tags-exclude-from-inheritance '("project"))
(setopt org-use-property-inheritance '("project"))

;; Constants used by org-capture templates
(defconst my-agenda-file-work (concat my-agenda-dir "agenda_work.org"))
(defconst my-agenda-file-personal (concat my-agenda-dir "agenda_personal.org"))

(setopt org-capture-templates
        '(("w" "WORK Templates")
          ("wp" "Work Project" entry (file my-agenda-file-work)
           "* TODO %^{Project} [/] %(org-set-tags \"project\")
:PROPERTIES:
:project: %^{project-name}
:END:
:LOGBOOK:
- State \"TODO\"       from              %U
  %?
:END:"
           :empty-lines 1
           :kill-buffer t)

          ("wt" "Work Todo" entry (file my-agenda-file-work)
           "* TODO %^{Task} %^G
:PROPERTIES:
:repo: %^{Repository}
:branch: %^{Branch}
:git_issue: #%^{Git Issue|None}
:merge_request: !%^{MR|None}
:END:
:LOGBOOK:
- State \"TODO\"       from              %U
  %?
:END:"
           :empty-lines 1
           :jump-to-captured t)

          ("wr" "Work Review" entry (file my-agenda-file-work)
           "* REVIEW %^{Task} %(org-set-tags \"review\")%^G
:PROPERTIES:
:repo: %^{Repository}
:branch: %^{Branch}
:merge_request: !%^{MR|None}
:requester: %^{Requester}
:END:
:LOGBOOK:
- State \"REVIEW\"       from              %U
  %?
:END:"
           :empty-lines 1
           :jump-to-capture t)

          ("p" "PERSONAL Templates")
          ("pp" "Personal Product" entry (file my-agenda-file-personal)
           "* TODO %^{Project} [/] %(org-set-tags \"project\")
:PROPERTIES:
:project: %^{project-name}
:END:
:LOGBOOK:
- State \"TODO\"       from              %U
  %?
:END:"
           :empty-lines 1
           :kill-buffer t)

          ("pt" "Personal Todo" entry (file my-agenda-file-personal)
           "* TODO %^{Task} %^G
:PROPERTIES:
:repo: %^{Repository}
:branch: %^{Branch}
:END:
:LOGBOOK:
- State \"TODO\"       from              %U
  %?
:END:"
           :empty-lines 1
           :jump-to-captured t)))

(setopt org-agenda-remove-tags t)

(use-package org-super-agenda
  :defer t
  :after org
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom (org-super-agenda-header-prefix "❯ ")
  :config
  (set-face-attribute 'org-super-agenda-header nil :weight 'bold))

(setq org-agenda-custom-commands
      '(("w" "MLP: Project Planning"
         ((alltodo "" ((org-agenda-overriding-header "Work Projects")
                       (org-super-agenda-groups
                        '((:discard (:not (:tag ("work"))))
                          (:auto-parent t)))))))
        ("r" "MLP: Merge Request Review"
         ((alltodo "" ((org-agenda-overriding-header "MLP: Active MRs")
                       (org-super-agenda-groups
                        '((:discard (:not (:tag "work" "review")))))))))))

(provide 'init-org)
;;; init-org.el ends here
