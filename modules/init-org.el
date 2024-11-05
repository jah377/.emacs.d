;;; init-org.el --- Configure Org-Mode -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; The 'org' package is a useful tool for note-taking, project management, and
;; literative coding (like this document). It is complex and full of features,
;; which we configure in this section.

;; 'init-org.el' configures all aspects of 'org-mode'.

;;; IMPORTANT:
;; Changes to this file should be done in 'README.org' and re-tangled.

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

(setopt org-confirm-babel-evaluate nil)

(setopt org-src-preserve-indentation t)

(setopt org-src-window-setup 'split-window-right)

(setopt org-src-ask-before-returning-to-edit-buffer t)

;; https://github.com/emacs-jupyter/jupyter/issues/366
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(setopt org-startup-with-inline-images t)

(setopt org-display-remote-inline-images 'cache)

(add-hook 'org-babel-after-execute-hook
          (lambda () (org-display-inline-images nil t)))

(defconst my-agenda-dir (concat user-emacs-directory ".agendas/"))
(defconst my-emacs-agenda (concat my-agenda-dir "emacs.org"))

(setopt org-agenda-files (list my-agenda-dir))

(setopt org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t)

(setopt org-log-states-order-reversed nil)

(setopt org-use-fast-todo-selection 'auto
        org-use-fast-tag-selection t)

(setopt org-agenda-hide-tags-regexp ".*"
        org-auto-align-tags t)

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
                       (org-super-agenda-groups '((:auto-tags t)))))))))

(setopt org-structure-template-alist
        '(("x" . "example")
          ("q" . "quote")
          ("e" . "src emacs-lisp")
          ("m" . "src emacs-lisp :tangle modules/init-XXX.el")
          ("s" . "src sh")
          ("p" . "src python")))

;; I use the ':project:' property to connect related tasks. It is imperative
;; that the value is consistent. Givne a specific file, these functions provide
;; a list of previously used project values for me to select from.
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
:project: %(org-capture--select-project-entry agenda-work)
:END:
:LOGBOOK:
- State \"TODO\"       from              %U
  %?
:END:"
         :empty-lines 1
         :kill-buffer t)))

(provide 'init-org)
;;; init-org.el ends here
