;;; init-writing.el --- Configure writing tools  -*- lexical-binding: t; no-byte-compile: t -*-

;;; IMPORTANT:
;; Changes to this file should be done in 'README.org' and re-tangled.

;;; Code:

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

(use-package jinx
  :hook (org-mode text-mode prog-mode conf-mode)
  :bind (("C-c j c" . jinx-correct)
         ("C-c j a" . jinx-correct-all)
         ("C-c j d" . my/jinx-save-word-at-point))
  :custom
  ;; 'jinx-mode' only checks text possessing specific face properties like
  ;; 'font-lock-comment-face' in 'prog-mode' for example.
  (jinx-include-faces
   '((yaml-mode . conf-mode)
     (yaml-ts-mode . conf-mode)
     ;; Only check docstrings and comments; not strings
     (conf-mode font-lock-comment-face)
     (prog-mode font-lock-comment-face
                font-lock-doc-face
                tree-sitter-hl-face:comment
                tree-sitter-hl-face:doc)))

  (jinx-languages "en_GB")
  :config
  ;; Quickly save word-at-point to dictionary used by 'jinx'
  (defalias 'my/jinx-save-word-at-point (kmacro "C-c j c @ RET"))

  ;; 'jinx-correct' suggestions displayed as grid instead of long list
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20))))

(use-package denote
  :after org
  :commands denote
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-directory (concat my-persist-dir "notes/"))
  (denote-file-type "org")
  (denote-prompts '(title keywords))
  (denote-known-keywords '("emacs" "python" "linux" "ml" "work"))
  ;; TODO: use separate templates for coding/ect
  (denote-templates nil)
  (denote-org-front-matter (concat "#+TITLE: %1$s\n"
                                   "#+DATE: %2$s\n"
                                   "#+ID: %4$s\n"
                                   "#+FILETAGS: %3$s\n"
                                   "#+STARTUP: overview\n")))

(add-hook 'before-save-hook (lambda ()
                              (when (denote-file-is-note-p (buffer-file-name))
                                (org-update-all-dblocks))))

(use-package consult-denote
  :after (consult denote)
  :commands (consult-denote-find))

(provide 'init-writing)

;;; init-writing.el ends here
