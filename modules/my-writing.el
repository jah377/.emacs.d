;;; -*- lexical-binding: t -*-

(use-package jinx
  :hook (org-mode text-mode prog-mode conf-mode)
  :bind (("C-c j c" . jinx-correct)
         ("C-c j a" . jinx-correct-all)
         ;; alias defined using 'jinx-correct' keybinding
         ("C-c j d" . jinx-save-word-at-point))
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
  (defalias 'jinx-save-word-at-point (kmacro "C-c j c @ RET"))

  ;; 'jinx-correct' suggestions displayed as grid instead of long list
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20))))

(use-package denote
  :commands denote
  :custom
  (denote-directory "~/notes/")
  (denote-file-type "org")
  (denote-prompts '(title keywords))
  (denote-known-keywords '("emacs" "python" "linux" "ml" "work"))
  ;; TODO: use separate templates for coding/ect
  (denote-templates nil)
  (denote-org-front-matter (concat "#+TITLE: %1$s\n"
                                   "#+DATE: %2$s\n"
                                   "#+ID: %4$s\n"
                                   "#+FILETAGS: %3$s\n"
                                   "#+STARTUP: overview\n"))
  :config
  ;; Good idea to update 'org-dblock' links/backlinks before saving buffers
  (add-hook 'before-save-hook (lambda ()
                                (when (denote-file-is-note-p (buffer-file-name))
                                  (org-update-all-dblocks))))

(use-package consult-denote
  :after (consult denote)
  :commands (consult-denote-find))

(provide 'my-writing)

;;; my-writing.el ends here
