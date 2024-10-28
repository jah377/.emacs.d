;;; -*- lexical-binding: t -*-

(use-package dired
  :ensure nil
  :commands dired
  :after my-functions
  :custom
  ;; -A :: Show hidden files but omit implied '.' and '..' targets
  ;; -h :: Make file sizes human-readable
  ;; -l :: Produce long=g, detailed listing (required by 'dired')
  ;; -v :: Sort file by version number
  ;; --group-directories-first :: List directories at top of buffer
  ;; --time-style=long-iso     :: List %Y-%m-%d %H:%M
  (dired-listing-switches (concat "-Ahlv"
                                  " --group-directories-first"
                                  " --time-style=long-iso"))

  ;; Refresh 'dired' buffer if directory changes
  (dired-auto-revert-buffer #'dired-buffer-changed-p)

  ;; If two 'dired' buffers open side-by-side, Emacs will suggest the
  ;; other buffer directory when moving files
  (dired-dwim-target t)

  :config
  ;; The variable 'dired-kill-when-opening-new-dired-buffer'
  ;; exists. However, there are use-cases for having multiple 'dired'
  ;; buffers open at the same time (see 'dired-dwim-target').
  (defun jh/dired-kill-all-buffers ()
    "Delete all open 'dired'-mode buffers."
    (interactive)
    (jh/kill-buffers-by-mode 'dired-mode)))

(use-package dired-x
  :ensure nil
  :after dired
  :commands dired
  :custom
  ;; Files to ignore in 'dired' buffer.
  (dired-omit-files (concat dired-omit-files
                            "\|"
                            (rx (or (: bos (or "\.DS_Store"
                                               "__MACOSX"
                                               "\.git")
                                       eos)
                                    (: bos "__pycache__")
                                    ".ipynb_checkpoints"
                                    "\.~.*#"))))
  :config
  ;; Must activate 'dired-omit-mode' to omit 'dired-omit-files'.
  (add-hook 'dired-after-readin-hook 'dired-omit-mode))

(use-package dired-aux
  :ensure nil
  :after dired
  :commands dired
  :bind (:map dired-mode-map
              ;; C-+ calls 'er/expand-region'
              ("M-+" . dired-create-empty-file))
  :custom
  ;; Revert dired-buffer after 'dired-do' operations
  (dired-do-revert-buffer t))

(use-package wdired
  :ensure nil
  :after dired
  :commands dired
  :custom
  ;; Change permission using 'SPC'
  (wdired-allow-to-change-permissions t))

(use-package nerd-icons-dired
  :ensure nil
  :after (dired nerd-icons)
  :hook (dired-mode))

(provide 'my-dired)

;;; my-dired.el ends here
