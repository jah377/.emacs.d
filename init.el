;;; init.el --- Emacs configuration file  -*- lexical-binding: t; no-byte-compile: t -*-
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

;; https://github.com/LionyxML/emacs-kick/blob/master/init.el
(add-hook 'after-init-hook
          (lambda ()
            (with-current-buffer (get-buffer-create "*scratch*")
              (insert (format "*Welcome to Emacs!*

+ Loading time :: %s secs
+ Packages :: %s
+ Garbage Collections :: %s"
                              (emacs-init-time "%.2f")
                              (number-to-string (length package-activated-list))
                              gcs-done)))))

;; Required for managing external packages
(require 'package)

(setopt package-archives
      '(("gnu elpa"  . "https://elpa.gnu.org/packages/")
        ("melpa"     . "https://melpa.org/packages/")
        ("nongnu"    . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("melpa"    . 6)
        ("gnu elpa" . 5)
        ("nongnu"   . 4)))

;; MANDITORY: Emacs must be aware of available packages before installing
(package-initialize)

;; Ensures backwards compatability; 'use-package' added in Emacs29
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Automatically install package if package not already installed
(require 'use-package-ensure)
(setopt use-package-always-ensure t)

;; Track load times and return loading/installation output messages
(setopt use-package-compute-statistics t
        use-package-verbose t)

(let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
  (unless (file-directory-p default-directory)
    (make-directory default-directory t))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Directory to be backed up in the cloud
(defconst my-persist-dir "/home/jon/.kb_persistent_emacs/")

(use-package no-littering
  :demand t
  :init
  (setopt no-littering-etc-directory (concat my-persist-dir "no-littering/"))
  (setopt no-littering-var-directory (concat my-persist-dir "no-littering/"))
  :config
  (setopt custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file :no-error-if-file-is-missing))

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'init-emacs)
(require 'init-ui)
(require 'init-org)
(require 'init-completion)
(require 'init-dired)
(require 'init-dev)
(require 'init-writing)
(require 'init-kbd)
;;; init.el ends here
