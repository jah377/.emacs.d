;;; -*- lexical-binding: t -*-

;; Profile emacs startup
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

;; Initialize package resources
(require 'package)
(setq package-archives
      '(("gnu elpa"  . "https://elpa.gnu.org/packages/")
        ("melpa"     . "https://melpa.org/packages/")
        ("nongnu"    . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("melpa"    . 6)
        ("gnu elpa" . 5)
        ("nongnu"   . 4)))
(package-initialize)

;; Is this still necessary since 'use-package' now builtin?
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Standardize `use-package` settings
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-compute-statistics t
      use-package-verbose t)

(use-package no-littering
  :demand t
  :config
  ;; Save customizations in 'etc' sub-directory and load on startup
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Garbage Collection Magic Hack
(use-package gcmh
  :init (gcmh-mode 1)
  :hook
  (after-init . garbage-collect)

  ;; Must reset GC threshold values after initialization
  (emacs-startup . (lambda () (setq gc-cons-percentage 0.1
                                    gc-cons-threshold (* 32 1024 1024)
                                    gcmh-high-cons-threshold (* 32 1024 1024)
                                    gcmh-idle-delay 30))))

;; Idle garbage collecting
(run-with-idle-timer 30 t (lambda () (garbage-collect)))

;; Change frame title w.r.t. current buffer
(setq frame-title-format
      '("emacs: " (:eval (if (buffer-file-name)
                             (abbreviate-file-name (buffer-file-name)) "%b"))))

;; Maximize frame size at init
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Unique buffers of identical files denoted with parent directory name
(setq uniquify-buffer-name-style 'forward)

;; Built-in mode to record changes in the windows configuration
;; See 'winner-undo' and 'winner-redo' functions
(winner-mode 1)

;; Too lazy to type 'yes-or-no'
(setq use-short-answers t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Kill buffer, even if live process attached
;; https://www.masteringemacs.org/article/disabling-prompts-emacs
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Closes minibuffer regardless of point location
(advice-add 'keyboard-quit :before (lambda ()
                                     (when (active-minibuffer-window)
                                       (abort-recursive-edit))))

;; Disable 'TAB' for indentation
(setq-default indent-tabs-mode nil)

;; Use 'TAB' for auto-completion selection
(setq-default tab-always-indent 'complete)

;; Number of spaces occupied by 'TAB'
(setq-default tab-width 4
              standard-indent 4)

;; Built-in Emacs variable highlights empty lines
(setq indicate-empty-lines t)

;; Visualize whitespace and remove on cleanup
(use-package whitespace
  :hook ((prog-mode . whitespace-mode)
         (before-save . whitespace-cleanup)
         ;; Org-links are always long
         (org-mode . (lambda () (whitespace-mode nil)))
         ;; Whitespace-mode a bit aggressive if editing make files
         (makefile-mode . (lambda ()
                            (setq indent-tabs-mode t
                                  whitespace-mode nil)
                            (add-hook 'before-save-hook
                                      #'delete-trailing-whitesapce))))
  :custom
  (whitespace-line-column 79 "Highlight text beyond column")
  (whitespace-style '(face
                      trailing
                      lines-tail
                      empty
                      indentation::space
                      space-before-tab::tab))
  :config
  ;; Turn off global whitespace mode
  (global-whitespace-mode 0))

(setq  mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; scroll one line at a time
       mouse-wheel-progressive-speed nil            ; don't accelerate scrolling
       mouse-wheel-follow-mouse 't                  ; scroll window under mouse
       mouse-yank-at-point t)                       ; Mouse paste at point, not cursor

;; Scrolling at end of document adds one line
(setq scroll-step 1)

;; Use pixel scrolling instead of by line
;; https://tony-zorman.com/posts/emacs-potpourri.html
(pixel-scroll-precision-mode 1)

(setq-default cursor-type 'bar)

;; Flash cursor location when switching buffers
(use-package beacon
  :config (beacon-mode 1))

;; Replace active region by typing text
(delete-selection-mode 1)

;; Specify desired column width of buffer
(setq fill-column 79)

;; Built-in Emacs minor-mode wraps long text to next line
(global-visual-line-mode 1)

;; Highlight line containing point
(global-hl-line-mode)

;; Highlight matching parentheses
(use-package paren
  :custom
  (show-paren-style 'parenthesis "Only highlight ()")
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  ;; If cursor on ), show overlay for (
  (show-paren-context-when-offscreen 'overlay)
  :config (electric-pair-mode 1))

;; Highlight killed region
;; https://www.youtube.com/watch?v=oQ9JE9kRwG8
(defun gopar/pulse-current-region (&rest _)
  "pulse the current implicit or active region"
  (if mark-active
      (pulse-momentary-highlight-region (region-beginning) (region-end))
    (pulse-momentary-highlight-region (mark) (point))))

(advice-add #'kill-ring-save :before #'gopar/pulse-current-region)

(use-package bookmark
  :custom
  ;; By default 'no-littering' package stores bookmark file to 'var/', which is
  ;; lost if performing a "fresh" Emacs install. To persist, set
  ;; 'bookmark-default-file' to store in personal 'scratch/' directory.
  ;; (bookmark-default-file "~/scratch/jon/.bookmarks.el")
  (bookmark-save-flag t "Save bookmarks when Emacs killed")
  (bookmark-fringe-mark t "Non-nil to show icon in fringe"))

(set-default-coding-systems 'utf-8)

;; Show column number in the modeline
(column-number-mode t)

;; Display line numbers in prog-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Open image files + automatically update buffer if image changes
(auto-image-file-mode 1)
(add-hook 'image-mode-hook 'auto-revert-mode)

;; Add configuration modules to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load Pertinent Modules
(require 'my-visuals)
(require 'my-org)
(require 'my-completion)
(require 'my-bindings)
(require 'my-vc)
(require 'my-writing)
(require 'my-productivity)
(require 'my-dev)
(require 'my-functions)

;;; init.el ends here
