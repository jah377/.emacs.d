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

;; MANDITORY; Emacs must be aware of available packages before installing
(package-initialize)

;; Ensures backwards compatability ('use-package' added in Emacs29)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setopt use-package-always-ensure t
        use-package-compute-statistics t
        use-package-verbose t)

;; Directory to be backed up in the cloud
(defconst my-persist-dir "/home/jon/.kb_persistent_emacs/")

(use-package no-littering
  :demand t
  :init
  (setopt no-littering-etc-directory (concat my-persist-dir "no-littering/"))
  (setopt no-littering-var-directory (concat my-persist-dir "no-littering/"))
  :config
  (setopt custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Garbage Collection Magic Hack
;; (use-package gcmh
;;   :hook
;;   (emacs-startup . gcmh-mode)
;;   (emacs-startup . (lambda ()
;;                      "Reset garbage collection parameters after startup"
;;                      (setopt gc-cons-threshold (* 32 1024 1024))))
;;   :custom
;;   ;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-start.el#L85
;;   (gcmh-idle-delay 'auto)
;;   (gcmh-auto-idle-delay-factor 10)
;;   (gcmh-high-cons-threhsold (* 16 1024 1024) "16mb"))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

(defun gopar/pulse-current-region (&rest _)
  "Pulse the current implicit or active region"
  (if mark-active
      (pulse-momentary-highlight-region (region-beginning) (region-end))
    (pulse-momentary-highlight-region (mark) (point))))

(advice-add #'kill-ring-save :before #'gopar/pulse-current-region)

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
