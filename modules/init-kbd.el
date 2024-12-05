;;; init-kbd.el --- Emacs configuration file  -*- lexical-binding: t; no-byte-compile: t -*-
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

(use-package which-key
  :config (which-key-mode)
  :custom
  (which-key-show-early-on-C-h t     "Trigger which-key manually")
  (which-key-idle-delay 0.5          "Delay before popup appears")
  (which-key-idle-second-delay 0.05  "Responsiveness after triggered")
  (which-key-popup-type 'minibuffer  "Where to show which-key")
  (which-key-max-display-columns nil "N-cols determined from monotor")
  (which-key-separator " → "         "ex: C-x DEL backward-kill-sentence")
  (which-key-add-column-padding 1    "Padding between columns of keys")
  (which-key-show-remaining-keys t   "Show count of keys in modeline"))

(use-package crux
  :commands (crux-move-beginning-of-line
             crux-kill-whole-line
             crux-switch-to-previous-buffer
             crux-kill-line-backwards)
  :bind (("C-a" . 'crux-move-beginning-of-line)
         ([remap kill-whole-line] . 'crux-kill-whole-line)
         ("M-o" . 'crux-switch-to-previous-buffer)
         ("C-<backspace>" . 'crux-kill-line-backwards)
         ("C-c 3" . 'crux-view-url)))

;; 'Find-File-At-Point' package adds additional functionality to
;; existing keybindings
(ffap-bindings)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x O") 'my/jump-to-minibuffer)
(global-set-key (kbd "C-x M-k") 'kill-current-buffer)

(use-package general
  :custom
  (general-describe-priority-keymaps nil)
  (general-describe-keymap-sort-function 'general-sort-by-car)
  (general-describe-keybinding-sort-function 'general-sort-by-car)

  :config
  ;; Good to unset before assigning to 'my-leader-def'
  (global-unset-key (kbd "M-m"))

  (general-create-definer my-leader-def
    :keymaps 'global
    :prefix "M-m"))

;; :ignore t to define sub-section headers
(my-leader-def

 "i" '(my/eval-init :which-key "eval-init")

 ;; BUFFERS
 "b"  '(:ignore t                        :which-key "buffer")
 "bn" '(my/kill-buffer-name              :which-key "copy-buff-name")
 "bp" '(my/kill-relative-buffer-path     :which-key "copy-buff-path")
 "bl" '(my/kill-buffer-orgmode-file-link :which-key "create-buff-orgmode-link")
 "bi" '(crux-find-user-init-file         :which-key "jump-to-init")
 "bc" '(my/find-config-file              :which-key "jump-to-config")
 "bm" '(my/jump-to-minibuffer            :which-key "jump-to-minibuff")
 "bf" '(ffap-other-window                :which-key "find-file-other-window")

 ;; KILL BUFFERS
 "k"  '(:ignore t                          :which-key "killing")
 "ka" '(my/kill-all-buffers-except-scratch :which-key "kill-buffers-except-scratch")
 "ke" '(crux-kill-other-buffers            :which-key "crux-kill-other-buffers")
 "km" '(my/magit-kill-all-buffers          :which-key "kill-magit-buffs")
 "kh" '(my/helpfull-kill-all-buffers       :which-key "kill-helpful-buffs")
 "kd" '(my/dired-kill-all-buffers          :which-key "kill-dired-buffs"))

(provide 'init-kbd)
;;; init-kbd.el ends here
