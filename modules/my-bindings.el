;;; -*- lexical-binding: t -*-

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

;; Collection of useful keybindings
(use-package crux
  :bind (([remap move-beginning-of-line] . 'crux-move-beginning-of-line)
         ([remap kill-whole-line] . 'crux-kill-whole-line)
         ("M-o" . 'crux-switch-to-previous-buffer)
         ("C-<backspace>" . 'crux-kill-line-backwards)
         ("C-c 3" . 'crux-view-url)))

;; 'Find-File-At-Point' package adds additional functionality to
;; existing keybindings
(ffap-bindings)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-;") 'copy-comment-region)
(global-set-key (kbd "C-x O") 'jh/jump-to-minibuffer)

;; Kill current buffer instead of selecting it from minibuffer
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

  ;; BUFFERS
  "b"  '(:ignore t                        :which-key "buffer")
  "bn" '(jh/kill-buffer-name              :which-key "get-buffer-name")
  "bp" '(jh/kill-relative-buffer-path     :which-key "get-relative-path")
  "bl" '(jh/kill-buffer-orgmode-file-link :which-key "get-buffer-orgmode-link")
  "bi" '(crux-find-user-init-file         :which-key "jump-to-init")
  "bc" '(jh/find-config-file              :which-key "jump-to-config")
  "bf" '(ffap-other-window                :which-key "find-file-other-window")

  ;; KILL BUFFERS
  "k"  '(:ignore t                          :which-key "killing")
  "ka" '(jh/kill-all-buffers-except-scratch :which-key "kill-all-buffers")
  "ke" '(crux-kill-other-buffers            :which-key "kill-buffers-except-current"))

(provide 'my-bindings)

;;; my-bindings.el ends here
