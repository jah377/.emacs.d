;;; -*- lexical-binding: t -*-

;; Quickly re-evaluate init file
(defun jh/eval-init ()
  (interactive)
  (load-file user-init-file))

(defun jh/kill-buffer-name ()
  (interactive)
  (kill-new (buffer-name)))

(defun jh/kill-relative-buffer-path ()
  "Copy relative buffer path to kill ring.

Replace '/home/<username>' prefix with '~' if applicable."
  (interactive)
  (if-let* ((full-path (buffer-file-name))
            (abbrev-path (abbreviate-file-name full-path)))
      (progn
        (kill-new abbrev-path)
        (message "Buffer path copied to kill ring: %s" full-path))
    (message "Buffer is not visiting a file.")))

(defun jh/kill-buffer-orgmode-file-link ()
  "Build [[:file file-path][file-name]] org-link from current
buffer.

The function 'buffer-file-name' returns the absolute path of the
buffer, which breaks should other users open the link. Instead,
the relative path is referenced using the 'abbreviate-file-name'
function."

  (interactive)
  (if-let ((absolute-path (buffer-file-name)))
      (kill-new (message "[[file:%s][%s]]"
                         (abbreviate-file-name absolute-path)
                         (buffer-name)))
    (message "Buffer is not a file")))

(defun jh/kill-buffers-except (buffer-name)
  "Kill all buffers except for BUFFER-NAME."
  (dolist (buffer (buffer-list))
    (unless (or (string-equal (buffer-name buffer) buffer-name)
                (string-equal (buffer-name buffer) (concat " " buffer-name)))
      (kill-buffer buffer))))

(defun jh/kill-all-buffers-except-scratch ()
  "Kill all buffers except for *scratch*."
  (interactive)
  (jh/kill-buffers-except "*scratch*"))

(defun jh/find-config-file ()
  "Edit '~/.emacs.d/README.org', in other window."
  (interactive)
  (find-file-other-window "~/.emacs.d/README.org"))

(defun jh/jump-to-minibuffer ()
  "Switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(provide 'my-functions)

;;; my-functions.el ends here
