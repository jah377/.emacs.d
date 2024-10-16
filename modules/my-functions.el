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

(defun jh/parse-package-defcustoms ()
  "Parse all `defcustom` variables and their docstrings in the
current buffer, and organize them in an `org-mode` description
list. The variables are sorted alphabetically, and only the first
sentence of each docstring is included."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Parsed defcustom Variables*"))
        ;; Match 'defcustom' followed by the variable name
        (regex "^(defcustom\\s-+\\(\\_<[^[:space:]]+\\_>\\)")
        ;; To store matched defcustom variables and docstrings
        (result '()))
    ;; Search the current buffer for all occurrences of defcustom
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (let ((var-name (match-string 1))
              docstring)
          ;; Move point to skip over the default value
          (forward-sexp)
          ;; Now search for the docstring (next string literal)
          (when (re-search-forward "\"\\([^\"]*\\)\"" nil t)
            (setq docstring (match-string 1)))
          ;; Extract only the first sentence of the docstring
          (when docstring
            (setq docstring (car (split-string docstring "\\.\\s-" t))))
          ;; Collect the variable name and docstring
          (push (list var-name docstring) result))))

    ;; Sort the collected variables alphabetically by name
    (setq result (sort result (lambda (a b) (string< (car a) (car b)))))

    ;; Insert the collected information into the temporary buffer with
    ;; org-mode formatting
    (with-current-buffer output-buffer
      (erase-buffer)
      (org-mode)  ;; Set buffer to org-mode
      (insert "#+TITLE: Parsed defcustom Variables\n\n")
      (dolist (item result)
        (let ((var-name (nth 0 item))
              (docstring (nth 1 item)))
          (insert (format "+ %s :: %s.\n\n" var-name docstring))))
      (goto-char (point-min)))
    (display-buffer output-buffer)
    (mark-whole-buffer)
    (fill-paragraph)))

(provide 'my-functions)

;;; my-functions.el ends here
