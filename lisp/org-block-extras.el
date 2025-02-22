;; org-block-extras.el --- Extras for org-block manipulation -*- lexical-binding: t; -*-
;;
;; Keywords: tools, org
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides utilities for "cell creation" and execution in
;; 'org-mode'. Added functionalities similar to jupyter-notebooks, and include:
;;
;; - 'obe-insert-prev-block' :: Insert org-block matching current/previous
;; - 'obe-insert-next-block' :: Insert org-block matching current/next
;; - 'obe-kill-block' :: Delete org-block at POINT + results
;; - 'obe-copy-block' :: Copy org-block at POINT + results
;; - 'obe-execute-above' :: Execute org-blocks above POINT
;; - 'obe-execute-below' :: Execute org-blocks below POINT
;; - 'obe-remove-empty-results' :: Remove empty results in org-file

;;; Code:

(require 'org)

;; Calling 'org-next-block' should move point inside block, not to block header
(advice-add 'org-next-block :after (lambda (&rest r) (forward-line)))
(advice-add 'org-previous-block :before (lambda (&rest r) (forward-line -1)))

;;; %% Accessory Functions

(defun org-on-any-block-p ()
  "Check if point is in any type of org block."
  (let ((line (thing-at-point 'line))
        (following-line (save-excursion
                          (forward-line)
                          (thing-at-point 'line))))
    (or (org-in-block-p
         '("src" "export" "center" "comment" "example" "quote" "verse"))
        (and (string-prefix-p "#+name:" line t)
             (string-prefix-p "#+begin" following-line t)))))

(defun org-block-type (org-block)
  "Return block type of ORG-BLOCK as string, ie. quote-block -> quote."
  (car (split-string (symbol-name (org-element-type org-block)) "-")))

(defun org-element-is-block-p (element)
  "Check if ELEMENT is org block."
  (string-equal "block" (cadr (split-string (symbol-name (org-element-type
                                                          element)) "-"))))

(defun org-block-with-results ()
  "Get element of block at point, including result markers (if applicable)."
  (interactive)
  (let* ((block (org-element-at-point))
         (end (org-element-property :end block)))
    ;; Blocks can contain other org elements - if current element is not an org
    ;; block, the previous block will be
    (when (not (org-element-is-block-p block))
      (save-excursion
        (org-previous-block 1)
        (setq block (org-element-at-point))
        (setq end (org-element-property :end block))))
    (save-excursion
      ;; END will be at beginning of next org element
      (goto-char end)
      ;; Results block exists
      (if (string-prefix-p "#+results" (thing-at-point 'line) t)
          (let ((result-end (org-babel-result-end)))
            (org-element-put-property block :result-beg end)
            (org-element-put-property block :result-end result-end)
            (org-element-put-property block :end-end result-end))
        ;; Need to skip backward
        (skip-chars-backward "\n")
        (forward-char 1)
        (org-element-put-property block :end-end (point))))
    block))


(defun org-insert-block (p type &optional lang header_args)
  "Insert org block of TYPE at P and move point into block.
Optionally takes src block language and additional header
arguments."
  (goto-char p)
  (let ((block-format type))
    (when lang
      (setq block-format (concat block-format (format " %s" lang))))
    (when header_args
      (setq block-format (concat block-format (format " %s" header_args))))
    (insert (format "#+begin_%s\n" block-format)))
  (insert (format "#+end_%s" type))
  (beginning-of-line)
  (open-line 1))

(defun org-babel-result-end ()
  "Return the point at the end of the current set of results."
  (cond ((looking-at-p "^[ \t]*$") (point)) ;no result
        ((looking-at-p (format "^[ \t]*%s[ \t]*$" org-link-bracket-re))
         (line-beginning-position 2))
        (t
         (let ((element (org-element-at-point)))
           (if (memq (org-element-type element)
                     ;; Possible results types.
                     '(drawer example-block export-block fixed-width item
                              plain-list special-block src-block table paragraph))
               (save-excursion
                 (goto-char (min (point-max) ;for narrowed buffers
                                 (org-element-property :end element)))
                 (skip-chars-backward " \r\t\n")
                 (line-beginning-position 2))
             (if (looking-at-p org-babel-result-regexp)
                 ;; If empty results line, end should be behind
                 (save-excursion
                   (forward-line 1)
                   (point))
               (point)))))))

;;; %% Autoload Functions

;;;###autoload
(defun obe-insert-prev-block ()
  "Insert org source block above:
\(a) above the current block (point needs to be inside a block)
uses same block type/header args as current block
\(b) at point, using the same block type/header args as the previous block"
  (interactive)
  (let ((line (thing-at-point 'line))
        (p (point))
        (type "src")
        block)
    (if (or (org-on-any-block-p) (string-prefix-p "#+end" line t))
        (progn
          (setq block (org-block-with-results))
          (setq type (org-block-type block))
          (goto-char (org-element-property :begin block))
          (setq p (point))
          ;; Need to open 2 extra lines to preserve whitespace
          (open-line 2))
      (org-babel-previous-src-block 1)
      (setq block (org-block-with-results)))
    (let ((lang (org-element-property :language block))
          (header_args (org-element-property :parameters block)))
      (org-insert-block p type lang header_args))))

;;;###autoload
(defun obe-insert-next-block ()
  "Insert org src block below:
\(a) below the current block (point needs to be inside a block)
    uses same block type/header args as current block
\(b) at point, using the same block type/header args as the next block"
  (interactive)
  (let ((line (thing-at-point 'line))
        (p (point))
        (type "src")
        block)
    ;; Use current src block if point in one
    (if (or (org-on-any-block-p) (string-prefix-p "#+end" line t))
        (progn
          (setq block (org-block-with-results))
          (setq type (org-block-type block))
          (goto-char (org-element-property :end-end block))
          ;; Need to open 2 extra lines to preserve whitespace
          (open-line 2)
          (forward-line 1)
          (setq p (point)))
      ;; Skip to and use next src block
      (org-babel-next-src-block 1)
      (setq block (org-block-with-results)))
    (let ((lang (org-element-property :language block))
          (header_args (org-element-property :parameters block)))
      (org-insert-block p type lang header_args))))

;;;###autoload
(defun obe-kill-block ()
  "Kill block at point."
  (interactive)
  (let ((block (org-block-with-results)))
    (unless (not (org-element-is-block-p block))
      (kill-region (org-element-property :begin
                                         block) (org-element-property :end-end block)))))

;;;###autoload
(defun obe-copy-block ()
  "Save block at point (or previous, if not in block) to kill ring.
Includes src block results, if there are any."
  (interactive)
  (let ((block (org-block-with-results)))
    (unless (not (org-element-is-block-p block))
      (kill-ring-save (org-element-property :begin
                                            block) (org-element-property :end-end block)))))

;;;###autoload
(defun obe-execute-above ()
  "Execute all src blocks at and above point."
  (interactive)
  (save-excursion
    (if (org-in-block-p '("src" "export" "center" "comment" "example" "quote" "verse"))
        (progn
          (re-search-forward "#\\+end")
          (forward-line 1)))
    (narrow-to-region 1 (point))
    (org-babel-execute-buffer)
    (widen)))

;;;###autoload
(defun obe-execute-below ()
  "Execute all src blocks at and below point."
  (interactive)
  (save-excursion
    (if (org-in-block-p '("src" "export" "center" "comment" "example" "quote" "verse"))
        (if (string-prefix-p "#+begin" (thing-at-point 'line) t)
            (forward-line -1)
          (org-previous-block 1)
          (forward-line -1)))
    (narrow-to-region (point) (point-max))
    (org-babel-execute-buffer)
    (widen)))

;;;###autoload
(defun obe-remove-empty-results ()
  "Remove all #+RESULTS: without content from buffer."
  (interactive)
  (save-excursion
    (org-babel-map-executables nil
      (let ((res (org-babel-where-is-src-block-result))
            (del))
        (when res
          (save-excursion
            (goto-char res)
            (forward-line)
            (when (char-equal (char-after (point)) 10)
              (setq del t)))
          (when del
            (org-babel-remove-result)))))))

(provide 'org-block-extras)
;;; ;; org-block-extras.el ends here
