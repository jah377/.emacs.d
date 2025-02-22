;; jupyter-kernel-extras.el --- Extras to Manage Jupyter Kernels -*- lexical-binding: t; -*-
;;
;; Keywords: tools, jupyter, python
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides utilities for managing Jupyter kernels and virtual
;; environments from within Emacs. It includes the following interactive
;; functions:
;;
;; - 'jupyter-kernel-create' :: To create user-selected kernel
;;
;; - 'jupyter-kernel-delete' :: To delete user-selected kernel
;;
;; - 'jupyter-kernel-refresh' :: To refresh list of available kernels
;;
;; The package also includes a number of customizable settings:
;;
;; - 'jupyter-kernel-venv-dir' :: Virtual Environment directory
;;
;; - 'jupyter-kernel-python-bin-dirs' :: Python directory(s)
;;
;; - 'jupyter-kernel-default-python' :: Default python version

;;; Code:

(require 'jupyter)


;;; %% Custom Settings

(defcustom jupyter-kernel-venv-dir
  (expand-file-name "~/.virtualenvs" "")
  "Directory containing all 'venvs'."
  :type 'directory)

(defcustom jupyter-kernel-python-bin-dirs
  (list "/usr/bin/"
        "/usr/local/bin"
        (expand-file-name "~/.local/bin/"))
  "List of directories to search for available python versions."
  :type '(repeat directory))

(defcustom jupyter-kernel-available-pythons
  (seq-sort
   #'string-lessp
   (seq-filter (apply-partially #'string-match-p "python3")
               (mapcan (lambda (dir)
                         (when (file-exists-p dir)
                           (directory-files dir nil "^python[0-9]\\.[0-9]+$")))
                       jupyter-kernel-python-bin-dirs)))
  "A list of available python interpreter versions or an expression.
The expression returns a list of python interpreters.")

(defcustom jupyter-kernel-default-python
  (nth 1 (string-split (shell-command-to-string "python --version")))
  "The default python version on the system.
This should be a string containing the python version number e.g. \"3.11.4\".")

;;; %% Accessory Functions

(defun jupyter-kernel-python-major-version (version-string)
  "Get major version from the version string.
VERSION-STRING should be a python version number provided as
string, e.g. \"3.11.4\"."
  (let* ((version-parts (split-string version-string "\\."))
         (major-parts (butlast version-parts (- (length version-parts) 2))))
    (mapconcat 'identity major-parts ".")))

(defun jupyter-kernel--call-process (main &rest args)
  "Accessory function to execute command-line instructions."
  (apply 'call-process main nil t t args))

;;; %% Autoload Functions

;;;###autoload
(defun jupyter-kernel-refresh-kernels ()
  "Refresh list of available kernels."
  (interactive)
  (jupyter-available-kernelspecs t))

;;;###autoload
(defun jupyter-kernel-delete-kernel (kernel)
  "Delete jupyter KERNEL and related virtual environment."
  (interactive
   (list (completing-read
          "Select kernel: "
          (mapcar 'jupyter-kernelspec-name (jupyter-available-kernelspecs t)))))
  (call-process "jupyter-kernelspec" nil nil nil "remove" kernel "-y")
  (delete-directory (format "%s/%s" jupyter-kernel-venv-dir kernel) t)
  (message "Kernel deleted: %s" kernel)
  (jupyter-available-kernelspecs t))

;;;###autoload
(defun jupyter-kernel-create-kernel (&optional python-version venv-name)
  "Setup a jupyter development environment.
This command creates a virtual environment, installs `ipkernel'
into the environment and creates a kernel connected to the
environment.

The command uses PYTHON-VERSION, which should be an available
python version given provided as a string e.g. \"3.11\", for
creating the environment.

The argument VENV-NAME should be a string and will be used for
naming the kernel and the virtual environment."
  (interactive
   (list (nth 1 (string-split (completing-read "Select python version: "
                                               (reverse jupyter-kernel-available-pythons))
                              "python"))
         (read-string "Enter name for virtual environment: ")))

  ;; Setup python and venv variables
  (let* ((output-buffer "*setup-development-environment*")
         (python-version (or python-version jupyter-kernel-default-python))
         (major-version (jupyter-kernel-python-major-version python-version))
         (virtualenv-dir
          (when venv-name
            (expand-file-name
             (format "%s/%s/" jupyter-kernel-venv-dir venv-name))))
         (venv-python-command (concat virtualenv-dir "bin/python")))

    ;; Remove 'output-buffer' if exists; pop to new one
    (when (get-buffer output-buffer)
      (kill-buffer output-buffer))
    (pop-to-buffer output-buffer)

    ;; Check if 'jupyter-kernel-venv-dir' exists; create if not
    (unless (file-directory-p jupyter-kernel-venv-dir)
      (insert (propertize
               (format "%s does not exist, creating it.\n"
                       jupyter-kernel-venv-dir)
               'face 'warning))
      (make-directory jupyter-kernel-venv-dir t))

    ;; Check if 'jupyter-kernel-python-bin-dirs' in path; add if not
    (dolist (bin jupyter-kernel-python-bin-dirs)
      (unless (member bin exec-path)
        (insert (propertize (format "%s not in path, adding it.\n" bin)
                            'face 'warning))
        (add-to-list 'exec-path bin)))

    ;; Confirm 'jupyter' installed
    (if (executable-find "jupyter")
        (insert (propertize "Jupyter has already been installed\n"
                            'face 'success))
      (insert (propertize "Reloading paths\n" 'face 'success))
      (call-process nil t t "source" ".profile")
      (unless (executable-find "jupyter")
        (error "Jupyter not installed in the system!")))

    ;; Confirm 'uv' installed
    (unless (executable-find "uv")
      (error "Uv not installed in the system!"))

    ;; Confirm virtualenvwrapper installed
    (let* ((py-package "virtualenvwrapper")
           (my-command (format "uv pip list | grep %s" py-package)))
      (unless (string-match-p py-package (shell-command-to-string my-command))
        (error (format "%s not installed!" py-package))))

    ;; Create venv if it does not exist
    (cond ((and venv-name (not (file-exists-p virtualenv-dir)))
           (insert (propertize
                    (format "\nCreating virtual environment %s\n" venv-name)
                    'face 'success))
           (jupyter-kernel--call-process "uv" "venv" virtualenv-dir "-p" major-version))
          (t (insert
              (propertize
               (format "\nVirtual environment %s already exists\n" venv-name)
               'face 'success))))

    ;; Install ipykernel
    (insert (propertize "\nInstalling ipykernel to venv\n"
                        'face 'success))
    (jupyter-kernel--call-process "uv""pip" "install" "-p" virtualenv-dir "ipykernel" "-q")

    ;; Create the jupyter kernel
    (insert (propertize "\nConnecting venv to jupyter kernel\n"
                        'face 'success))
    (jupyter-kernel--call-process venv-python-command
                                  "-m" "ipykernel" "install" "--user" "--name" venv-name)
    (insert (propertize "\nFinished!\n" 'face 'success))

    ;; Refresh kernel list
    (jupyter-available-kernelspecs t)

    ;; Jump to vterm to install packages or kill splash page
    (if (not (yes-or-no-p "Open vterm and jump to kernel?"))
        (kill-buffer output-buffer)
      (vterm-mode)
      (vterm-send-string (format "workon %s" venv-name))
      (vterm-send-return))))

(provide 'jupyter-kernel-extras)
;;; jupyter-kernel-extras.el ends here
