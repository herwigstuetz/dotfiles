;; -*- mode: Lisp -*-

;; ---------------------------------------------------------------------------
;; Packages

;; From http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))


;; ---------------------------------------------------------------------------
;; General

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ---------------------------------------------------------------------------
;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)

(column-number-mode t)

(ensure-package-installed 'atom-one-dark-theme)
(load-theme 'atom-one-dark t)

;; ---------------------------------------------------------------------------
;; Magit
(ensure-package-installed 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

;; ---------------------------------------------------------------------------
;; Haskell
(ensure-package-installed 'haskell-mode)
