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

;; normalization of files
(setq require-final-newline t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; ---------------------------------------------------------------------------
;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)

(column-number-mode t)

(show-paren-mode 1)
(size-indication-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(ensure-package-installed 'atom-one-dark-theme)
(load-theme 'atom-one-dark t)

;; ---------------------------------------------------------------------------
;; Magit
(ensure-package-installed 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

;; ---------------------------------------------------------------------------
;; Haskell
(ensure-package-installed 'haskell-mode)

;; ---------------------------------------------------------------------------
;; God-mode
(ensure-package-installed 'god-mode)

(require 'god-mode)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(global-set-key (kbd "<escape>") 'god-local-mode)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
(define-key god-local-mode-map (kbd ".") 'repeat)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(add-to-list 'god-exempt-major-modes 'dired-mode)
