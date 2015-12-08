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

;; haskell-mode
(ensure-package-installed 'haskell-mode)

;; Add haskell executables installed by stack to path
(let ((stack-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat stack-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path stack-path))

;; use hasktags
(custom-set-variables '(haskell-tags-on-save t))


(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f5] 'haskell-stylish-buffer))


;; enable keybindings for interactive mode
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)

;; haskell-mode bindings
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;; cabal-mode bindings
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

(setq haskell-process-type 'stack-ghci)

;; print output of do-type and do-info in separate buffer
(setq haskell-process-use-presentation-mode t)


;; ghc-mod
(ensure-package-installed 'company-ghc)

;; make sure ghc-mod is started when haskell-mode is
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(global-company-mode t)
(add-to-list 'company-backends 'company-ghc)

(defun company-ghc-toggle-show-info ()
  (interactive)
  (if company-ghc-show-info
      (setq company-ghc-show-info nil)
    (setq company-ghc-show-info t)))


(setq company-ghc-show-info t)

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
