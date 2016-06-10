;; -*- mode: Lisp -*-

;; ---------------------------------------------------------------------------
;; Packages

; From http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
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

; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))


;; ---------------------------------------------------------------------------
;; General

; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; normalization of files
(setq require-final-newline t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-auto-revert-mode t)

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

(set-face-attribute 'default nil :height 100)

;; Sticky Windows
(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

; Press [pause] key in each window you want to "freeze"
(global-set-key [pause] 'toggle-window-dedicated)

; Rainbow mode
(ensure-package-installed 'rainbow-mode)

; Speedbar
(ensure-package-installed 'sr-speedbar)

(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)

(speedbar-add-supported-extension ".hs")

;; ---------------------------------------------------------------------------
;; Magit
(ensure-package-installed 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

;; ---------------------------------------------------------------------------
;; Haskell

; haskell-mode
(ensure-package-installed 'haskell-mode)

; Add haskell executables installed by stack to path
(if (eq system-type 'darwin)
    ;; Add stack to path
    (let ((stack-path (expand-file-name "/usr/local/bin")))
      (setenv "PATH" (concat stack-path path-separator (getenv "PATH")))
      (add-to-list 'exec-path stack-path)))

(let ((stack-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat stack-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path stack-path))

(setq haskell-tags-on-save t)

(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f5] 'haskell-stylish-buffer))


; enable keybindings for interactive mode
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)

(defun haskell-insert-undefined ()
  (interactive)
  (insert "undefined"))

(define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)


; haskell-mode bindings
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

; cabal-mode bindings
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

(setq haskell-process-type 'stack-ghci)

; print output of do-type and do-info in separate buffer
(setq haskell-process-use-presentation-mode t)


; ghc-mod
(ensure-package-installed 'company-ghc)

; make sure ghc-mod is started when haskell-mode is
(autoload 'ghc-init "stack ghc" nil t)
(autoload 'ghc-debug "stack ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(global-company-mode t)
(add-to-list 'company-backends 'company-ghc)
(global-set-key (kbd "C-x TAB") 'company-complete)

(defun company-ghc-toggle-show-info ()
  (interactive)
  (if company-ghc-show-info
      (setq company-ghc-show-info nil)
    (setq company-ghc-show-info t)))

; might be problematic if buffer contains errors
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
(add-to-list 'god-exempt-major-modes 'magit-mode)


;; ---------------------------------------------------------------------------
;; multiple-cursors
(ensure-package-installed 'multiple-cursors)

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; ---------------------------------------------------------------------------
;; LaTeX
(ensure-package-installed 'auctex)


;; ---------------------------------------------------------------------------
;; R
(ensure-package-installed 'ess)


;; Org-mode

; Must have org-mode loaded before we can configure org-babel
(require 'org-install)

; Some initial langauges we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ))

(setq org-confirm-babel-evaluate nil)

; Automatically set task to DONE if all children are DONE.
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-highlight-latex-and-related '(latex script entities))

(setq org-latex-listings t)
(setq org-latex-listings-langs (quote ((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp") (c "C") (cc "C++") (fortran "fortran") (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby") (html "HTML") (xml "XML") (tex "TeX") (latex "TeX") (shell-script "bash") (gnuplot "Gnuplot") (ocaml "Caml") (caml "Caml") (sql "SQL") (sqlite "sql") (R-mode "R"))))

;; ---------------------------------------------------------------------------
;; Customization

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit god-mode company-ghc atom-one-dark-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
