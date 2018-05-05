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
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))


;; ---------------------------------------------------------------------------
;; MacOS specific

(when (memq window-system '(mac ns))
  (ensure-package-installed 'exec-path-from-shell)
  (exec-path-from-shell-initialize))


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
; Do not disable the menu bar on MacOS since there it's on the top anyways.
(if (eq system-type 'darwin)
    (menu-bar-mode 1)
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)

(column-number-mode t)

(show-paren-mode 1)
(size-indication-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(ensure-package-installed 'atom-one-dark-theme)
(load-theme 'atom-one-dark t)
;(load-theme 'tango-dark t)

(cond
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Fira Code")
;  (set-face-attribute 'default nil :font "Andale Mono")
  (set-face-attribute 'default nil :height 140)
  (set-face-attribute 'default nil :weight 'ultra-light))
 ((not (eq system-type 'darwin))
  (set-face-attribute 'default nil :font "Ubuntu Mono")
  (set-face-attribute 'default nil :height 100)))

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
(ensure-package-installed 'speedbar)
(ensure-package-installed 'sr-speedbar)

(require 'speedbar)
(require 'sr-speedbar)

(setq sr-speedbar-right-side nil)

(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)
(define-key speedbar-mode-map (kbd "q") 'sr-speedbar-toggle)

; switch-windows
(ensure-package-installed 'switch-window)
(global-set-key (kbd "M-P") 'switch-window)
(global-set-key (kbd "M-C-p") 'switch-window-then-swap-buffer)


;; ---------------------------------------------------------------------------
;; Magit
(ensure-package-installed 'magit)
(ensure-package-installed 'magit-lfs)

(magit-wip-after-save-mode t)
(magit-wip-after-apply-mode t)

(setq magit-diff-refine-hunk 'all)

(global-set-key (kbd "C-x g") 'magit-status)

;; ---------------------------------------------------------------------------
;; Haskell

; haskell-mode
(ensure-package-installed 'haskell-mode)
(ensure-package-installed 'intero)

(add-hook 'haskell-mode-hook 'intero-mode)

(with-eval-after-load 'intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))


(setq haskell-stylish-on-save t)

(speedbar-add-supported-extension ".hs")

;; ---------------------------------------------------------------------------
;; yaml
(ensure-package-installed 'yaml-mode)

;; ---------------------------------------------------------------------------
;; Docker
(ensure-package-installed 'dockerfile-mode)

;; ---------------------------------------------------------------------------
;; nix-mode
(ensure-package-installed 'nix-mode)

;; ---------------------------------------------------------------------------
;; lsp-mode
(ensure-package-installed 'lsp-mode)

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
(ensure-package-installed 'mic-paren)

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	     (progn
	       (paren-toggle-matching-quoted-paren 1)
	       (paren-toggle-matching-paired-delimiter 1)

	       (local-set-key (kbd "C-c ]") 'org-ref-helm-insert-cite-link)
	       (local-set-key (kbd "C-c C-o") 'org-ref-latex-click)
	       (add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t)))))

;; ---------------------------------------------------------------------------
;; R
(ensure-package-installed 'ess)


;; ---------------------------------------------------------------------------
;; markdown
(ensure-package-installed 'markdown-mode)


;; ---------------------------------------------------------------------------
;; auto-complete
(ensure-package-installed 'company)

(global-company-mode t)

(global-set-key (kbd "C-c TAB") 'company-complete)


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

(speedbar-add-supported-extension ".org")

(ensure-package-installed 'org-ref)
(require 'org-ref)
(require 'org-ref-latex)

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
