;; -*- mode: Lisp -*-

;;; .emacs --- My emacs configuration
;;; Commentary:
;;; Code:

;; ---------------------------------------------------------------------------
;; Packages

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(eval-when-compile
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; Allow installation of system packages from emacs and use-package
(use-package system-packages
  :ensure t
  :config
  (setq system-packages-use-sudo t))
;  (setq system-packages-package-manager 'brew))

(use-package use-package-ensure-system-package
  :ensure t)


(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


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
(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)


(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-nord t))

(cond
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Fira Code")
  (set-face-attribute 'default nil :height 120)
  (set-face-attribute 'default nil :weight 'ultra-light)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil))
 ((not (eq system-type 'darwin))
  (set-face-attribute 'default nil :font "Ubuntu Mono")
  (set-face-attribute 'default nil :height 100)))


;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
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


;; Folding
(use-package origami
  :ensure t
  :commands (origami-open-node origami-close-node origami-open-node-recursively origami-close-node-recursively)
  :bind
  ("C-+" . origami-open-node-recursively)
  ;("C-_" . origami-close-node-recursively) ;; C-_ bound to undo
  ("C-=" . origami-open-node)
  ("C--" . origami-close-node))



;; Window switching
(use-package switch-window
  :ensure t
  :commands (switch-window switch-window-then-swap-buffer)
  :bind
  ("M-P" . switch-window)
  ("M-C-P" . switch-window-then-swap-buffer))

;; Visual replacing
(use-package visual-regexp
  :ensure t)

;; Git
(use-package magit
  :ensure t
  :commands (magit-status)
  :bind
  ("C-x g" . magit-status)
  :config
  (setq magit-save-repository-buffers nil)

  (setq magit-diff-refine-hunk 'all))

(use-package magit-lfs
  :ensure
  :after magit)

(use-package magit-gerrit
  :ensure
  :after magit)


(use-package multiple-cursors
  :ensure t
  :commands (mc/edit-lines mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :bind
  ("C-c C->" . mc/edit-lines)
  ("C->"     . mc/mark-next-like-this)
  ("C-<"     . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package god-mode
  :ensure t
  :commands (god-local-mode)
  :bind
  ("<escape>" . god-local-mode)
  ;; Allow "xn" instead of "x n"
  ("C-x C-1" . delete-other-windows)
  ("C-x C-2" . split-window-below)
  ("C-x C-3" . split-window-right)
  ("C-x C-0" . delete-window)
  (:map isearch-mode-map
        ("<escape>" . god-mode-isearch-activate))
  (:map god-mode-isearch-map
        ("<escape>" . god-mode-isearch-disable))
  (:map god-local-mode-map
        ("." . repeat))
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)

  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'box
                        'bar)))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)

  (add-to-list 'god-exempt-major-modes 'dired-mode)
  (add-to-list 'god-exempt-major-modes 'magit-mode))

(use-package projectile
  :ensure t
  :bind (;("C-x s" . projectile-switch-open-project)
	 ("C-x p" . projectile-switch-project))
  :config
  (projectile-mode)
  (setq projectile-enable-caching t))


(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package company
  :ensure t
  :defer t
  :bind
  ("<C-tab>" . company-complete)
  :config
  (progn
    (global-company-mode)
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

(use-package company-quickhelp          ; Documentation popups for Company
  :after (company)
  :ensure t
  :defer t
  :config (add-hook 'global-company-mode-hook #'company-quickhelp-mode))


(use-package lsp-mode
  :ensure t
  :requires flycheck)

(use-package lsp-ui
  :ensure t
  :after (flycheck lsp-mode))


;; Languages

;; LaTeX
(use-package tex
  :ensure auctex
  :bind (:map LaTeX-mode-map
	   ("C-c ]" . org-ref-helm-insert-cite-link)
	   ("C-c C-o" . org-ref-latex-click))
  :config
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t))
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (progn
	        (paren-toggle-matching-quoted-paren 1)
	        (paren-toggle-matching-paired-delimiter 1)))))

(use-package intero
  :after (flycheck company)
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  (add-hook 'haskell-mode-hook 'company-mode)

  (setq haskell-stylish-on-save t)

  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

;; purescript
(use-package purescript-mode :ensure t)
(use-package psc-ide
  :ensure t
  :after (flycheck company)
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation))))
;; markdown
(use-package markdown-mode
  :ensure t)

;; R
(use-package ess
  :ensure t
  :commands R)

;; python
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))


(use-package lsp-python
  :ensure t
  :requires lsp-mode
  :hook
  (python-mode-hook . lsp-python-enable)
  (python-mode-hook . lsp-ui-mode))


;; C/C++
;(load-file "/Users/herwig/.nix-profile/bin/../share/emacs/site-lisp/rtags/rtags.el")
;(use-package rtags
;  :ensure t
;  :ensure-system-package rtags
;  )

(use-package flycheck-rtags
  :ensure t
  :after (flycheck rtags))

;; nix
(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode))

;; yaml
(use-package yaml-mode
  :ensure t
  :commands (yaml-mode)
  :mode ("\\.yaml\\'" . yaml-mode))

;; org
(use-package org
  :ensure t)

(use-package org-ref
  :ensure t
  :after org
  :config
  (require 'org-ref-latex))


;; Haskell
(use-package company-ghc
  :after (company ghc)
  :config
  (push 'company-ghc company-backends))


;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode visual-regexp purescript-mode purescript psc-ide intero flycheck-rtags rtags nix-mode projectile lsp-python lsp-ui lsp-mode org-ref org-ref-latex ess auctex god-mode-isearch god-mode multiple-cursors company-quickhelp magit doom-themes auto-package-update use-package-ensure-system-package system-packages exec-path-from-shell use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'emacs)
;;; emacs ends here
