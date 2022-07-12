;;; -*- lexical-binding: t -*-

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(unless (fboundp 'use-package)
  (require 'package)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("e" . "C-x C-e")
   '("s" . split-window-right)
   '("-" . split-window-below)
   '("w" . other-window)
   '("i" . imenu)
   '("b" . switch-to-buffer)
   '("o" . delete-other-windows)
   '("k" . kill-current-buffer)
   '("u" . meow-universal-argument)
   '("v" . "C-x g")
   '("p" . "C-x p")
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))


(defun +meow-config ()
  (meow-setup)
  (meow-global-mode t)
  (meow-setup-indicator))

(use-package meow
  :ensure t
  :config
  (+meow-config))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package company
  :ensure t
  :defer t
  :bind (:map company-active-map
              ("{" . company-select-previous)
              ("}" . company-select-next))
  :hook ((prog-mode conf-mode) . company-mode))

(use-package smartparens
  :ensure t
  :defer t
  :bind (:map smartparens-mode-map
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("M-r" . sp-raise-sexp)
              ("M-s" . sp-splice-sexp))
  :hook ((prog-mode conf-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package magit
  :ensure t
  :defer t
  :commands magit
  :bind (("C-x g" . magit)))

(use-package no-littering :ensure t)

(use-package rg
  :ensure t
  :defer t
  :config
  (with-eval-after-load "project"
    (define-key project-prefix-map (kbd "g") 'rg-dwim-project-dir)))

(use-package dumb-jump
  :ensure t
  :defer t)

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

;; (set-frame-font "DejaVu Sans Mono-12")
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(setq-default inhibit-startup-screen t
              indent-tabs-mode nil
              line-spacing 3)

(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "C-x C-p"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "<down>") 'scroll-up-line)
(global-set-key (kbd "<up>") 'scroll-down-line)


(defun +jump-go ()
  (interactive)
  (dumb-jump-go))

(defun +jump-back ()
  (interactive)
  (dumb-jump-back))

(defvar +code-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") '+jump-go)
    (define-key map (kbd "b") '+jump-back)
    map))

(defalias '+code-keymap +code-keymap)
(global-set-key (kbd "C-c j") '+code-keymap)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)
