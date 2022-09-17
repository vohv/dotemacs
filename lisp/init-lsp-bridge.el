;;; -*- lexical-binding: t -*-
(use-package lsp-bridge
  :load-path "site-lisp/lsp-bridge"
  :commands global-lsp-bridge-mode
  :bind (("M-g d" . lsp-bridge-jump)
         ("M-g b" . lsp-bridge-jump-back)
         ("M-g r" . lsp-bridge-find-references))
  :init
  (use-package posframe)
  (use-package markdown-mode)
  (use-package yasnippet
    :init
    (yas-global-mode))

  (setq acm-enable-english-helper nil)
  (global-lsp-bridge-mode)
  )

(use-package dumb-jump)

(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun lsp-bridge-jump-back ()
  (interactive)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-return-from-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

(provide 'init-lsp-bridge)
