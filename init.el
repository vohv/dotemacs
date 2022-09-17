;;; -*- lexical-binding: t -*-

(defvar +site-lisp-dir (locate-user-emacs-file "site-lisp"))
(add-to-list 'load-path +site-lisp-dir)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "theme"))
(setq custom-file (locate-user-emacs-file "custom.el"))

(let ((conf (locate-user-emacs-file "config.el")))
  (when (file-exists-p conf)
    (load-file conf)))

;;; load first
(require 'core)

(require 'init-modal)
(require 'init-completion)
(require 'init-cc)
(require 'init-git)
(require 'init-lsp-bridge)
(require 'init-color-rg)
(require 'init-blink-search)
