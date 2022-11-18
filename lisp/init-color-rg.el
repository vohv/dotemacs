;;; -*- lexical-binding: t -*-

(use-package color-rg
  :load-path "site-lisp/color-rg"
  :commands (color-rg-search-project color-rg-search-symbol-in-project)
  :config
  (setq color-rg-search-no-ignore-file nil))
(provide 'init-color-rg)
