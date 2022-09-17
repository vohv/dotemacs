;;; -*- lexical-binding: t -*-

(use-package blink-search
  :load-path "site-lisp/blink-search"
  :commands blink-search
  :bind ("M-s" . blink-search)
  :config
  (with-eval-after-load 'meow
    (add-to-list 'meow-mode-state-list '(blink-search-mode . insert)))
  (setq blink-search-enable-posframe t
        blink-search-common-directory '(("EMACS" "~/.emacs.d/lisp"))) )



(provide 'init-blink-search)
