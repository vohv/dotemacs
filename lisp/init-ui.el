;;; -*- lexical-binding: t -*-

(use-package nano-modeline
  :hook (after-init . nano-modeline-mode)
  :config
  (setq nano-modeline-position 'bottom))

(provide 'init-ui)
