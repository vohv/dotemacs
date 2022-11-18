;;; -*- lexical-binding: t -*-

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 128000000))

(provide 'init-undo)
