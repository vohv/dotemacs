;;; -*- lexical-binding: t -*-

(use-package clang-format+
  :bind ("M-g =" . clang-format))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(provide 'init-cc)
