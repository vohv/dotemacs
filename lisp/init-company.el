;;; -*- lexical-binding: t -*-

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-mode-map
         ([remap indent-for-tab-command] . company-indent-or-complete-common))
  :config
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-insertion-on-trigger t)
  (setq company-idle-delay nil)

  (setq company-global-modes
        '(not
          eshell-mode
          comint-mode
          gud-mode)))

(provide 'init-company)
