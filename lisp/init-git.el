;;; -*- lexical-binding: t -*-
(use-package magit
  :commands (magit-status +magit-submodule-remove +magit-submodule-add))

(defun +magit-submodule-remove ()
  (interactive)
  (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil))


(defun +magit-submodule-add (url)
  (interactive "sURL: ")
  (let* ((module-name (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                           (match-string 1 url)))
         (parent-dir (cadr (split-string (file-name-as-directory +site-lisp-dir) user-emacs-directory))))
    (magit-submodule-add
     url
     (concat parent-dir module-name)
     module-name)))

(use-package diff-hl
  :hook (find-file . diff-hl-mode)
  :hook (vc-dir-mode . diff-hl-dir-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :config
  (setq vc-git-diff-switches '("--histogram"))
  (setq diff-hl-flydiff-delay 0.5)
  (setq diff-hl-show-staged-changes nil)
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (with-eval-after-load 'meow
    (defun +diff-hl-show-hunk-a () (meow--switch-state 'motion))
    (defun +diff-hl-inline-popup-hide-a () (meow--switch-state 'normal))
    (advice-add 'diff-hl-show-hunk :after #'+diff-hl-show-hunk-a)
    (advice-add 'diff-hl-inline-popup-hide :after #'+diff-hl-inline-popup-hide-a)))

(provide 'init-git)
