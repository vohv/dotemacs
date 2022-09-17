;;; -*- lexical-binding: t -*-
(use-package magit
  :commands (magit-status +magit-submodule-remove +magit-submodule-add))

(defun magit-submodule-remove+ ()
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

(provide 'init-git)
