;;; -*- lexical-binding: t -*-

;;; Global constants
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

(unless (daemonp)
  (unless noninteractive
    (setq frame-inhibit-implied-resize t)
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (advice-add #'display-startup-screen :override #'ignore)
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)))

(require 'core-package)
(require 'core-ui)
(require 'core-start)
(require 'core-editor)

(provide 'core)
