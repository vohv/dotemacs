;;; -*- lexical-binding: t -*-

(defvar +theme nil
  "A symbol representing the Emacs theme to load at startup.

Set to `nil' to load no theme at all. This variable is changed by
`load-theme'.")

(defvar +font nil
  "The default font to use.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string.

This affects the `default' and `fixed-pitch' faces.

Examples:
  (setq +font (font-spec :family \"Fira Mono\" :size 12))
  (setq +font \"Terminus (TTF):pixelsize=12:antialias=off\")
  (setq +font \"Fira Code-14\")")

(defvar +variable-pitch-font nil
  "The default font to use for variable-pitch text.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`+font' for examples.

An omitted font size means to inherit `+font''s size.")

(defvar +serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`+font' for examples.

An omitted font size means to inherit `+font''s size.")

(defvar +unicode-font nil
  "Fallback font for Unicode glyphs.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`+font' for examples.

The defaults on macOS and Linux are Apple Color Emoji and Symbola, respectively.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")

(defvar +emoji-fallback-font-families
  '("Apple Color Emoji"
    "Segoe UI Emoji"
    "Noto Color Emoji"
    "Noto Emoji")
  "A list of fallback font families to use for emojis.")

(defvar +symbol-fallback-font-families
  '("Segoe UI Symbol"
    "Apple Symbols")
  "A list of fallback font families for general symbol glyphs.")


;;
;;; General UX

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)


;;
;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; UX: GUIs are inconsistent across systems, desktop environments, and themes,
;;   and don't match the look of Emacs. They also impose inconsistent shortcut
;;   key paradigms. I'd rather Emacs be responsible for prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)


;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; DEPRECATED: Remove when we drop 27.x support
  (advice-add #'yes-or-no-p :override #'y-or-n-p))

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;
;;; Built-in packages

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)


(with-eval-after-load 'comint
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048)) ; double the default


(with-eval-after-load 'compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  ;; Handle ansi codes in compilation buffer
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))


(with-eval-after-load 'ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  (defvar +ediff-saved-wconf nil)
  ;; Restore window config after quitting ediff
  (defun +ediff-save-wconf-h ()
    (setq +ediff-saved-wconf (current-window-configuration)))
  (defun +ediff-restore-wconf-h ()
    (when (window-configuration-p +ediff-saved-wconf)
      (set-window-configuration +ediff-saved-wconf)))
  (add-hook 'ediff-before-setup-hook #'+ediff-save-wconf-h)
  (dolist (hook '(ediff-quit-hook ediff-suspend-hook))
    (add-hook hook #'+ediff-restore-wconf-h t)))


(defvar global-hl-line-modes
  '(prog-mode text-mode conf-mode special-mode org-agenda-mode dired-mode))

(setq winner-dont-bind-my-keys t)
(setq winner-boring-buffers
      '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
        "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
        "*esh command on file*"))
(add-hook 'after-init-hook #'winner-mode)

(setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
(add-hook 'after-init-hook #'show-paren-mode)


(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
             trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))


;; Hide the mode line in completion popups and MAN pages because they serve
;; little purpose there, and is better hidden.
(dolist (hook '(completion-list-mode-hook Man-mode-hook))
  (add-hook hook #'hide-mode-line-mode))


;;
;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))


;;
;;; Theme & font

(defun +make-font-specs (face font)
  (let* ((base-specs (cadr (assq 'user (get face 'theme-face))))
         (base-specs (or base-specs '((t nil))))
         (attrs '(:family :foundry :slant :weight :height :width))
         (new-specs nil))
    (dolist (spec base-specs)
      ;; Each SPEC has the form (DISPLAY ATTRIBUTE-PLIST)
      (let ((display (car spec))
            (plist   (copy-tree (nth 1 spec))))
        ;; Alter only DISPLAY conditions matching this frame.
        (when (or (memq display '(t default))
                  (face-spec-set-match-display display this-frame))
          (dolist (attr attrs)
            (setq plist (plist-put plist attr (face-attribute face attr)))))
        (push (list display plist) new-specs)))
    (nreverse new-specs)))

(defun +init-fonts-h ()
  (dolist (map `((default . ,+font)
                 (fixed-pitch . ,+font)
                 (fixed-pitch-serif . ,+serif-font)
                 (variable-pitch . ,+variable-pitch-font)))
    (when-let* ((face (car map))
                (font (cdr map)))
      (dolist (frame (frame-list))
        (when (display-multi-font-p frame)
          (set-face-attribute face frame
                              :width 'normal :weight 'normal
                              :slant 'normal :font font)))
      (let ((new-specs (+make-font-specs face font)))
        (custom-push-theme 'theme-face face 'user 'set new-specs)
        (put face 'face-modified nil))))
  (when (fboundp 'set-fontset-font)
    (let ((fn (lambda (font) (member font (font-family-list)))))
      (when-let (font (cl-find-if fn +symbol-fallback-font-families))
        (set-fontset-font t 'symbol font))
      (when-let (font (cl-find-if fn +emoji-fallback-font-families))
        (set-fontset-font t 'unicode font))
      (when +unicode-font
        (set-fontset-font t 'unicode +unicode-font)))))

(defun +init-theme-h (&rest _)
  "Load the theme specified by `+theme' in FRAME."
  (when +theme
    (load-theme +theme t)))

(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'+init-fonts-h -100)
  (add-hook hook #'+init-theme-h -90))


(provide 'core-ui)
