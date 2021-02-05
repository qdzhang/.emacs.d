(setq inhibit-splash-screen 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(require 'package)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)

(set-face-attribute 'default nil :family "Sarasa Mono SC" :height 160)

(global-set-key (kbd "M-i") 'imenu)

(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(global-display-line-numbers-mode 1)

;; use-package settings
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; ==============================

(use-package helm
  :config (require 'helm-config))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("x" . meow-line)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-keyboard-quit)
   '("G" . meow-goto-line)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("m" . meow-join)
   '("M" . delete-indentation)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-replace-save)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("u" . undo)
   '("v" . meow-visit)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("<escape>" . meow-last-buffer)))

(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-setup)
  ;; 如果你需要在 NORMAL 下使用相对行号（基于 display-line-numbers-mode）
  (meow-setup-line-number)
  ;; 如果你需要自动的 mode-line 设置（如果需要自定义见下文对 `meow-indicator' 说明）
  (meow-setup-indicator))

(use-package company
  :init
  (setq company-idle-delay 0)
  :hook
  (after-init . global-company-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
