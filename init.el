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

(use-package company
  :init
  (setq company-idle-delay 0)
  :hook
  (after-init . global-company-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
