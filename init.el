(setq inhibit-splash-screen 1)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(require 'package)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(set-face-attribute 'default nil :family "Sarasa Mono SC" :height 160)

(global-set-key (kbd "M-i") 'imenu)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

;; highlight current line
(global-hl-line-mode 1)

;; Line number
(column-number-mode)
(global-display-line-numbers-mode 1)
;; Disable line numbers for some modes
(defun my/disable-line-numbers (&optional dummy)
    (display-line-numbers-mode -1))
(add-hook 'shell-mode-hook 'my/disable-line-numbers)
(add-hook 'term-mode-hook 'my/disable-line-numbers)

;; Use-package settings
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; ==============================

(use-package acme-theme
  :config
  (load-theme 'acme t))

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-valley-light t))

(use-package ivy
  :diminish
  :init (ivy-mode 1)
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)))

;; (use-package doom-modeline
;;   :init
;;   (doom-modeline-mode 1))

;; (use-package mood-line
;;   :init (mood-line-mode 1))

(use-package company
  :init
  (setq company-idle-delay 0)
  :hook
  (after-init . global-company-mode))

(use-package yasnippet
  :defer 1
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :diminish
  :config
  (evil-commentary-mode))

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
(global-undo-fu-session-mode)

(use-package rime
  :config
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-space-after-cc-p
          rime-predicate-prog-in-code-p))
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  :custom
  (default-input-method "rime"))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'lispy-mode-hook #'lispyville-mode)
;; (with-eval-after-load 'lispyville
;;   (lispyville-set-key-theme
;;    '(operators
;;      c-w
;;      (escape insert)
;;      (additional-movement normal visual motion))))

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config (progn (show-smartparens-global-mode t)))
(add-hook 'prog-mode-hook #'smartparens-strict-mode)
(add-hook 'smartparens-strict-mode-hook #'evil-cleverparens-mode)
