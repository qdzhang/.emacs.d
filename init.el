(setq inhibit-splash-screen 1)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(require 'package)

(setq package-archives '(("gnu"   . "https://mirrors.bfsu.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.bfsu.edu.cn/elpa/melpa/")
                         ("org" . "https://mirrors.bfsu.edu.cn/elpa/org/"))
      package-archive-priorities '(("org" . 10)
				   ("gnu" . 5)
				   ("melpa" . 0)))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(set-face-attribute
  'default nil
  :font (font-spec :family "Sarasa Mono SC" :size 24))
(set-fontset-font t 'han "LXGW WenKai")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

;; Global key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (ido-mode 1)
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)
;; (global-set-key (kbd "M-i") 'imenu)

;; highlight current line
(global-hl-line-mode 1)

;; Line number
(global-linum-mode 0)
(global-display-line-numbers-mode 1)
(column-number-mode)  ; Display line number in the mode line
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start t)
;; Disable line numbers for some modes
(defun my/disable-line-numbers (&optional dummy)
    (display-line-numbers-mode -1))
(add-hook 'shell-mode-hook 'my/disable-line-numbers)
(add-hook 'term-mode-hook 'my/disable-line-numbers)

;; Remember cursor position
(save-place-mode 1) 

;; Coding system
(set-language-environment "UTF-8")  ; Seems to cause using Japanese fonts for Chinese Characters if not setting Chinese font explicitly
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Packages managed by git submodules
;;===================================
(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Use Lazycat's awesome-pair package
;; (require 'awesome-pair)
;; (dolist (hook (list
;;                'c-mode-common-hook
;;                'c-mode-hook
;;                'c++-mode-hook
;;                'java-mode-hook
;;                'haskell-mode-hook
;;                'emacs-lisp-mode-hook
;;                'lisp-interaction-mode-hook
;;                'lisp-mode-hook
;;                'maxima-mode-hook
;;                'ielm-mode-hook
;;                'sh-mode-hook
;;                'makefile-gmake-mode-hook
;;                'php-mode-hook
;;                'python-mode-hook
;;                'js-mode-hook
;;                'go-mode-hook
;;                'qml-mode-hook
;;                'jade-mode-hook
;;                'css-mode-hook
;;                'ruby-mode-hook
;;                'coffee-mode-hook
;;                'rust-mode-hook
;;                'qmake-mode-hook
;;                'lua-mode-hook
;;                'swift-mode-hook
;;                'minibuffer-inactive-mode-hook
;;                ))
;;   (add-hook hook '(lambda () (awesome-pair-mode 1))))
;; (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
;; (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
;; (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
;; (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
;; (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
;; (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
;; (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

;; (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
;; (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

;; (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

;; (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
;; (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
;; (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

;; (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
;; (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
;; (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
;; (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
;; (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

;; (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
;; (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
;; (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)

;; Use-package settings
;; ==============================
;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish)

;; The built-in tango theme looks fairly well
;; (load-theme 'tango t)

;; (use-package acme-theme
;;   :config
;;   (load-theme 'acme t))

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-valley-light t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-day t))

;; (use-package modus-themes
;;   :init
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-operandi))

(use-package solarized-theme
  :init
  (setq x-underline-at-descent-line t)
  :config
  (load-theme 'solarized-light-high-contrast t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode))

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

(use-package counsel
  :bind(("C-x C-f" . counsel-find-file)
        ("C-c f" . counsel-fzf)))

(use-package company
  :diminish
  :init
  (setq company-idle-delay 0)
  :hook
  (after-init . global-company-mode))

(use-package yasnippet
  :diminish yas-minor-mode
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

(use-package magit
  :if (executable-find "git")
  :bind
  (("C-x g" . magit-status)
   (:map magit-status-mode-map
         ("M-RET" . magit-diff-visit-file-other-window)))
  :config
  (defun magit-log-follow-current-file ()
    "A wrapper around `magit-log-buffer-file' with `--follow' argument."
    (interactive)
    (magit-log-buffer-file t)))

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

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config (progn (show-smartparens-global-mode t)))

(use-package smartparens
  :hook
  (prog-mode . smartparens-strict-mode))

(use-package evil-cleverparens
  :diminish
  :hook
  (smartparens-strict-mode . evil-cleverparens-mode))

(use-package terminal-here
  :bind
  ("C-<f12>" . terminal-here-launch)
  :config
  (setq terminal-here-linux-terminal-command '("termite")))

(use-package org
  :defer t
  :ensure org-plus-contrib
  :pin org
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-default-notes-file (concat org-directory "/notes.org")))

(use-package org-superstar
  :after org
  :hook
  (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "✚" "🞷" "▶")))
