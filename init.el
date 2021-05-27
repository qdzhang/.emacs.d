;;; init.el -*- lexical-binding: t; -*-

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
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (ido-mode 1)
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)
;; (global-set-key (kbd "M-i") 'imenu)

;; highlight current line
;; (global-hl-line-mode 1)

;; Line number
(global-linum-mode 0)
(global-display-line-numbers-mode 1)
(column-number-mode)  ; Display line number in the mode line
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start t)
;; Disable line numbers for some modes
(defun my/disable-line-numbers (&optional arg)
    (display-line-numbers-mode -1))
(add-hook 'shell-mode-hook 'my/disable-line-numbers)
(add-hook 'eshell-mode-hook 'my/disable-line-numbers)
(add-hook 'term-mode-hook 'my/disable-line-numbers)
(add-hook 'vterm-mode-hook 'my/disable-line-numbers)

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

;; Show init time in a message
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(put 'narrow-to-region 'disabled nil)


;; Term and ansi-term settings
;;===========================
;; Open ansi-term in a split window
(defun my/open-term-in-split-window ()
  "Start a terminal emulator in a new window."
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (ansi-term (executable-find "bash")))


;; Open vterm in a split window
(defun my/open-vterm-in-split-window ()
  "Start vterm in a new split window."
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (vterm))

(defun my/ansi-term-bash ()
  "Start a ternimal emulator using bash without confirming"
  (interactive)
  (ansi-term "/bin/bash"))

(defun my/exit-term-kill-buffer ()
  (let* ((buff (current-buffer))
	 (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
	(if (and (string= event "finished\n")
		 (one-window-p))
	    (kill-buffer ,buff)
	  (progn (kill-buffer ,buff)
		 (delete-window)))))))

(add-hook 'term-exec-hook 'my/exit-term-kill-buffer)


;; Packages managed by git submodules
;;===================================

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path "~/.emacs.d/site-lisp/")

(require 'gitattributes-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)

;; Use Lazycat's awesome-pair package
;; (show-paren-mode 1)
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
(setq use-package-compute-statistics t)

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
  (setq solarized-scale-org-headlines nil)
  :config
  (load-theme 'solarized-dark-high-contrast t))

;; (use-package simple-modeline
;;   :hook (after-init . simple-modeline-mode))

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Evil and keybinding settings
;;=============================

(use-package general
  :demand t
  :config
  (general-evil-setup)

  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer my/local-leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix ","
    :global-prefix "SPC m")

  (my/leader-keys
    "SPC" '(execute-extended-command :which-key "execute command")
    "TAB" 'evil-indent-line

    "b" '(:ignore t :which-key "buffer")
    "br"  'revert-buffer
    "bs" '((lambda () (interactive)
             (pop-to-buffer "*scratch*"))
           :wk "scratch")
    "bd" 'kill-current-buffer
    "bb" 'switch-to-buffer
    "bk" 'kill-current-buffer
    "bl" 'evil-switch-to-windows-last-buffer

    "e" '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ed" 'sly-eval-defun
    "ee" 'eval-last-sexp
    "el" 'sly-eval-last-expression
    "ep" 'sly-eval-print-last-expression
    "er" 'sly-eval-region

    "f" '(:ignore t :which-key "file")
    "fD" '((lambda () (interactive) (delete-file (buffer-file-name))) :wk "delete")
    "ff" 'find-file
    "fs" 'save-buffer
    "fr" 'counsel-recentf
    "fR" '((lambda (new-path)
             (interactive (list (read-file-name "Move file to: ") current-prefix-arg))
             (rename-file (buffer-file-name) (expand-file-name new-path)))
           :wk "move/rename")

    "g" '(:ignore t :which-key "git")
    "gg" 'magit-status

    "h" '(:ignore t :which-key "describe")
    "he" 'view-echo-area-messages
    "hf" 'describe-function
    "hF" 'describe-face
    "hi" 'info
    "hl" 'view-lossage
    "hL" 'find-library
    "hm" 'describe-mode
    "hk" 'describe-key
    "hK" 'describe-keymap
    "hs" 'use-package-report
    "hp" 'describe-package
    "hv" 'describe-variable

    "n" '(:ignore t :which-key "notes")
    "na" 'org-agenda
    "nc" 'org-capture
    "nl" 'org-store-link

    "o" '(:ignore t :which-key "open")
    "od" '(dired-jump :wk "dired")
    "oe" 'eshell
    "ot" '(my/open-vterm-in-split-window :wk "split-term")
    "oT" 'vterm
    ;; "oT" '(my/ansi-term-bash :wk "term")

    "s" '(:ignore t :which-key "search")
    "sb" 'swiper
    "sB" 'swiper-all
    "si" '(counsel-imenu :wk "imenu")
    "sy" 'ivy-yasnippet

    "t" '(:ignore t :which-key "toggle")
    "ts" 'sly

    "w" '(:ignore t :which-key "window")))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  ;; Move evil-mode-line-tag to beginning of modeline
  (setq evil-mode-line-format '(before . mode-line-front-space))
  (setq evil-normal-state-tag   (propertize " NORMAL " 'face '((:foreground "dark khaki")))
	evil-emacs-state-tag    (propertize " EMACS " 'face '((:foreground "turquoise")))
	evil-insert-state-tag   (propertize " INSERT " 'face '((:foreground "dark sea green")))
	evil-replace-state-tag  (propertize " REPLACE " 'face '((:foreground "dark orange")))
	evil-motion-state-tag   (propertize " MOTION " 'face '((:foreground "khaki")))
	evil-visual-state-tag   (propertize " VISUAL " 'face '((:foreground "light salmon")))
	evil-operator-state-tag (propertize " OPERATE " 'face '((:foreground "sandy brown"))))
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

;;==================
;; Evil settings end

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
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

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
  :hook
  ((prog-mode org-mode markdown-mode text-mode snippet-mode gitignore-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
  :defer t
  :after yasnippet)

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
    (magit-log-buffer-file t))

  ;; Make magit status show full screen
  ;; https://github.com/magit/magit/issues/1953#issuecomment-221134023
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :general
  (my/leader-keys
    "gl" 'magit-log-buffer-file))

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

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config (progn (show-smartparens-global-mode t)))

(use-package smartparens
  :hook
  (prog-mode . smartparens-strict-mode)
  :bind (:map smartparens-strict-mode-map
	      ("M-<up>" . sp-splice-sexp-killing-backward)
	      ("M-<down>" . sp-splice-sexp-killing-forward)
	      ("M-<delete>". sp-unwrap-sexp)))

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
  (setq org-superstar-headline-bullets-list '("◉" "○" "❖" "◈" "✿" "✚" "▶")))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)))

(use-package helpful
  :after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package eglot
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")

  ;; Define functionality for interacting with the sly repl using counsel
  (defun counsel-sly-mrepl-shortcut ()
    (interactive)
    (ivy-read
     "Action: "
     (mapcar #'car sly-mrepl-shortcut-alist)
     :action (lambda (string)
               (let ((command (and string
                                   (cdr (assoc string sly-mrepl-shortcut-alist)))))
                 (call-interactively command)))))

  (defun sly-read-package-name (prompt &optional initial-value allow-blank)
    (ivy-read
     "Package: "
     (sly-eval `(slynk:list-all-package-names t))
     :action #'identity))

  (defun counsel-sly-mrepl-history ()
    (interactive)
    (ivy-read
     "History: "
     (ring-elements comint-input-ring)
     :action (lambda (e)
               (insert e))))

  (defun eval-grab-output (string)
    (let ((res nil))
      (sly-eval-async `(slynk:eval-and-grab-output ,string)
        (lambda (result)
          (cl-destructuring-bind (output value) result
            (setf res (car (read-from-string value))))))
      (while (null res)
        (sleep-for 0.1))
      res))

  (defun counsel-sly-eval (string action)
    (let ((result (eval-grab-output string)))
      (ivy-read
       "Symbol: "
       result
       :action action)))

  (defun send-input (expr)
    (insert expr)
    (comint-send-input))

  (defun counsel-sly-package-internal-symbols ()
    (interactive)
    (counsel-sly-eval "(common-lisp-user::package-internal-symbols \*package\*)"
                      `(1 ("o" ,#'insert "insert")
                          ("f" ,(lambda (candidate)
                                  (send-input (format "(find-symbol \"%s\")" candidate)))
                           "find symbol"))))

  ;; sly-mrepl-mode-map symbol not available at the time of
  ;; use-package, so :bind cannot be used here
  (with-eval-after-load 'sly-mrepl
    (define-key sly-mrepl-mode-map (kbd "M-r") 'counsel-sly-mrepl-history)))

(use-package vterm
  :defer t
  :config
  (defun my/vterm-exit-kill-buffer-window (process event)
    "Kill buffer and window on shell process termination."
    (when (not (process-live-p process))
      (let ((buf (process-buffer process)))
	(when (buffer-live-p buf)
	  (with-current-buffer buf
            (kill-buffer)
            (unless (one-window-p)
	      (delete-window)))))))
  :hook
  (vterm-mode . (lambda () (set-process-sentinel (get-buffer-process (buffer-name) ) #'my/vterm-exit-kill-buffer-window))))

(use-package dired
  :ensure nil
  :general
  (my/leader-keys
    "fd" '(dired :wk "directory"))
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (use-package dired-x
    :ensure nil)
  (setq dired-dwim-target t)
  (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "llpp")
				       ("\\.mkv\\'"  "mpv")
                                       ("\\.avi\\'"  "mpv")
                                       ("\\.mp4\\'"  "mpv")
                                       ("\\.m4v\\'"  "mpv")
                                       ("\\.flv\\'"  "mpv")
                                       ("\\.wmv\\'"  "mpv")
                                       ("\\.mpg\\'"  "mpv")
                                       ("\\.mpeg\\'" "mpv")
                                       ("\\.webm\\'" "mpv"))))

(use-package aggressive-indent
  :defer t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode))


;; Restore file-name-hander-alist
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist doom--file-name-handler-alist)))
