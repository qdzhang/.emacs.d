(setq inhibit-splash-screen 1)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)

(set-face-attribute 'default nil :family "Sarasa Mono SC" :height 160)

(global-set-key (kbd "M-i") 'imenu)

(require 'helm-config)

(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
