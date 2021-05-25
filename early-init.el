;;; early-init.el -*- lexical-binding: t; -*-

(setq inhibit-splash-screen 1)

(and (bound-and-true-p scroll-bar-mode)
     (scroll-bar-mode -1))        ; Disable visible scrollbar
(and (bound-and-true-p tool-bar-mode)
     (tool-bar-mode -1))          ; Disable the toolbar
(and (bound-and-true-p tooltip-mode)
     (tooltip-mode -1))           ; Disable tooltips
(and (bound-and-true-p menu-bar-mode)
     (menu-bar-mode -1))          ; Disable the menu bar
(set-fringe-mode 10)        ; Give some breathing room

;; Speed up startup
;;=================
(setq file-name-handler-alist nil)
;; Minimize garbage collection during startup
(setq gc-cons-threshold (expt 2 24))

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))
