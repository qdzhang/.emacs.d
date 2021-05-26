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

;; Unset file-name-handler-alist temporarily
;; This manner is borrowed from Doom Emacs
;; Restore it later, refer to the end of init.el
(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; Minimize garbage collection during startup
(setq gc-cons-threshold (expt 2 24))

(defun defer-garbage-collection-h ()
  (setq gc-cons-threshold (expt 2 24)))

(defun restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold (expt 2 23)))))

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook #'restore-garbage-collection-h)
(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

;; Load custom.el contains custom-set-variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(when (file-exists-p custom-file)
  (load custom-file))
