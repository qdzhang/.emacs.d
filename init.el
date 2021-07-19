;;; init.el -*- lexical-binding: t; -*-

(load "server")
(unless (server-running-p) (server-start))

;; Setting after display system init list
;; If you start an emacsclient, this macro is helpful to set font faces
;; or some other things should be set after frame created
;; Copied from spacemacs
;; https://github.com/syl20bnr/spacemacs/blob/ca04186cfcde09f5b03951353cedd4f828c69378/core/core-display-init.el
(defvar spacemacs--after-display-system-init-list '()
  "List of functions to be run after the display system is initialized.")

(defadvice server-create-window-system-frame
    (after spacemacs-init-display activate)
  "After Emacs server creates a frame, run functions queued in
`SPACEMACS--AFTER-DISPLAY-SYSTEM-INIT-LIST' to do any setup that needs to have
the display system initialized."
  (progn
    (dolist (fn (reverse spacemacs--after-display-system-init-list))
      (funcall fn))
    (ad-disable-advice 'server-create-window-system-frame
                       'after
                       'spacemacs-init-display)
    (ad-activate 'server-create-window-system-frame)))

(defmacro spacemacs|do-after-display-system-init (&rest body)
  "If the display-system is initialized, run `BODY', otherwise,
add it to a queue of actions to perform after the first graphical frame is
created."
  `(let ((init (cond ((boundp 'ns-initialized) ns-initialized)
                     ;; w32-initialized gets set too early, so
                     ;; if we're on Windows, check the list of fonts
                     ;; instead (this is nil until the graphics system
                     ;; is initialized)
                     ((boundp 'w32-initialized) (font-family-list))
                     ((boundp 'x-initialized) x-initialized)
                     ;; fallback to normal loading behavior only if in a GUI
                     (t (display-graphic-p)))))
     (if init
         (progn
           ,@body)
       (push (lambda () ,@body) spacemacs--after-display-system-init-list))))

(spacemacs|do-after-display-system-init
 (set-face-attribute 'default nil :font (font-spec :family "Sarasa Mono SC" :size 24))
 (set-fontset-font t 'han "LXGW WenKai")
 (set-fontset-font t 'kana "Sarasa Mono J")
 (set-fontset-font t 'hangul "Sarasa Mono K")
 (set-fontset-font t 'cjk-misc "Sarasa Mono SC")
 (set-fontset-font t 'bopomofo "Sarasa Mono SC")

 ;; Color emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
 (set-fontset-font t 'symbol "Noto Color Emoji")
 ;; (set-fontset-font t 'symbol "Noto Sans Symbols" nil 'append)
 ;; (set-fontset-font t 'symbol "Noto Sans Symbols2" nil 'append)
 (set-fontset-font t 'symbol "JetBrainsMono Nerd Font Mono" nil 'append)
 (set-fontset-font t 'symbol "Symbola" nil 'append))


(require 'package)

;; TODO: Org ELPA will be shutting down and Org contrib will be moving to NonGNU ELPA
;; Keep an eye on the changes of mirrors
(setq package-archives '(("gnu"   . "https://mirrors.bfsu.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.bfsu.edu.cn/elpa/melpa/")
                         ("org" . "https://mirrors.bfsu.edu.cn/elpa/org/"))
      package-archive-priorities '(("org" . 10)
                                   ("gnu" . 5)
                                   ("melpa" . 0)))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Refresh package contents before package-install
;;; https://genehack.blog/2020/04/a-bit-of-emacs-advice/
(defvar my/packages-refreshed nil
  "Flag for whether package lists have been refreshed yet.")

(defun my/package-refresh (&rest args)
  "Refresh package metadata, if needed.
Ignores `ARGS'."
  (unless (eq my/packages-refreshed t)
    (progn
      (package-refresh-contents)
      (setq my/packages-refreshed t))))

(advice-add 'package-install :before #'my/package-refresh)


;;; Add directories that containing elisp files
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path "~/.emacs.d/site-lisp/")

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


;; Global key bindings
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (ido-mode 1)
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)
;; (global-set-key (kbd "M-i") 'imenu)

;; highlight current line
;; (global-hl-line-mode 1)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'vterm-mode-hook (lambda () (hl-line-mode -1)))

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq-default display-fill-column-indicator-column 80)


;;; Mode-line configurations
;;;=========================
;;; Use (add-to-list 'mode-line-format '(:eval (format " %s" buffer-file-coding-system)))
;;; can set mode-line-format without override it
;;; Or use this:
;;; (setq-default mode-line-format (substitute
;;; 'my-mode-line-coding-format
;;; 'mode-line-mule-info
;;; mode-line-format))
;;; to substitute partial contents of mode-line

;;; References:
;;; https://github.com/gexplorer/simple-modeline
;;; https://github.com/jamesnvc/dotfiles/blob/master/emacs.d/modules/cogent-modeline.el
;;; https://www.reddit.com/r/emacs/comments/1nihkt/how_to_display_full_charset_name_in_modeline_eg/
;;; https://emacs-china.org/t/topic/655
;;; https://emacs.stackexchange.com/questions/13652/how-to-customize-mode-line-format

(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  :config
  (setq nyan-minimum-window-width 75)
  (setq nyan-bar-length 25))

(use-package minions
  :config (minions-mode 1))

(defun modeline--mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))))



;;; Following configurations adapted from simple-modeline
;;; https://github.com/gexplorer/simple-modeline

;;; Faces definitions
(defface simple-modeline-space
  '((t))
  "Face for space used to alight the right segments in the mode-line.")

(defface simple-modeline-main-mode
  '((t :inherit (font-lock-type-face)
       :weight bold))
  "Face for main mode in the mode-line")

(defface simple-modeline-unimportant
  '((t (:inherit (shadow))))
  "Face for less important mode-line elements.")

(defface simple-modeline-status-modified
  '((t (:inherit (font-lock-variable-name-face))))
  "Face for the 'modified' indicator symbol in the mode-line.")

(defface simple-modeline-status-info
  '((t (:inherit (font-lock-string-face))))
  "Face for generic status indicators in the mode-line.")

(defface simple-modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line.")

(defface simple-modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line.")

(defface simple-modeline-status-error
  '((t (:inherit (error))))
  "Face for error status indicators in the mode-line.")

;;; Some helper functions to format mode-line
(defun simple-modeline--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and RIGHT-SEGMENTS, aligned respectively."
  (let* ((left (simple-modeline--format-segments left-segments))
         (right (simple-modeline--format-segments right-segments))
         (reserve (length right)))
    (concat
     left
     (propertize " "
                 'display `((space :align-to (- right ,reserve)))
                 'face '(:inherit simple-modeline-space))
     right)))

(defun simple-modeline--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
                     segments)))

(defun simple-modeline-make-mouse-map (mouse function)
  "Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

;;; Mode-line segments configurations
(defvar simple-modeline-segment-encoding-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (e)
        (interactive "e")
        (with-selected-window (posn-window (event-start e))
          (when (and enable-multibyte-characters
                     buffer-file-coding-system)
            (describe-coding-system buffer-file-coding-system)))))
    (define-key map [mode-line mouse-3]
      (lambda (e)
        (interactive "e")
        (with-selected-window (posn-window (event-start e))
          (call-interactively #'set-buffer-file-coding-system))))
    (purecopy map))
  "Local keymap for the coding-system part of the simple-modeline.")

(defun simple-modeline-segment-encoding ()
  "Displays the encoding style of the buffer in the mode-line."
  `(" "
    ,(propertize
      "%z"
      'help-echo
      (lambda (window)
        (with-current-buffer (window-buffer window)
          (if buffer-file-coding-system
              (format "Buffer coding system (%s): %s\nmouse-1: Describe coding system\nmouse-3: Set coding system"
                      (if enable-multibyte-characters "multi-byte" "unibyte")
                      (symbol-name buffer-file-coding-system))
            "Buffer coding system: none specified")))
      'mouse-face 'mode-line-highlight
      'local-map simple-modeline-segment-encoding-map)))

(defun simple-modeline-segment-position ()
  "Displays the current cursor position in the mode-line."
  `((line-number-mode
     ((column-number-mode
       (column-number-indicator-zero-based
        (8 " %l:%c")
        (8 " %l:%C"))
       (5 " L%l")))
     ((column-number-mode
       (column-number-indicator-zero-based
        (5 " C%c")
        (5 " C%C")))))
    ,(if (region-active-p)
         (propertize (format "+%s"
                             (apply #'+ (mapcar
                                         (lambda (pos)
                                           (- (cdr pos)
                                              (car pos)))
                                         (region-bounds))))
                     'font-lock-face 'font-lock-variable-name-face))))

(defun simple-modeline-segment-eol ()
  "Displays the EOL style of the current buffer in the mode-line."
  (let* ((eol (coding-system-eol-type buffer-file-coding-system))
         (mnemonic (pcase eol
                     ('0 " LF")
                     ('1 " CRLF")
                     ('2 " CR")
                     (_ "")))
         (desc (pcase eol
                 ('0 "Unix-style")
                 ('1 "DOS-style")
                 ('2 "Mac-style")
                 (_ "Undecided"))))
    (propertize
     mnemonic
     'help-echo (format "End-of-line style: %s\nmouse-1: Cycle" desc)
     'local-map (purecopy
                 (simple-modeline-make-mouse-map
                  'mouse-1
                  (lambda (event)
                    (interactive "e")
                    (with-selected-window (posn-window (event-start event))
                      (let ((eol (coding-system-eol-type buffer-file-coding-system)))
                        (set-buffer-file-coding-system
                         (cond ((eq eol 0) 'dos) ((eq eol 1) 'mac) (t 'unix))))))))
     'mouse-face 'mode-line-highlight)))

(defun simple-modeline-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (string-trim (format-mode-line mode-line-misc-info 'simple-modeline-unimportant))))
    (unless (string= misc-info "")
      (concat " " misc-info))))

(defun simple-modeline-segment-minor-modes ()
  "Displays the current minor modes in the mode-line."
  (replace-regexp-in-string
   "%" "%%%%"
   (format-mode-line minor-mode-alist)
   t t))

(defun simple-modeline-segment-minions-mode ()
  "Display minions mode to diminish other minor modes in the mode-line
Adapted from https://github.com/seagle0128/doom-modeline/commit/5cf1857add945f72450c8fabf5dcb994f21a3f27.
Thanks for doom-modeline!"
  (concat
   " "
   (propertize minions-mode-line-lighter
               'help-echo "Minions
mouse-1: Display minor modes menu"
               'mouse-face 'mode-line-highlight
               'local-map (make-mode-line-mouse-map
                           'mouse-1 #'minions-minor-modes-menu))
   " "))

(defun simple-modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (when mode-line-process
    (concat " " (string-trim (format-mode-line mode-line-process)))))

(defun simple-modeline-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize
   (concat " "
           (or (and (boundp 'delighted-modes)
                    (cadr (assq major-mode delighted-modes)))
               (format-mode-line mode-name))
           " ")
   'face 'simple-modeline-main-mode))

(buffer-local-value 'major-mode (other-buffer))
(defun simple-modeline-segment-modified ()
  "Display different icons according to the buffer status"
  (cond
   ((equal 'dired-mode
           (buffer-local-value 'major-mode (current-buffer)))
    (propertize ""
                'face '(:foreground "orange")
                'help-echo "Dired mode"))
   (buffer-read-only
    (propertize ""
                'face '(:foreground "orange")
                'help-echo "buffer is read-only!!!"))
   ((buffer-modified-p)
    (propertize "⬤"
                'face '(:foreground "#f36e71")
                'help-echo "buffer modified."))
   (t
    (propertize "⬤"
                'face '(:foreground "#99bd6a")))))

(defun simple-modeline-segment-nyan ()
  "Display nyan cat in the mode-line"
  (list " "
        (nyan-create)
        " "))

(defun simple-modeline-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  '(vc-mode vc-mode))

;;; Add advice to show icon before vc-mode
;;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line
;;; https://www.reddit.com/r/emacs/comments/5fjri7/how_to_use_git_logo_in_modeline_instead_of/
;; (advice-add #'vc-git-mode-line-string :filter-return #'my-replace-git-status)
;; (defun my-replace-git-status (tstr)
;;   (let* ((tstr (replace-regexp-in-string "Git" "" tstr))
;;          (first-char (substring tstr 0 1))
;;          (rest-chars (substring tstr 1)))
;;     (cond
;;      ((string= ":" first-char) ;;; Modified
;;       (replace-regexp-in-string "^:" (concat [#xf126] ":") tstr))
;;      ((string= "-" first-char) ;; No change
;;       (replace-regexp-in-string "^-" "✔:" tstr))
;;      (t tstr))))

(defun simple-modeline-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (propertize " %b " 'face 'mode-line-buffer-id))

(defun simple-modeline-rime-indicator ()
  "Display rime indicator in the mode-line"
  (rime-lighter))

(defun simple-modeline-narrowed-status ()
  "Display an indicator to show whether buffer is narrowed"
  (when (buffer-narrowed-p)
    (propertize " Narrowed " 'face 'simple-modeline-status-warning)))

(defun simple-modeline-flycheck-status ()
  "Displya flycheck status in the modeline"
  (flycheck-mode-line-status-text))

(defun simple-modeline-client-status ()
  "Determine whether current frame is a server/client frame"
  '(""
    (:propertize
     (""
      (:eval
       (if
           (frame-parameter nil 'client)
           "@" "")))
     help-echo "emacsclient frame")))

(defcustom simple-modeline-segments
  '((simple-modeline-segment-modified
     simple-modeline-segment-buffer-name
     simple-modeline-segment-nyan
     simple-modeline-segment-position)
    (simple-modeline-rime-indicator
     simple-modeline-narrowed-status
     simple-modeline-client-status
     simple-modeline-segment-eol
     simple-modeline-segment-encoding
     simple-modeline-segment-minions-mode
     simple-modeline-segment-misc-info
     simple-modeline-segment-process
     simple-modeline-segment-vc
     simple-modeline-segment-major-mode))
  "Simple modeline segments."
  :type '(list (repeat :tag "Left aligned" function)
               (repeat :tag "Right aligned" function)))

(when (not (string= "emacstty" (daemonp)))
  (setq-default mode-line-format
                (list
                 mode-line-front-space
                 '(:eval evil-mode-line-tag)
                 '(:eval
                   (simple-modeline--format
                    (car simple-modeline-segments)
                    (cadr simple-modeline-segments)))
                 mode-line-end-spaces)))


;; Line number
(global-linum-mode 0)
(global-display-line-numbers-mode 1)
(column-number-mode)  ; Display line number in the mode line
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start t)
;; Disable line numbers for some modes
(defun my/disable-line-numbers (&optional arg)
  (display-line-numbers-mode -1))

;;; Group hooks
;;; https://emacs.stackexchange.com/questions/501/how-do-i-group-hooks
;;; For the opposite case, group all functions to a mode-hook
;;; https://emacs.stackexchange.com/questions/55949/mutilpe-add-hook-merge-to-one
(setq my/disable-line-numbers-mode-hook
      '(shell-mode-hook
        eshell-mode-hook
        term-mode-hook
        vterm-mode-hook
        org-mode-hook))
(dolist (hook my/disable-line-numbers-mode-hook)
  (add-hook hook 'my/disable-line-numbers))

;;; Whitespace-mode settings

;;; Add a keybinding to show whitespace
;;; https://www.reddit.com/r/emacs/comments/33vah8/whitespace_mode/
(defun better-whitespace ()
  (interactive)
  (whitespace-mode -1)
  (let ((ws-small '(face lines-tail))
        (ws-big '(face tabs spaces trailing lines-tail space-before-tab
                       newline indentation empty space-after-tab space-mark
                       tab-mark newline-mark)))
    (if (eq whitespace-style ws-small)
        (setq whitespace-style ws-big)
      (setq whitespace-style ws-small)))
  (whitespace-mode 1))
(define-key prog-mode-map (kbd "C-c w") 'better-whitespace)

;;; Show tabs
(defun my/show-tabs ()
  (setq whitespace-style
        '(face
          ;; show tab as » (see `whitespace-display-mappings')
          tab-mark))
  (whitespace-mode 1))

;;; Show trailing whitespace
(defun my/show-trailing-whitespace ()
  (set-face-attribute 'trailing-whitespace nil
                      :background "green")
  (setq show-trailing-whitespace 1))
(add-hook 'prog-mode-hook 'my/show-trailing-whitespace)
(add-hook 'prog-mode-hook 'my/show-tabs)

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
(defadvice narrow-to-region (after my/deactivate-mark activate)
  (if (use-region-p)
      (deactivate-mark)))


;;; When use FIND-FILE to create a new file, if the directory doesn't exist yet,
;;; create the directory
;;; https://superuser.com/questions/131538/can-i-create-directories-that-dont-exist-while-creating-a-new-file-in-emacs
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))


;;; Some useful functions
;;;======================

;;; Increment number
;;; https://www.emacswiki.org/emacs/IncrementNumber
(defun my/increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun my/decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))


;;;===================
;;; Auto save settings
;;;===================

(setq auto-save-default nil)

(require 'super-save)
(super-save-mode +1)
(diminish 'super-save-mode)
(add-to-list 'super-save-hook-triggers 'find-file-hook)
(setq super-save-exclude '(".gpg"))

;;; Tabs and spaces settings
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun infer-indent-hook ()
  (setq indent-tabs-mode nil)
  (infer-indentation-style))
(add-hook 'c-mode-hook 'infer-indent-hook)
(add-hook 'emacs-lisp-mode-hook 'infer-indent-hook)

;;; General indent settings
;;;========================
;;; Use C-u <count> my/shift-right(left) will shift current line <count> spaces right(left)
(defun my/shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun my/shift-right (count)
  (interactive "p")
  (my/shift-text count))

(defun my/shift-left (count)
  (interactive "p")
  (my/shift-text (- count)))



;; Term and ansi-term settings
;;===========================

;;; A general function to create a new split window and switch to it
;;; https://emacsredux.com/blog/2013/04/29/start-command-or-switch-to-its-buffer/
(defun my/start-or-switch-to-split (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

;; Open ansi-term in a split window
(defun my/open-term-in-split-window ()
  "Start `ansi-term' in a new split window."
  (interactive)
  (my/start-or-switch-to-split (lambda ()
                                 (ansi-term (executable-find "bash")))
                               "*ansi-term*"))

;; Open vterm in a split window
(defun my/open-vterm-in-split-window ()
  "Start `vterm' in a new split window."
  (interactive)
  (my/start-or-switch-to-split 'vterm "*vterm*"))

(defun my/open-ielm-in-split-window ()
  "Switch to default `ielm' buffer.
Start `ielm' in a split window if it's not already running."
  (interactive)
  (my/start-or-switch-to-split 'ielm "*ielm*"))


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

;;; Add some advice for upcase and downcase functions
;;; https://oremacs.com/2014/12/23/upcase-word-you-silly/
(defadvice upcase-word (before upcase-word-advice activate)
  (unless (looking-back "\\b" nil)
    (backward-word)))

(defadvice downcase-word (before downcase-word-advice activate)
  (unless (looking-back "\\b" nil)
    (backward-word)))

(defadvice capitalize-word (before capitalize-word-advice activate)
  (unless (or (looking-back "\\b" nil)
              (bound-and-true-p subword-mode))
    (backward-word)))


;; Packages managed by git submodules
;;===================================

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


;;; Packages managed by use-package
;;;================================

;; Some built-in theme looks fairly well
;; (load-theme 'tango t)
;; (load-theme 'tsdh-light t)

(cond ((string= "emacstty" (daemonp))
       (load-theme 'tango-dark))
      (t
       (use-package doom-themes
         :config
         ;; Global settings (defaults)
         (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
               doom-themes-enable-italic t
               doom-themes-padded-modeline 5) ; if nil, italics is universally disabled
         (load-theme 'doom-one-light t)

         ;; Enable flashing mode-line on errors
         ;; (doom-themes-visual-bell-config)
         ;; Enable custom neotree theme (all-the-icons must be installed!)
         ;; (doom-themes-neotree-config)
         ;; or for treemacs users
         ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
         ;; (doom-themes-treemacs-config)
         ;; Corrects (and improves) org-mode's native fontification.
         (doom-themes-org-config))))

;; (use-package acme-theme
;;   :config
;;   (load-theme 'acme t))

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-valley-light t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t))

;; (use-package modus-themes
;;   :init
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-operandi))

;; (use-package solarized-theme
;;   :init
;;   (setq x-underline-at-descent-line t)
;;   (setq solarized-scale-org-headlines nil)
;;   :config
;;   (load-theme 'solarized-dark-high-contrast t))

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
    "SPC" '(counsel-M-x :which-key "execute command")
    "TAB" 'evil-indent-line
    ";" '(my/sp-comment-with-two-semicolon :wk "sp-comment")

    "b" '(:ignore t :which-key "buffer")
    "br"  'revert-buffer
    "bs" '((lambda () (interactive)
             (pop-to-buffer "*scratch*"))
           :wk "scratch")
    "bd" 'kill-current-buffer
    "bb" 'switch-to-buffer
    "bk" 'kill-current-buffer
    "bl" '(my/switch-to-previous-buffer :wk "last buffer")

    "c" '(:ignore t :which-key "change text")
    "c;" '(my/semicolon-at-end-of-line :which-key "semicolon(end)")
    "cc" 'capitalize-word
    "cd" 'downcase-word
    "cg" '(my/sp-comment-with-three-semicolon :wk "sp-comment-3-semicolon")
    "cu" 'upcase-word

    "e" '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ed" 'sly-eval-defun
    "ee" 'eval-last-sexp
    "ef" 'package-refresh-contents
    "el" 'sly-eval-last-expression
    "ep" 'sly-eval-print-last-expression
    "er" 'sly-eval-region

    "f" '(:ignore t :which-key "file")
    "fd" '(dired :wk "directory")
    "fD" '((lambda () (interactive) (delete-file (buffer-file-name))) :wk "delete")
    "ff" 'counsel-find-file
    "fs" 'save-buffer
    "fr" 'counsel-recentf
    "fR" '(my/rename-file-and-buffer :wk "rename")
    "fn" '(my/org-journal-next-day-file :wk "next file")
    "fp" '(my/org-journal-previous-day-file :wk "previous file")

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

    "o" '(:ignore t :which-key "open")
    "od" '(dired-jump :wk "dired")
    "oe" 'eshell
    "oi" '(my/open-ielm-in-split-window :wk "ielm")
    "ot" '(my/open-vterm-in-split-window :wk "split-term")
    "oT" 'vterm
    ;; "oT" '(my/ansi-term-bash :wk "term")

    "s" '(:ignore t :which-key "search")
    "sb" '(counsel-bookmark :wk "bookmarks")
    "sB" 'swiper-all
    "sd" '(counsel-bookmarked-directory :wk "bookmarks(dir)")
    "sf" 'counsel-fzf
    "sg" 'counsel-grep-or-swiper
    "si" '(counsel-imenu :wk "imenu")
    "sm" '(counsel-evil-marks :wk "marks")
    "sr" 'counsel-rg
    "st" '(counsel-load-theme :wk "themes")
    "sy" 'ivy-yasnippet

    "t" '(:ignore t :which-key "toggle")
    "ts" 'sly

    "w" '(:ignore t :which-key "window")
    "wt" '(my/window-split-toggle :wk "toggle split")
    "ww" 'other-window)

  ;; Comment sexps and keep parentheses balanced
  ;; Keybinding is SPC+;
  ;; https://github.com/Fuco1/smartparens/issues/942
  ;; There is another method to comment and keep balanced:
  ;; Use C+= to expand selecting sexp, then use M+;(comment-dwim) to comment this sexp
  (defun my/sp-comment-with-two-semicolon ()
    "Indent sexp with all parentheses balanced, and use two semicolon to express comment"
    (interactive)
    (my/smarter-move-beginning-of-line 1)
    (sp-comment)
    (insert "; "))

  (defun my/sp-comment-with-three-semicolon ()
    "Indent sexp with all parentheses balanced, and use three semicolon to express comment"
    (interactive)
    (my/smarter-move-beginning-of-line 1)
    (sp-comment)
    (insert ";; "))

  ;; Smart beginning of the line
  ;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

  (defun my/smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  ;; Move to next/previous file
  ;; Adapted from https://github.com/howardabrams/dot-files/blob/master/emacs-fixes.org#next-and-previous-file
  (defun my/org-extract-filename-date-day-mon (string)
    "Use parse-time-string to parse org-journal file name.
parse-time-string return (SEC MIN HOUR DAY MON YEAR DOW DST TZ)
DAY is 3rd of the list, and MON is 4th of the list.
This function return a list contains two element:
first is filename DAY, second is filename MONTH"
    (let ((filename-date (parse-time-string string)))
      (list (nth 3 filename-date)
            (nth 4 filename-date))))

  (defun my/org-journal-file-number-change (f)
    "Receive a function and apply this function to org-journal file name
Such as 1+ to increment the org file according to the date number"
    (let* ((file-directory (file-name-directory (buffer-file-name)))
           (filename-day (nth 0 (my/org-extract-filename-date-day-mon (buffer-name))))
           (retain-parts-index (string-match (concat "-" (number-to-string filename-day)) (buffer-name)))
           (retain-parts (substring (buffer-name) 0 retain-parts-index))
           (new-day (number-to-string
                     (funcall f filename-day))))
      (concat file-directory
              retain-parts
              "-"
              new-day
              ".org")))

  (defun my/org-journal-next-day-file ()
    "Move to next journal file"
    (interactive)
    (find-file (my/org-journal-file-number-change '1+)))

  (defun my/org-journal-previous-day-file ()
    "Move to previous journal file"
    (interactive)
    (find-file (my/org-journal-file-number-change '1-)))


  ;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
  (defun my/rename-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive "sNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not filename)
          (message "Buffer '%s' is not visiting a file!" name)
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file name new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))))))

  (defun my/semicolon-at-end-of-line ()
    (interactive)
    (save-excursion
      (end-of-line)
      (insert ";")))

  ;; Change Emacs windows from vertical split to horizontal split
  ;; https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
  (defun my/window-split-toggle ()
    "Toggle between horizontal and vertical split with two windows."
    (interactive)
    (if (> (length (window-list)) 2)
        (error "Can't toggle with more than 2 windows!")
      (let ((func (if (window-full-height-p)
                      #'split-window-vertically
                    #'split-window-horizontally)))
        (delete-other-windows)
        (funcall func)
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))

  ;; Same function to `evil-switch-to-windows-last-buffer'
  ;; If you dont't using evil, use this function
  ;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
  (defun my/switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-respect-visual-line-mode t)
  :config
  ;; Move evil-mode-line-tag to beginning of modeline
  ;; If mode-line-format is not configured explicitly, following line can move
  ;; evil-mode-line-tag to beginning of mode-line
  ;; (setq evil-mode-line-format '(before . mode-line-front-space))

  ;; https://www.reddit.com/r/emacs/comments/70rjc9/which_modeline_package_integrates_well_with_evil/
  ;; (setq evil-normal-state-tag   (propertize " NORMAL " 'face '((:foreground "dark khaki")))
  ;;    evil-emacs-state-tag    (propertize " EMACS " 'face '((:foreground "turquoise")))
  ;;    evil-insert-state-tag   (propertize " INSERT " 'face '((:foreground "dark sea green")))
  ;;    evil-replace-state-tag  (propertize " REPLACE " 'face '((:foreground "dark orange")))
  ;;    evil-motion-state-tag   (propertize " MOTION " 'face '((:foreground "khaki")))
  ;;    evil-visual-state-tag   (propertize " VISUAL " 'face '((:foreground "light salmon")))
  ;;    evil-operator-state-tag (propertize " OPERATE " 'face '((:foreground "sandy brown"))))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :diminish evil-collection-unimpaired-mode
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
         ;; ("TAB" . ivy-partial)
         ("TAB" . my/ivy-done)
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
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-selectable-prompt t)
  (setq ivy-wrap t)


  ;; Ivy complete hack
  ;; Make ivy complete more "intelligent" Use TAB to complete the selected item,
  ;; if the selected item is a directory, expand it.
  ;; Copied from https://honmaple.me/articles/2018/06/%E8%87%AA%E5%AE%9A%E4%B9%89helm%E5%BC%8F%E7%9A%84ivy.html
  (defun my/ivy-done ()
    (interactive)
    (let ((dir ivy--directory))
      (ivy-partial-or-done)
      (when (string= dir ivy--directory)
        (ivy-insert-current)
        (when (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                   (setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last))))
          (ivy--cd dir)
          (setq this-command 'ivy-cd))))))

(use-package counsel
  :bind
  (("C-x C-f" . counsel-find-file)
   :map counsel-find-file-map
   ("C-~" . my/counsel-goto-local-home))
  :general
  (my/leader-keys
    "fR" '(my/counsel-recent-directory :wk "recent-dir")
    "bj" '(my/counsel-all-opened-dired-buffer :wk "jump-dired"))
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

  (defun my/counsel-goto-local-home ()
    "Go to the $HOME of the local machine."
    (interactive)
    (ivy--cd "~/"))

  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
  ;; https://emacs-china.org/t/topic/5948/3?u=et2010
  (defvar counsel-recent-dir--selected "~/")

  (defvar counsel-recent-dir--map (let ((map (make-sparse-keymap)))
                                    (define-key map  (kbd "TAB") 'counsel-recent-dir--find-file)
                                    (define-key map  [(tab)] 'counsel-recent-dir--find-file)
                                    map))

  (defun counsel-recent-dir--find-file()
    (interactive)
    (ivy-exit-with-action
     (lambda(c)
       (setq counsel-recent-dir--selected c)
       (run-at-time 0.05 nil (lambda()
                               (let ((default-directory counsel-recent-dir--selected))
                                 ;; (find-file counsel-recent-dir--selected)
                                 (counsel-find-file)))))))

  (defun my/counsel-recent-directory ()
    "Open recent directory with dired"
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
            (append (mapcar 'file-name-directory recentf-list)
                    ;; fasd history
                    (if (executable-find "fasd")
                        (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "directories:" collection
                :keymap counsel-recent-dir--map
                :action (lambda (x) (dired x)))))

  ;; Jump between directories
  (defun my/determine-buffer-mode (buffer-or-string)
    "Returns the major mode associated with a buffer."
    (with-current-buffer buffer-or-string
      major-mode))

  (defun my/buffer-dired-mode-p (buffer)
    "Determine whether a buffer's major mode is dired-mode"
    (equal (my/determine-buffer-mode buffer)
           'dired-mode))

  (defun my/counsel-all-opened-dired-buffer ()
    "All opened directory can jumpt between theme"
    (interactive)
    (let ((all-buffers (buffer-list))
          (my--dir-collection '()))
      (dolist (buffer all-buffers)
        (if (my/buffer-dired-mode-p buffer)
            (setq my--dir-collection (cons buffer my--dir-collection))))
      (ivy-read "Jump to dired:" (mapcar #'buffer-name my--dir-collection)
                :action (lambda (x) (switch-to-buffer x))))))

(use-package swiper
  :general
  (my/leader-keys
    ;; "sc" '(my/swiper-region-or-point :wk "swiper-current")
    "ss" 'swiper-isearch)
  :config

  ;;https://emacs.stackexchange.com/questions/28355/how-to-unmark-selection-in-elisp
  (defun my/swiper-region-or-point (beg end)
    "Swiper region or current thing at point if none highlighted."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil)))
    (if (and beg end)
        (progn
          (deactivate-mark)
          (swiper (buffer-substring-no-properties beg end)))
      (swiper-thing-at-point))))

;;;====================================================
;;; Setting for predicates when using swiper
;;; Things at point can be pre-filled as search keywords
;;; Use M-p and M-n to traverse the search history
;;; https://github.com/with-emacs/mcfly
;;;
;;; And also, ivy has a built-in function:
;;; Using M-n in ivy will insert thing-at-point into
;;; the minibuffer
;;; https://github.com/abo-abo/swiper/issues/1875#issuecomment-460689608
;;;====================================================

(defvar mcfly-commands
  '(query-replace-regexp
    flush-lines
    swiper-isearch
    keep-lines))

(defvar mcfly-back-commands
  '(self-insert-command
    ivy-yank-char
    ivy-yank-word
    ivy-yank-symbol
    swiper-isearch))

(defun mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command mcfly-back-commands)
         (delete-region (point)
                        (point-max)))))

(defun mcfly-time-travel ()
  (when (memq this-command mcfly-commands)
    (let* ((kbd (kbd "M-n"))
           (cmd (key-binding kbd))
           (future (and cmd
                        (with-temp-buffer
                          (when (ignore-errors
                                  (call-interactively cmd) t)
                            (buffer-string))))))
      (when future
        (save-excursion
          (insert (propertize future 'face 'shadow)))
        (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)))))

(add-hook 'minibuffer-setup-hook #'mcfly-time-travel)

(with-eval-after-load 'ivy
  (push (cons 'swiper 'mcfly-swiper)
        ivy-hooks-alist)
  (defun mcfly-swiper ()
    (let ((sym (with-ivy-window
                 (thing-at-point 'symbol))))
      (when sym
        (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)
        (save-excursion
          (insert (propertize sym 'face 'shadow)))))))

(use-package amx
  :defer t
  :after ivy)

(use-package avy
  :after evil
  :config

  ;;========================================
  ;; Replace evil f/F/t/T motions with avy
  ;; Adapted from https://github.com/louy2/evil-avy
  ;;========================================
  (defun avy-forward-char-in-line (char &optional back)
    "Jump forward to the currently visible CHAR in the current line.
If BACK is t, jump backward."
    (interactive (list (read-char "char: " t)))

    (let ((avy-all-windows nil))
      (avy-with avy-goto-char
        (avy--process
         (save-restriction
           (if (null back)
               (narrow-to-region (point)
                                 (line-end-position))
             (narrow-to-region (line-beginning-position)
                               (point)))
           (avy--regex-candidates (regexp-quote (string char))))
         (avy--style-fn avy-style)))))

  (evil-define-motion evil-avy-find-char (count char)
    "Use avy to move forward to char in line."
    :jump t
    :type inclusive
    (interactive "<c><C>")
    (if (null count) (avy-forward-char-in-line char)
      (evil-find-char count char)))

  (evil-define-motion evil-avy-find-char-to (count char)
    "Use avy to move till char in line"
    :jump t
    :type inclusive
    (interactive "<c><C>")
    (if (null count)
        (progn
          (avy-forward-char-in-line char)
          (backward-char))
      (evil-find-char-to count char)))

  (evil-define-motion evil-avy-find-char-backward (count char)
    "Use avy to move backward to char in line."
    :jump t
    :type exclusive
    (interactive "<c><C>")
    (if (null count)
        (avy-forward-char-in-line char t)
      (evil-find-char-backward count char)))

  (evil-define-motion evil-avy-find-char-to-backward (count char)
    "Use avy to move backward till char in line."
    :jump t
    :type exclusive
    (interactive "<c><C>")
    (if (null count)
        (progn
          (avy-forward-char-in-line char t)
          (forward-char))
      (evil-find-char-to-backward count char)))

  ;; Replace motions

  (evil-define-key 'normal 'global
    "f" 'evil-avy-find-char
    "F" 'evil-avy-find-char-backward
    "t" 'evil-avy-find-char-to
    "T" 'evil-avy-find-char-to-backward
    "s" 'evil-avy-goto-char-2)

  (evil-define-key 'operator 'global
    "f" 'evil-avy-find-char
    "F" 'evil-avy-find-char-backward
    "t" 'evil-avy-find-char-to
    "T" 'evil-avy-find-char-to-backward)

  (evil-define-key 'visual 'global
    "f" 'evil-avy-find-char
    "F" 'evil-avy-find-char-backward
    "t" 'evil-avy-find-char-to
    "T" 'evil-avy-find-char-to-backward)

  (evil-define-key 'motion 'global
    "f" 'evil-avy-find-char
    "F" 'evil-avy-find-char-backward
    "t" 'evil-avy-find-char-to
    "T" 'evil-avy-find-char-to-backward))

(use-package ivy-avy
  :after (ivy avy))

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
  :general
  (my/leader-keys
    "gl" 'magit-log-buffer-file
    "gc" '(my/git-clone-clipboard-url :wk "clone-clipboard")
    "gd" '(my/magit-dotfiles-status :wk "dotfiles")
    "ga" '(my/magit-add-current-file-to-dotfiles :wk "dotfiles add")

    "ev" '(my/run-in-vterm :wk "run-in-vterm"))

  :config
  (setq magit-diff-refine-hunk 'all)

  (defun magit-log-follow-current-file ()
    "A wrapper around `magit-log-buffer-file' with `--follow' argument."
    (interactive)
    (magit-log-buffer-file t))

  ;; Make magit status show full screen
  ;; https://github.com/magit/magit/issues/1953#issuecomment-221134023
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)


  ;; git clone from clipboard
  ;; https://xenodium.com/emacs-clone-git-repo-from-clipboard/
  (defun my/git-clone-clipboard-url ()
    "Clone git URL in clipboard asynchronously and open in dired when finished."
    (interactive)
    (cl-assert (string-match-p "^\\(http://\\|https://\\|ssh://\\|git@\\)" (current-kill 0)) nil "No URL in clipboard")
    (let* ((url (current-kill 0))
           (download-dir (expand-file-name default-directory))
           (project-dir (concat (file-name-as-directory download-dir)
                                (file-name-base url)))
           (default-directory download-dir)
           (command (format "git clone %s" url))
           (buffer (generate-new-buffer (format "*%s*" command)))
           (proc))
      (when (file-exists-p project-dir)
        (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
            (delete-directory project-dir t)
          (user-error "Bailed")))
      (switch-to-buffer buffer)
      (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
      (with-current-buffer buffer
        (setq default-directory download-dir)
        (shell-command-save-pos-or-erase)
        (require 'shell)
        (shell-mode)
        (view-mode +1))
      (set-process-sentinel proc (lambda (process state)
                                   (let ((output (with-current-buffer (process-buffer process)
                                                   (buffer-string))))
                                     (kill-buffer (process-buffer process))
                                     (if (= (process-exit-status process) 0)
                                         (progn
                                           (message "finished: %s" command)
                                           (dired project-dir))
                                       (user-error (format "%s\n%s" command output))))))
      (set-process-filter proc #'comint-output-filter)))

  ;; This function can make magit working in home directory
  ;; with a bare git respository,such as dotfiles directory
  (defun ~/magit-process-environment (env)
    "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
https://github.com/magit/magit/issues/460 (@cpitclaudel)."
    (let ((default (file-name-as-directory (expand-file-name default-directory)))
          (home (expand-file-name "~/")))
      (when (string= default home)
        (let ((gitdir (expand-file-name "~/.dotfiles/")))
          (push (format "GIT_WORK_TREE=%s" home) env)
          (push (format "GIT_DIR=%s" gitdir) env))))
    env)

  (advice-add 'magit-process-environment
              :filter-return #'~/magit-process-environment)

  (defun my/magit-dotfiles-status ()
    "Show magit status in home directory containing dotfiles"
    (interactive)
    (magit-status "~/"))

  ;; Run a shell command in vterm
  ;; This is used to add a untracked file to dotfiles respository
  ;; https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/
  (defun vterm--run-in-vterm-kill (process event)
    "A process sentinel. Kills PROCESS's buffer if it is live."
    (let ((b (process-buffer process)))
      (and (buffer-live-p b)
           (kill-buffer b)
           (evil-normal-state))))

  (defun my/run-in-vterm (command)
    "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
    (interactive
     (list
      (let* ((f (cond (buffer-file-name)
                      ((eq major-mode 'dired-mode)
                       (dired-get-filename nil t))))
             (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
        (read-shell-command "Terminal command: "
                            (cons filename 0)
                            (cons 'shell-command-history 1)
                            (list filename)))))
    (with-current-buffer (vterm (concat "*" command "*"))
      (set-process-sentinel vterm--process #'vterm--run-in-vterm-kill)
      (vterm-send-string command)
      (vterm-send-return)))

  (defun my/magit-add-current-file-to-dotfiles ()
    (interactive)
    (let* ((command "dotfiles add ")
           (filename (buffer-file-name))
           (full-command (concat command filename)))
      (my/run-in-vterm full-command)
      (evil-collection-vterm-insert-line))))

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
(global-undo-fu-session-mode)

(use-package rime
  :bind
  (:map rime-mode-map
        ("M-j" . 'rime-force-enable))
  :config
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-space-after-cc-p
          rime-predicate-org-in-src-block-p
          rime-predicate-prog-in-code-p))

  ;; Change cursor color when input method is opening
  ;; Adapted from https://emacs-china.org/t/topic/17717
  (defvar input-method-cursor-color "Orange"
    "Default cursor color if using an input method.")

  (defun get-frame-cursor-color ()
    "Get the cursor-color of current frame."
    (interactive)
    (frame-parameter nil 'cursor-color))

  (defvar default-cursor-color (get-frame-cursor-color)
    "Default text cursor color.")

  (defun my/rime-disable-p ()
    "Determine whether rime is disabled, following `rime-disable-predicates'"
    (and (rime--should-enable-p)
         (not (rime--should-inline-ascii-p))))

  (defun change-cursor-color-on-input-method ()
    "Set cursor color depending on whether an input method is used or not."
    (interactive)
    (set-cursor-color (if (and (my/rime-disable-p)
                               current-input-method)
                          input-method-cursor-color
                        default-cursor-color)))

  (add-hook 'post-command-hook 'change-cursor-color-on-input-method)

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
              ("M-<delete>". sp-unwrap-sexp))
  :config
  ;; Some config to replace the function `my/go-electric-brace'
  ;; https://emacs.stackexchange.com/questions/12368/make-ending-curly-brace-of-block-go-down-an-extra-newline-in-golang
  ;; https://github.com/Fuco1/smartparens/wiki/Permissions#pre-and-post-action-hooks
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))

  ;; Remove quote ' in emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

  ;; Enable smartparens-strict-mode in the minibuffer, during eval-expression
  ;; https://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
  (defun my/conditionally-enable-smartparens-mode ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
        (smartparens-strict-mode 1)))

  (add-hook 'minibuffer-setup-hook 'my/conditionally-enable-smartparens-mode))

(use-package evil-cleverparens
  :diminish
  :hook
  (smartparens-strict-mode . evil-cleverparens-mode)
  :bind
  (:map  evil-cleverparens-mode-map
         ("<normal-state> s" . nil)))

(use-package terminal-here
  :bind
  ("C-<f12>" . terminal-here-launch)
  :config
  (setq terminal-here-linux-terminal-command 'urxvt))

(use-package org
  :defer t
  :ensure org-plus-contrib
  :pin org
  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (setq evil-auto-indent nil)))
  :general
  (my/leader-keys
    "n" '(:ignore t :which-key "notes")
    "n TAB" 'org-indent-item-tree
    "na" 'org-agenda
    "nc" 'org-capture
    "nd" '(my/toggle-side-bullet-org-buffer :wk "daily plan")
    "nD" '(my/toggle-bullet-org-buffer :wk "daily(fullframe)")
    "nl" 'org-store-link
    "nn" 'org-add-note
    "nf" '(my/org--indent-src-block :wk "format src block")
    "np" 'org-toggle-inline-images
    "nt" 'org-todo)
  :config
  ;; Fix CJK word wrap
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29364
  (setq word-wrap-by-category t)

  (setq org-default-notes-file "~/org/notes.org"
        org-agenda-files '("~/org/agenda.org")
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
        org-ellipsis "  "
        org-startup-indented t
        org-log-into-drawer "LOGBOOK"
        org-archive-location "~/org/archive.org::datetree/"
        org-src-fontify-natively t

        ;; edit in current window
        org-src-window-setup 'current-window

        ;; do not put two spaces on the left
        org-src-preserve-indentation nil
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0)

  ;; Org-capture templates
  ;; https://www.zmonster.me/2018/02/28/org-mode-capture.html
  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates '("t" "Tasks"))
  (add-to-list 'org-capture-templates
               '("tr" "Book Reading Task" entry
                 (file+olp "~/org/task.org" "Reading" "Book")
                 "* TODO %^{书名}\n%u\n%a\n" :clock-in t :clock-resume t))
  (add-to-list 'org-capture-templates
               '("tw" "Work Task" entry
                 (file+headline "~/org/task.org" "Work")
                 "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t))
  (add-to-list 'org-capture-templates
               '("i" "Inbox" entry (file "~/org/inbox.org")
                 "* %U - %^{heading} %^g\n %?\n"))
  (add-to-list 'org-capture-templates
               '("s" "New snippet" entry
                 (file+headline "~/org/snippets.org" "Code snippets")
                 "* %^{代码片段描述} %^g\n:PROPERTIES:\n:time: %U\n:origin: %^{代码来源}\n:describes: %?\n:END:\n\n#+begin_src\n \n#+end_src\n" :empty-lines 1))



  ;; Org crypt
  ;; Now any text below a headline that has a :crypt: tag will be automatically be encrypted when the file is saved. If you want to use a different tag just customize the org-crypt-tag-matcher setting.

  ;; Preventing tag inheritance stops you having encrypted text inside encrypted text.

  ;; To decrypt the text just call M-x org-decrypt-entry and the encrypted text where the point is will be replaced with the plain text. If you use this feature a lot, you will probably want to bind M-x org-decrypt-entry to a key.

  ;; Entries with a :crypt: tag will be automatically be encrypted when you save the file.
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil)


  (defun my/org--indent-buffer ()
    "Indent current buffer"
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun my/org--indent-src-block ()
    "Indent src block in org mode"
    (interactive)
    (when (org-in-src-block-p)
      (org-edit-special)
      (my/org--indent-buffer)
      (org-edit-src-exit)))

  ;; Open daily plan org file in new split buffer
  ;; https://isamert.net/2021/01/25/how-i-do-keep-my-days-organized-with-org-mode-and-emacs.html#fnr.2
  (defun my/toggle-side-bullet-org-buffer ()
    "Toggle `daily.org` in a side buffer for quick note taking.  The buffer is opened in side window so it can't be accidentaly removed."
    (interactive)
    (my/toggle-side-buffer-with-file "~/org/daily.org"))

  ;; Open daily plan org file in new full-frame buffer
  (defun my/toggle-bullet-org-buffer ()
    "Open daily plan org file in new buffer"
    (interactive)
    (find-file "~/org/daily.org"))

  (defun my/buffer-visible-p (buffer)
    "Check if given BUFFER is visible or not.  BUFFER is a string representing the buffer name."
    (or (eq buffer (window-buffer (selected-window)))
        (get-buffer-window buffer)))

  (defun my/display-buffer-in-side-window (buffer)
    "Just like `display-buffer-in-side-window' but only takes a BUFFER and rest of the parameters are for my taste."
    (select-window
     (display-buffer-in-side-window
      buffer
      (list (cons 'side 'right)
            (cons 'slot 0)
            (cons 'window-width 70)
            (cons 'window-parameters (list (cons 'no-delete-other-windows t)
                                           (cons 'no-other-window nil)))))))

  (defun my/remove-window-with-buffer (the-buffer-name)
    "Remove window containing given THE-BUFFER-NAME."
    (mapc (lambda (window)
            (when (string-equal (buffer-name (window-buffer window)) the-buffer-name)
              (delete-window window)))
          (window-list (selected-frame))))

  (defun my/toggle-side-buffer-with-file (file-path)
    "Toggle FILE-PATH in a side buffer. The buffer is opened in side window so it can't be accidentaly removed."
    (interactive)
    (let ((fname (file-name-nondirectory file-path)))
      (if (my/buffer-visible-p fname)
          (my/remove-window-with-buffer fname)
        (my/display-buffer-in-side-window
         (save-window-excursion
           (find-file file-path)
           (current-buffer))))))


  ;; Replace org checkbox to unicode symbol and add strike through line
  ;; https://jft.home.blog/2019/07/17/use-unicode-symbol-to-display-org-mode-checkboxes/
  ;; https://www.reddit.com/r/emacs/comments/brt0sk/prettifysymbolsmode_is_so_cool/
  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" .  "☐") prettify-symbols-alist)
                             (push '("[X]" . "☑" ) prettify-symbols-alist)
                             (push '("[-]" . "❍" ) prettify-symbols-alist)
                             (prettify-symbols-mode)))
  (defface org-checkbox-done-text
    '((t :inherit (font-lock-comment-face)
         :strike-through t))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)

  ;; Use `:hidden' keyword to hide an individual org src block
  ;; https://emacs.stackexchange.com/questions/44914/choose-individual-startup-visibility-of-org-modes-source-blocks/44923#44923
  (defun individual-visibility-source-blocks ()
    "Fold some blocks in the current buffer."
    (interactive)
    (org-show-block-all)
    (org-block-map
     (lambda ()
       (let ((case-fold-search t))
         (when (and
                (save-excursion
                  (beginning-of-line 1)
                  (looking-at org-block-regexp))
                (cl-assoc
                 ':hidden
                 (cl-third
                  (org-babel-get-src-block-info))))
           (org-hide-block-toggle))))))

  (add-hook 'org-mode-hook 'individual-visibility-source-blocks))

;;; Change org-mode src blocks display style, make them more simple and elegant
;;; https://emacs-china.org/t/org-source-code/9762/5
(with-eval-after-load 'org
  (defvar-local my/org-at-src-begin -1
    "Variable that holds whether last position was a ")

  (defvar my/ob-header-symbol ?…
    "Symbol used for babel headers")

  (defun my/org-prettify-src--update ()
    (let ((case-fold-search t)
          (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
          found)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (goto-char (match-end 0))
          (let ((args (org-trim
                       (buffer-substring-no-properties (point)
                                                       (line-end-position)))))
            (when (org-string-nw-p args)
              (let ((new-cell (cons args my/ob-header-symbol)))
                (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
                (cl-pushnew new-cell found :test #'equal)))))
        (setq prettify-symbols-alist
              (cl-set-difference prettify-symbols-alist
                                 (cl-set-difference
                                  (cl-remove-if-not
                                   (lambda (elm)
                                     (eq (cdr elm) my/ob-header-symbol))
                                   prettify-symbols-alist)
                                  found :test #'equal)))
        ;; Clean up old font-lock-keywords.
        (font-lock-remove-keywords nil prettify-symbols--keywords)
        (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (while (re-search-forward re nil t)
          (font-lock-flush (line-beginning-position) (line-end-position))))))

  (defun my/org-prettify-src ()
    "Hide src options via `prettify-symbols-mode'.

  `prettify-symbols-mode' is used because it has uncollpasing. It's
  may not be efficient."
    (let* ((case-fold-search t)
           (at-src-block (save-excursion
                           (beginning-of-line)
                           (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
      ;; Test if we moved out of a block.
      (when (or (and my/org-at-src-begin
                     (not at-src-block))
                ;; File was just opened.
                (eq my/org-at-src-begin -1))
        (my/org-prettify-src--update))
      ;; Remove composition if at line; doesn't work properly.
      ;; (when at-src-block
      ;;   (with-silent-modifications
      ;;     (remove-text-properties (match-end 0)
      ;;                             (1+ (line-end-position))
      ;;                             '(composition))))
      (setq my/org-at-src-begin at-src-block)))

  (defun my/org-prettify-symbols ()
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
          (cl-reduce 'append
                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                             `(("#+begin_src" . ?＄) ; ➤ ➟ ➤ ✎  〜
                               ("#+end_src"   . ?□) ; ⏹
                               ("#+header:" . ,my/ob-header-symbol)
                               ("#+begin_quote" . ?❝) ; » «
                               ("#+end_quote" . ?❞)))))
    (turn-on-prettify-symbols-mode)
    (add-hook 'post-command-hook 'my/org-prettify-src t t))
  (add-hook 'org-mode-hook #'my/org-prettify-symbols))

(use-package org-journal
  :defer t
  :general
  (my/leader-keys
    "nj" #'org-journal-new-entry)
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-prefix "#+TITLE: "
        org-journal-time-prefix "* "
        org-journal-date-format "%A, %Y-%m-%d"
        org-journal-file-format "%Y-%m-%d.org"))


(use-package org-superstar
  :after org
  :hook
  (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⬤" "◉" "○" "✸" "◆" "▲" "▶")))

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
    "d" '(:ignore t :which-key "dired")
    "dp" 'image-dired
    "dw" '(wdired-change-to-wdired-mode :wk "wdired")
    "ds" 'xah-dired-sort
    "dz" '(my/dired-get-size :wk "marked-files-size")
    "d /" '(my/dired-filter :wk "narrow"))
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
                                       ("\\.webm\\'" "mpv")
                                       ("\\.jpg\\'" "qview")
                                       ("\\.png\\'" "qview")
                                       ("\\.gif\\'" "qview")
                                       ("\\.jpeg\\'" "qview")))
  (setq dired-listing-switches "-alFh")

  (defun my/dired-filter ()
    "Dired show filtered files"
    (interactive)
    (call-interactively #'dired-mark-files-regexp)
    (progn (dired-toggle-marks)
           (dired-do-kill-lines)))

  (defun xah-dired-sort ()
    "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2018-12-23"
    (interactive)
    (let ($sort-by $arg)
      (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" )))
      (cond
       ((equal $sort-by "name") (setq $arg "-Al "))
       ((equal $sort-by "date") (setq $arg "-Al -t"))
       ((equal $sort-by "size") (setq $arg "-Al -S"))
       ;; ((equal $sort-by "dir") (setq $arg "-Al --group-directories-first"))
       (t (error "logic error 09535" )))
      (dired-sort-other $arg )))

  ;; https://oremacs.com/2015/01/12/dired-file-size/
  (defun my/dired-get-size ()
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message
         "Size of all marked files: %s"
         (progn
           (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
           (match-string 1)))))))

(use-package aggressive-indent
  :defer t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode))

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

(use-package racket-mode
  :defer t
  :hook
  (racket-mode . racket-unicode-input-method-enable)
  (racket-repl-mode . racket-unicode-input-method-enable))

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t)
  (eval-after-load 'grep
    '(define-key grep-mode-map
       (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

  (eval-after-load 'wgrep
    '(define-key grep-mode-map
       (kbd "C-c C-c") 'wgrep-finish-edit)))

(use-package web-mode
  :ensure t
  :mode
  ("\\.ejs\\'" "\\.mjs\\'" "\\.hbs\\'" "\\.html\\'" "\\.php\\'" "\\.[jt]sx?\\'")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")
                                       ("jsx" . "\\.mjs\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)

  ;; Remove < auto pair in web-mode
  (eval-after-load smartparens-strict-mode
    (sp-local-pair 'web-mode "<" nil :actions :rem)))

(use-package tide
  :hook
  (web-mode . my/setup-tide-mode)
  (before-save-hook . tide-format-before-save)
  :config
  (defun my/setup-tide-mode ()
    "Use hl-identifier-mode only on js or ts buffers."
    (when (and (stringp buffer-file-name)
               (string-match "\\.[tj]sx?\\'" buffer-file-name))
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))))

(use-package flycheck
  :defer t
  :hook
  (eglot--managed-mode . sanityinc/eglot-prefer-flycheck)
  (go-mode . flycheck-mode)
  :config
  ;; Use eglot with flycheck
  ;; https://gist.github.com/purcell/ca33abbea9a98bb0f8a04d790a0cadcd
  (defvar-local flycheck-eglot-current-errors nil)

  (defun flycheck-eglot-report-fn (diags &rest _)
    (setq flycheck-eglot-current-errors
          (mapcar (lambda (diag)
                    (save-excursion
                      (goto-char (flymake--diag-beg diag))
                      (flycheck-error-new-at (line-number-at-pos)
                                             (1+ (- (point) (line-beginning-position)))
                                             (pcase (flymake--diag-type diag)
                                               ('eglot-error 'error)
                                               ('eglot-warning 'warning)
                                               ('eglot-note 'info)
                                               (_ (error "Unknown diag type, %S" diag)))
                                             (flymake--diag-text diag)
                                             :checker 'eglot)))
                  diags))
    (flycheck-buffer))

  (defun flycheck-eglot--start (checker callback)
    (funcall callback 'finished flycheck-eglot-current-errors))

  (defun flycheck-eglot--available-p ()
    (bound-and-true-p eglot--managed-mode))

  (flycheck-define-generic-checker 'eglot
    "Report `eglot' diagnostics using `flycheck'."
    :start #'flycheck-eglot--start
    :predicate #'flycheck-eglot--available-p
    :modes '(prog-mode text-mode))

  (push 'eglot flycheck-checkers)

  (defun sanityinc/eglot-prefer-flycheck ()
    (when eglot--managed-mode
      (flycheck-add-mode 'eglot major-mode)
      (flycheck-select-checker 'eglot)
      (flycheck-mode)
      (flymake-mode -1)
      (setq eglot--current-flymake-report-fn 'flycheck-eglot-report-fn))))

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)


(use-package go-mode
  :defer t
  :init
  (setq go-fontify-function-calls nil)
  :hook
  (go-mode . eglot-ensure)
  (before-save . gofmt-before-save)
  ;; (before-save . eglot-format-buffer)
  :general
  (my/leader-keys
    :keymaps 'go-mode-map
    "hg" 'my/godoc-package
    "hd" 'godoc)
  ;; Use smartparens post-handler to replace this function
  ;; (general-def go-mode-map
  ;;   "{" 'my/go-electric-brace)
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  ;; https://lupan.pl/dotemacs/
  (defun my/go-electric-brace ()
    "Insert an opening brace may be with the closing one.
If there is a space before the brace also adds new line with
properly indented closing brace and moves cursor to another line
inserted between the braces between the braces."
    (interactive)
    (insert "{")
    (when (looking-back " {")
      (newline)
      (indent-according-to-mode)
      (save-excursion
        (newline)
        (insert "}")
        (indent-according-to-mode))))

  (defun my/godoc-package ()
    "Display godoc for given package (with completion)."
    (interactive)
    (godoc (ivy-read "Package: " (go-packages) :require-match t))))

(setq-default eglot-workspace-configuration
              '((:gopls .
                        ((staticcheck . t)
                         (matcher . "CaseSensitive")))))


;;; Restore file-name-hander-alist
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist doom--file-name-handler-alist)))
