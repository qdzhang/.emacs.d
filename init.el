;;; init.el -*- lexical-binding: t; -*-


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

(use-package server
  :ensure nil
  :hook (after-init . server-mode))


;; Global key bindings
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (ido-mode 1)
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)
;; (global-set-key (kbd "M-i") 'imenu)

;; highlight current line
;; (global-hl-line-mode 1)
;; (add-hook 'prog-mode-hook #'hl-line-mode)
;; (add-hook 'vterm-mode-hook (lambda () (hl-line-mode -1)))

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq-default display-fill-column-indicator-column 80)
(setq-default fill-column 80)

;; Fix CJK word wrap
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29364
(setq-default word-wrap-by-category t)

;;;  Set unfill function
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'my/unfill-paragraph)


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

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

(use-package indent-info
  :init
  (global-indent-info-mode 1)
  :custom
  (indent-info-sync-from-editorconfig t)
  (indent-info-space-format "SPC[%s]")
  (indent-info-tab-format "TAB[%s]"))

(use-package shrink-path
  :ensure t
  :demand t)

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

(defface simple-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `anzu', `evil-substitute' and`iedit', etc.")

(defface simple-modeline-evil-emacs-state
  '((t (:inherit (font-lock-builtin-face bold))))
  "Face for the Emacs state tag in evil state indicator.")

(defface simple-modeline-evil-insert-state
  '((t (:inherit (font-lock-keyword-face bold))))
  "Face for the insert state tag in evil state indicator.")

(defface simple-modeline-evil-motion-state
  '((t :inherit (font-lock-doc-face bold) :slant normal))
  "Face for the motion state tag in evil state indicator.")

(defface simple-modeline-evil-normal-state
  '((t (:inherit bold)))
  "Face for the normal state tag in evil state indicator.")

(defface simple-modeline-evil-operator-state
  '((t (:inherit simple-modeline-status-success bold)))
  "Face for the operator state tag in evil state indicator.")

(defface simple-modeline-evil-visual-state
  '((t (:inherit simple-modeline-status-warning bold)))
  "Face for the visual state tag in evil state indicator.")

(defface simple-modeline-evil-replace-state
  '((t (:inherit simple-modeline-status-error bold)))
  "Face for the replace state tag in evil state indicator.")

(defface simple-modeline-project-path-face
  '((t (:inherit (mode-line-emphasis bold))))
  "Face for the project path")

;;; Some helper functions to format mode-line
(defun simple-modeline--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and RIGHT-SEGMENTS, aligned respectively."
  (let* ((left (simple-modeline--format-segments left-segments))
         (right (simple-modeline--format-segments right-segments))
         (reserve (length right)))
    (concat
     left
     (propertize " "
                 'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
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

(defun simple-modeline-segment-indent-info ()
  "Display `indent-info-mode' text in the modeline"
  (indent-info--mode-line-format))

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
    (propertize "◯"))))

(defun simple-modeline-segment-nyan ()
  "Display nyan cat in the mode-line"
  (list " "
        (nyan-create)
        " "))

(defun simple-modeline-segment-evil-indicator ()
  "Display evil tags indicator"

  (when (bound-and-true-p evil-local-mode)
    (let ((tag (evil-state-property evil-state :tag t)))
      (concat (propertize (s-trim-right tag) 'face
                          (cond ((eq tag evil-normal-state-tag) 'simple-modeline-evil-normal-state)
                                ((eq tag evil-emacs-state-tag) 'simple-modeline-evil-emacs-state)
                                ((eq tag evil-replace-state-tag) 'simple-modeline-evil-replace-state)
                                ((eq tag evil-insert-state-tag) 'simple-modeline-evil-insert-state)
                                ((eq tag evil-motion-state-tag) 'simple-modeline-evil-motion-state)
                                ((eq tag evil-visual-state-tag) 'simple-modeline-evil-visual-state)
                                ((eq tag evil-operator-state-tag) 'simple-modeline-evil-operator-state)))
              " "))))

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

(defun simple-modeline-segment-pretty-buffer-and-path ()
  "Displpay the buffer file path in shrunken way"
  (concat " "
          (propertize
           (abbreviate-file-name
            (if buffer-file-truename
                (let* ((cur-dir (file-name-directory (buffer-file-name)))
                       (one-up-dir (-as-> cur-dir it (or (f-parent it) "")))
                       (shrunk (shrink-path-file-mixed one-up-dir cur-dir (buffer-file-name))))
                  (concat (car shrunk)
                          (propertize
                           (mapconcat #'identity (butlast (cdr shrunk)) "/")
                           'face 'simple-modeline-project-path-face)
                          (propertize (car (last shrunk)) 'face 'mode-line-buffer-id)))
              (buffer-name))))
          " "))

(defun simple-modeline-rime-indicator ()
  "Display rime indicator in the mode-line"
  (rime-lighter))

(defun simple-modeline-narrowed-status ()
  "Display an indicator to show whether buffer is narrowed"
  (when (buffer-narrowed-p)
    (propertize " Narrowed " 'face 'simple-modeline-status-warning)))

(defun simple-modeline--flycheck-lighter (state)
  "Return flycheck information for the given error type STATE.

Source: https://git.io/vQKzv"
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s" err))))

;; Flycheck mode line style
;; https://www.reddit.com/r/emacs/comments/701pzr/flycheck_error_tally_in_custom_mode_line/
(defun simple-modeline-flycheck-status ()
  "Displya flycheck status in the modeline"
  '(:eval
    (when (and (bound-and-true-p flycheck-mode)
               (or flycheck-current-errors
                   (eq 'running flycheck-last-status-change)))
      (concat
       " "
       (cl-loop for state in '((error . "#FB4933")
                               (warning . "#FF8E00")
                               (info . "#83A598"))
                as lighter = (simple-modeline--flycheck-lighter (car state))
                when lighter
                concat (propertize
                        lighter
                        'face `(:foreground ,(cdr state))))
       " "))))

(defun simple-modeline-client-status ()
  "Determine whether current frame is a server/client frame"
  '(""
    (:propertize
     (""
      (:eval
       (if
           (frame-parameter nil 'client)
           "@ " "")))
     help-echo "emacsclient frame")))

(defun simple-modeline-evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and (bound-and-true-p evil-local-mode)
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face 'simple-modeline-panel)))

(defcustom simple-modeline-segments
  '((simple-modeline-segment-evil-indicator
     simple-modeline-segment-modified
     simple-modeline-segment-pretty-buffer-and-path
     simple-modeline-segment-nyan
     simple-modeline-segment-position
     simple-modeline-evil-substitute)
    (simple-modeline-client-status
     simple-modeline-rime-indicator
     simple-modeline-narrowed-status
     simple-modeline-segment-indent-info
     simple-modeline-segment-eol
     simple-modeline-segment-encoding
     simple-modeline-segment-minions-mode
     simple-modeline-segment-misc-info
     simple-modeline-segment-process
     simple-modeline-segment-vc
     simple-modeline-segment-major-mode
     simple-modeline-flycheck-status))
  "Simple modeline segments."
  :type '(list (repeat :tag "Left aligned" function)
               (repeat :tag "Right aligned" function)))

(when (not (string= "emacstty" (daemonp)))
  (setq-default mode-line-format
                (list
                 mode-line-front-space
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

(require 'imenu-list)

(require 'tmtxt-async-tasks)

(require 'hl-todo)
(global-hl-todo-mode 1)
(which-key-add-key-based-replacements "C-c t" "hl-todo")
(define-key hl-todo-mode-map (kbd "C-c t p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c t n") 'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c t o") 'hl-todo-occur)
(define-key hl-todo-mode-map (kbd "C-c t i") 'hl-todo-insert)


;; Origami is used as evil folding backend

;; Alternative:
;; - `hs-minor-mode', can be used in conjunction with [hideshowvis](https://www.emacswiki.org/emacs/hideshowvis.el)
;; - `outline-mode', can be used in conjunction with [backline](https://github.com/tarsius/backline)
;;                   and some config examples: https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/

;; Depends on fringer-helper
;; Maybe should load after evil
(require 'origami)
(global-origami-mode 1)
;; Some config can be used for reference
;; TODO: adapt some config examples
;; https://www.reddit.com/r/emacs/comments/6fmpwb/evil_and_builtin_folding/
;; https://www.reddit.com/r/emacs/comments/5ei7wa/awesome_vimlike_folding_for_evilmode_with_markers/

(require 'idle-highlight-mode)
;; TODO see highlight-overlay


;; Themes path

(add-subdirs-to-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; purp theme
;; dark theme
;; (load-theme 'purp t)
;; light theme
;; (load-theme 'purp-light t)
;; blurb theme
;; (load-theme 'blurb t)

;; (load-theme 'minimal-light t)


;;; Packages managed by use-package
;;;================================

;; Some built-in theme looks fairly well
;; (load-theme 'tango t)
;; (load-theme 'tsdh-light t)

;; (cond ((string= "emacstty" (daemonp))
;;        (load-theme 'tango-dark))
;;       (t
;;        (use-package doom-themes
;;          :config
;;          ;; Global settings (defaults)
;;          (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
;;                doom-themes-enable-italic t ; if nil, italics is universally disabled
;;                doom-themes-padded-modeline 5)
;;          (load-theme 'doom-one-light t)

;;          ;; Enable flashing mode-line on errors
;;          ;; (doom-themes-visual-bell-config)
;;          ;; Enable custom neotree theme (all-the-icons must be installed!)
;;          ;; (doom-themes-neotree-config)
;;          ;; or for treemacs users
;;          ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;          ;; (doom-themes-treemacs-config)
;;          ;; Corrects (and improves) org-mode's native fontification.
;;          (doom-themes-org-config))))

;; (use-package acme-theme
;;   :config
;;   (load-theme 'acme t))

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-valley-light t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t))

(use-package modus-themes
  :init
  (setq modus-themes-no-mixed-fonts t
        modus-themes-org-blocks 'gray-background)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi))

;; (use-package apropospriate-theme
;;   :config
;;   (load-theme 'apropospriate-light t))

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
    "bb" 'counsel-switch-buffer
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

    ;; TODO: This can use nested keystroke to invoke more commands in groups
    ;; Learn the `map!' macro of doom emacs
    "g" '(:ignore t :which-key "git")
    "gg" 'magit-status
    "gs" 'counsel-git-grep
    "gL" 'counsel-git-log

    "h" '(:ignore t :which-key "describe")
    "he" 'view-echo-area-messages
    "hf" '(counsel-describe-function :wk "describe-function")
    "hF" 'describe-face
    "hi" 'info
    "hl" 'view-lossage
    "hL" 'find-library
    "hm" 'describe-mode
    "hk" 'describe-key
    "hK" 'describe-keymap
    "hs" 'use-package-report
    "hp" 'describe-package
    "hv" '(counsel-describe-variable :wk "describe-variable")

    "m" '(:ignore t :which-key "bookmark")
    "mb" '(counsel-bookmark :wk "bookmarks")
    "md" '(counsel-bookmarked-directory :wk "bookmarks(dir)")
    "mm" 'bookmark-set
    "me" '(counsel-evil-marks :wk "evil-marks")
    "mr" '(counsel-mark-ring :wk "mark-ring")

    "o" '(:ignore t :which-key "open")
    "ob" '(my/browse-current-file :wk "open-in-browser")
    "od" '(dired-jump :wk "dired")
    "oe" 'eshell
    "oi" '(my/open-ielm-in-split-window :wk "ielm")
    "ot" '(my/open-vterm-in-split-window :wk "split-term")
    "oT" 'vterm
    ;; "oT" '(my/ansi-term-bash :wk "term")

    "s" '(:ignore t :which-key "search")
    "sB" 'swiper-all
    "sf" '((lambda ()
             (interactive)
             (let ((home-dir (expand-file-name "~/")))
               (if (equal home-dir (expand-file-name default-directory))
                   (progn (message "Current directory is HOME directory. Choose a directory first!")
                          (sit-for 0.5)
                          (counsel-find-file))
                 (counsel-fzf)))) :wk "counsel-fzf")
    "sg" 'counsel-grep-or-swiper
    "si" '(counsel-imenu :wk "imenu")
    "sr" 'counsel-rg
    "st" '(counsel-load-theme :wk "themes")
    "sy" 'ivy-yasnippet

    "t" '(:ignore t :which-key "toggle")
    "ts" 'sly
    "ti" '(imenu-list-smart-toggle :wk "imenu-list"))

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

  ;; Same function to `evil-switch-to-windows-last-buffer'
  ;; If you dont't using evil, use this function
  ;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
  (defun my/switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))


  ;; Browse current HTML file
  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el#L78

  (defun my/browse-current-file ()
    "Open the current file as a URL using `browse-url'."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (if (and (fboundp 'tramp-tramp-file-p)
               (tramp-tramp-file-p file-name))
          (error "Cannot open tramp file")
        (setq browse-url-browser-function 'browse-url-firefox)
        (browse-url (concat "file://" file-name))))))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-respect-visual-line-mode t)
  (setq isearch-lazy-count t)
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

  (setq evil-normal-state-tag   (propertize " N  " 'face 'bold)
        evil-emacs-state-tag    (propertize " E  " 'face 'bold)
        evil-insert-state-tag   (propertize " I  " 'face 'bold)
        evil-replace-state-tag  (propertize " R  " 'face 'bold)
        evil-motion-state-tag   (propertize " M  " 'face 'bold)
        evil-visual-state-tag   (propertize " V  " 'face 'bold)
        evil-operator-state-tag (propertize " O  " 'face 'bold))
  (evil-mode 1)

  ;; Add XML attributes text object
  (require 'exato)

  ;;==================================================
  ;; Evil text objects
  ;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup=evil.el
  ;;==================================================

  (evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
    "Text object to select the whole buffer."
    (evil-range (point-min) (point-max) type))

  (evil-define-text-object +evil:defun-txtobj (count &optional _beg _end type)
    "Text object to select the top-level Lisp form or function definition at
point."
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'defun)
      (evil-range beg end type)))

  (general-def
    :keymaps 'evil-inner-text-objects-map
    "f" #'+evil:defun-txtobj
    "g" #'+evil:whole-buffer-txtobj
    :keymaps 'evil-outer-text-objects-map
    "f" #'+evil:defun-txtobj
    "g" #'+evil:whole-buffer-txtobj)

  ;; Define some functions in `evil-window-map'
  (general-define-key
   :keymaps 'evil-window-map
   "e" 'doom/window-enlargen
   "g" 'my/window-split-toggle
   "m" 'my/toggle-maximize-window)

  (defun doom/window-enlargen (&optional arg)
    "Enlargen the current window to focus on this one. Does not close other
windows (unlike `doom/window-maximize-buffer'). Activate again to undo."
    (interactive "P")
    (let ((param 'doom--enlargen-last-wconf))
      (cl-destructuring-bind (window . wconf)
          (or (frame-parameter nil param)
              (cons nil nil))
        (set-frame-parameter
         nil param
         (if (and (equal window (selected-window))
                  (not arg)
                  wconf)
             (ignore
              (let ((source-window (selected-window)))
                (set-window-configuration wconf)
                (when (window-live-p source-window)
                  (select-window source-window))))
           (prog1 (cons (selected-window) (or wconf (current-window-configuration)))
             (let* ((window (selected-window))
                    (dedicated-p (window-dedicated-p window))
                    (preserved-p (window-parameter window 'window-preserved-size))
                    (ignore-window-parameters t)
                    (window-resize-pixelwise nil)
                    (frame-resize-pixelwise nil))
               (unwind-protect
                   (progn
                     (when dedicated-p
                       (set-window-dedicated-p window nil))
                     (when preserved-p
                       (set-window-parameter window 'window-preserved-size nil))
                     (maximize-window window))
                 (set-window-dedicated-p window dedicated-p)
                 (when preserved-p
                   (set-window-parameter window 'window-preserved-size preserved-p))
                 (add-hook 'doom-switch-window-hook #'doom--enlargened-forget-last-wconf-h)))))))))

  (defun my/toggle-maximize-window ()
    "Maximize current window"
    (interactive)
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows))))

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


  ;; Config `evil-shift-width' depending on `evil-indent-variable-alist'
  ;; https://github.com/tshu-w/.emacs.d/blob/05c1d6240a4ab76dccc316ff5bab40729fb39476/lisp/core-keybinds.el#L258
  (progn
    ;; Thanks to `editorconfig-emacs' for many of these
    (defvar evil-indent-variable-alist
      ;; Note that derived modes must come before their sources
      '(((awk-mode c-mode c++-mode java-mode
                   idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
        (groovy-mode . groovy-indent-offset)
        (python-mode . python-indent-offset)
        (cmake-mode . cmake-tab-width)
        (coffee-mode . coffee-tab-width)
        (cperl-mode . cperl-indent-level)
        (css-mode . css-indent-offset)
        (elixir-mode . elixir-smie-indent-basic)
        ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
        (enh-ruby-mode . enh-ruby-indent-level)
        (erlang-mode . erlang-indent-level)
        (js2-mode . js2-basic-offset)
        (js3-mode . js3-indent-level)
        ((js-mode json-mode) . js-indent-level)
        (latex-mode . (LaTeX-indent-level tex-indent-basic))
        (livescript-mode . livescript-tab-width)
        (mustache-mode . mustache-basic-offset)
        (nxml-mode . nxml-child-indent)
        (perl-mode . perl-indent-level)
        (puppet-mode . puppet-indent-level)
        (ruby-mode . ruby-indent-level)
        (rust-mode . rust-indent-offset)
        (scala-mode . scala-indent:step)
        (sgml-mode . sgml-basic-offset)
        (sh-mode . sh-basic-offset)
        (typescript-mode . typescript-indent-level)
        (web-mode . web-mode-markup-indent-offset)
        (yaml-mode . yaml-indent-offset))
      "An alist where each key is either a symbol corresponding
  to a major mode, a list of such symbols, or the symbol t,
  acting as default. The values are either integers, symbols
  or lists of these.")

    (defun set-evil-shift-width ()
      "Set the value of `evil-shift-width' based on the indentation settings of the
  current major mode."
      (let ((shift-width
             (catch 'break
               (dolist (test evil-indent-variable-alist)
                 (let ((mode (car test))
                       (val (cdr test)))
                   (when (or (and (symbolp mode) (derived-mode-p mode))
                             (and (listp mode) (apply 'derived-mode-p mode))
                             (eq 't mode))
                     (when (not (listp val))
                       (setq val (list val)))
                     (dolist (v val)
                       (cond
                        ((integerp v) (throw 'break v))
                        ((and (symbolp v) (boundp v))
                         (throw 'break (symbol-value v))))))))
               (throw 'break (default-value 'evil-shift-width)))))
        (when (and (integerp shift-width)
                   (< 0 shift-width))
          (setq-local evil-shift-width shift-width))))

    ;; after major mode has changed, reset evil-shift-width
    (add-hook 'after-change-major-mode-hook #'set-evil-shift-width 'append))

  ;; This will keep eldoc active when you are in a method and you go in insert mode.
  (with-eval-after-load 'eldoc
    (eldoc-add-command #'evil-insert)
    (eldoc-add-command #'evil-insert-line)
    (eldoc-add-command #'evil-append)
    (eldoc-add-command #'evil-append-line)
    (eldoc-add-command #'evil-force-normal-state)
    (eldoc-add-command #'evil-cp-insert)
    (eldoc-add-command #'evil-cp-insert-at-end-of-form)
    (eldoc-add-command #'evil-cp-insert-at-beginning-of-form)
    (eldoc-add-command #'evil-cp-append)))

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

  ;; Ensure a jump point is registered before jumping to new locations with ivy
  (defun +ivy--record-position-maybe-fn ()
    (with-ivy-window
      (push-mark (point-marker))))

  (push '(t . +ivy--record-position-maybe-fn)
        ivy-hooks-alist)


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
          (setq this-command 'ivy-cd)))))


  ;; Add filter functions for `ivy-occur'
  ;; https://emacs-china.org/t/ivy-occur/12083
  ;; Use `/' to filter, `C-/' undo
  ;; =======================================
  (defvar ivy-occur-filter-prefix ">>> ")

;;;###autoload
  (defun ivy-occur/filter-lines ()
    (interactive)
    (unless (string-prefix-p "ivy-occur" (symbol-name major-mode))
      (user-error "Current buffer is not in ivy-occur mode"))

    (let ((inhibit-read-only t)
          (regexp (read-regexp "Regexp(! for flush)"))
          (start (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "[0-9]+ candidates:"))))
      (if (string-prefix-p "!" regexp)
          (flush-lines (substring regexp 1) start (point-max))
        (keep-lines regexp start (point-max)))
      (save-excursion
        (goto-char (point-min))
        (let ((item (propertize (format "[%s]" regexp) 'face 'ivy-current-match)))
          (if (looking-at ivy-occur-filter-prefix)
              (progn
                (goto-char (line-end-position))
                (insert item))
            (insert ivy-occur-filter-prefix item "\n"))))))

;;;###autoload
  (defun ivy-occur/undo ()
    (interactive)
    (let ((inhibit-read-only t))
      (if (save-excursion
            (goto-char (point-min))
            (looking-at ivy-occur-filter-prefix))
          (undo)
        (user-error "Filter stack is empty"))))

  (defun ivy|occur-mode-setup ()
    (local-set-key "/" #'ivy-occur/filter-lines)
    (local-set-key (kbd "C-/") #'ivy-occur/undo))

  (add-hook 'ivy-occur-mode-hook 'ivy|occur-mode-setup)
  (add-hook 'ivy-occur-grep-mode-hook 'ivy|occur-mode-setup))

(use-package ace-window
  :general
  (my/leader-keys
    "w" 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '((?m aw-swap-window "Swap Windows")
     (?M aw-move-window "Move Window")
     (?c aw-copy-window "Copy Window")
     (?j aw-switch-buffer-in-window "Select Buffer")
     (?n aw-flip-window)
     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?c aw-split-window-fair "Split Fair Window")
     (?v aw-split-window-vert "Split Vert Window")
     (?b aw-split-window-horz "Split Horz Window")
     (?o delete-other-windows "Delete Other Windows")
     (?x ace-delete-window "Delete Window")
     (?? aw-show-dispatch-help)))
  (aw-minibuffer-flag t)
  (aw-ignore-current nil)
  (aw-display-mode-overlay t)
  (aw-background nil)
  :config
  (ace-window-display-mode -1)

  ;; Make aw-show-dispatch-help show in tabulate
  ;; https://github.com/abo-abo/ace-window/issues/172
  (defun aw-show-dispatch-help ()
    "Display action shortucts in echo area."
    (interactive)
    (let* ((action-strings
            (cl-map 'list
                    (lambda (action)
                      (cl-destructuring-bind (key fn &optional description) action
                        (format "%s: %s"
                                (propertize
                                 (char-to-string key)
                                 'face 'aw-key-face)
                                (or description fn))))
                    aw-dispatch-alist))
           ;; Ensure first col is longer for odd #, pad second col with blank element
           ;; so cl-map combining lines doesn't stop early
           (rows (ceiling (/ (length action-strings) 2.0)))
           (as1 (cl-subseq action-strings 0 rows))
           (as2 (append (cl-subseq action-strings rows) '(nil)))
           (max-first-col-width (apply 'max (cl-map 'list 'string-width as1))))
      (message (mapconcat 'identity
                          (cl-map 'list
                                  (lambda (a1 a2)
                                    (concat
                                     (truncate-string-to-width a1 max-first-col-width nil ?\s)
                                     "  " a2))
                                  as1 as2)
                          "\n")))
    ;; Prevent this from replacing any help display
    ;; in the minibuffer.
    (let (aw-minibuffer-flag)
      (mapc #'delete-overlay aw-overlays-back)
      (call-interactively 'ace-window))))

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

  (defconst my/rg-arguments
    `("--hidden"
      ;; "--no-ignore-vcs"                   ;Ignore files/dirs ONLY from `.ignore'
      "--no-heading"
      "--line-number"                     ;Line numbers
      "--smart-case"
      "--follow"                 ;Follow symlinks
      "--max-columns" "150"      ;Emacs doesn't handle long line lengths very well
      ;; "--ignore-file" ,(expand-file-name ".ignore" (cdr (project-current)))
      )
    "Default rg arguments used in the functions in `counsel' and `project'
packages.")

  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (setq counsel-rg-base-command
        (append '("rg")
                my/rg-arguments
                '("%s"
                  )))
  (setq counsel-git-cmd "rg --files")

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
                :action (lambda (x) (switch-to-buffer x)))))


  ;; Add ivy actions to counsel-fzf and counsel-rg
  ;; to make theme work seamlessly
  ;; https://gitlab.com/protesilaos/dotfiles/-/blob/v2.1.0-void/emacs/.emacs.d/emacs-init.org#h:87b37547-5941-4f20-baf6-4d00cde1151a
  (defun prot/counsel-fzf-rg-files (&optional input dir)
    "Run `fzf' in tandem with `ripgrep' to find files in the
present directory.  If invoked from inside a version-controlled
repository, then the corresponding root is used instead."
    (interactive)
    (let* ((process-environment
            (cons (concat "FZF_DEFAULT_COMMAND=rg -Sn --color never --files --no-follow --hidden -g \"!{node_modules/**,.git/**}\"")
                  process-environment))
           (vc (vc-root-dir)))
      (if dir
          (counsel-fzf input dir)
        (if (eq vc nil)
            (counsel-fzf input default-directory)
          (counsel-fzf input vc)))))

  (defun prot/counsel-fzf-dir (arg)
    "Specify root directory for `counsel-fzf'."
    (prot/counsel-fzf-rg-files ivy-text
                               (read-directory-name
                                (concat (car (split-string counsel-fzf-cmd))
                                        " in directory: "))))

  (defun prot/counsel-rg-dir (arg)
    "Specify root directory for `counsel-rg'."
    (let ((current-prefix-arg '(4)))
      (counsel-rg ivy-text nil "")))

  (defun prot/counsel-fzf-ace-window (arg)
    "Use `ace-window' on `prot/counsel-fzf-rg-files' candidate."
    (ace-window t)
    (let ((default-directory (if (eq (vc-root-dir) nil)
                                 counsel--fzf-dir
                               (vc-root-dir))))
      (if (> (length (aw-window-list)) 1)
          (find-file arg)
        (find-file-other-window arg))
      (balance-windows (current-buffer))))

  ;; Pass functions as appropriate Ivy actions (accessed via M-o)
  (ivy-add-actions
   'counsel-fzf
   '(("r" prot/counsel-fzf-dir "change root directory")
     ("g" prot/counsel-rg-dir "use ripgrep in root directory")
     ("a" prot/counsel-fzf-ace-window "ace-window switch")))

  (ivy-add-actions
   'counsel-rg
   '(("r" prot/counsel-rg-dir "change root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

  (ivy-add-actions
   'counsel-find-file
   '(("g" prot/counsel-rg-dir "use ripgrep in root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory"))))

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
  (setq company-idle-delay 0.1)
  :hook
  (after-init . global-company-mode)
  :general
  (company-active-map
   "RET" 'company-complete-selection))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook
  ((prog-mode org-mode markdown-mode text-mode snippet-mode gitignore-mode) . yas-minor-mode)
  :config
  (yas-reload-all)
  (which-key-add-key-based-replacements "C-c &" "yasnippet"))

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

  (add-hook 'minibuffer-setup-hook 'my/conditionally-enable-smartparens-mode)

  ;; https://github.com/JimMoen/Emacs-Config/blob/6d2055d3f30210819a7bf42b041f49bb71143e24/etc/ad-editing.el#L148
  (defmacro def-pairs (pairs)
    "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.
  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))
defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
    `(progn
       ,@(cl-loop for (key . val) in pairs
                  collect
                  `(defun ,(read (concat
                                  "my/sp-wrap-with-"
                                  (prin1-to-string key)
                                  "s"))
                       (&optional arg)
                     (interactive "p")
                     (sp-wrap-with-pair ,val))))))

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

(use-package visual-fill-column
  :hook
  (visual-line-mode . visual-fill-column-mode))

(use-package org
  :defer t
  :ensure org-plus-contrib
  :pin org
  :hook
  (org-mode . my-org-mode-hook)
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
  (defun my-org-mode-hook ()
    (visual-line-mode 1)
    (setq evil-auto-indent nil)
    (setq word-wrap-by-category t))

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
    "Toggle `daily.org` in a side buffer for quick note taking.  The buffer is opened in side window so it can't be accidentally removed."
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
    "Toggle FILE-PATH in a side buffer. The buffer is opened in side window so it can't be accidentally removed."
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
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . my-markdown-mode-hook)
  :config
  (add-to-list 'auto-mode-alist '("presentation.html" . markdown-mode))
  (defun my-markdown-mode-hook ()
    (visual-line-mode 1)
    (setq word-wrap-by-category t)))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)))

(use-package helpful
  :after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
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
    "db" '(my/browse-marked-file :wk "open-in-browser")
    "dc" '(tda/rsync :wk "async-rsync")
    "di" 'image-dired
    "dp" '(tda/zip :wk "async-zip")
    "du" '(tda/unzip :wk "async-unzip")
    "ds" 'xah-dired-sort
    "dw" '(wdired-change-to-wdired-mode :wk "wdired")
    "dz" '(tda/get-files-size :wk "async-files-size")
    "d /" '(my/dired-filter :wk "narrow"))
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user '(("\\.pdf\\'" "llpp")
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
                                  ("\\.jpeg\\'" "qview")
                                  ("\\.epub\\'" "llpp")
                                  ("\\.azw3\\'" "ebook-viewer")))
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-isearch-filenames 'dwim)
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-AFhlv --group-directories-first")
  :config
  (use-package dired-x
    :ensure nil)

  ;; Avoid popup `Async Shell Command' window when using `dired-do-async-shell-command'
  ;; https://emacs.stackexchange.com/questions/5553/async-shell-process-buffer-always-clobbers-window-arrangement
  (add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

  ;; Dired async
  ;; https://vxlabs.com/2018/03/30/asynchronous-rsync-with-emacs-dired-and-tramp/
  ;; https://oremacs.com/2016/02/24/dired-rsync/
  (require 'tmtxt-dired-async)

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
           (match-string 1))))))

  (defun my/browse-marked-file ()
    "Open the marked file in dired as a URL using `browse-url'."
    (interactive)
    (let ((marked-files (dired-get-marked-files nil)))
      (dolist (file-name marked-files)
        (if (and (fboundp 'tramp-tramp-file-p)
                 (tramp-tramp-file-p file-name))
            (error "Cannot open tramp file")
          (browse-url (concat "file://" file-name))))))

  (defun my/dired-find-all-marked-files (&optional arg)
    "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
    (interactive "P")
    (let* ((fn-list (dired-get-marked-files nil arg)))
      (mapc 'find-file fn-list))))

(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :general
  (dired-mode-map
   :states 'normal
   "<tab>" 'dired-subtree-toggle
   "<C-tab>" 'dired-subtree-cycle))

(use-package aggressive-indent
  :defer t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode))

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

(use-package add-node-modules-path
  :config
  (dolist (mode '(web-mode typescript-mode js-mode js2-mode css-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))

(use-package dumb-jump
  :init
  (setq dumb-jump-prefer-searcher 'rg)
  :custom
  (dumb-jump-selector 'ivy))

(use-package web-mode
  :ensure t
  :mode
  ("\\.ejs\\'" "\\.mjs\\'" "\\.hbs\\'" "\\.html\\'" "\\.php\\'" "\\.[jt]sx?\\'" "\\.vue\\'")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")
                                       ("jsx" . "\\.mjs\\'")))
  (add-to-list 'web-mode-content-types '("html" . "\\.vue\\'"))
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
    (sp-local-pair 'web-mode "<" nil :actions :rem))

  (which-key-add-key-based-replacements
    "C-c C-a" "web-mode attribute"
    "C-c C-b" "web-mode block"
    "C-c C-d" "web-mode dom"
    "C-c C-e" "web-mode element"
    "C-c C-t" "web-mode tag"))

(use-package tide
  :hook
  (web-mode . my/setup-tide-mode)
  :general
  (general-nmap
    :keymaps 'tide-mode-map
    "<f2>" 'tide-rename-symbol
    "g r" 'tide-references)
  :config
  (setq tide-completion-ignore-case t
        tide-server-max-response-length (* 1024 1024))

  (defun my/setup-tide-mode ()
    "Use hl-identifier-mode only on js or ts buffers."
    (when (and (stringp buffer-file-name)
               (string-match "\\.[tj]sx?\\'" buffer-file-name))
      ;; (setq gc-cons-threshold 100000000)
      (tide-setup)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))))

(use-package flycheck
  :defer t
  :general
  (my/leader-keys
    "of" 'flycheck-mode)
  :init
  (setq flycheck-global-modes '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                                    org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode
                                    emacs-lisp-mode)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-display-errors-delay 0.25
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode (if (display-graphic-p) 'left-fringe 'left-margin)
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  :config
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  ;; Prettify indication styles
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Use eglot with flycheck
  ;;
  ;; Thanks to Doom Emacs
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/tools/lsp/autoload/flycheck-eglot.el
  ;;
  ;; Eglot-flycheck-adaptor can also be used, but it is using eglot-checker in all major mode default.
  ;; https://github.com/akash-akya/eglot-flycheck-adaptor
  ;;
  ;; So I adopt the Doom approach

  (defvar-local +lsp--flycheck-eglot--current-errors nil)

  (defun +lsp--flycheck-eglot-init (checker callback)
    "CHECKER is the checker (eglot).
CALLBACK is the function that we need to call when we are done, on all the errors."
    (eglot-flymake-backend #'+lsp--flycheck-eglot--on-diagnostics)
    (funcall callback 'finished +lsp--flycheck-eglot--current-errors))

  (defun +lsp--flycheck-eglot--on-diagnostics (diags &rest _)
    (cl-labels
        ((flymake-diag->flycheck-err
          (diag)
          (with-current-buffer (flymake--diag-buffer diag)
            (flycheck-error-new-at-pos
             (flymake--diag-beg diag)
             (pcase (flymake--diag-type diag)
               ('eglot-note 'info)
               ('eglot-warning 'warning)
               ('eglot-error 'error)
               (_ (error "Unknown diagnostic type, %S" diag)))
             (flymake--diag-text diag)
             :end-pos (flymake--diag-end diag)
             :checker 'eglot
             :buffer (current-buffer)
             :filename (buffer-file-name)))))
      (setq +lsp--flycheck-eglot--current-errors
            (mapcar #'flymake-diag->flycheck-err diags))
      ;; Call Flycheck to update the diagnostics annotations
      (flycheck-buffer-deferred)))

  (defun +lsp--flycheck-eglot-available-p ()
    (bound-and-true-p eglot--managed-mode))

  (flycheck-define-generic-checker 'eglot
    "Report `eglot' diagnostics using `flycheck'."
    :start #'+lsp--flycheck-eglot-init
    :predicate #'+lsp--flycheck-eglot-available-p
    :modes '(prog-mode text-mode))

  (push 'eglot flycheck-checkers)

  (add-hook 'eglot-managed-mode-hook
            (defun +lsp-eglot-prefer-flycheck-h ()
              (when eglot--managed-mode
                (flymake-mode -1)
                (when-let ((current-checker (flycheck-get-checker-for-buffer)))
                  (unless (equal current-checker 'eglot)
                    (flycheck-add-next-checker 'eglot current-checker)))
                (flycheck-add-mode 'eglot major-mode)
                (flycheck-mode 1)
                ;; Call flycheck on initilization to make sure to display initial
                ;; errors
                (flycheck-buffer-deferred))))

  ;; Use eglot with flycheck end


  (defun spacemacs/enable-flycheck (mode)
    "Use flycheck in MODE by default, if `syntax-checking-enable-by-default' is
true."
    (when (and (listp flycheck-global-modes)
               (not (eq 'not (car flycheck-global-modes))))
      (add-to-list 'flycheck-global-modes mode)))

  ;; toggle flycheck window
  (defun spacemacs/toggle-flycheck-error-list ()
    "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
    (interactive)
    (-if-let (window (flycheck-get-error-list-window))
        (quit-window nil window)
      (flycheck-list-errors)))

  (defun spacemacs/goto-flycheck-error-list ()
    "Open and go to the error list buffer."
    (interactive)
    (if (flycheck-get-error-list-window)
        (switch-to-buffer flycheck-error-list-buffer)
      (progn
        (flycheck-list-errors)
        (switch-to-buffer-other-window flycheck-error-list-buffer))))

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; If find gloabl `eslint_d', use it
  ;; npm install -g eslint_d
  ;; (when (executable-find "eslint_d")
  ;;   (setq flycheck-javascript-eslint-executable "eslint_d"))

  ;; Use json-jq for json files
  (define-derived-mode my-json-mode web-mode "MyJSON"
    "My custom JSON mode")
  (add-to-list 'auto-mode-alist '("\\.json\\'" . my-json-mode))

  (flycheck-add-mode 'json-jq 'my-json-mode)

  (flycheck-add-mode 'css-stylelint 'css-mode)
  (setq flycheck-stylelintrc ".stylelintrc.json"))

(use-package flycheck-inline
  :after flycheck
  :hook
  (global-flycheck-mode . flycheck-inline-mode)
  (flycheck-mode . flycheck-inline-mode))

(use-package prettier
  ;; :hook
  ;; ((web-mode css-mode my-json-mode) . prettier-mode)
  :general
  (my/leader-keys
    "bp" '(my/prettier-prettify-and-message :wk "prettier-buffer"))
  :config
  (defun my/prettier-prettify-and-message ()
    (interactive)
    (prettier-prettify)
    (message "Prettier prettify done")))

;; DONE config built-in project or projectile
(use-package project
  :ensure nil
  :general
  (my/leader-keys
    "p" '(:ignore t :which-key "project")
    "p!" 'project-shell-command
    "p&" 'project-async-shell-command
    "pb" 'project-switch-to-buffer
    "pd" 'project-dired
    "pf" 'project-find-file
    "pg" 'project-find-regexp
    "pk" 'project-kill-buffers
    "pp" 'project-switch-project
    "pr" '(my/counsel-rg-project :wk "project-rg")
    "pt" '(my/counsel-rg-extra-glob-project :wk "project-filetype-rg"))
  :config

  ;; More example about add a new file to specify project root
  ;; https://www.reddit.com/r/emacs/comments/lfbyq5/specifying_projectroot_in_projectel/

  ;; Declare directories with "go.mod" as a project
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (defun my/project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  ;; Some other extending of project.el
  ;; https://www.manueluberti.eu//emacs/2020/11/14/extending-project/

  ;; Declare directories with ".project" as a project
  (cl-defmethod project-root ((project (head local)))
    (cdr project))

  (defun my/project-try-local (dir)
    "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
    (let ((root (locate-dominating-file dir ".project")))
      (and root (cons 'local root))))

  (add-hook 'project-find-functions #'my/project-find-go-module)
  (add-hook 'project-find-functions #'my/project-try-local)


  (defun my--project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my--project-files-in-directory
            (or dirs (list (project-root project)))))

  ;; Add the command `project-switch-to-buffer' when using `project-switch-project'
  (add-to-list 'project-switch-commands '(?b "Switch buffer" project-switch-to-buffer))


  (defun my/counsel-rg-project ()
    "use `counsel-rg' to search for the word in the project"
    (interactive)
    (let ((root default-directory)
          (project (project-current)))
      (when project
        (setq root (cdr project)))
      (when root
        (counsel-rg nil root))))

  (defun my/counsel-rg-extra-glob-project ()
    "use `counsel-rg' to search for the word with extra glob in the project.
Search for specify filetype.
Reference: https://philjackson.github.io//emacs/search/rg/2021/06/25/search-specific-extensions-with-counsel-projectile-rg/"
    (interactive)
    (let ((root default-directory)
          (project (project-current))
          (glob (ivy-completing-read "Glob?: " '("*.md"
                                                 "*.org"
                                                 "*.css"
                                                 "*.js"
                                                 "*.html"
                                                 "*.jsx"
                                                 "*.json"
                                                 "*.el"))))
      (when project
        (setq root (cdr project)))
      (when root
        (counsel-rg nil root (concat "--glob " glob))))))


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

(use-package lua-mode
  :defer t)

;; Takes a color string like #ffe0e0 and returns a light
;; or dark foreground color to make sure text is readable.
(defun fg-from-bg (bg)
  (let* ((avg (/ (+ (string-to-number (substring bg 1 3) 16)
                    (string-to-number (substring bg 3 5) 16)
                    (string-to-number (substring bg 5 7) 16)
                    ) 3)))
    (if (> avg 128) "#000000" "#ffffff")))

;; Improved from http://ergoemacs.org/emacs/emacs_CSS_colors.html
;; * Avoid mixing up #abc and #abcabc regexps
;; * Make sure dark background have light foregrounds and vice versa
(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `https://github.com/mariusk/emacs-color'
Version 2016-08-09"
  (interactive)
  (font-lock-add-keywords
   nil
   '(
     ("#[ABCDEFabcdef[:digit:]]\\{6\\}"
      (0 (progn (let* ((bgstr (match-string-no-properties 0))
                       (fgstr (fg-from-bg bgstr)))
                  (put-text-property
                   (match-beginning 0)
                   (match-end 0)
                   'face (list :background bgstr :foreground fgstr))))))
     ("#[ABCDEFabcdef[:digit:]]\\{3\\}[^ABCDEFabcdef[:digit:]]"
      (0 (progn (let* (
                       (ms (match-string-no-properties 0))
                       (r (substring ms 1 2))
                       (g (substring ms 2 3))
                       (b (substring ms 3 4))
                       (bgstr (concat "#" r r g g b b))
                       (fgstr (fg-from-bg bgstr)))
                  (put-text-property
                   (match-beginning 0)
                   (- (match-end 0) 1)
                   'face (list :background bgstr :foreground fgstr)
                   )))))
     ))
  (font-lock-fontify-buffer))

;; Following hook is an anternative for `rainbow-mode'
;; Use one of theme
;; (setq my/syntax-color-hex
;;       '(prog-mode-hook
;;         org-mode-hook))
;; (dolist (hook my/syntax-color-hex)
;;   (add-hook hook 'xah-syntax-color-hex))

(use-package rainbow-mode
  :defer t
  :hook
  ((prog-mode helpful-mode org-mode conf-mode) . rainbow-mode)
  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'emacs-lisp-mode)
  (add-to-list 'rainbow-html-colors-major-mode-list 'conf-mode))

(use-package auto-rename-tag
  :defer t
  :hook
  (web-mode . auto-rename-tag-mode))

(use-package link-hint
  :defer t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link)
  :init
  (which-key-add-key-based-replacements "C-c l" "link-hint"))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package abn-funcs-benchmark
  :ensure nil)

;;; Restore file-name-hander-alist
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist doom--file-name-handler-alist)))
