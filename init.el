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
 ;; (set-face-attribute 'default nil :font (font-spec :family "Sarasa Mono SC" :size 24))
 (set-face-attribute 'default nil :font (font-spec :family "M+ 1mn" :size 23))
 (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")
 (set-face-attribute 'fixed-pitch nil :family "Sarasa Mono SC")
 (set-fontset-font t 'han "Sarasa Mono SC")
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

;;; A macro to determine system type
;;; https://stackoverflow.com/a/26137517
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))


(require 'package)

;; DONE: Org ELPA will be shutting down and Org contrib will be moving to NonGNU ELPA
;; Keep an eye on the changes of mirrors
(setq package-archives '(("gnu"   . "https://mirrors.bfsu.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.bfsu.edu.cn/elpa/nongnu/")
                         ("melpa" . "https://mirrors.bfsu.edu.cn/elpa/melpa/"))
      package-archive-priorities '(("gnu" . 10)
                                   ("melpa" . 5)
                                   ("nongnu" . 1)))
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
(add-subdirs-to-load-path (expand-file-name "~/.emacs.d/site-lisp/"))
(add-subdirs-to-load-path (expand-file-name "~/.emacs.d/config/"))

(require 'init-proxy)

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
  :commands (server-running-p)
  :hook (after-init . (lambda() (unless (server-running-p) (server-start)))))


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

;; (use-package nyan-mode
;;   :init
;;   (setq nyan-animate-nyancat t)
;;   (setq nyan-wavy-trail t)
;;   :config
;;   (setq nyan-minimum-window-width 75)
;;   (setq nyan-bar-length 25))

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
        (8 " %l:%c ")
        (8 " %l:%C "))
       (5 " L%l ")))
     ((column-number-mode
       (column-number-indicator-zero-based
        (5 " C%c ")
        (5 " C%C ")))))
    ,(if (region-active-p)
         (propertize (format "+%s"
                             (apply #'+ (mapcar
                                         (lambda (pos)
                                           (- (cdr pos)
                                              (car pos)))
                                         (region-bounds))))
                     'font-lock-face 'font-lock-variable-name-face))))

(defun simple-modeline-segment-percent-location ()
  "Return the percent location info.
Reference: https://github.com/AmaiKinono/Tokimacs/blob/master/site-lisp/toki-modeline.el"
  (let ((percent (format-mode-line "%p")))
    (setq percent
          (pcase percent
            ("All" "All%")
            ("Top" " 0%")
            ("Bottom" "100%")
            (val val)))
    (concat " " percent "%%%" " ")))

(defvar-local simple-modeline--pdf-pages nil)
(defun doom-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq simple-modeline--pdf-pages
        (format " P%d/%d "
                (or (pdf-view-current-page) 0)
                (pdf-cache-number-of-pages))))
(add-hook 'pdf-view-change-page-hook #'doom-modeline-update-pdf-pages)

(defun simple-modeline-pdf-pages ()
  "Display PDF pages."
  (when (eq major-mode 'pdf-view-mode)
    (propertize simple-modeline--pdf-pages
                'face 'mode-line-highlight)))

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

(defun simple-modeline-rbenv-ruby-version ()
  "Display rbenv ruby version in the mode-line"
  '(:eval (when (eq major-mode 'ruby-mode)
            (rbenv--modeline-with-face (rbenv--active-ruby-version)))))


;; The mode line segment shows current python executable
;; hover text is the full path
(defun simple-modeline-venv-python-version ()
  "Show current python executable, hover text is the full path"
  '(:eval (if (eq major-mode 'python-mode)
              (if pyvenv-virtual-env
                  (propertize (concat " [" pyvenv-virtual-env-name "]")
                              'help-echo (format "%s" pyvenv-virtual-env-path-directories))
                (propertize " [system]"
                            'help-echo (executable-find python-shell-interpreter))))))

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
                       (shrunk (shrink-path-file-mixed one-up-dir cur-dir (buffer-file-name)))
                       (full-file-name (car (last shrunk)))
                       (len-file-name (length full-file-name))
                       (shrunk-file-name (if (> len-file-name 40)
                                             (concat (substring full-file-name 0 20)
                                                     "..."
                                                     (substring full-file-name (- len-file-name 7) len-file-name))
                                           full-file-name)))
                  (concat (car shrunk)
                          (propertize
                           (mapconcat #'identity (butlast (cdr shrunk)) "/")
                           'face 'simple-modeline-project-path-face)
                          (propertize shrunk-file-name 'face 'mode-line-buffer-id)))
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
       (if (frame-parameter nil 'client)
           (propertize (concat "@" server-name " ")
                       'face 'simple-modeline-status-info))))
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

(defun doom-modeline-themes--overlay-sort (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defun simple-modeline-iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (save-excursion (iedit-prev-occurrence)
                                        (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'doom-modeline-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face 'simple-modeline-panel)))


(defcustom simple-modeline-segments
  '((simple-modeline-segment-evil-indicator
     simple-modeline-segment-modified
     simple-modeline-segment-pretty-buffer-and-path
     simple-modeline-segment-percent-location
     ;; simple-modeline-segment-nyan
     simple-modeline-pdf-pages
     simple-modeline-segment-position
     simple-modeline-evil-substitute
     simple-modeline-iedit)
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
     simple-modeline-rbenv-ruby-version
     simple-modeline-venv-python-version
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


;;;=============================
;;; Mode-line configurations end


;; Line number
(global-linum-mode 0)
;; (global-display-line-numbers-mode 1)
(column-number-mode)  ; Display line number in the mode line
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-type 'visual)
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
;; (better-pixel-scroll-mode) ;; This function needs Emacs 29

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

;;; When changing window or some other operator happenning,
;;; pulse highlight current line
(defun my/pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

;; (dolist (command '(scroll-up-command scroll-down-command
;;                                      recenter-top-bottom other-window
;;                                      evil-window-next ace-window))
;;   (advice-add command :after #'my/pulse-line))


;;; Pulse highlight evil yank
;;; https://blog.meain.io/2020/emacs-highlight-yanked/
;;; Also see: https://github.com/k-talo/volatile-highlights.el
;;; or https://github.com/edkolev/evil-goggles
;;; If no evil, see https://github.com/minad/goggles
(defun my/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(advice-add 'evil-yank :around 'my/evil-yank-advice)


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
(setq-default indent-tabs-mode nil)

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

(require 'idle-highlight-mode)
;; TODO see highlight-overlay


;; Themes path

(add-subdirs-to-load-path (expand-file-name "~/.emacs.d/themes/"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

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

(if (not (string= "emacstty" (daemonp)))
    (use-package modus-themes
      :init
      (setq modus-themes-no-mixed-fonts t
            modus-themes-org-blocks 'gray-background)
      ;; Override colors from
      ;; https://www.reddit.com/r/emacs/comments/rn6qz0/comment/hprlsrl/?utm_source=share&utm_medium=web2x&context=3
      (setq modus-themes-vivendi-color-overrides '((fg-main . "#fdf3ec")
                                                   (bg-main . "#282828")
                                                   (bg-tab-active . "#24242d")
                                                   (bg-paren-match . "#E02C6D")
                                                   (bg-region . "#4f3d88")
                                                   (bg-inactive . "#32302f")
                                                   (bg-hl-line . "#2f2f3b")))

      (modus-themes-load-themes)
      :config
      ;; (modus-themes-load-operandi)

      (setq modus-themes-syntax '(yellow-comments faint alt-syntax green-strings))

      (defun my-modus-themes-custom-faces ()
        (pcase (modus-themes--current-theme)
          ('modus-operandi
           (modus-themes-with-colors
             (custom-set-faces
              `(cursor ((,class :background ,fg-main))))))
          ('modus-vivendi
           (modus-themes-with-colors
             (custom-set-faces
              `(cursor ((,class :background ,fg-main)))
              `(mode-line-inactive ((,class :box ,bg-hl-line))))))))

      (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

      (defun my/customize-modus-vivendi nil
        (setq modus-themes-syntax '(yellow-comments faint alt-syntax green-strings)))
      (defun my/customize-modus-operandi nil
        (setq modus-themes-syntax '(yellow-comments)))
      (advice-add 'modus-themes-load-vivendi :before 'my/customize-modus-vivendi)
      (advice-add 'modus-themes-load-operandi :before 'my/customize-modus-operandi))
  (use-package color-theme-sanityinc-tomorrow
    :config
    (load-theme 'sanityinc-tomorrow-eighties t)))

(use-package heaven-and-hell
  :init
  (setq heaven-and-hell-themes
        '((light . modus-operandi)
          (dark . modus-vivendi))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))

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
    :global-prefix "C-,")

  (my/leader-keys
    "SPC" '(counsel-M-x :which-key "execute command")
    "TAB" 'evil-indent-line
    ";" '(my/sp-comment-with-two-semicolon :wk "sp-comment")

    "b" '(:ignore t :which-key "buffer")
    "br"  'revert-buffer
    "bR" '(my/rename-file-and-buffer :wk "rename-file")
    "bs" '((lambda () (interactive)
             (pop-to-buffer "*scratch*"))
           :wk "scratch")
    "bd" 'kill-current-buffer
    "bb" 'ivy-switch-buffer
    "bi" 'counsel-ibuffer
    "bk" 'kill-current-buffer
    "bl" '(my/switch-to-previous-buffer :wk "last buffer")

    "a" '(:ignore t :which-key "append")
    "a;" '(my/semicolon-at-end-of-line :which-key "semicolon(end)")
    "ac" 'capitalize-word
    "ad" 'downcase-word
    "ag" '(my/sp-comment-with-three-semicolon :wk "sp-comment-3-semicolon")
    "ai" 'auto-insert
    "au" 'upcase-word

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
    "fr" '(my/counsel-recentf :wk "counsel-recentf")
    "fn" '(my/org-journal-next-day-file :wk "next file")
    "fp" '(my/org-journal-previous-day-file :wk "previous file")

    ;; TODO: This can use nested keystroke to invoke more commands in groups
    ;; Learn the `map!' macro of doom emacs
    "g" '(:ignore t :which-key "git")
    "gg" 'magit-status
    "gf" 'magit-file-dispatch
    "gs" 'counsel-git-grep
    "gL" 'counsel-git-log

    "h" '(:ignore t :which-key "describe")
    "hb" 'describe-bindings
    "he" 'view-echo-area-messages
    "hf" '(counsel-describe-function :wk "describe-function")
    "hF" 'describe-face
    "hi" 'info
    "hl" 'view-lossage
    "hL" 'find-library
    "hm" 'describe-mode
    "hk" 'describe-key
    "hK" '(my/describe-keymap :wk "describe-keymap")
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
    "og" '(google-search :wk "google")
    "oi" '(my/open-ielm-in-split-window :wk "ielm")
    "om" 'man
    ;; "ot" '(my/open-vterm-in-split-window :wk "split-term")
    ;; "oT" '(my/open-vterm-in-new-tab :wk "vterm")
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
    "sm" '(counsel-semantic-or-imenu :wk "semantic/imenu")
    "sr" 'counsel-rg
    "st" '(counsel-load-theme :wk "themes")
    "sy" 'ivy-yasnippet

    "t" '(:ignore t :which-key "toggle")
    "tl" 'display-line-numbers-mode
    "ts" 'sly
    "ti" '(imenu-list-smart-toggle :wk "imenu-list")

    ;; Inspired by https://emacs-china.org/t/topic/20504
    "x" '(:ignore t :which-key "execute")
    "x0" 'delete-window
    "x1" 'delete-other-windows
    "xx" 'kill-buffer-and-window
    "-" 'split-window-horizontally
    "/" 'split-window-vertically)

  (defun my/open-vterm-in-new-tab ()
    "Open vterm in new tab"
    (interactive)
    (tab-bar-new-tab)
    (multi-vterm))

  ;; https://stackoverflow.com/a/36994486
  (defun my/describe-keymap (keymap)
    "Describe a keymap using `substitute-command-keys'."
    (interactive
     (list (completing-read
            "Keymap: " (let (maps)
                         (mapatoms (lambda (sym)
                                     (and (boundp sym)
                                          (keymapp (symbol-value sym))
                                          (push sym maps))))
                         maps)
            nil t)))
    (with-output-to-temp-buffer (format "*keymap: %s*" keymap)
      (princ (format "%s\n\n" keymap))
      (princ (substitute-command-keys (format "\\{%s}" keymap)))
      (with-current-buffer standard-output ;; temp buffer
        (setq help-xref-stack-item (list #'my/describe-keymap keymap)))))

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

;;;###autoload
  (defun my/counsel-recentf ()
    "Find a file on `recentf-list'."
    (interactive)
    (require 'recentf)
    (recentf-mode)
    (ivy-read "Recentf: " (mapcar 'abbreviate-file-name (counsel-recentf-candidates))
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :require-match t
              :caller 'counsel-recentf))

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
        (browse-url (concat "file://" file-name)))))

  ;; Search google for the keywords from minibuffer
  ;; https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:01E26AB9-2829-4076-9665-E218832FB1A3
  (defun google-search-str (str)
    (browse-url
     (concat "https://www.google.com/search?q=" str)))
  (defun google-search ()
    "Google search region, if active, or ask for search string."
    (interactive)
    (if (region-active-p)
        (google-search-str
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
      (google-search-str (read-from-minibuffer "Google Search: "))))

  ;; https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
  (defun my/sudo-edit (&optional arg)
    "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (ido-read-file-name "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-respect-visual-line-mode t)
  (setq isearch-lazy-count t)
  (setq evil-move-beyond-eol t)
  (setq evil-highlight-closing-paren-at-point-states nil)
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

  (general-define-key
   :keymaps 'evil-insert-state-map
   "C-]" 'up-list)

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

(use-package better-jumper
  :after evil
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (better-jumper-mode 1)
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "<C-i>") 'better-jumper-jump-forward)

  (defun evil-better-jumper/set-jump-a (orig-fn &rest args)
    "Set a jump point and ensure ORIG-FN doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply orig-fn args)))

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  (advice-add #'kill-current-buffer :around #'evil-better-jumper/set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'evil-better-jumper/set-jump-a))

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package embrace
  :defer t)

(use-package evil-embrace
  :after (evil evil-surround)
  :commands embrace-add-pair embrace-add-pair-regexp
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (LaTeX-mode . +evil-embrace-latex-mode-hook-h)
  :hook (org-mode . embrace-org-mode-hook)
  :hook (ruby-mode . embrace-ruby-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook ((lisp-mode emacs-lisp-mode clojure-mode racket-mode hy-mode)
         . +evil-embrace-lisp-mode-hook-h)
  :hook ((c++-mode rustic-mode csharp-mode java-mode swift-mode typescript-mode)
         . +evil-embrace-angle-bracket-modes-hook-h)
  :hook (scala-mode . +evil-embrace-scala-mode-hook-h)
  :init
  (evil-embrace-enable-evil-surround-integration)
  :config
  (setq evil-embrace-show-help-p nil)

;;;###autoload
  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
        pair
      (if-let* ((pair (assoc-default char embrace--pairs-list)))
          (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                    (funcall (embrace-pair-struct-read-function pair)))))
              real-pair
            (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
        (cons char char))))

;;;###autoload
  (defun +evil--embrace-escaped ()
    "Backslash-escaped surround character support for embrace."
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair (string char)))
              (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
          (cons (format text (car pair))
                (format text (cdr pair)))))))

;;;###autoload
  (defun +evil--embrace-latex ()
    "LaTeX command support for embrace."
    (cons (format "\\%s{" (read-string "\\")) "}"))

;;;###autoload
  (defun +evil--embrace-elisp-fn ()
    "Elisp function support for embrace."
    (cons (format "(%s " (or (read-string "(") "")) ")"))

;;;###autoload
  (defun +evil--embrace-angle-brackets ()
    "Type/generic angle brackets."
    (cons (format "%s<" (or (read-string "") ""))
          ">"))

  (defun +evil-embrace-scala-mode-hook-h ()
    (embrace-add-pair ?$ "${" "}"))

  (defun +evil-embrace-latex-mode-hook-h ()
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))

  (defun +evil-embrace-lisp-mode-hook-h ()
    ;; Avoid `embrace-add-pair-regexp' because it would overwrite the default
    ;; `f' rule, which we want for other modes
    (push (cons ?f (make-embrace-pair-struct
                    :key ?f
                    :read-function #'+evil--embrace-elisp-fn
                    :left-regexp "([^ ]+ "
                    :right-regexp ")"))
          embrace--pairs-list))

  (defun +evil-embrace-angle-bracket-modes-hook-h ()
    (let ((var (make-local-variable 'evil-embrace-evil-surround-keys)))
      (set var (delq ?< evil-embrace-evil-surround-keys))
      (set var (delq ?> evil-embrace-evil-surround-keys)))
    (embrace-add-pair-regexp ?< "\\_<[a-z0-9-_]+<" ">" #'+evil--embrace-angle-brackets)
    (embrace-add-pair ?> "<" ">"))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]")))


;;==================
;; Evil settings end

(use-package ivy
  :diminish
  :init (ivy-mode 1)
  :bind (("C-s" . nil)
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
  :general
  (my/leader-keys
    "sj" '(+ivy/jump-list :wk "jump-list"))
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

  (evil-define-key 'normal ivy-occur-mode-map
    "/" 'ivy-occur/filter-lines
    "C-/" 'ivy-occur/undo)

  (evil-define-key 'normal ivy-occur-grep-mode-map
    "/" 'ivy-occur/filter-lines
    "C-/" 'ivy-occur/undo)


;;;###autoload
  (defun +ivy/jump-list ()
    "Go to an entry in evil's (or better-jumper's) jumplist."
    (interactive)
    ;; REVIEW Refactor me
    (let (buffers)
      (unwind-protect
          (ivy-read "jumplist: "
                    (nreverse
                     (delete-dups
                      (delq
                       nil
                       (mapcar (lambda (mark)
                                 (when mark
                                   (cl-destructuring-bind (path pt _id) mark
                                     (let ((buf (get-file-buffer path)))
                                       (unless buf
                                         (push (setq buf (find-file-noselect path t))
                                               buffers))
                                       (with-current-buffer buf
                                         (goto-char pt)
                                         (font-lock-fontify-region (line-beginning-position) (line-end-position))
                                         (cons (format "%s:%d: %s"
                                                       (buffer-name)
                                                       (line-number-at-pos)
                                                       (string-trim-right (or (thing-at-point 'line) "")))
                                               (point-marker)))))))
                               (cddr (better-jumper-jump-list-struct-ring
                                      (better-jumper-get-jumps (better-jumper--get-current-context))))))))
                    :sort nil
                    :require-match t
                    :action (lambda (cand)
                              (let ((mark (cdr cand)))
                                (setq buffers
                                      (delq
                                       (marker-buffer mark)
                                       buffers))
                                (mapc #'kill-buffer buffers)
                                (setq buffers nil)
                                (with-current-buffer (switch-to-buffer (marker-buffer mark))
                                  (goto-char (marker-position mark)))))
                    :caller '+ivy/jump-list)
        (mapc #'kill-buffer buffers))))


  ;; Integrate `ivy' with `better-jumper'; ensure a jump point is registered
  ;; before jumping to new locations with ivy
  (setf (alist-get 't ivy-hooks-alist)
        (lambda ()
          (with-ivy-window
            (setq +ivy--origin (point-marker))))))

(use-package ace-window
  :general
  ("M-p" 'ace-window)
  :custom
  (aw-keys '(?h ?j ?k ?l ?y ?u ?i ?o ?p))
  (aw-scope 'frame)
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '((?m aw-swap-window "Swap Windows")
     (?M aw-move-window "Move Window")
     (?c aw-copy-window "Copy Window")
     (?b aw-switch-buffer-in-window "Select Buffer")
     (?n aw-flip-window)
     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?f aw-split-window-fair "Split Fair Window")
     (?2 aw-split-window-vert "Split Vert Window")
     (?3 aw-split-window-horz "Split Horz Window")
     (?0 delete-other-windows "Delete Other Windows")
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
    "si" '(my/counsel-rg-in :wk "rg-in-dir")
    "bj" '(my/counsel-all-opened-dired-buffer :wk "jump-dired"))
  :config

  (defconst my/rg-arguments
    `("--hidden"
      ;; "--no-ignore-vcs"     ;Ignore files/dirs ONLY from `.ignore'
      "--no-heading"
      "--line-number"       ;Line numbers
      "--smart-case"
      "--follow"            ;Follow symlinks
      "--max-columns" "150" ;Emacs doesn't handle long line lengths very well
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
      (ivy-read "directories:" (mapcar 'abbreviate-file-name collection)
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
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")
     ("a" prot/counsel-fzf-ace-window "ace-window switch")))

  (defun my/counsel-rg-in ()
    "Use `counsel-rg' in specifed directories"
    (interactive)
    (counsel-rg nil (read-directory-name "rg in: ") "")))

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
  :init

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

  :config
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
    "T" 'evil-avy-find-char-to-backward)

  (evil-define-key 'normal 'global
    (kbd "C-s") 'evil-avy-goto-char-timer)

  (evil-define-key 'normal 'global
    (kbd "M-g l") 'evil-avy-goto-line)

  ;;===========================
  ;; Config avy dispatch list
  ;; https://karthinks.com/software/avy-can-do-anything/
  ;;===========================
  (defun my/avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun my/avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun my/avy-goto-char-this-window (&optional arg)
    "Goto char in this window with hints."
    (interactive "P")
    (let ((avy-all-windows)
          (current-prefix-arg (if arg 4)))
      (call-interactively 'avy-goto-char)))

  (defun my/avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun my/avy-action-yank-whole-line (pt)
    (my/avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun my/avy-action-teleport-whole-line (pt)
    (my/avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setq avy-dispatch-alist '((?k . avy-action-kill-move)
                             (?K . avy-action-kill-stay)
                             (?x . my/avy-action-kill-whole-line)
                             (?t . avy-action-teleport)
                             (?m . avy-action-mark)
                             (?  . my/avy-action-mark-to-char)
                             (?w . avy-action-copy)
                             (?W . my/avy-action-copy-whole-line)
                             (?y . avy-action-yank)
                             (?Y . my/avy-action-yank-whole-line)
                             (?i . avy-action-ispell)
                             (?z . avy-action-zap-to-char))))

(use-package ivy-avy
  :after (ivy avy))

(use-package company
  :diminish
  :init
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode markdown-mode))
  :hook
  (after-init . global-company-mode)
  :config
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map [return] 'company-complete-selection))

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
      (evil-collection-vterm-insert-line)))

  ;; https://www.reddit.com/r/emacs/comments/bcpexy/comment/eksftnx/?utm_source=share&utm_medium=web2x&context=3
  (defun my/magit-log-visit-changed-file ()
    "Visit a changed file of revision under point in `magit-log-mode'.

Uses `general-simulate-key', so `general-simulate-RET' will
become defined after invocation."
    (interactive)
    (general-simulate-key "RET")
    ;; visit the commit
    (general-simulate-RET)
    ;; move to first changed file in diff buffer
    (setf (point) (point-min))
    (search-forward "|" nil t)
    ;; open the revision
    (general-simulate-RET))

  (general-define-key
   :keymaps '(magit-log-mode-map)
   :states 'normal
   "C-<return>" #'my/magit-log-visit-changed-file))

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
  :config
  (setq show-smartparens-global-mode nil)
  (setq show-smartparens-mode nil))

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

  ;; Remove ) auto pair in web-mode
  (sp-local-pair 'sh-mode "(" nil :actions nil)

  ;; Add comments quote in c-mode
  (sp-local-pair '(c-mode c++-mode objc-mode java-mode)
                 "/*!" "*/"
                 :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))

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

(use-package paren
  :ensure nil
  :config
  ;; Show matched parens overlay offscreen
  ;; https://with-emacs.com/posts/ui-hacks/show-matching-lines-when-parentheses-go-off-screen/
  ;; we will call `blink-matching-open` ourselves...
  (remove-hook 'post-self-insert-hook
               #'blink-paren-post-self-insert-function)
  ;; this still needs to be set for `blink-matching-open` to work
  (setq blink-matching-paren 'show)

  (let ((ov nil))                       ; keep track of the overlay
    (advice-add
     #'show-paren-function
     :after
     (defun show-paren--off-screen+ (&rest _args)
       "Display matching line for off-screen paren."
       (when (overlayp ov)
         (delete-overlay ov))
       ;; check if it's appropriate to show match info,
       ;; see `blink-paren-post-self-insert-function'
       (when (and (overlay-buffer show-paren--overlay)
                  (not (or cursor-in-echo-area
                           executing-kbd-macro
                           noninteractive
                           (minibufferp)
                           this-command))
                  (and (not (bobp))
                       (memq (char-syntax (char-before)) '(?\) ?\$)))
                  (= 1 (logand 1 (- (point)
                                    (save-excursion
                                      (forward-char -1)
                                      (skip-syntax-backward "/\\")
                                      (point))))))
         ;; rebind `minibuffer-message' called by
         ;; `blink-matching-open' to handle the overlay display
         (cl-letf (((symbol-function #'minibuffer-message)
                    (lambda (msg &rest args)
                      (let ((msg (apply #'format-message msg args)))
                        (setq ov (display-line-overlay+
                                  (window-start) msg ))))))
           (blink-matching-open))))))

  (defun display-line-overlay+ (pos str &optional face)
    "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
    (let ((ol (save-excursion
                (goto-char pos)
                (make-overlay (line-beginning-position)
                              (line-end-position)))))
      (overlay-put ol 'display str)
      (overlay-put ol 'face
                   (or face '(:inherit default :inherit highlight)))
      ol))

  (setq show-paren-delay 0.03
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren nil
        show-paren-when-point-in-periphery t)
  (show-paren-mode 1))

(use-package terminal-here
  :bind
  ("C-<f12>" . terminal-here-launch)
  :config
  (setq terminal-here-linux-terminal-command 'urxvt))

(use-package visual-fill-column
  :hook
  (visual-line-mode . visual-fill-column-mode))

(use-package org
  :ensure nil
  :defer t
  :hook
  (org-mode . my-org-mode-hook)
  (org-mode . (lambda ()
                (push '(?= . ("=" . "=")) evil-surround-pairs-alist)))
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
  ;; ensure TAB is bound to org-cycle in normal mode
  (with-eval-after-load 'evil
    (evil-define-key 'normal outline-mode-map (kbd "<tab>") #'org-cycle)
    (evil-define-key 'normal outline-mode-map (kbd "TAB") #'org-cycle))

  (defun my-org-mode-hook ()
    (visual-line-mode 1)
    (org-phscroll-mode 1)
    (setq evil-auto-indent nil)
    (setq word-wrap-by-category t))

  (setq org-default-notes-file "~/org/notes.org"
        org-agenda-files '("~/org/agenda.org")
        org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
        ;; org-ellipsis "  "
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

  (defface org-bold
    '((t :foreground "#d2268b"
         :background "#fefefe"
         :weight extra-bold
         :underline t
         :overline nil))
    "Face for org-mode bold."
    :group 'org-faces)

  (setq org-emphasis-alist
        '(("*" org-bold)
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+"
           (:strike-through t))))

  (setq org-startup-truncated nil)
  (require 'org-phscroll)

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


  ;;=================================================================================
  ;; Replace org checkbox to unicode symbol and add strike through line
  ;; https://jft.home.blog/2019/07/17/use-unicode-symbol-to-display-org-mode-checkboxes/
  ;; https://www.reddit.com/r/emacs/comments/brt0sk/prettifysymbolsmode_is_so_cool/
  ;;=================================================================================
  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                            "Beautify Org Checkbox Symbol"
  ;;                            (push '("[ ]" .  "☐") prettify-symbols-alist)
  ;;                            (push '("[X]" . "☑" ) prettify-symbols-alist)
  ;;                            (push '("[-]" . "❍" ) prettify-symbols-alist)
  ;;                            (prettify-symbols-mode)))
  ;; (defface org-checkbox-done-text
  ;;   '((t :inherit (font-lock-comment-face)
  ;;        :strike-through t))
  ;;   "Face for the text part of a checked org-mode checkbox.")

  ;; (font-lock-add-keywords
  ;;  'org-mode
  ;;  `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
  ;;     1 'org-checkbox-done-text prepend))
  ;;  'append)

  ;;=================================================================================

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
;; (with-eval-after-load 'org
;;   (defvar-local my/org-at-src-begin -1
;;     "Variable that holds whether last position was a ")

;;   (defvar my/ob-header-symbol ?…
;;     "Symbol used for babel headers")

;;   (defun my/org-prettify-src--update ()
;;     (let ((case-fold-search t)
;;           (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
;;           found)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (re-search-forward re nil t)
;;           (goto-char (match-end 0))
;;           (let ((args (org-trim
;;                        (buffer-substring-no-properties (point)
;;                                                        (line-end-position)))))
;;             (when (org-string-nw-p args)
;;               (let ((new-cell (cons args my/ob-header-symbol)))
;;                 (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
;;                 (cl-pushnew new-cell found :test #'equal)))))
;;         (setq prettify-symbols-alist
;;               (cl-set-difference prettify-symbols-alist
;;                                  (cl-set-difference
;;                                   (cl-remove-if-not
;;                                    (lambda (elm)
;;                                      (eq (cdr elm) my/ob-header-symbol))
;;                                    prettify-symbols-alist)
;;                                   found :test #'equal)))
;;         ;; Clean up old font-lock-keywords.
;;         (font-lock-remove-keywords nil prettify-symbols--keywords)
;;         (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
;;         (font-lock-add-keywords nil prettify-symbols--keywords)
;;         (while (re-search-forward re nil t)
;;           (font-lock-flush (line-beginning-position) (line-end-position))))))

;;   (defun my/org-prettify-src ()
;;     "Hide src options via `prettify-symbols-mode'.

;;   `prettify-symbols-mode' is used because it has uncollpasing. It's
;;   may not be efficient."
;;     (let* ((case-fold-search t)
;;            (at-src-block (save-excursion
;;                            (beginning-of-line)
;;                            (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
;;       ;; Test if we moved out of a block.
;;       (when (or (and my/org-at-src-begin
;;                      (not at-src-block))
;;                 ;; File was just opened.
;;                 (eq my/org-at-src-begin -1))
;;         (my/org-prettify-src--update))
;;       ;; Remove composition if at line; doesn't work properly.
;;       ;; (when at-src-block
;;       ;;   (with-silent-modifications
;;       ;;     (remove-text-properties (match-end 0)
;;       ;;                             (1+ (line-end-position))
;;       ;;                             '(composition))))
;;       (setq my/org-at-src-begin at-src-block)))

;;   (defun my/org-prettify-symbols ()
;;     (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
;;           (cl-reduce 'append
;;                      (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
;;                              `(("#+begin_src" . ?＄) ; ➤ ➟ ➤ ✎  〜
;;                                ("#+end_src"   . ?□) ; ⏹
;;                                ("#+header:" . ,my/ob-header-symbol)
;;                                ("#+begin_quote" . ?❝) ; » «
;;                                ("#+end_quote" . ?❞)))))
;;     (turn-on-prettify-symbols-mode)
;;     (add-hook 'post-command-hook 'my/org-prettify-src t t))
;;   (add-hook 'org-mode-hook #'my/org-prettify-symbols))

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


;; (use-package org-superstar
;;   :after org
;;   :hook
;;   (org-mode . org-superstar-mode)
;;   :config
;;   (setq org-superstar-headline-bullets-list '("⬤" "◉" "○" "✸" "◆" "▲" "▶")))

(use-package edit-indirect)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . my-markdown-mode-hook)
  (markdown-mode . embrace-markdown-mode-hook)
  :config
;;;###autoload
  (defun my/markdown-mode-time-stamp ()
    "Insert time stamp in markdown mode"
    (interactive)
    (insert (format-time-string "%Y-%m-%d")))

;;;###autoload
  (defun embrace-markdown-mode-hook ()
    (embrace-add-pair ?` "`" "`"))

  (add-to-list 'auto-mode-alist '("presentation.html" . markdown-mode))
  (defun my-markdown-mode-hook ()
    (visual-line-mode 1)
    (setq word-wrap-by-category t))

;;;###autoload
  (defun my/preview-markdown ()
    "Preview github flavored markdown"
    (interactive)
    (when (executable-find "markdown-preview")
      (tat/execute-async (concat "markdown-preview " (buffer-file-name)) "markdown-preview"))))

(use-package separedit
  :config
  (define-key prog-mode-map (kbd "C-c e") #'separedit)
  (setq separedit-default-mode 'markdown-mode))

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
  ;; :hook
  ;; (c-mode . eglot-ensure)
  ;; (c++-mode . eglot-ensure)
  :general
  (my/local-leader-keys
    "oe" 'eglot
    "oE" 'eglot-shutdown)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "ccls"))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(d-mode . ("/usr/bin/serve-d"))))

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

(use-package sly-quicklisp)

(use-package vterm
  :defer t
  :hook
  (vterm-mode . (lambda () (set-process-sentinel (get-buffer-process (buffer-name) ) #'my/vterm-exit-kill-buffer-window)))
  :bind (:map vterm-mode-map
              ("C-x C-f" . (lambda (&rest _)
                             (interactive)
                             (with-system gnu/linux
                               (my/vterm-directory-sync))
                             (call-interactively 'counsel-find-file))))
  :config
  ;; Set evil initial state to `emacs' in vterm-mode
  (evil-set-initial-state 'vterm-mode 'emacs)

  ;; Make vterm render corrent evil state cursor shape
  (advice-add #'vterm--redraw :after (lambda (&rest args) (evil-refresh-cursor evil-state)))

  (defun my/vterm-exit-kill-buffer-window (process event)
    "Kill buffer and window on shell process termination."
    (when (not (process-live-p process))
      (let ((buf (process-buffer process)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (kill-buffer)
            (unless (one-window-p)
              (delete-window)))))))

  (evil-define-key 'emacs vterm-mode-map
    (kbd "C-\\") 'toggle-input-method
    (kbd "C-a") 'vterm-send-C-a
    (kbd "C-b") 'vterm-send-C-b
    (kbd "C-c") 'vterm-send-C-c
    (kbd "C-d") 'vterm-send-C-d
    (kbd "C-e") 'vterm-send-C-e
    (kbd "C-f") 'vterm-send-C-f
    (kbd "C-g") 'vterm-send-C-g
    (kbd "C-k") 'vterm-send-C-k
    (kbd "C-l") 'vterm-send-C-l
    (kbd "C-n") 'vterm-send-C-n
    (kbd "C-p") 'vterm-send-C-p
    (kbd "C-r") 'vterm-send-C-r
    (kbd "C-s") 'vterm-send-C-s
    (kbd "C-t") 'vterm-send-C-t
    (kbd "C-u") 'vterm-send-C-u
    (kbd "C-S-v") 'vterm-yank)

  ;; Directory synchronization (linux-only)
  (with-system gnu/linux
    (defun my/vterm-directory-sync ()
      "Synchronize current working directory."
      (when vterm--process
        (let* ((pid (process-id vterm--process))
               (dir (file-truename (format "/proc/%d/cwd" pid))))
          (setq-local default-directory (concat dir "/")))))))

(use-package multi-vterm
  :after vterm
  :defer t
  :config
  (evil-define-key 'normal vterm-mode-map (kbd ",c") #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n") #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p") #'multi-vterm-prev))

(use-package dired
  :ensure nil
  :general
  (my/leader-keys
    "d" '(:ignore t :which-key "dired")
    "db" '(my/browse-marked-file :wk "open-in-browser")
    "dc" '(tda/rsync :wk "async-rsync")
    "de" '(my/ediff-files :wk "ediff-files")
    "di" 'image-dired
    "dp" '(tda/zip :wk "async-zip")
    "d RET" 'dired-start-process
    "du" '(tda/unzip :wk "async-unzip")
    "ds" 'xah-dired-sort
    "dw" '(wdired-change-to-wdired-mode :wk "wdired")
    "dz" '(tda/get-files-size :wk "async-files-size")
    "d /" '(my/dired-filter :wk "narrow"))
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . auto-revert-mode)
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
                                  ("\\.webp\\'" "vwebp")
                                  ("\\.jpg\\'" "pqiv")
                                  ("\\.png\\'" "pqiv")
                                  ("\\.gif\\'" "pqiv")
                                  ("\\.jpeg\\'" "pqiv")
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

  (use-package dired-aux
    :ensure nil
    :config
    (add-to-list 'dired-compress-file-suffixes
                 '("\\.rar\\'" ".rar" "unrar x")))

  (defvar dired-filelist-cmd
    '(("mpv")
      ("llpp")))

  ;; Create a new procedure to start a process in dired without popup windows.
  ;; The process will persist when Emacs is closed.
  ;; https://emacs.stackexchange.com/a/5558
  (defun dired-start-process (cmd &optional file-list)
    (interactive
     (let ((files (dired-get-marked-files t current-prefix-arg)))
       (list
        (dired-read-shell-command "& on %s: " current-prefix-arg files)
        files)))
    (start-process
     cmd nil shell-file-name
     shell-command-switch
     (format "nohup 1>/dev/null 2>/dev/null %s \"%s\""
             (if (> (length file-list) 1)
                 (format "%s %s" cmd
                         (cadr (assoc cmd dired-filelist-cmd)))
               cmd)
             (mapconcat #'expand-file-name file-list "\" \""))))

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
      (mapc 'find-file fn-list)))

  ;; Ediff marked files in dired
  ;; https://oremacs.com/2017/03/18/dired-ediff/
  (defun my/ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "no more than 2 files should be marked")))))

(use-package arview
  :ensure nil)

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
  (dumb-jump-selector 'ivy)
  :config
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

(use-package xref
  :ensure nil
  :general
  (:keymaps 'evil-normal-state-map
            "gr" 'xref-find-references)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read))

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
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-auto-close-style 1)

  ;; Remove < auto pair in web-mode
  (eval-after-load smartparens-strict-mode
    (sp-local-pair 'web-mode "<" nil :actions :rem))


  ;; 1. Remove web-mode auto pairs whose end pair starts with a letter
  ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
  ;;    better.
  ;; 2. Strips out extra closing pairs to prevent redundant characters
  ;;    inserted by smartparens.
  (eval-after-load smartparens-strict-mode
    (progn
      (dolist (alist web-mode-engines-auto-pairs)
        (setcdr alist
                (cl-loop for pair in (cdr alist)
                         unless (string-match-p "^[a-z-]" (cdr pair))
                         collect (cons (car pair)
                                       (string-trim-right (cdr pair)
                                                          "\\(?:>\\|]\\|}\\)+\\'")))))
      (setq web-mode-engines-auto-pairs
            (delq nil web-mode-engines-auto-pairs))))


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
    "<f2>" 'tide-rename-symbol)
  :config
  (setq tide-completion-ignore-case t
        tide-server-max-response-length (* 1024 1024))

  ;; Use better-jumper to manipulate jump-list
  (advice-add #'tide-jump-to-definition :around #'evil-better-jumper/set-jump-a)
  (advice-add #'tide-references :around #'evil-better-jumper/set-jump-a)

  (defun my/setup-tide-mode ()
    "Use hl-identifier-mode only on js or ts buffers."
    (when (and (stringp buffer-file-name)
               (string-match "\\.[tj]sx?\\'" buffer-file-name))
      ;; (setq gc-cons-threshold 100000000)
      (tide-setup)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)))

  (defvar tide-jsconfig-content
    "{\n\
  \"compilerOptions\": {\n\
    \"target\": \"es2017\",\n\
    \"allowSyntheticDefaultImports\": true,\n\
    \"noEmit\": true,\n\
    \"checkJs\": false,\n\
    \"jsx\": \"react\",\n\
    \"lib\": [\"dom\", \"es2017\"]\n\
  },\n\
  \"exclude\": [\"build\", \"node_modules\", \"assets/dependencies\"]\n\
}\n\
"
    "Content of jsconfig.json file.")

  (defun spacemacs//tide-create-jsconfig-file ()
    "Create a jsconfig file at project root."
    (interactive)
    (let ((jsconfig (cdr (project-current))))
      (if jsconfig
          (let ((jsconfig-file (concat jsconfig "jsconfig.json")))
            (if (file-exists-p jsconfig-file)
                (message "File exists")
              (with-temp-file jsconfig-file
                (insert tide-jsconfig-content))))
        (message "Project not found")))))

(use-package flycheck
  :defer t
  :general
  (my/leader-keys
    "of" 'flycheck-mode)
  (my/local-leader-keys
    "f" '(:ignore t :which-key "flycheck")
    "fl" '(spacemacs/goto-flycheck-error-list :wk "flycheck-error-list")
    "fn" 'flycheck-next-error
    "fp" 'flycheck-previous-error
    "ff" '(spacemacs/toggle-flycheck-error-list :wk "togglg-flycheck-error-list")
    "fv" 'flycheck-verify-setup
    "fs" 'flycheck-select-checker)
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
  (flycheck-add-mode 'json-jq 'my-json-mode)

  (flycheck-add-mode 'css-stylelint 'css-mode)
  (setq flycheck-stylelintrc ".stylelintrc.json"))

;; Define a custom json mode
;; Load after web-mode
;; Configure `flycheck' to use json-jq as checker in `my-json-mode'
;; Configure `reformatter' use jq as formater in `my-json-mode'
(define-derived-mode my-json-mode web-mode "MyJSON"
  "My custom JSON mode")
(add-to-list 'auto-mode-alist '("\\.json\\'" . my-json-mode))

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
    :keymaps '(web-mode-map css-mode-map my-json-mode-map)
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
        (counsel-rg nil root (concat "--glob " glob)))))

  (defun my/create-project-root-file ()
    "Create .project file at project root."
    (interactive)
    (let ((projectroot (cdr (project-current))))
      (if projectroot
          (let ((projectroot-file (concat projectroot ".project")))
            (if (file-exists-p projectroot-file)
                (message "Project root file exists")
              (with-temp-buffer (write-file projectroot-file))))
        (let ((projectroot-file (concat default-directory ".project")))
          (with-temp-buffer (write-file projectroot-file))
          (message ".project file created")))))

  (defvar fdignore-content
    "/node_modules\n\
/.git\n\
/.ccls-cache\n\
"
    "Content of .fdignore file.")

  (defun my/create-fd-ignore-file ()
    "Create a fdignore file at project root."
    (interactive)
    (let ((fdignore (cdr (project-current))))
      (if fdignore
          (let ((fdignore-file (concat fdignore ".fdignore")))
            (if (file-exists-p fdignore-file)
                (message "File exists")
              (with-temp-file fdignore-file
                (insert fdignore-content))))
        (message ".fdignore file created")))))


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
    "hd" 'godoc
    "bp" 'gofmt)
  ;; Use smartparens post-handler to replace this function
  ;; (general-def go-mode-map
  ;;   "{" 'my/go-electric-brace)
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (setq gofmt-show-errors nil)

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
    (godoc (ivy-read "Package: " (go-packages) :require-match t)))

  (use-package go-playground
    :after go-mode))

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

(use-package abn-funcs-benchmark
  :ensure nil)

(use-package macrostep
  :defer t)

(use-package ediff
  :ensure nil
  :defer t
  :general
  (my/leader-keys
    "ol" 'ediff)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function (if (> (frame-width) 150)
                                        'split-window-horizontally
                                      'split-window-vertically)))

(use-package org-variable-pitch
  :ensure nil)

(use-package org-appear
  :init
  ;; Instant toggle raw format on insert mode, 0.5 second delay on normal mode.
  (add-hook 'evil-insert-state-entry-hook (lambda() (setq org-appear-delay 0)))
  (add-hook 'evil-normal-state-entry-hook (lambda() (setq org-appear-delay 0.5)))
  :hook
  (org-mode . org-appear-mode)
  :config
  ;; Hide emphasis makers.
  (setq org-hide-emphasis-markers t))

(use-package darkroom
  :ensure nil
  :defer t
  :general
  (my/leader-keys
    "td" '(my/toggle-darkroom-tentative-mode :wk "darkroom"))
  :config
  (defun my/toggle-darkroom-tentative-mode ()
    (interactive)
    (visual-fill-column-mode 'toggle)
    (darkroom-tentative-mode 'toggle)
    (org-variable-pitch-minor-mode 'toggle)))

(use-package journalctl-mode
  :defer t)

(use-package yaml-mode
  :ensure nil)

(use-package quickrun
  :defer t
  :general
  (my/leader-keys
    "or" 'quickrun))

;; Package `hl-todo'
(require 'hl-todo)
(global-hl-todo-mode 1)
(eval-after-load 'which-key (which-key-add-key-based-replacements "C-c t" "hl-todo"))
(define-key hl-todo-mode-map (kbd "C-c t p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c t n") 'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c t o") 'hl-todo-occur)
(define-key hl-todo-mode-map (kbd "C-c t i") 'hl-todo-insert)

;;; Origami is used as evil folding backend

;;; Alternative:
;;; - `hs-minor-mode', can be used in conjunction with [hideshowvis](https://www.emacswiki.org/emacs/hideshowvis.el)
;;; - `outline-mode', can be used in conjunction with [backline](https://github.com/tarsius/backline)
;;;                   and some config examples: https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/

;;; Depends on fringer-helper
;;; Maybe should load after evil
;;;
;;; This is a further development fork from original project origami.
;;; https://github.com/elp-revive/origami.el
;;; Need to install this package manually
;;;
;; (use-package origami
;;   :after evil
;;   :ensure nil
;;   :load-path "~/.emacs.d/site-lisp/origami.el/"
;;   :config
;;   (global-origami-mode 1))

;;; Some config can be used for reference
;;; TODO: adapt some config examples
;;; https://www.reddit.com/r/emacs/comments/6fmpwb/evil_and_builtin_folding/
;;; https://www.reddit.com/r/emacs/comments/5ei7wa/awesome_vimlike_folding_for_evilmode_with_markers/

;; Use hideshow(`hs-minor-mode') as evil folding backend
(use-package hideshow
  :after evil
  :defer t
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; Make hideshow overlay more conspicuous
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :background "#ff8" :box t))))

  (defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

  (setq hs-set-up-overlay 'hideshow-folded-overlay-fn))

(use-package tab-bar
  :if(> emacs-major-version 26)
  :bind
  ([remap project-switch-project] . my/switch-project-in-new-tab)
  :general
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "g[" #'tab-bar-switch-to-prev-tab
   "g]" #'tab-bar-switch-to-next-tab
   "M-g t" #'tab-bar-switch-to-tab)
  :config
  (setq-default tab-bar-border 5
                tab-bar-close-button nil
                tab-bar-back-button nil
                tab-bar-new-button nil
                tab-bar-tab-hints t)

  ;; Config `tab-bar-tab-name-format-function' to show pretty numbers
  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defvar ct/circle-numbers-alist
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨"))
    "Alist of integers to strings of circled unicode numbers.")
  (alist-get 4 ct/circle-numbers-alist)

  (defun ct/tab-bar-tab-name-format-default (tab i)
    (let* ((current-p (eq (car tab) 'current-tab))
           (tab-num (if (and tab-bar-tab-hints (< i 10))
                        (alist-get i ct/circle-numbers-alist) ""))
           (tab-name (alist-get 'name tab))
           (tab-name-length (length tab-name))
           (tab-name-too-long? (> tab-name-length 20))
           (tab-shorten-name (if tab-name-too-long?
                                 (concat (substring tab-name 0 15)
                                         "..."
                                         (substring tab-name (- tab-name-length 7) tab-name-length))
                               tab-name)))
      (propertize
       (concat tab-num
               " "
               tab-shorten-name
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   "")
               " ")
       'face (funcall tab-bar-tab-face-function tab))))
  (setq tab-bar-tab-name-format-function #'ct/tab-bar-tab-name-format-default)

  (defun my/switch-project-in-new-tab ()
    (interactive)
    (let (succ)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (call-interactively #'project-switch-project)
            (when-let ((proj (project-current)))
              (tab-bar-rename-tab (format "%s" (file-name-nondirectory (directory-file-name (cdr proj)))))
              (setq succ t)))
        (unless succ
          (tab-bar-close-tab)))))

  (when (> emacs-major-version 27)
    (add-to-list 'tab-bar-format #'tab-bar-format-menu-bar))

  (tab-bar-mode 1)
  (setq tab-bar-new-tab-choice "*scratch*"))

(use-package cmake-mode
  :defer t)

(use-package iedit
  :bind
  (:map global-map ("C-;" . nil)))

(use-package flyspell
  :general
  (my/leader-keys
    "os" 'flyspell-mode)
  :config
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US"))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(use-package evil-iedit-state
  :after iedit
  :config
  (setq evil-iedit-state-tag (propertize " [E]  ")
        evil-iedit-insert-state-tag (propertize " [Ei]  ")))

(use-package git-gutter
  :init
  (which-key-add-key-based-replacements "C-c g" "git-gutter")
  :general
  ("C-c g l" '(my/ivy-goto-git-gutter :wk "git-gutter-list"))
  ("C-c g j" 'git-gutter:next-hunk)
  ("C-c g k" 'git-gutter:previous-hunk)
  ("C-c g r" 'git-gutter:revert-hunk)
  ("C-c g s" 'git-gutter:stage-hunk)
  ("C-c g p" 'git-gutter:popup-hunk)
  ("C-c g v" 'git-gutter:statistic)
  ("C-c g m" 'git-gutter:mark-hunk)
  :config
  (global-git-gutter-mode 1)

  ;; Use ivy to navigate git gutter chunks
  ;; https://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
  (defun my--reshape-git-gutter (gutter)
    "Re-shape gutter for `ivy-read'."
    (let* ((linenum-start (aref gutter 3))
           (linenum-end (aref gutter 4))
           (target-line "")
           (target-linenum 1)
           (tmp-line "")
           (max-line-length 0))
      (save-excursion
        (while (<= linenum-start linenum-end)
          (goto-line linenum-start)
          (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                   (buffer-substring (line-beginning-position)
                                                                     (line-end-position))))
          (when (> (length tmp-line) max-line-length)
            (setq target-linenum linenum-start)
            (setq target-line tmp-line)
            (setq max-line-length (length tmp-line)))

          (setq linenum-start (1+ linenum-start))))
      ;; build (key . linenum-start)
      (cons (format "%s %d: %s"
                    (if (eq 'deleted (aref gutter 1)) "-" "+")
                    target-linenum target-line)
            target-linenum)))

  (defun my/ivy-goto-git-gutter ()
    (interactive)
    (if git-gutter:diffinfos
        (ivy-read "git-gutters:"
                  (mapcar 'my--reshape-git-gutter git-gutter:diffinfos)
                  :action (lambda (e)
                            ;; ivy9+ keep `(car e)'
                            ;; ivy8- strip the `(car e)'
                            ;; we handle both data structure
                            (unless (numberp e) (setq e (cdr e)))
                            (goto-line e)))
      (message "NO git-gutters!"))))

(use-package git-overlay
  :ensure nil
  :general
  (my/leader-keys
    "tg" 'git-overlay))

(use-package too-long-lines-mode
  :ensure nil
  :config
  (too-long-lines-mode 1)
  (setq too-long-lines-threshold 1000)
  (add-hook 'image-mode-hook (lambda ()
                               (too-long-lines-mode -1))))

(use-package firefox-bookmarks
  :ensure nil)

(use-package beacon
  :ensure nil
  :config
  (beacon-mode 1)

  ;; Fix beacon blinking constantly in *scratch* buffer
  ;; https://github.com/Malabarba/beacon/issues/76
  (setq beacon-blink-when-window-scrolls nil)
  :custom
  (beacon-push-mark 35))

;; Required by `source-peek'
;; TODO Create a new package: use quick-peek to show flycheck diagnostics
(use-package quick-peek)

;; TODO Add more backends of `source-peek':
;;       - eglot
;;       - lsp-mode
;;       - elisp-def
(use-package source-peek
  :ensure nil)

;; Find elisp definitions
(use-package elisp-def)


;; A function to use `rgrep' or `vc-git-grep' in a specific directory
;; https://www.manueluberti.eu/emacs/2021/09/10/rgrep-and-vc-git-grep/
(defun mu-recursive-grep (search-term search-path)
  "Recursively search for SEARCH-TERM in SEARCH-PATH."
  (interactive
   (progn
     (unless grep-command
       (grep-compute-defaults))
     (let ((search-term (grep-read-regexp))
           (search-path (expand-file-name
                         (read-directory-name
                          "Directory: " nil default-directory t))))
       (list search-term search-path))))
  (if (vc-root-dir)
      (vc-git-grep search-term "*" search-path)
    (rgrep search-term "*" search-path)))

(use-package el2markdown
  :ensure nil
  :config
  (autoload 'el2markdown-view-buffer  "el2markdown" nil t)
  (autoload 'el2markdown-write-file   "el2markdown" nil t)
  (autoload 'el2markdown-write-readme "el2markdown" nil t))

(use-package editcmacro
  :ensure nil
  :config
  (add-hook 'c-mode-hook   #'editcmacro-mode)
  (add-hook 'c++-mode-hook #'editcmacro-mode))

(use-package reformatter
  :defer t
  :general
  (my/leader-keys
    :keymaps '(c-mode-map c++-mode-map)
    "bp" 'clang-format)
  (my/leader-keys
    :keymaps 'sh-mode-map
    "bp" 'shfmt)
  :config

  ;; Config shfmt
  (defgroup shfmt nil
    "Reformat shell scripts using shfmt."
    :group 'languages)

  (defcustom shfmt-command "shfmt"
    "Command used for reformatting."
    :type 'string)

  (defcustom shfmt-arguments ()
    "Arguments passed to shfmt."
    :type '(list string))

;;;###autoload (autoload 'shfmt-buffer "shfmt" nil t)
;;;###autoload (autoload 'shfmt-region "shfmt" nil t)
;;;###autoload (autoload 'shfmt-on-save-mode "shfmt" nil t)
  (reformatter-define shfmt
    :program shfmt-command
    :args shfmt-arguments
    :lighter " ShFmt"
    :group 'shfmt)

  ;; Config go fmt
  (defgroup gofmt nil
    "Reformat Golang files using gofmt."
    :group 'languages)

  (defcustom gofmt-command "gofmt"
    "Command used for reformatting."
    :type 'string)

  (defcustom gofmt-arguments ()
    "Arguments passed to gofmt."
    :type '(list string))

;;;###autoload (autoload 'gofmt-buffer "gofmt" nil t)
;;;###autoload (autoload 'gofmt-region "gofmt" nil t)
;;;###autoload (autoload 'gofmt-on-save-mode "gofmt" nil t)
  (reformatter-define gofmt
    :program gofmt-command
    :args gofmt-arguments
    :lighter " Gofmt"
    :group 'gofmt)

  ;; Config clang-format
  (defgroup clang-format nil
    "Reformat shell scripts using clang-format."
    :group 'languages)

  (defcustom clang-format-command "clang-format"
    "Command used for reformatting."
    :type 'string)

  (defcustom clang-format-arguments ()
    "Arguments passed to clang-format."
    :type '(list string))

;;;###autoload (autoload 'clang-format-buffer "clang-format" nil t)
;;;###autoload (autoload 'clang-format-region "clang-format" nil t)
;;;###autoload (autoload 'clang-format-on-save-mode "clang-format" nil t)
  (reformatter-define clang-format
    :program clang-format-command
    :args clang-format-arguments
    :lighter " Clang-Format"
    :group 'clang-format)

  (defun my/set-clang-format-style ( )
    "Set `clang-format' style according to whether .clang-format file exists"
    (let ((project-root (cdr (project-current))))
      (unless (or (f-exists-p (expand-file-name ".clang-format"))
                  (f-exists-p (expand-file-name ".clang-format" project-root)))
        (setq-local clang-format-arguments (list "-style=llvm")))))

  (add-hook 'c-mode-hook #'my/set-clang-format-style)
  (add-hook 'c++-mode-hook #'my/set-clang-format-style)

  ;; Config jq
  (defgroup jq-format nil
    "JSON reformatting using jq."
    :group 'json)

  (defcustom jq-format-command "jq"
    "Name of the jq executable."
    :group 'jq-format
    :type 'string)

  (defcustom jq-format-sort-keys t
    "Whether to sort keys."
    :group 'jq-format
    :type 'boolean)

  (defcustom jq-format-extra-args nil
    "Extra arguments to pass to jq."
    :group 'jq-format
    :type '(repeat string))

;;;###autoload (autoload 'jq-format-json-buffer "jq-format" nil t)
;;;###autoload (autoload 'jq-format-json-region "jq-format" nil t)
;;;###autoload (autoload 'jq-format-json-on-save-mode "jq-format" nil t)
  (reformatter-define jq-format-json
    :program jq-format-command
    :args (jq-format--make-args)
    :lighter " JSONFmt"
    :group 'jq-format)

;;;###autoload (autoload 'jq-format-jsonlines-buffer "jq-format" nil t)
;;;###autoload (autoload 'jq-format-jsonlines-region "jq-format" nil t)
;;;###autoload (autoload 'jq-format-jsonlines-on-save-mode "jq-format" nil t)
  (reformatter-define jq-format-jsonlines
    :program jq-format-command
    :args (append '("--compact-output") (jq-format--make-args))
    :lighter " JSONLFmt"
    :group 'jq-format)

  (defun jq-format--make-args ()
    "Helper to build the argument list for jq."
    (append
     (when jq-format-sort-keys '("--sort-keys"))
     jq-format-extra-args
     '("." "-"))))

(use-package ggtags
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (progn (ggtags-mode 1)
                       (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))))
  (define-key ggtags-mode-map (kbd "C-c k s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c k h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c k r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c k d") 'ggtags-find-definition)
  (define-key ggtags-mode-map (kbd "C-c k f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c k c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c k u") 'ggtags-update-tags)

  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

  (setq ggtags-update-on-save nil)
  (setq ggtags-highlight-tag nil)
  (setq ggtags-navigation-mode-lighter nil)
  (setq ggtags-mode-line-project-name nil))

(use-package semantic
  :ensure nil
  :defer t
  :hook
  (c-mode . my/c-semantic-hooks)
  (c++-mode . my/c-semantic-hooks)
  :general
  (my/leader-keys
    :keymaps '(emacs-lisp-mode-map lisp-mode-map)
    "bp" '(srefactor-lisp-format-buffer :wk "lisp-format"))
  :config
  (advice-add 'semantic-idle-scheduler-function :around #'ignore)
  (use-package srefactor
    :config
    (require 'srefactor-lisp))

  (add-hook 'srefactor-ui-menu-mode-hook 'evil-emacs-state)

  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-add-system-include "/usr/include/gtk-3.0/" 'c-mode)

  (global-set-key (kbd "<f5>") (lambda ()
                                 (interactive)
                                 (setq-local compilation-read-command nil)
                                 (call-interactively 'compile)))

  (defun my/c-semantic-hooks ()
    (semantic-mode 1)
    ;; Use `company-clang' to auto complate
    (setq company-backends (delete 'company-semantic company-backends))
    (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
    (local-set-key "\C-c\C-s" 'semantic-ia-show-summary)))

(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))


(use-package pcre2el)

(use-package doom-lookup
  :ensure nil
  :config
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "gd" #'+lookup/definition
   "gr" #'+lookup/references
   "gf" #'+lookup/file)

  (set-lookup-handlers! 'emacs-lisp-mode
    :definition #'elisp-def)
  (set-lookup-handlers! 'ggtags-mode
    :definition #'ggtags-find-tag-dwim
    :references #'ggtags-find-reference
    :file #'ggtags-find-file)
  (set-lookup-handlers! 'tide-mode
    :references #'tide-references)
  (set-lookup-handlers! 'go-mode
    :references #'+lookup-mu-rgrep-search-backend-fn))

(use-package newsticker
  :ensure nil
  :defer t
  :init
  (setq newsticker-retrieval-interval 0
        newsticker-ticker-interval 0)
  :general
  (my/leader-keys
    "on" '(my/newsticker-treeview-in-new-tab :wk "newsticker"))
  :config
  (defun my/newsticker-treeview-in-new-tab ()
    (interactive)
    (let (succ)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (call-interactively #'newsticker-treeview)
            (tab-bar-rename-tab "newsticker")
            (setq succ t))
        (unless succ
          (tab-bar-close-tab)))))

  (defun my/newsticker-treeview-quit-and-close-tab ()
    (interactive)
    (newsticker-treeview-quit)
    (newsticker-stop)
    (tab-close))

  (general-define-key
   :keymaps 'newsticker-treeview-mode-map
   :states 'normal
   "q" 'my/newsticker-treeview-quit-and-close-tab)

  :custom
  (newsticker-url-list '(
                         ;; ("title" "URL" other options)
                         ("emacs redux" "https://emacsredux.com/atom.xml" nil nil nil)
                         ("batsov" "https://batsov.com/atom.xml" nil nil nil)
                         ("meta redux" "https://metaredux.com/feed.xml" nil nil nil)
                         ("Ruanyifeng" "https://www.ruanyifeng.com/blog/atom.xml" nil nil nil)
                         ("Sachachua emacs news" "https://sachachua.com/blog/category/emacs-news/feed" nil nil nil)
                         ("Planet Emacs Life" "https://planet.emacslife.com/atom.xml" nil nil nil)
                         ("Karthinks" "https://karthinks.com/index.xml" nil nil nil)
                         ("ruzkuku" "https://ruzkuku.com/all.atom" nil nil nil)
                         ("lazycat" "https://manateelazycat.github.io/feed.xml" nil nil nil)
                         ("jlelse" "https://jlelse.blog/.rss")
                         ("honmaple" "https://honmaple.me/atom.xml")
                         ("ianthehenry" "https://ianthehenry.com/feed.xml")
                         ("manueluberti" "https://www.manueluberti.eu/feed.xml")
                         ("kevq" "https://kevq.uk/feed.xml")
                         ("hacker news" "https://hnrss.org/frontpage")
                         ("lobsters" "https://lobste.rs/rss")
                         ("jameslittle" "https://jameslittle.me/feed.xml")
                         ("ag91" "https://ag91.github.io/rss.xml")
                         ))
  (newsticker-retrieval-method 'extern)
  (newsticker-wget-name "curl")
  (newsticker-wget-arguments '("--disable" "--silent" "--location" "--proxy" "socks5://127.0.0.1:7890"))
  (newsticker-url-list-defaults nil)    ;remove default list (i.e. emacswiki)
  (newsticker-automatically-mark-items-as-old nil))

(use-package tiny
  :ensure nil
  :config
  ;; Full syntax:
  ;; m{range start:=0}{separator:= }{range end}{Lisp expr:=indentity}|{format expr:=%d}
  (general-define-key
   :states 'insert
   "C-;" 'tiny-expand))

(use-package annotate
  :general
  (my/leader-keys
    "oa" 'annotate-mode))

(use-package proced
  :ensure nil
  :init
  (setq-default proced-auto-update-flag t
                proced-auto-update-interval 3)
  :general
  (my/leader-keys
    "op" 'proced))

(use-package redacted
  :general
  (my/local-leader-keys
    "o" '(:ignore t :wk "open")
    "or" 'redacted-mode)
  :config
  ;; Enable `read-only-mode' to ensure that we don't change what we can't read.
  (add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1)))))

(use-package cbm
  :ensure nil
  :general
  (my/leader-keys
    "bc" 'cbm-switch-buffer))

(use-package wwg
  :ensure nil
  :general
  (my/leader-keys
    "ow" 'wwg-mode))

(use-package pdf-tools
  :commands (pdf-loader-install)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (evil-set-initial-state 'pdf-view-mode 'normal)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-width)

  ;; Config isearch in pdf-tools
  ;; https://emacs-china.org/t/pdf-tools-isearch-repeat-forward-backward/16417/2
  (with-eval-after-load 'pdf-tools
    (defun my/isearch-failed? ()
      (or (not isearch-success) isearch-error))

    (defvar-local my/pdf-isearch-highlight-matches nil)
    (defun my/pdf-isearch-cleanup-highlight ()
      (setq my/pdf-isearch-highlight-matches nil)
      (pdf-view-redisplay))

    (defun my/pdf-isearch-hl-matches-controllable-highlight (orig-fun current matches &optional occur-hack-p)
      (funcall orig-fun current matches (or my/pdf-isearch-highlight-matches occur-hack-p)))
    (advice-add #'pdf-isearch-hl-matches
                :around #'my/pdf-isearch-hl-matches-controllable-highlight)

    (defun my/pdf-isearch-repeat-forward (&optional arg)
      (interactive "P")
      (setq my/pdf-isearch-highlight-matches t)
      (isearch-repeat-forward arg))

    (defun my/pdf-isearch-repeat-backward (&optional arg)
      (interactive "P")
      (setq my/pdf-isearch-highlight-matches t)
      (isearch-repeat-backward arg))

    ;; 搜索超过文档结尾时，直接wrap到文档头开始搜索，不要暂停，
    ;; 如果不这样设置，则搜索超过文档结尾时，暂停的时候，结尾的高亮会不见
    ;; 估计不难修复，但是这样搞更简单。
    (add-hook 'pdf-view-mode-hook
              (lambda () (setq-local isearch-wrap-pause nil)))

    (defun my/pdf-view-force-normal-state ()
      (interactive)
      (evil-force-normal-state)
      (my/pdf-isearch-cleanup-highlight))

    (advice-add #'pdf-isearch-mode-initialize
                :before
                (lambda (&rest args)
                  "在正常使用C-s等进行搜索的时候，重置 `my/pdf-isearch-highlight-matches'。"
                  (setq my/pdf-isearch-highlight-matches nil)))

    (defun my/pdf-isearch-mode-cleanup ()
      "按/键搜索，如果搜索成功，则按回车后不要清除高亮，如果搜索失败，则清除高亮。"
      (pdf-isearch-active-mode -1)
      (when (my/isearch-failed?) (pdf-view-redisplay)))
    (advice-add #'pdf-isearch-mode-cleanup
                :override #'my/pdf-isearch-mode-cleanup)

    (defmacro my/modify-evil-collection-key-bindings (mode &rest body)
      (declare (indent defun))
      (let ((feature-name (intern (concat "evil-collection-" (symbol-name mode))))
            (setup-fun-name (intern (concat "evil-collection-" (symbol-name mode) "-setup"))))
        `(if (featurep ',feature-name)
             ;; 在当前evil-collection的实现中，如果feature-name对应的文件
             ;; 已经加载，则对应的按键setup函数也已经调用，故在这种情况下，
             ;; 我们可以直接执行body。
             (progn ,@body)
           (with-eval-after-load ',feature-name
             (define-advice ,setup-fun-name (:after (&rest _))
               ,(concat "修改evil-collection设置的" (symbol-name mode) "的按键绑定。")
               ,@body)))))

    (my/modify-evil-collection-key-bindings pdf
      (evil-define-key 'normal pdf-view-mode-map
        ;; 按n/N 向前/向后搜索，按ESC则返回normal state的同时清除搜索高亮
        (kbd "n") #'my/pdf-isearch-repeat-forward
        (kbd "N") #'my/pdf-isearch-repeat-backward
        (kbd "<escape>") #'my/pdf-view-force-normal-state)))

  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-normal-state-cursor) (list nil))))

  ;; Save pdf information in save-place
  (use-package saveplace-pdf-view
    :after pdf-tools)

  (general-define-key
   :keymaps 'pdf-view-mode-map
   :states 'normal
   "y" 'pdf-view-kill-ring-save
   "I" 'pdf-misc-display-metadata
   "o" 'pdf-outline
   "al" 'pdf-annot-list-annotations
   "ad" 'pdf-annot-delete
   "aa" 'pdf-annot-attachment-dired
   "am" 'pdf-annot-add-markup-annotation
   "at" 'pdf-annot-add-text-annotation
   "h" 'image-backward-hscroll
   "l" 'image-forward-hscroll
   "j" 'pdf-view-next-line-or-next-page
   "k" 'pdf-view-previous-line-or-previous-page))

(use-package org-noter
  :config
  (setq org-noter-always-create-frame nil
        org-noter-notes-search-path '("~/org/pdf-annotation"))

  (general-define-key
   :keymaps 'pdf-view-mode-map
   :states 'normal
   "i" 'org-noter))

(use-package gif-screencast
  :defer t
  :bind (("<f7>" . gif-screencast)
         ("<f8>" . gif-screencast-toggle-pause)
         ("<f9>" . gif-screencast-stop))
  :config
  (setq gif-screencast-output-directory (expand-file-name "~/Pictures/Screencasts")))

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :general
  (my/leader-keys
    "ot" 'multi-vterm)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          ("\\*Async Shell Command\\*" . hide)
          "\\*Flycheck errors\\*"
          help-mode
          compilation-mode))
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                  "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                  "^\\*term.*\\*$"   term-mode   ;term as a popup
                  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                  )))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  ;; (setq popper-group-function #'popper-group-by-directory)
  (setq popper-window-height '20))

(use-package pkgbuild-mode
  :ensure nil)

(use-package geiser-guile)
(use-package macrostep-geiser
  :after geiser-mode
  :config
  (add-hook 'geiser-mode-hook #'macrostep-geiser-setup)
  (add-hook 'geiser-repl-mode-hook #'macrostep-geiser-setup))

(use-package shelldon
  :config
  (add-hook 'shelldon-mode-hook 'ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

  ;; Use `display-buffer-alist' to put shelldon output buffer at buttom of window
  ;; (setf (alist-get "*\\(shelldon.*\\)" display-buffer-alist)
  ;;       `((display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
  ;;         (side . bottom)
  ;;         (slot . 0)
  ;;         (reusable-frames . visible)))
  )

(use-package bongo
  :general
  (my/local-leader-keys
    "b" '(:ignore t :wk "bongo")
    "bb" 'bongo-list-buffers
    "bd" 'bongo-dired-library-mode
    "bk" 'bongo-kill
    "bl" 'bongo-dired-play-line
    "be" 'bongo-dired-append-enqueue-lines
    "bi" 'bongo-dired-insert-enqueue-lines
    "bp" 'bongo-playlist
    "bq" 'bongo-quit
    "b RET" 'bongo-dired-dwim
    "b SPC" 'bongo-pause/resume))

(use-package ruby-mode
  :ensure nil
  :defer t
  :mode (("Appraisals\\'" . ruby-mode)
         ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . ruby-mode)
         ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\|pryrc\\)\\'" . ruby-mode))
  :custom
  (ruby-insert-encoding-magic-comment nil "Not needed in Ruby 2")
  :config
  (use-package rbenv
    :defer t
    :init
    (setq rbenv-installation-dir "/usr/")
    (defun spacemacs//enable-rbenv ()
      "Enable rbenv, use .ruby-version if exists."
      (require 'rbenv)
      (let ((version-file-path (rbenv--locate-file ".ruby-version")))
        (global-rbenv-mode)
        ;; try to use the ruby defined in .ruby-version
        (if version-file-path
            (progn
              (rbenv-use (rbenv--read-version-from-file
                          version-file-path))
              (message (concat "[rbenv] Using ruby version "
                               "from .ruby-version file.")))
          (message "[rbenv] Using the currently activated ruby."))))
    (setq rbenv-show-active-ruby-in-modeline nil)
    :hook
    (ruby-mode . spacemacs//enable-rbenv)))

(use-package inf-ruby)

;; (use-package robe
;;   :hook
;;   (ruby-mode . robo-mode))

(use-package ruby-test-mode
  :after ruby-mode
  :diminish ruby-test-mode
  :config
  (defun amk-ruby-test-pretty-error-diffs (old-func &rest args)
    "Make error diffs prettier."
    (let ((exit-status (cadr args)))
      (apply old-func args)
      (when (> exit-status 0)
        (diff-mode)
        ;; Remove self
        (advice-remove #'compilation-handle-exit #'amk-ruby-test-pretty-error-diffs))))
  (defun amk-ruby-test-pretty-error-diffs-setup (old-func &rest args)
    "Set up advice to enable pretty diffs when tests fail."
    (advice-add #'compilation-handle-exit :around #'amk-ruby-test-pretty-error-diffs)
    (apply old-func args))
  (advice-add #'ruby-test-run-command :around #'amk-ruby-test-pretty-error-diffs-setup))

(use-package anaconda-mode
  :defer t
  ;; :hook
  ;; (python-mode . anaconda-mode)
  :config
  (set-lookup-handlers! 'anaconda-mode
    :definition #'anaconda-mode-find-definitions
    :references #'anaconda-mode-find-references))

(use-package company-anaconda
  :after anaconda-mode
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package cython-mode
  :defer t
  :general
  (my/leader-keys 'cython-mode
    "hh" 'anaconda-mode-show-doc
    "gu" 'anaconda-mode-find-references))

(defvar my/project-after-switch-project-hook nil
  "Hoook for `project-switch-project'. Hook functions are called after switching project")

(defun my/project-after-switch-project ()
  "Switch project in projec.el"
  (project-switch-project)
  (run-hooks 'my/project-after-switch-project-hook))

(defvar python-auto-set-local-pyvenv-virtualenv 'on-visit
  "Automatically set pyvenv virtualenv from \"venv\".
Possible values are `on-visit', `on-project-switch' or `nil'.")

(defvar spacemacs--python-pyvenv-modes nil
  "List of major modes where to add pyvenv support.")

(defun spacemacs//pyvenv-mode-set-local-virtualenv ()
  "Set pyvenv virtualenv from \"venv\" by looking in parent directories.
Handle \"venv\" being a virtualenv directory or a file specifying either
absolute or relative virtualenv path. Relative path is checked relative to
location of \"venv\" file, then relative to pyvenv-workon-home()."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory "venv")))
    (when root-path
      (let ((file-path (expand-file-name "venv" root-path)))
        (cond ((file-directory-p file-path)
               (pyvenv-activate file-path) (setq-local pyvenv-activate file-path))
              (t (let* ((virtualenv-path-in-file
                         (with-temp-buffer
                           (insert-file-contents-literally file-path)
                           (buffer-substring-no-properties (line-beginning-position)
                                                           (line-end-position))))
                        (virtualenv-abs-path
                         (if (file-name-absolute-p virtualenv-path-in-file)
                             virtualenv-path-in-file
                           (format "%s/%s" root-path virtualenv-path-in-file))))
                   (cond ((file-directory-p virtualenv-abs-path)
                          (pyvenv-activate virtualenv-abs-path)
                          (setq-local pyvenv-activate virtualenv-abs-path))
                         (t (pyvenv-workon virtualenv-path-in-file)
                            (setq-local pyvenv-workon virtualenv-path-in-file))))))))))

(defun spacemacs/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around https://github.com/pyenv/pyenv-which-ext
"
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command)))
              (pyenv-version-names (split-string (string-trim (shell-command-to-string "pyenv version-name")) ":"))
              (executable nil)
              (i 0))
          (if (not (string-match "not found" pyenv-string))
              (while (and (not executable)
                          (< i (length pyenv-version-names)))
                (if (string-match (elt pyenv-version-names i) (string-trim pyenv-string))
                    (setq executable (string-trim pyenv-string)))
                (if (string-match (elt pyenv-version-names i) "system")
                    (setq executable (string-trim (executable-find command))))
                (setq i (1+ i))))
          executable))
    (executable-find command)))

(defun spacemacs//python-setup-shell (&rest args)
  (if (spacemacs/pyenv-executable-find "ipython")
      (progn
        (setq python-shell-interpreter "ipython")
        (let ((version (replace-regexp-in-string "\\(\\.dev\\)?[\r\n|\n]$" ""
                                                 (shell-command-to-string
                                                  (format "\"%s\" --version"
                                                          (string-trim (spacemacs/pyenv-executable-find "ipython")))))))
          (if (or (version< version "5")
                  (string-blank-p version))
              (setq python-shell-interpreter-args "-i")
            (setq python-shell-interpreter-args "--simple-prompt -i"))))
    (progn
      (setq python-shell-interpreter-args "-i"
            python-shell-interpreter "python"))))


(defun spacemacs//python-setup-checkers (&rest args)
  (when (fboundp 'flycheck-set-checker-executable)
    (let ((pylint (spacemacs/pyenv-executable-find "pylint"))
          (flake8 (spacemacs/pyenv-executable-find "flake8")))
      (when pylint
        (flycheck-set-checker-executable "python-pylint" pylint))
      (when flake8
        (flycheck-set-checker-executable "python-flake8" flake8)))))

(defun spacemacs/python-setup-everything (&rest args)
  (apply 'spacemacs//python-setup-shell args)
  (apply 'spacemacs//python-setup-checkers args))

(use-package pyvenv
  :init
  (progn
    (add-hook 'python-mode-hook #'pyvenv-tracking-mode)
    (add-to-list 'spacemacs--python-pyvenv-modes 'python-mode)
    (pcase python-auto-set-local-pyvenv-virtualenv
      ('on-visit
       (dolist (m spacemacs--python-pyvenv-modes)
         (add-hook (intern (format "%s-hook" m))
                   'spacemacs//pyvenv-mode-set-local-virtualenv)))
      ('on-project-switch
       (add-hook 'my/project-after-switch-project-hook
                 'spacemacs//pyvenv-mode-set-local-virtualenv)))
    ;; setup shell correctly on environment switch
    (dolist (func '(pyvenv-activate pyvenv-deactivate pyvenv-workon))
      (advice-add func :after 'spacemacs/python-setup-everything))))


(use-package python-mode
  :ensure nil
  :mode "\\.py\\'"
  :hook
  (python-mode . pyvenv-tracking-mode)
  :custom
  (python-indent-offset 4))

(use-package py-isort
  :defer t
  :preface
  ;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
  (defun spacemacs//python-remove-unused-imports ()
    "Use Autoflake to remove unused function"
    "autoflake --remove-all-unused-imports -i unused_imports.py"
    (interactive)
    (if (executable-find "autoflake")
        (progn
          (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                                 (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t))
      (message "Error: Cannot find autoflake executable.")))

  (defun spacemacs//python-sort-imports ()
    ;; py-isort-before-save checks the major mode as well, however we can prevent
    ;; it from loading the package unnecessarily by doing our own check
    (interactive)
    (when (derived-mode-p 'python-mode)
      (py-isort-before-save))))

(use-package python-black
  :demand t
  :after python
  :general
  (my/leader-keys
    :keymaps 'python-mode-map
    "bp" 'python-black))

(use-package gendoxy
  :ensure nil
  :general
  (my/leader-keys
    "at" 'gendoxy-tag
    "ah" 'gendoxy-header))

(use-package highlight-doxygen
  :ensure nil
  :hook
  (c-mode . highlight-doxygen-mode)
  (c++-mode . highlight-doxygen-mode))

(use-package meson-mode
  :config
  (add-hook 'meson-mode-hook 'company-mode))

(use-package gdb-mi
  :ensure nil
  :general
  (my/local-leader-keys
    "og" 'gdb)
  :init
  (setq gdb-many-windows t))

(use-package d-mode)

(use-package show-keys
  :ensure nil)

;;; Restore file-name-hander-alist
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist doom--file-name-handler-alist)))
