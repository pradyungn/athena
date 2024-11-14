;;; -- Settings and Setup --
;; (setq gc-cons-threshold most-positive-fixnum)
;; (add-hook 'emacs-startup-hook
;; 	        (lambda ()
;; 	          (setq gc-cons-threshold (* 2 1000 1000))))

;; shut up emacs ;-;
(setq inhibit-startup-message t)
(setq auto-revert-verbose nil)
(setq ring-bell-function 'ignore)

(scroll-bar-mode -1) ;; no scrollbar
(tool-bar-mode -1)   ;; no toolbar
(tooltip-mode -1)    ;; no tooltips
(menu-bar-mode -1)   ;; no menu bar

;; add some left/right padding for pane, don't show trunc arrows
;; (fringe-mode '(15 . 15))
;; (setq-default fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))
(fringe-mode 0)

;; horizontal splits by default
(setq split-width-threshold nil)

;; default font - if mountain fails, don't want to be blinded
;; (load-theme 'wombat t) ;; disable if possible, has some weird override behavior with other themes

;; font scaling - allows for standardized font sizing
(setq face-font-rescale-alist '((".*Crimson Text.*" . 1.175)))

;; fonts
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil        :font "Myosevka Semi-Condensed" :height 110 :weight 'regular)
  (set-face-attribute 'fixed-pitch nil    :font "Myosevka Semi-Condensed" :height 110 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font "Crimson Text"             :height 110 :weight 'regular))

(when (eq system-type 'darwin)
  ;; (set-face-attribute 'default nil :font "PragmataPro Mono Liga" :height 130 :weight 'light)
  ;; (set-face-attribute 'fixed-pitch nil :font "PragmataPro Mono Liga" :height 130 :weight 'light)
  (set-face-attribute 'default nil        :font "Myosevka Semi-Condensed" :height 130 :weight 'light)
  (set-face-attribute 'fixed-pitch nil    :font "Myosevka Semi-Condensed" :height 130 :weight 'light)
  (set-face-attribute 'variable-pitch nil :font "Crimson Text"             :height 130 :weight 'regular)

  (setenv "LIBRARY_PATH" (string-join
                          '("/opt/homebrew/opt/gcc/lib/gcc/14"
                            "/opt/homebrew/opt/libgccjit/lib/gcc/14"
                            "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin24/14")
                          ":")))

;; prevent resize window on startup
(setq frame-inhibit-implied-resize t)
(add-to-list 'initial-frame-alist '(internal-border-width . 20))

;; smooth scroll settings - pulled from emacs wiki
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time

;; don't hide trailing whitespace (vscode is bad)
(setq-default show-trailing-whitespace nil)

;; show line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers if in a prose mode
(dolist (mode '(org-mode-hook
		            term-mode-hook
		            dashboard-mode-hook
		            eshell-mode-hook
		            vterm-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; prog mode reset
(setq-default tab-width 2)
(setq-default fill-column 80)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; personal settings
(setq user-full-name "Pradyun Narkadamilli"
      user-mail-address "pradyungn@gmail.com")

;; default org-mode location
(setq org-directory "~/Documents/Notes/")

;; Better window titling
(setq frame-title-format "Emacs (%b)")

;; set default PDF viewer
(when (eq system-type 'gnu/linux)
  (setq athena/pdf-viewer "zathura"))
(when (eq system-type 'darwin)
  (setq athena/pdf-viewer "open"))

;; other random settings
(setq undo-limit 80000000
      evil-want-fine-undo t)

(delete-selection-mode 1)
(setq backup-directory-alist `(("." . "~/.saves")))

(setq help-window-keep-selected t)

;; preventing large file idiocies
(defun athena/large-file-opt ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'athena/large-file-opt)

;; treesitter grammars - install and default
(setq treesit-language-source-alist
      '((bash     . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c        . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp      . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (python   . ("https://github.com/tree-sitter/tree-sitter-python"))
        (rust     . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (tcl      . ("https://github.com/tree-sitter-grammars/tree-sitter-tcl"))
        (verilog  . ("https://github.com/gmlarumbe/tree-sitter-systemverilog"))))

(dolist (lang treesit-language-source-alist)
  (when (not (treesit-language-available-p (car lang)))
    (treesit-install-language-grammar (car lang))))

(setq major-mode-remap-alist
      '((bash-mode       . bash-ts-mode)
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (python-mode     . python-ts-mode)
        (rust-mode       . rust-ts-mode)
        (tcl-mode        . tcl-ts-mode)
        (verilog-mode    . verilog-ts-mode)))
