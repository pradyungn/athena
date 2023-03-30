;; startup time - stolen directly from efs
(setq gc-cons-threshold (* 50 1000 1000))

(defun hades/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'hades/display-startup-time)

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;; no scrollbar
(tool-bar-mode -1) ;; no toolbar
(tooltip-mode -1) ;; no tooltips
(menu-bar-mode -1) ;; no menu bar

(set-fringe-mode 5) ;; padding!

(set-face-attribute 'default nil :font "Iosevka" :height 90) ;; set font

(add-to-list 'custom-theme-load-path "/home/pradyungn/Documents/emacs.hades/themes/")
(load-theme 'wombat) ;; temporary theme, it'll work for now

;; smooth scroll settings - pulled from emacs wiki
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; vim-like cancellation
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; trailing whitespace
(setq-default show-trailing-whitespace nil)

;; set font
;; (on-platform-do
;; (osx (set-face-attribute 'default nil :font "Fira Mono" :height 12))
;; (linux ))

;; package manager
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Init use-package on non-linux. we need this for macbook :/
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; make sure all usd packages are installed
(require 'use-package)
(setq use-package-always-ensure t)

(setq explicit-shell-file-name "/bin/bash")

;; disable line numbers if in a "writing" mode
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; replace default emacs functionality with saner alternatives
(use-package swiper)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("V-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; enable doom theme
(use-package doom-themes
  :init (load-theme 'doom-mountain t))

;; Doom modeline with icons
(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (
	   (doom-modeline-height 40)))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.25))

(use-package ivy-rich :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; better help functions
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; vim bindings, my love
;; undo system for evil
(use-package undo-fu)

(use-package general
  :after evil
  :config
  (general-evil-setup t)

  (general-create-definer hades/leader-keys
    :states '(insert visual emacs normal)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  )

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; sane defaults
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Oonicorn
(use-package org)

;; Hydra for nice stuffs
(use-package hydra)

;; buffer hydra
(defhydra hydra-buffers (:exit t)
  ("b" counsel-switch-buffer "change buffer")
  ("k" kill-this-buffer "kill da buffer")
  ("n" next-buffer "next buffer")
  ("l" previous-buffer "prev buffer")
  ("o" evil-switch-to-windows-last-buffer "last buffer"))

;; window management hydra
(defhydra hydra-windows (:exit t :timeout 3 :hint nil :idle 1.5)
  "
  ^_k_^     _c_lose     _=_ inc height
_h_   _l_   _n_ew       _-_ dec height
  ^_j_^     _v_split    _>_ inc width
^^^^        ^ ^         _<_ dec height
"
  ;;   "
  ;; HadesWM is active
  ;; "
  ("h" evil-window-left)
  ("j" evil-window-down)
  ("k" evil-window-up)
  ("l" evil-window-right)
  ("c" evil-window-delete)
  ("n" evil-window-split)
  ("v" evil-window-vsplit)
  ("H" windmove-swap-states-left)
  ("J" windmove-swap-states-down)
  ("K" windmove-swap-states-up)
  ("L" windmove-swap-states-right)
  (">" evil-window-increase-width :exit nil)
  ("<" evil-window-decrease-width :exit nil)
  ("=" evil-window-increase-height :exit nil )
  ("-" evil-window-decrease-height :exit nil)
  ("?" (setq hydra-is-helpful t) :exit nil))

;; vterm
(use-package vterm)

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; assembling leader-based keybinds 
(hades/leader-keys
  ";" '(counsel-M-x :which-key "M-x")
  "b" '(hydra-buffers/body :which-key "buffer commands")
  "w" '(hydra-windows/body :which-key "window management")
  "nn" '(comment-dwim :which-key "comment toggle")
  "." '(find-file :which-key "browser")
  "p" '(:keymap projectile-command-map :which-key "projects")
  "gg" '(magit-status :which-key "magit"))


(use-package magit)

;; text-scaling
(general-define-key
 "C--" 'text-scale-decrease)
(general-define-key
 "C-=" 'text-scale-increase)

;; fun binds
(general-define-key
 "M-w" 'counsel-switch-buffer :which-key "windows")

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
