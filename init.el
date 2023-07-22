(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
	      (lambda ()
	        (setq gc-cons-threshold (* 2 1000 1000))))

;; startup time - stolen directly from efs
;; (defun hades/display-startup-time ()
;;   (message "Emacs loaded in %s with %d garbage collections."
;; 		   (format "%.2f seconds"
;; 				   (float-time
;; 					(time-subtract after-init-time before-init-time)))
;; 		   gcs-done))

;; (add-hook 'emacs-startup-hook #'hades/display-startup-time)

;; shut up emacs ;-;
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(scroll-bar-mode -1) ;; no scrollbar
(tool-bar-mode -1) ;; no toolbar
(tooltip-mode -1) ;; no tooltips
(menu-bar-mode -1) ;; no menu bar
(fringe-mode 0)

;; horizontal splits by default
(setq split-width-threshold nil)

;; default font - if mountain fails, don't want to be blinded
(load-theme 'wombat t)

;; fonts
(set-face-attribute 'default nil :font "Iosevka" :height 90)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 90)
(set-face-attribute 'variable-pitch nil :font "EB Garamond" :height 115 :weight 'regular)

;; (on-platform-do (osx (set-face-attribute 'default nil :font "Fira Mono" :height 12))
;; 				(linux ))

;; prevent resize window on startup
(setq frame-inhibit-implied-resize t)
(add-to-list 'initial-frame-alist '(internal-border-width . 12))

;; smooth scroll settings - pulled from emacs wiki
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; vim-like cancellation
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; trailing whitespace
(setq-default show-trailing-whitespace nil)

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

;; disable line numbers if in a "writing" mode
(dolist (mode '(org-mode-hook
		        term-mode-hook
		        dashboard-mode-hook
		        eshell-mode-hook
		        vterm-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; prog mode reset
(setq-default tab-width 4)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; Init use-package on non-linux. we need this for macbook :/
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; make sure all usd packages are installed
(require 'use-package)
(setq use-package-always-ensure t)

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
	     ("C-d" . ivy-switch-buffer-kill)
	     :map ivy-reverse-i-search-map
	     ("C-k" . ivy-previous-line)
	     ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

;; enable doom theme
(add-to-list 'custom-theme-load-path "/home/pradyungn/Documents/emacs.hades/themes/")
(use-package doom-themes
  :init (load-theme 'doom-mountain t)
  :config
  (doom-themes-org-config))

;; Icons
(use-package all-the-icons)
(use-package nerd-icons)

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (
	       (doom-modeline-height 40)
	       ))
(use-package hide-mode-line)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;; (setq user-emacs-directory "~/.cache/emacs")

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
(use-package undo-fu
  :after evil)

;; session persistence
(use-package undo-fu-session)
(undo-fu-session-global-mode)

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

(use-package evil-snipe
  :after evil
  :hook (prog-mode . (lambda () (evil-snipe-local-mode +1))))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; sane defaults
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; window functions - stolen from doom emacs (hlissner ily)
(defun hades/window-swap (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
	     (this-buffer (current-buffer))
	     (that-window (windmove-find-other-window direction nil this-window))
	     (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
	          (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
	    (funcall (pcase direction
		           ('left  #'evil-window-move-far-left)
		           ('right #'evil-window-move-far-right)
		           ('up    #'evil-window-move-very-top)
		           ('down  #'evil-window-move-very-bottom)))
      (unless that-window
	    (setq that-window
	          (split-window this-window nil
			                (pcase direction
			                  ('up 'above)
			                  ('down 'below)
			                  (_ direction))))
	    (with-selected-window that-window
	      (switch-to-buffer (doom-fallback-buffer)))
	    (setq that-buffer (window-buffer that-window)))
      (window-swap-states this-window that-window)
      (select-window that-window))))

(defun hades/window-move-left ()
  "Swap windows to the left."
  (interactive) (hades/window-swap 'left))
(defun hades/window-move-right ()
  "Swap windows to the right"
  (interactive) (hades/window-swap 'right))
(defun hades/window-move-up ()
  "Swap windows upward."
  (interactive) (hades/window-swap 'up))
(defun hades/window-move-down ()
  "Swap windows downward."
  (interactive) (hades/window-swap 'down))

;; Hydra for nice stuffs
(use-package hydra)

;; buffer hydra
(defhydra hydra-buffers (:exit t :idle 1 :timeout 2)
  ("b" counsel-switch-buffer              "change buffer")
  ("k" kill-this-buffer                   "kill da buffer")
  ("n" next-buffer                        "next buffer")
  ("l" previous-buffer                    "prev buffer")
  ("o" evil-switch-to-windows-last-buffer "last buffer")
  ("c" clone-indirect-buffer              "clone buffer")
  ("s" clone-indirect-buffer-other-window "split buffer")
  ("m" counsel-bookmark                   "bookmarks"))

;; window management hydra
(defhydra hydra-windows (:exit t :idle 1.5 :timeout 3 :hint nil)
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
  ("H" hades/window-move-left)
  ("J" hades/window-move-down)
  ("K" hades/window-move-up)
  ("L" hades/window-move-right)
  (">" evil-window-increase-width :exit nil)
  ("<" evil-window-decrease-width :exit nil)
  ("=" evil-window-increase-height :exit nil )
  ("-" evil-window-decrease-height :exit nil)
  ("?" (setq hydra-is-helpful t) :exit nil))

;; vterm - config stolen from doom emacs
(use-package vterm
  :hook (vterm-mode . hide-mode-line-mode)
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

;; (add-to-list 'display-buffer-alist
;;              '("\\`\\*vterm\\*\\(?:<[[:digit:]]+>\\)?\\'"
;;                (display-buffer-in-side-window (side . bottom))))

(add-hook 'vterm-mode-hook
	      (lambda ()
	        (setq config-kill-processes nil)
	        (setq hscroll-margin 0)
            (add-to-list 'vterm-tramp-shells '("ssh" "/bin/zsh"))
            (add-to-list 'vterm-tramp-shells '("sudo" "/bin/zsh"))
            (add-to-list 'vterm-tramp-shells '("ssh" "/bin/zsh"))
            (add-to-list 'vterm-tramp-shells '("sudo" "/bin/zsh"))
            ))

;; (cl-loop for file in '("/usr/local/bin/zsh" "/bin/zsh")
;;         when (file-exists-p file)
;;         do (progn
;;             (setq shell-file-name file)
;;             (cl-return)))
;; (setenv "SHELL" shell-file-name)


;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; path management
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; auto-format
(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

;; magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; markdown
(use-package markdown-mode
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	          ("C-c C-e" . markdown-do)))

;; Verilog
(use-package verilog-mode)

;; org mode!!
(defun hades/org-init ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq org-fontify-quote-and-verse-blocks t))

(defun hades/org-fonts ()
  (dolist (face '((org-level-1 . 1.5)
		          (org-level-2 . 1.4)
		          (org-level-3 . 1.2)
		          (org-level-4 . 1.0)))
    (set-face-attribute (car face) nil :font "Outfit" :weight 'bold :height (cdr face)))

  (plist-put org-format-latex-options :scale 1.4)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch :height 100)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch :height 100)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch :height 100)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch) :height 100)
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch) :height 100)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch) :height 100)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch) :height 100)
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch) :height 100)
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch :height 100)

  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE") (sequence "STALE" "|") (sequence "REQUIRED" "IP" "|" "DONE"))))

(use-package org
  :pin org
  :hook (org-mode . hades/org-init)
  :config
  (hades/org-fonts)
  (add-to-list 'org-modules 'org-tempo)
  :custom
  (org-capture-bookmark nil)
  (org-hide-emphasis-markers t)
  (org-agenda-files '("~/Documents/Notes/Tasks.org"))
  (org-todo-keyword-faces '(("REQUIRED" . "#ac8a8c")
                            ("IP" . org-todo)
                            ("TODO" . org-todo)
                            ("DONE" . org-done)
                            ("STALE" . "#8aacab"))))

(general-def 'normal org-mode-map
  "RET" 'org-open-at-point)

;; Aesthetics :)
(setq org-startup-folded t)

;; org-roam setup
(defvar hades/roam-map
  (let ((map (make-sparse-keymap)))
    (define-key map "l" 'org-roam-buffer-toggle)
    (define-key map "f" 'org-roam-node-find)
    (define-key map "i" 'org-roam-node-insert)
    map)
  "roam keymap")

(use-package org-roam
  :custom
  (org-roam-directory "~/Documents/Notes/Roam")
  :config
  (hades/leader-keys "n" '(:keymap hades/roam-map :package org-roam))
  (org-roam-setup))

;; weird pdflatex bug
(setq org-latex-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(defun hades/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	    visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . hades/org-mode-visual-fill))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list
   ;; '("◉" "○" "●" "○" "●" "○" "●"))
   '("\u200b"))
  )

(font-lock-add-keywords 'org-mode
			            '(("^ *\\([-]\\) "
			               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title "Oh, not this shit again...")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-projects-backend 'projectile)
  (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (dashboard-items '((recents  . 5)
		             (bookmarks . 3)
		             (projects . 3)
		             (agenda . 3))))

;; dashboard hook doesn't really work
(setq initial-buffer-choice (lambda ()
			                  (get-buffer-create "*dashboard*")
			                  (dashboard-refresh-buffer)))

;; dired customization ... stolen from EFS and dired-single docs
(defun hades/dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>:config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  (evil-collection-define-key 'normal 'dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (evil-collection-define-key 'normal 'dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (evil-collection-define-key 'normal 'dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config (hades/dired-init))

(use-package dired-single)

(use-package all-the-icons-dired
  :if
  (display-graphic-p)
  :hook
  (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

;; LSP stuff
(defun hades/lsp-init ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (general-define-key
   :keymaps 'local
   :states '(normal visual) :prefix "SPC"
   "c" '(:keymap lsp-command-map :which-key "lsp")))

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . hades/lsp-init)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy
  :after lsp)

;; assembling leader-based keybinds
(defun hades/find-file ()
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       #'counsel-projectile-find-file
     #'counsel-find-file)))

(byte-compile 'hades/find-file)

(hades/leader-keys
  "SPC" '(hades/find-file :which-key "dynamic file-find")
  "." '(find-file :which-key "file finder")
  "/" '(projectile-ripgrep :which-key "rg nyoom")

  ";" '(counsel-M-x :which-key "M-x")
  "b" '(hydra-buffers/body :which-key "buffer commands")
  "w" '(hydra-windows/body :which-key "window management")
  "nn" '(comment-dwim :which-key "comment toggle")
  "p" '(:keymap projectile-command-map :which-key "projects")

  "gg" '(magit-status :which-key "magit")
  "gb" '(magit-blame :which-key "whodunnit")
  "gi" '(vc-annotate :which-key "investigate")

  "ce" '(lsp :which-key "lsp-enable"))

;; text-scaling
(general-define-key
 "C--" 'text-scale-decrease)
(general-define-key
 "C-=" 'text-scale-increase)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; personal settings
(setq user-full-name "Pradyun Narkadamilli"
      user-mail-address "pradyungn@gmail.com")

(setq org-directory "~/Documents/Notes/")

;; other random settings
(setq undo-limit 80000000
      evil-want-fine-undo t)

(delete-selection-mode 1)
(setq backup-directory-alist `(("." . "~/.saves")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d225c008d53d789cdd96e5f3f1a1be77f1eeb4883a82f6345c2a2782bc603275" "603876c8fe23371998d7aa13dc488fd6cb6167f2a74ae9db46ffdf6987d90018" "6d4309dd9dcab7cbb8fd8cb3982273d7923e8aea903a397eacf042e1ed4473f4" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "5a616566cd92da30acd38f0c403e46e214301651db2a66c4062c7801adc7d24b" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "0ed3704b821ef38be5bfa7f2d10639b3cfb7ecbea9d86edf6a85214074eb2212" "9aff615f9069aff51f92b1463c21d47ad6138f5ffcd546cc245383be0b3d7a0f" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" default))
 '(package-selected-packages
   '(evil-surround org-roam evil-snipe hide-mode-line lsp-mode ein markdown-mode which-key vterm visual-fill-column use-package undo-fu-session undo-fu rainbow-delimiters org-bullets no-littering magit ivy-rich hydra helpful general format-all evil-collection doom-themes doom-modeline dired-single dashboard counsel-projectile all-the-icons-dired))
 '(warning-suppress-types '((emacs) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
