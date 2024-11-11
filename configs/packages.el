;;; -- Package Installs and Configs --

;; index local packages
(add-to-list 'load-path (concat user-emacs-directory "pkg"))

;; garbage collector magic hack
;; via https://akrl.sdf.org
(require 'gcmh)
(gcmh-mode 1)

;; package manager
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			                   ("org" . "https://orgmode.org/elpa/")
			                   ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Init use-package on non-linux. we need this for macbook :/
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; make sure all used packages are installed
(require 'use-package)
(setq use-package-always-ensure t)

;; replace default emacs functionality with saner alternatives
(use-package swiper)
(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package doom-themes
  :init (load-theme 'doom-mountain t)
  :config
  (doom-themes-org-config))

;; Icons
;; (use-package all-the-icons)
(use-package nerd-icons)

;; Default Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 40)
	         (doom-modeline-bar-width 1)))

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
  :bind (:map minibuffer-local-map
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
  (general-evil-setup t))

(use-package evil-snipe
  :after evil
  :hook
  (prog-mode . (lambda () (evil-snipe-local-mode +1)))
  (org-mode . (lambda () (evil-snipe-local-mode +1))))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; (setq evil-want-C-u-scroll t)
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

;; Hydra for nice stuffs
(use-package hydra)

;; vterm - config stolen from doom emacs
(use-package vterm
  :hook (vterm-mode . hide-mode-line-mode)
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  ;;(setq vterm-buffer-name-string "term %s")
  )

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
            (add-to-list 'vterm-tramp-shells '("sudo" "/bin/zsh"))))

;; (cl-loop for file in '("/usr/local/bin/zsh" "/bin/zsh")
;;         when (file-exists-p file)
;;         do (progn
;;             (setq shell-file-name file)
;;             (cl-return)))
;; (setenv "SHELL" shell-file-name)


;; projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; path management - disabled to not clobber binary management
;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

;; auto-format
(setq athena/format-on-write-enable 1)
(defun athena/format-hook ()
  (if (> athena/format-on-write-enable 0)
      (apheleia-mode)))

(use-package apheleia
  :hook
  (prog-mode . athena/format-hook))

;; magit
(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

;; markdown
(use-package markdown-mode
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	            ("C-c C-e" . markdown-do)))

;; Outlining for org mode
(use-package outshine)

;; Verilog
(use-package verilog-mode
  :defer t)

;; more verilog features
(use-package verilog-ext
  :custom
  (verilog-ext-feature-list
   '(font-lock
     xref
     hierarchy
     template
     formatter
     hideshow
     typedefs
     block-end-comments
     ports))
  (verilog-ext-formatter-indentation-spaces 2)
  :config
  (verilog-ext-mode-setup)
  :hook
  (verilog-mode . verilog-ext-mode))

;; org mode!!
(defun athena/org-init ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq org-fontify-quote-and-verse-blocks t))

(defun athena/org-fonts ()
  (dolist (face '((org-level-1 . 1.5)
		              (org-level-2 . 1.35)
		              (org-level-3 . 1.2)
		              (org-level-4 . 1.0)))
    (set-face-attribute (car face) nil :font "Outfit" :weight 'bold :height (* 1.4 (cdr face))))

  (plist-put org-format-latex-options :scale 1)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (let ((fixedheight (if (eq system-type 'gnu/linux) 110
                       (if (eq system-type 'darwin) 130 110))))
    (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch                          :height fixedheight)
    (set-face-attribute 'org-table nil                          :inherit 'fixed-pitch                          :height fixedheight)
    (set-face-attribute 'org-formula nil                        :inherit 'fixed-pitch                          :height fixedheight)
    (set-face-attribute 'org-code nil                           :inherit '(shadow fixed-pitch)                 :height fixedheight)
    (set-face-attribute 'org-table nil                          :inherit '(shadow fixed-pitch)                 :height fixedheight)
    (set-face-attribute 'org-verbatim nil                       :inherit '(shadow fixed-pitch)                 :height fixedheight)
    (set-face-attribute 'org-special-keyword nil                :inherit '(font-lock-comment-face fixed-pitch) :height fixedheight)
    (set-face-attribute 'org-meta-line nil                      :inherit '(font-lock-comment-face fixed-pitch) :height fixedheight)
    (set-face-attribute 'org-checkbox nil                       :inherit 'fixed-pitch                          :height fixedheight))

  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE") (sequence "STALE" "|") (sequence "REQUIRED" "IP" "|" "DONE"))))

(use-package org
  :pin org
  :hook (org-mode . athena/org-init)

  :config
  (athena/org-fonts)
  (add-to-list 'org-modules 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (general-def 'normal org-mode-map
    "RET"    'org-open-at-point
    "SPC op" '(org-latex-export-to-pdf  :which-key "pdf export")
    "SPC ob" '(org-beamer-export-to-pdf :which-key "beamer export")
    "SPC ov" '(athena/open-org-pdf      :which-key "pdf viewer"))

  :custom
  (org-startup-folded t)
  (org-capture-bookmark nil)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-babel-python-command "python3")
  (org-agenda-files '("~/Documents/Notes/Roam/main/"))
  (org-todo-keyword-faces '(("REQUIRED" . "#ac8a8c")
                            ("IP"       . org-todo)
                            ("TODO"     . org-todo)
                            ("DONE"     . org-done)
                            ("STALE"    . "#8aacab")))
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))))

;; weird pdflatex bug

(defun athena/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	      visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . athena/org-mode-visual-fill))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list
   ;; '("◉" "○" "●" "○" "●" "○" "●"))
   '("\u200b")))

(use-package org-roam
  :custom
  (org-roam-directory "~/Documents/Notes/Roam")
  :config
  (org-roam-setup))

;; org roam UI
(use-package org-roam-ui
  :defer t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-update-on-save t))


(font-lock-add-keywords 'org-mode
			                  '(("^ *\\([-]\\) "
			                     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("n" "notes" plain "%?"
         :if-new (file+head "notes/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("t" "thoughts" plain "%?"
         :if-new (file+head "thoughts/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

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
  (dashboard-vertically-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-projects-backend 'projectile)
  (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (dashboard-items '((recents  . 8))))

;; dashboard hook doesn't really work
(setq initial-buffer-choice (lambda ()
			                        (get-buffer-create "*dashboard*")
			                        (dashboard-open)))

(defun athena/dired-init ()
  (require 'dired+)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'diredp-up-directory-reuse-dir-buffer
    "l" 'diredp-find-file-reuse-dir-buffer)
  (diredp-toggle-find-file-reuse-dir 1)
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil))

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (insert-directory-program (if (eq system-type 'darwin) "gls" "ls"))
           (dired-use-ls-dired (eq system-type 'darwin)))
  :config (athena/dired-init))

(use-package nerd-icons-dired
  :if
  (display-graphic-p)
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; search
(use-package rg)

;; Konchantrate
(use-package darkroom
  :defer t)

;; snippets (v useful)
(use-package yasnippet-snippets)
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; auctex
(use-package tex
  :defer t
  :ensure auctex
  :custom
  (TeX-engine 'luatex)
  (TeX-PDF-mode t)
  :config
  (general-def 'normal TeX-mode-map
    "SPC op" '(athena/compile-tex-pdf :which-key "pdf export")
    "SPC ov" '(athena/open-tex-pdf    :which-key "pdf viewer")))

;; GPU time baybee
(use-package cuda-mode
  :defer t)

;; crab rave
(use-package rust-mode
  :defer t)
