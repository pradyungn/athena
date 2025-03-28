;;; -- Package Installs and Configs --

;; index local packages
(add-to-list 'load-path (concat user-emacs-directory "pkg"))
(byte-recompile-directory (concat user-emacs-directory "pkg") 0)

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

;; garbage collector magic hack
;; via https://akrl.sdf.org
(use-package gcmh
  :ensure nil)
(gcmh-mode 1)

;; Vertico
(use-package vertico
  :init (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-;" . vertico-exit-input)
              :map minibuffer-local-map
              ("M-h" . backward-kill-word)))

(use-package vertico-directory
  :after vertico
  :ensure nil  ;; no need to install, it comes with vertico
  :bind (:map vertico-map
    ("DEL" . vertico-directory-delete-char)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :custom
  (consult-preview-key nil))

;; enable doom theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package doom-themes
  :init (load-theme 'doom-mountain t)
  :config
  (doom-themes-org-config))

;; Icons
(use-package nerd-icons)

;; Athena Modeline
(use-package athena-modeline
  :after nerd-icons
  :ensure nil
  :config
  (athena-modeline))

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

;; better help functions
(defun athena/display-buffer-here (buffer)
  (display-buffer-same-window buffer nil))

(use-package helpful
  :custom
  (helpful-switch-buffer-function 'athena/display-buffer-here)
  ;; (counsel-describe-function-function #'helpful-callable)
  ;; (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-function)
  ([remap describe-variable] . helpful-variable))

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
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  ;; (setq evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  (evil-undo-system 'undo-fu)
  (evil-echo-state nil)

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
  :custom
  ;;(setq vterm-buffer-name-string "term %s")
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000))

(add-hook 'vterm-mode-hook
          (lambda ()
            (face-remap-add-relative 'mode-line-active   '(:background "#0f0f0f" :foreground "#bfbfbf" :overline "#191919"))
            (face-remap-add-relative 'mode-line-inactive '(:background "#0f0f0f" :overline "#191919"))
            (setq mode-line-format
                  (list "  %b"
                        '(:eval (let ((host (file-remote-p default-directory 'host)))
                                  (and host (concat "[" host "]"))))
                        '(:eval (if (eq evil-state 'emacs) " | bypass mode"))))
            (setq hscroll-margin 0)
            (set-frame-parameter nil 'bottom-divider-width 1)
            (setq config-kill-processes nil)
            (add-to-list 'vterm-tramp-shells '("ssh" "/bin/zsh"))
            (add-to-list 'vterm-tramp-shells '("sudo" "/bin/zsh"))
            (add-to-list 'vterm-tramp-shells '("ssh" "/bin/zsh"))
            (add-to-list 'vterm-tramp-shells '("sudo" "/bin/zsh"))))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

;; path management
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; install tool dependencies
(let ((athena/bindir (concat user-emacs-directory "bin")))
  (mkdir (concat athena/bindir "/verible") 'p-opt)
  (add-to-list 'exec-path (concat athena/bindir "/verible"))
  (when (not (executable-find "verible-verilog-format"))
    (if (eq system-type 'darwin)
        (url-copy-file "https://github.com/chipsalliance/verible/releases/download/v0.0-3836-g86ee9bab/verible-v0.0-3836-g86ee9bab-macOS.tar.gz"
                       (concat athena/bindir "/verible.tar.gz") 'overwrite)
      (url-copy-file "https://github.com/chipsalliance/verible/releases/download/v0.0-3836-g86ee9bab/verible-v0.0-3836-g86ee9bab-linux-static-x86_64.tar.gz"
                     (concat athena/bindir "/verible.tar.gz") 'overwrite))
    (call-process-shell-command (concat "tar -xzvf " athena/bindir
                                        "/verible.tar.gz --strip-components=2 -C "
                                        athena/bindir "/verible"))))

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
  :hook (markdown-ts-mode . visual-line-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	            ("C-c C-e" . markdown-do)))

;; Outlining for org mode
(use-package outshine)

;; Verilog
(use-package verilog-ts-mode
  :defer t
  :custom
  (verilog-case-indent 2)
  (verilog-cexp-indent 2)
  (verilog-indent-level 2)
  (verilog-indent-level-module 2)
  (verilog-indent-level-directive 2)
  (verilog-indent-level-declaration 2)
  (verilog-ts-indent-level 2))

;; more verilog features
(use-package verilog-ext
  :custom
  (verilog-ext-feature-list
   '(font-lock
     xref
     hierarchy
     template
     ;; formatter
     hideshow
     typedefs
     block-end-comments
     ports))
  (verilog-ext-formatter-indentation-spaces 2)
  (verilog-ext-formatter-wrap-spaces 2)
  :config
  (verilog-ext-mode-setup)
  :hook
  (verilog-ts-mode . verilog-ext-mode))

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
  (org-latex-compiler "lualatex")
  (org-startup-folded t)
  (org-capture-bookmark nil)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (ort-export-with-smart-quotes t)
  (org-latex-pdf-process
   '("lualatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-babel-python-command "python3")
  (org-agenda-files '("~/Documents/Notes/Roam/main/"))
  (org-todo-keyword-faces '(("REQUIRED" . "#ac8a8c")
                            ("IP"       . org-todo)
                            ("TODO"     . org-todo)
                            ("DONE"     . org-done)
                            ("STALE"    . "#8aacab")))
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))))

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
  (org-roam-database-connector 'sqlite-builtin)
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
  (dashboard-projects-switch-function 'projectile-switch-project-by-name)
  (dashboard-items '((recents  . 8))))

;; dashboard hook doesn't really work
(setq initial-buffer-choice (lambda ()
			                        (get-buffer-create "*dashboard*")
			                        (dashboard-open)))

(defun athena/diredp-init ()
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'diredp-up-directory-reuse-dir-buffer
    "l" 'diredp-find-file-reuse-dir-buffer
    "s" 'athena/dired-open-in-system)
  (diredp-toggle-find-file-reuse-dir 1)
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil))

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (insert-directory-program (if (eq system-type 'darwin) "gls" "ls"))
           (dired-use-ls-dired (eq system-type 'darwin))))

(use-package dired+
  :ensure nil
  :after dired
  :config (athena/diredp-init))

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
(use-package yasnippet
  :custom
  (yas-snippet-dirs
   `(,(concat user-emacs-directory "snippets")))
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :after yasnippet)

;; auctex
(use-package tex
  :defer t
  :ensure auctex
  :custom
  (TeX-engine 'xetex)
  (TeX-PDF-mode t)
  :config
  (general-def 'normal TeX-mode-map
    "SPC op" '(athena/compile-tex-pdf :which-key "pdf export")
    "SPC ob" '(athena/compile-tex-bib :which-key "pdf export")
    "SPC ov" '(athena/open-tex-pdf    :which-key "pdf viewer")))

;; GPU time baybee
(use-package cuda-mode
  :defer t)

;; crab rave
(use-package rust-mode
  :defer t
  :custom
  (rust-ts-mode-indent-offset 2))

;; embrace the darkness
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; return to monke, embrace RSS
(defun athena/elfeed-show-config ()
  (visual-line-mode 1)
  (display-line-numbers-mode -1))

(use-package elfeed
  :hook
  (elfeed-show-mode . athena/elfeed-show-config)
  :custom
  (elfeed-feeds '("https://chrisdone.com/rss.xml"
                  "https://planet.emacslife.com/atom.xml"
                  "https://www.bloomberg.com/opinion/authors/ARbTQlRLRjE/matthew-s-levine.rss"
                  "https://hnrss.org/frontpage"
                  "https://lobste.rs/rss")))

;; Better PDF viewer
(use-package pdf-tools
  :init
  (pdf-loader-install))

(use-package wavedrom-mode
  :custom
  (wavedrom-output-format "svg")
  (wavedrom-output-directory "~/.wavedrom"))
