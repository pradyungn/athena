;; -*- lexical-binding: t -*-
;; -------------------------------------------------------------------
;; GNU Emacs / athena
;;
;; This file takes inspiration from Nicolas Rougier's infamous
;; nano-modeline and heavy snippets from occasionallyathena's as well.
;; -------------------------------------------------------------------
(require 'nerd-icons)

(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                             (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun athena-mode-name ()
  (propertize (if (listp mode-name) (car mode-name) mode-name)
              'help-echo (symbol-name major-mode)))

(defvar athena-line-selected-window (frame-selected-window))
(defun athena-line-selected-window-active-p ()
  (eq athena-line-selected-window (selected-window)))

;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; hooking to determine "active" window
(defun athena-line-set-selected-window (&rest _args)
  (if (frame-focus-state)
      (when (not (minibuffer-window-active-p (frame-selected-window)))
        (setq athena-line-selected-window (frame-selected-window))
        (force-mode-line-update))
    (progn
      (setq athena-line-selected-window nil)
      (force-mode-line-update))))

(defun athena-line-unset-selected-window ()
  (setq athena-line-selected-window nil)
  (force-mode-line-update))

(defun athena-modeline/macro-hook (&rest _args)
  (force-mode-line-update))

;; faces
(defgroup athena nil
  "Faces intended for Athena Emacs"
  :group 'convenience)

(defface athena/modeline-normal
  `((t (:foreground ,(face-background 'default)
                    :background "green"
                    :inherit 'bold)))
  "Normal mode indicator face" :group 'athena)

(defface athena/modeline-insert
  `((t (:foreground ,(face-background 'default)
                    :background "yellow"
                    :inherit 'bold)))
  "Insert mode indicator face" :group 'athena)

(defface athena/modeline-visual
  `((t (:foreground ,(face-background 'default)
                    :background "blue"
                    :inherit 'bold)))
  "Visual mode indicator face" :group 'athena)

(defface athena/modeline-remove
  `((t (:foreground ,(face-background 'default)
                    :background "red"
                    :inherit 'bold)))
  "Remove mode indicator face" :group 'athena)

(defface athena/modeline-motion
  `((t (:foreground ,(face-background 'default)
                    :background "cyan"
                    :inherit 'bold)))
  "Motion mode indicator face" :group 'athena)

(defface athena/modeline-operator
  `((t (:foreground ,(face-background 'default)
                    :background "cyan"
                    :inherit 'bold)))
  "Operator mode indicator face" :group 'athena)

(defface athena/modeline-emacs
  `((t (:foreground ,(face-background 'default)
                    :background "purple"
                    :inherit 'bold)))
  "Emacs mode indicator face" :group 'athena)

(defface athena/file-stale
  `((t (:foreground "cyan" :background ,(face-background 'mode-line))))
  "Stale file icon face" :group 'athena)

(defface athena/file-remote
  `((t (:foreground "purple" :background ,(face-background 'mode-line))))
  "TRAMP indicator face" :group 'athena)

(defface athena/modeline-recording
  `((t (:foreground ,(face-background 'default)
                    :background "magenta"
                    :inherit 'bold)))
  "Emacs mode indicator face" :group 'athena)

(defface athena/modeline-evil-inactive
  `((t (:foreground ,(face-background 'mode-line-inactive)
                    :background ,(face-foreground 'mode-line-inactive)
                    :inherit 'bold)))
  "Emacs mode indicator face" :group 'athena)

;; additional segments
(defun athena-modeline/modeline-macro-recording ()
  (if defining-kbd-macro
      (propertize "  Q  " 'face
                  (if (athena-line-selected-window-active-p)
                      'athena/modeline-recording 'athena/modeline-evil-inactive))
    ""))

;; (defun athena-modeline/file-changed ()
;;   (if (buffer-file-name)
;;       (if (file-has-changed-p (buffer-file-name)j
;;           (concat "  " (propertize "󰜗" 'face `(:inherit ,(if (athena-line-selected-window-active-p)
;;                                                              'athena/file-modified 'mode-line-inactive)
;;                                                         :family ,nerd-icons-font-family))
;;                   " ")
;;         "  ")
;;     "  "))

(defun athena-modeline/file-is-remote ()
  (if (file-remote-p default-directory)
      (concat (propertize "󰒋" 'face
                          `(:inherit ,(if (athena-line-selected-window-active-p)
                                          'athena/file-remote
                                        'mode-line-inactive)
                                     :family ,nerd-icons-font-family))
              " ")))

(defun athena-modeline/file-icon ()
  (if (nerd-icons-icon-for-mode major-mode)
      (concat
       (apply 'propertize
              (append
               `(,(nerd-icons-icon-for-mode major-mode))
               (if (athena-line-selected-window-active-p)
                   '(font-lock-face `(:height 1.0))
                 `(face ,`(:family ,nerd-icons-font-family :height 1.0 :inherit mode-line-inactive)))
               ))
       " ")
    ""))

(defun athena-modeline/right-segments ()
  (concat (athena-modeline/file-is-remote)
          (athena-modeline/file-icon)
          (athena-mode-name)))

(defun athena-modeline/right-segments-length ()
  (+ (if (athena-modeline/file-is-remote) 3 0)
     (if (athena-modeline/file-icon)      3 0)
     (length (athena-mode-name))))

(defun athena-modeline/evil-state-segment ()
  (cond
   ((eq evil-state 'emacs)
    (propertize "  E  " 'face
                (if (athena-line-selected-window-active-p)
                    'athena/modeline-emacs 'athena/modeline-evil-inactive)))
   ((eq evil-state 'normal)
    (propertize "  N  " 'face
                (if (athena-line-selected-window-active-p)
                    'athena/modeline-normal 'athena/modeline-evil-inactive)))
   ((eq evil-state 'insert)
    (propertize "  I  " 'face
                (if (athena-line-selected-window-active-p)
                    'athena/modeline-insert 'athena/modeline-evil-inactive)))
   ((eq evil-state 'visual)
    (propertize "  V  " 'face
                (if (athena-line-selected-window-active-p)
                    'athena/modeline-visual 'athena/modeline-evil-inactive)))
   ((eq evil-state 'replace)
    (propertize "  R  " 'face
                (if (athena-line-selected-window-active-p)
                    'athena/modeline-remove 'athena/modeline-evil-inactive)))
   ((eq evil-state 'motion)
    (propertize "  N  " 'fac
                (if (athena-line-selected-window-active-p)
                    'athena/modeline-normal 'athena/modeline-evil-inactive)))
   ((eq evil-state 'operator)
    (propertize "  O  " 'face
                (if (athena-line-selected-window-active-p)
                    'athena/modeline-operator 'athena/modeline-evil-inactive)))
   (t " ? ")))

;; modeline magic
(defun athena-modeline ()
  (interactive)
  (add-hook 'window-configuration-change-hook #'athena-line-set-selected-window)
  (add-hook 'after-change-major-mode-hook #'athena-modeline/macro-hook)
  (add-function :after after-focus-change-function #'athena-line-set-selected-window)

  (advice-add 'select-window :after #'athena-line-set-selected-window)
  (advice-add 'handle-switch-frame :after #'athena-line-set-selected-window)

  (advice-add 'start-kbd-macro :after #'athena-modeline/macro-hook)
  (advice-add 'end-kbd-macro   :after #'athena-modeline/macro-hook)

  (setq-default mode-line-format
                (list
                 (propertize "\u200b" 'display '((raise -0.25) (height 1.5)))
                 '(:eval (athena-modeline/evil-state-segment))
                 `(:eval (athena-modeline/modeline-macro-recording))
                 ;; `(:eval (athena-modeline/file-changed))
                 "  "
                 `(:eval (propertize "%b" 'face (if (buffer-modified-p) 'bold-italic 'bold) 'help-echo (buffer-file-name)))
                 "  %l:%c"
                 `(:eval (propertize " " 'display `(space :align-to
                                                          (- (+ right right-fringe right-margin)
                                                             2 ,(athena-modeline/right-segments-length)))))
                 '(:eval (athena-modeline/right-segments))))

  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format (default-value 'mode-line-format)))))

(provide 'athena-modeline)
