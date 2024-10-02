;;; -- Keybinds --

;; assembling leader-based keybinds
(defun athena/find-file ()
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       #'counsel-projectile-find-file
     #'counsel-find-file)))

(byte-compile 'athena/find-file)

(defun athena/global-format-toggle ()
  (interactive)
  (setq athena/format-on-write-enable (* -1 athena/format-on-write-enable))
  (message (format "Formatting is %s for new buffers."
                   (if (> athena/format-on-write-enable 0) "enabled" "disabled"))))

;; vim-like cancellation
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; text-scaling
(general-define-key
 "C--" 'text-scale-decrease
 "C-=" 'text-scale-increase
 "M-;" 'shell-command)

;; org-roam keybinds
(defvar athena/roam-map
  (let ((map (make-sparse-keymap)))
    (define-key map "l" 'org-roam-buffer-toggle)
    (define-key map "f" 'org-roam-node-find)
    (define-key map "i" 'org-roam-node-insert)
    map)
  "roam keymap")

;; buffer hydra
(defhydra athena/buffer-map (:exit t :idle 1 :timeout 2)
  ("b" ivy-switch-buffer                  "change buffer")
  ("k" kill-this-buffer                   "kill da buffer")
  ("n" next-buffer                        "next buffer")
  ("l" previous-buffer                    "prev buffer")
  ("o" evil-switch-to-windows-last-buffer "last buffer")
  ("c" clone-indirect-buffer              "clone buffer")
  ("s" clone-indirect-buffer-other-window "split buffer")
  ("m" counsel-bookmark                   "bookmarks")
  ("d" diff-buffers                       "diff"))

;; window management hydra
(defhydra athena/win-map (:exit t :idle 1.5 :timeout 3 :hint nil)
  "
  ^_k_^     _c_lose     _=_ inc height   _\__ set height
_h_   _l_   _n_ew       _-_ dec height   _\|_  set width
  ^_j_^     _v_split    _>_ inc width
^^^^        ^ ^         _<_ dec height
"
  ;;   "
  ;; AthenaWM is active
  ;; "
  ("h" evil-window-left)
  ("j" evil-window-down)
  ("k" evil-window-up)
  ("l" evil-window-right)
  ("c" evil-window-delete)
  ("n" evil-window-split)
  ("v" evil-window-vsplit)
  ("H" athena/window-move-left)
  ("J" athena/window-move-down)
  ("K" athena/window-move-up)
  ("L" athena/window-move-right)
  (">" evil-window-increase-width :exit nil)
  ("<" evil-window-decrease-width :exit nil)
  ("=" evil-window-increase-height :exit nil)
  ("-" evil-window-decrease-height :exit nil)
  ("?" (setq hydra-is-helpful t) :exit nil)
  ("_" evil-window-set-height)
  ("|" evil-window-set-width))

;; Create the leader-based keymap
(general-create-definer athena/leader-keys
  :states '(insert visual emacs normal)
  :keymaps 'override
  :prefix "SPC"
  :global-prefix "C-SPC")

(athena/leader-keys
  ;; top-level shortcuts (reserved for very useful stuff)
  "SPC" '(athena/find-file :which-key "dynamic file-find")
  "j" '(swiper :which-key "better search")
  "." '(find-file :which-key "file finder")
  "/" '(projectile-ripgrep :which-key "rg nyoom")
  ";" '(counsel-M-x :which-key "M-x")

  ;; Hydras (fancy multilevel chords)
  "b" '(athena/buffer-map/body :which-key "buffer commands")
  "w" '(athena/win-map/body :which-key "window management")

  ;; Explicit multilevel chords
  ;; projectile
  "p" '(:keymap projectile-command-map :which-key "projects")

  ;; roam
  "n" '(:keymap athena/roam-map :package org-roam)

  ;; git
  "gg" '(magit-status :which-key "magit")
  "gb" '(magit-blame :which-key "whodunnit")
  "gi" '(vc-annotate :which-key "investigate")

  ;; format
  "fn" '(comment-dwim :which-key "comment toggle")
  "ff" '(athena/global-format-toggle :which-key "global format toggle")
  "fa" '(align-regexp :which-key "align")

  ;; open
  "ot" '(vterm :which-key "term")
  "oz" '(darkroom-mode :which-key "zen")
  "oa" '(org-agenda :which-key "agenda")

  ;; macros
  "mt" '(timestamp :which-key "timestamp"))
