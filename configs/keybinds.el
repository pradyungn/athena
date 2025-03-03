;;; -- Keybinds --

;; assembling leader-based keybinds
(defun athena/find-file ()
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       #'projectile-find-file
     #'find-file)))

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

;; AthenaWM hydra - allows for spammed resizes
(defhydra athena/win-map (:exit t :idle 1.5 :timeout 3 :hint nil)
  "
  ^_k_^     _c_lose     _=_ inc height   _\__ set height
_h_   _l_   _n_ew       _-_ dec height   _\|_  set width
  ^_j_^     _v_split    _>_ inc width
^^^^        ^ ^         _<_ dec height
"
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
  (">" evil-window-increase-width  :exit nil)
  ("<" evil-window-decrease-width  :exit nil)
  ("=" evil-window-increase-height :exit nil)
  ("-" evil-window-decrease-height :exit nil)
  ("?" (setq hydra-is-helpful t)   :exit nil)
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
  "SPC" '(athena/find-file         :which-key "dynamic file-find")
  "."   '(find-file                :which-key "file finder")
  "/"   '(consult-ripgrep          :which-key "rg nyoom")
  ";"   '(execute-extended-command :which-key "M-x")
  "j"   '(consult-line             :which-key "better search")

  ;; Hydras (fancy multilevel chords)
  "w" '(athena/win-map/body :which-key "window management")

  ;; buffer management
  "bb" '(switch-to-buffer                   :which-key "change buffer")
  "bk" '(kill-current-buffer                :which-key "kill da buffer")
  "bn" '(next-buffer                        :which-key "next buffer")
  "bl" '(previous-buffer                    :which-key "prev buffer")
  "bo" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
  "bc" '(clone-indirect-buffer              :which-key "clone buffer")
  "bs" '(clone-indirect-buffer-other-window :which-key "split buffer")
  "bm" '(consult-bookmark                   :which-key "bookmarks")
  "br" '(consult-recent-file                :which-key "buffer hist")
  "bd" '(diff-buffers                       :which-key "diff")

  ;; projectile
  "p" '(:keymap projectile-command-map :which-key "projects")

  ;; roam
  "n" '(:keymap athena/roam-map :package org-roam)

  ;; git
  "gg" '(magit-status :which-key "magit")
  "gb" '(magit-blame  :which-key "whodunnit")
  "gi" '(vc-annotate  :which-key "investigate")

  ;; format
  "fn" '(comment-dwim                :which-key "comment toggle")
  "ff" '(athena/global-format-toggle :which-key "global format toggle")
  "fa" '(align-regexp                :which-key "align")

  ;; open
  "ot" '(vterm               :which-key "term")
  "oz" '(darkroom-mode       :which-key "zen")
  "oa" '(org-agenda          :which-key "agenda")
  "or" '(revert-buffer-quick :which-key "reload-buf")

  ;; macros
  "mt" '(athena/timestamp :which-key "timestamp"))
