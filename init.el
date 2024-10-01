;;; -- Main Config File --

(load (concat user-emacs-directory "configs/setup.el"))
(load (concat user-emacs-directory "configs/helpers.el"))
(load (concat user-emacs-directory "configs/packages.el"))
(load (concat user-emacs-directory "configs/keybinds.el"))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Don't want the source file to have custom garbage
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
