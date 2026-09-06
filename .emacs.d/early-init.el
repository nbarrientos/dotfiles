;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

;; Build the initial frame with the desired final look instead of
;; creating widgets that init.el would immediately tear down.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; The frame's final size is set above; don't let Emacs resize it
;; implicitly as fonts and UI elements are configured.
(setq frame-inhibit-implied-resize t)

(when (file-exists-p "~/.emacs.d/monkeys.el")
  (load-file "~/.emacs.d/monkeys.el"))
