;;; init.el --- Nacho Barrientos' Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Nacho Barrientos

;; Author: Nacho Barrientos <nacho.barrientos@cern.ch>
;; URL: https://github.com/nbarrientos/dotfiles/tree/master/.emacs.d
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; As you can see I mostly live inside Emacs, which apart from being a
;; magnificent editor, I also use as my window manager, my news reader,
;; my shell, my e-mail client, my calendar/planner, my web browser
;; (combined with Firefox) and as an interface to many non-Emacs-native
;; utilities like Git or Ripgrep.

;;; Code:

;;; Basic configuration of built-in features
(setq-default
 inhibit-startup-message t
 visible-bell t
 vc-follow-symlinks t
 indent-tabs-mode nil
 custom-file null-device
 make-backup-files nil
 help-window-select t
 use-short-answers t
 native-comp-async-report-warnings-errors nil
 auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-saves/" t)))

(server-start)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(mouse-avoidance-mode 'jump)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key (kbd "M-<up>") 'backward-sexp)
(global-set-key (kbd "M-<down>") 'forward-sexp)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "C-d") 'mark-word)
(global-set-key (kbd "M-d") 'my/delete-word)
(global-set-key (kbd "M-<backspace>") 'my/backward-delete-word)
(global-unset-key (kbd "C-x C-c"))

(add-to-list 'yank-excluded-properties 'face)

(put 'narrow-to-region 'disabled nil)

;;;; Fonts
(set-face-attribute 'default nil :font "JetBrainsMono" :height 110)

;;;; Remedies for to-be-reeducated muscle memory
(global-unset-key (kbd "C-z"))

;;; Use-package

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Visibility
;;;; Line numbers
(define-minor-mode my/line-numbers-highlight-line-mode
  "Shows line numbers and highlights the current line"
  :initial nil
  (if my/line-numbers-highlight-line-mode
      (progn
        (display-line-numbers-mode 1)
        (hl-line-mode 1))
    (progn
      (display-line-numbers-mode 0)
      (hl-line-mode 0))))
(bind-key "<f9>" #'my/line-numbers-highlight-line-mode)

;;;; White spaces
(use-package whitespace
  :config
  (global-whitespace-mode)
  :hook
  (prog-mode . (lambda () (setq show-trailing-whitespace t)))
  (text-mode . (lambda () (setq show-trailing-whitespace t)))
  (conf-mode . (lambda () (setq show-trailing-whitespace t)))
  :custom
  (whitespace-style '(face trailing tabs empty big-indent))
  (whitespace-global-modes '(not erc-mode magit-mode)))

;;;; Sexp delimiters highlighting
(use-package paren
  :ensure nil
  :config
  (show-paren-mode)
  :custom
  (show-paren-style 'parenthesis))

;;;; Outline
(use-package outline
  :ensure nil
  :config
  :hook
  ((emacs-lisp-mode . outline-minor-mode)
  (emacs-lisp-mode . hs-minor-mode)
  (markdown-mode . outline-minor-mode)
  (markdown-mode . hs-minor-mode)))

(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)))

;;;; URLs
(require 'url-util)
(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
          (prog-mode . goto-address-prog-mode)
          (magit-mode . goto-address-mode)
          (yaml-mode . goto-address-prog-mode)
          (mu4e-view-mode . goto-address-mode))
  :commands (goto-address-prog-mode
             goto-address-mode))

;;;; Grep
(use-package rg
  :bind (:map rg-mode-map
              ("C-<down>" . rg-next-file)
              ("C-<up>" . rg-prev-file))
  :config
  (add-to-list 'rg-custom-type-aliases '("texi" . "*.texi"))
  (add-to-list 'rg-custom-type-aliases '("puppet" . "*.pp"))
  :custom
  (rg-group-result t)
  (rg-buffer-name "ripgrep"))

;;; Navigation
;;;; Xref
(use-package xref
  :ensure nil
  :bind (("M-/" . xref-find-references)))

;;;; Project management
(use-package project
  :ensure nil
  :bind-keymap ("C-p" . project-prefix-map)
  :bind ((:map project-prefix-map
               ("b" . my/project-ivy-switch-buffer)
               ("C" . magit-clone)
               ("f" . my/project-counsel-fzf)
               ("F" . nil)
               ("g" . rg-project)
               ("G" . rg-dwim-project-dir)
               ("H" . my/clone-hostgroup)
               ("m" . magit-project-status)
               ("M" . my/clone-module)
               ("s" . project-eshell)
               ("t" . my/regenerate-ctags)))
  :config
  (global-unset-key (kbd "C-x p"))
  (setenv
   "FZF_DEFAULT_COMMAND"
   "find -type f -not -path '*/\.git/*' -not -path '*/spec/fixtures/*' -printf '%P\n'")
  (defun my/project-counsel-fzf ()
    (interactive)
    (let* ((default-directory (project-root (project-current t))))
      (counsel-fzf nil default-directory (format "fzf in %s: " default-directory))))
  (defun my/project-ivy-switch-buffer ()
    (interactive)
    (let* ((pr (project-current t))
           (buffers (project-buffers pr))
           (buffer-names (mapcar 'buffer-name buffers)))
      (ivy-read "Switch to project buffer: " buffer-names
            :keymap ivy-switch-buffer-map
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer)))
  :custom
  (project-switch-commands
   '((my/project-counsel-fzf "Find file")
     (rg-project "Ripgrep")
     (project-find-dir "Find directory")
     (project-eshell "Eshell")
     (magit-project-status "Magit"))))

;;; Auth Source
(use-package auth-source
  :ensure nil
  :custom
  (auth-source-save-behavior nil)
  ;; Read credentials from KeepassXC too via the Secret Service API.
  ;; For this to work, KXC has to have this integration enabled and
  ;; some password groups shared via SS API. Also, those secrets must
  ;; expose attributes (advanced tab) like 'host', 'port' and/or
  ;; 'user' so they can be found by auth-source. Ex:
  ;; (auth-source-search :type 'secrets :host "irc.libera.chat")
  (auth-sources '("secrets:Passwords")))

;;; Killing, Yanking, Comments and Undo
(use-package whole-line-or-region
  :init
  (whole-line-or-region-global-mode)
  ;; This package takes over the default binding for `comment-dwim'
  ;; which is bound to M-;. Unfortunatelly this is not
  ;; configurable. I'm rebinding it globally later on but to avoid
  ;; ordering issues, make sure that it's not part of this keymap at
  ;; all.
  :bind (:map whole-line-or-region-local-mode-map
              ("M-;" . nil)))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-visualizer-diff nil)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-relative-timestamps t))

(use-package newcomment
  :ensure nil
  :bind (("M-;" . comment-line)
         ("M-RET" . comment-indent-new-line))
  :hook ((prog-mode . (lambda ()
                        (set (make-local-variable
                               'comment-auto-fill-only-comments)
                              t)))))

(use-package simple
  :ensure nil
  :hook ((prog-mode . auto-fill-mode)))

(use-package expand-region
  :bind ("C-f" . er/expand-region)
  :custom
  (expand-region-show-usage-message nil))

(use-package crux
  :bind ("C-o" . crux-smart-open-line-above))

;;; Spelling and grammar
(use-package ispell
  :ensure nil
  :bind (("<f7>" . my/ispell-cycle-dictionary))
  :init
  (setq my/ispell-dictionary-list '("british" "french" "spanish" "british"))
  ;; When we're back to the British English dictionary we want to set
  ;; the input method to nil, but that's implicit when calling assoc
  ;; below :)
  (setq my/dictionary-input-method-alist
        '(("french" . "latin-1-prefix")
          ("spanish" . "latin-1-prefix")))
  :hook ((text-mode . flyspell-mode)
         (mu4e-compose-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (ispell-change-dictionary
          .
          (lambda ()
            (activate-input-method
             (cdr
              (assoc ispell-local-dictionary my/dictionary-input-method-alist))))))
  :config
  ;; When using a keyword to set the dict to use
  ;; (ispell-dictionary-keyword), ispell-internal-change-dictionary is
  ;; called when the buffer is loaded (instead of
  ;; ispell-dictionary-keyword) so the change dict hooks are not
  ;; called. I need this to happen here as well so when I visit a file
  ;; with dict settings (using Local IspellDict, for example) the
  ;; input method is changed automatically as well.
  (advice-add 'ispell-internal-change-dictionary
              :after (lambda ()
                       (run-hooks 'ispell-change-dictionary-hook)))

  (defun my/ispell-cycle-dictionary ()
    "Cycle through the list of dictionaries that I typically use.
The 'circular' list is defined in the variable
`my/ispell-dictionary-list'."
    (interactive)
    (let ((next-dictionary)
          (current-dictionary ispell-local-dictionary))
      (when (null current-dictionary)
        (setq current-dictionary ispell-current-dictionary))
      (setq next-dictionary
            (nth
             (+ 1
                (seq-position
                 my/ispell-dictionary-list
                 current-dictionary))
             my/ispell-dictionary-list))
      (ispell-change-dictionary next-dictionary)))
  :custom
  (ispell-dictionary "british"))

;;; TRAMP
;; Option (1-2): and Your 2nd factor: are typical prompts for 2FA
;; tokens at CERN
(add-to-list 'password-word-equivalents "Option")
(add-to-list 'password-word-equivalents "Your 2nd factor")
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :custom
  (tramp-verbose 2)
  (tramp-default-method "ssh"))

(use-package tramp-sh
  :ensure nil
  :custom
  (tramp-use-ssh-controlmaster-options nil))

(use-package em-tramp
  :ensure nil
  :config
  (setq password-cache-expiry 300))

(use-package pinentry
  :config
  (pinentry-start))
(when (file-exists-p "~/.gnupg/gpg-agent.conf")
  (shell-command "gpg-connect-agent /bye")
  ;; Don't forget to add the keygrips to .gnupg/sshcontrol!
  (setenv "SSH_AUTH_SOCK"
          (replace-regexp-in-string
           "\n$"
           ""
           (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket 2>/dev/null"))))

;;; Dired
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-NGalhv --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ([tab] . dired-subtree-toggle)
              ([C-tab] . dired-subtree-cycle))
  :custom
  (dired-subtree-line-prefix "  |-")
  (dired-subtree-use-backgrounds nil))

(use-package dired-aux
  :ensure nil
  :defer t
  :config
  (add-to-list 'dired-compress-file-suffixes
               '("\\.txz\\'" "" "xz -dc %i | tar -xf -")))

;;; Auto completion
;;;; Ivy-Counsel-Swiper
(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
               '('puppet-mode all-the-icons-fileicon "api-blueprint"
                 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-extension-icon-alist
               '("pp" all-the-icons-fileicon "api-blueprint"
                 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-extension-icon-alist
               '("epp" all-the-icons-fileicon "api-blueprint"
                 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-extension-icon-alist
               '("erb" all-the-icons-fileicon "api-blueprint"
                 :face all-the-icons-orange))
  ;; Pending https://github.com/domtronn/all-the-icons.el/pull/267
  (add-to-list 'all-the-icons-mode-icon-alist
               '(exwm-mode
                 all-the-icons-octicon "browser"
                 :v-adjust 0.2 :face all-the-icons-purple)))

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ([C-return] . ivy-restrict-to-matches))
  :config
  (defun my/ivy-switch-buffer-by-prefix (prefix)
    "Use ivy to select a buffer prefixed by PREFIX#."
    (minibuffer-with-setup-hook
        (lambda ()
          (insert (concat "^" prefix "# ")))
      (ivy-switch-buffer)))
  (defun my/ivy-switch-buffer-firefox ()
    "Use ivy to select a Firefox window (buffer)."
    (interactive)
    (my/ivy-switch-buffer-by-prefix "f"))
  (defun my/ivy-switch-buffer-urxvt ()
    "Use ivy to select an URXVT window (buffer)."
    (interactive)
    (my/ivy-switch-buffer-by-prefix "u"))
  (defun my/ivy-switch-buffer-detached-command ()
    "Use ivy to select a compilation buffer."
    (interactive)
    (my/ivy-switch-buffer-by-prefix "d"))
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers 'recentf)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package ivy-posframe
  :config
  (ivy-posframe-mode 1)
  (defun my/ivy-posframe-get-size ()
    (let ((height (or ivy-posframe-height ivy-height))
          (width (round (* .70 (frame-width)))))
      (list :height height :width width :min-height height :min-width width)))
  :custom
  (posframe-mouse-banish-function #'posframe-mouse-banish-simple)
  (ivy-posframe-display-functions-alist
   '((swiper . ivy-display-function-fallback)
     (t      . ivy-posframe-display)))
  (ivy-posframe-height-alist '((counsel-yank-pop . 40)
                               (t                . 20)))
  (ivy-posframe-size-function 'my/ivy-posframe-get-size))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (ivy-rich-mode 1)
  (ivy-rich-project-root-cache-mode)
  (ivy-rich-set-columns
   'counsel-find-file
   '((all-the-icons-ivy-rich-file-icon)
     (ivy-read-file-transformer)))
  (ivy-rich-set-columns
   'counsel-fzf
   '((all-the-icons-ivy-rich-file-icon)
     (all-the-icons-ivy-rich-file-name)))
  (ivy-rich-modify-columns
   'ivy-switch-buffer
   '((ivy-rich-switch-buffer-project (:width 30))
     (ivy-rich-switch-buffer-major-mode (:width 20)))))

(use-package amx
  :after (ivy)
  :custom
  (amx-backend 'ivy)
  (amx-history-length 50))

(use-package counsel
  :after (helpful)
  :bind (("M-x" . counsel-M-x)
         ("C-b" . ivy-switch-buffer)
         ("M-b" . ivy-switch-buffer-other-window)
         ("C-x f" . my/counsel-find-file-no-tramp)
         ("C-x F" . counsel-find-file)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-h k" . helpful-key)
         ("C-x r b" . counsel-bookmark)
         ("M-y" . counsel-yank-pop))
  :config
  (defun my/counsel-find-file-no-tramp (&optional initial-input initial-directory)
    (interactive)
    (if (string-prefix-p "/ssh:" default-directory)
        (counsel-find-file "" "~/")
      (counsel-find-file initial-input initial-directory)))
  ;; Pending https://github.com/abo-abo/swiper/pull/2844/
  (defun counsel--esh-dir-history-action-cd (pair)
    "Change the current working directory to the selection.
This function is the default action for `counsel-esh-dir-history'
and changes the working directory in Eshell to the selected
candidate which must be provided as the `car' of PAIR."
    (eshell/cd (car pair)))
  (defun counsel--esh-dir-history-action-edit (pair)
    "Insert the selection to the Eshell buffer prefixed by \"cd \".
This function is an action for `counsel-esh-dir-history' to
insert the selected directory (provided as the `car' of PAIR) to
the Eshell buffer prefixed by \"cd \", allowing the caller to
modify parts of the directory before switching to it."
    (insert (format "cd %s" (car pair))))
  (defun counsel-esh-dir-history ()
    "Use Ivy to browse Eshell's directory stack."
    (interactive)
    (require 'em-dirs)
    (defvar eshell-last-dir-ring)
    (ivy-read "Directory to change to: " (ivy-history-contents eshell-last-dir-ring)
              :keymap ivy-reverse-i-search-map
              :action #'counsel--esh-dir-history-action-cd
              :caller #'counsel-esh-dir-history))
  (ivy-set-actions
   'counsel-esh-dir-history
   '(("e" counsel--esh-dir-history-action-edit "edit")))
  (defun counsel-fzf-action (x)
    (with-ivy-window
      (let ((default-directory counsel--fzf-dir))
        (find-file-other-window x)))
    (other-window -1))
  :custom
  (counsel-yank-pop-separator "\n-------------------\n")
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-M-s" . swiper-thing-at-point)))

;;;; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet))

;;;; Expand
(use-package hippie-expand
  :ensure nil
  :bind
  (("C-n" . hippie-expand))
  :config
  (global-unset-key (kbd "M-/"))
  :custom
  (hippie-expand-try-functions-list
   '(yas-hippie-try-expand
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))

;;; Look and feel
(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-visual-bell-config)
  (custom-set-faces
   '(ivy-modified-buffer ((t (:inherit default :foreground unspecified)))))
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-irc-buffers t)
  (doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package frame
  :ensure nil
  :custom
  (window-divider-default-right-width 10)
  :config
  (window-divider-mode 1))

;;; Modes for coding
;;;; Syntax checking
(use-package flycheck
  :bind
  ("<f12>" . flycheck-mode)
  :custom
  (flycheck-highlighting-mode . nil))

;;;; Programming languages
(use-package ruby-mode
  :ensure nil
  :hook ((ruby-mode . (lambda ()
                        (setq
                         flycheck-ruby-rubocop-executable
                         "~/.local/bin/rubocop")))))
(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-minor-mode)))

(use-package puppet-mode
  :hook ((puppet-mode . (lambda ()
                          (setq
                           flycheck-puppet-lint-executable
                           "~/.local/bin/puppet-lint")))))
(use-package rspec-mode)

(use-package go-mode
  :hook
  ((go-mode . (lambda ()
                (make-local-variable 'whitespace-style)
                (setq whitespace-style
                      (remove 'tabs whitespace-style))))))
(use-package go-dlv)

;;;; Markup, scripting and conf
(use-package json-mode)

(use-package yaml-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package rpm-spec-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.spec" . rpm-spec-mode)))

(use-package archive-rpm)

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.epp\\'" . web-mode))
  (add-to-list 'safe-local-variable-values '(web-mode-enable-auto-indentation . nil))
  (setq web-mode-engines-alist
        '(("erb" . "\\.epp\\'"))))

(use-package csv-mode)

(use-package jq-mode)

;;;; Misc
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package gitignore-templates)

(use-package virtualenvwrapper
  :config
  (venv-initialize-eshell)
  :custom
  (venv-location "~/venvs"))

;;; Eshell
(use-package eshell-bookmark
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

(use-package eshell
  :ensure nil
  :hook
  (eshell-mode . (lambda ()
                   (define-key eshell-mode-map (kbd "M-<up>") 'eshell-previous-prompt)
                   (define-key eshell-mode-map (kbd "M-<down>") 'eshell-next-prompt)
                   (define-key eshell-mode-map (kbd "C-c C-o") 'my/eshell-kill-ring-save-outputs)
                   (define-key eshell-mode-map (kbd "C-c o") 'my/eshell-export-last-output)
                   (define-key eshell-mode-map (kbd "C-c r") 'counsel-esh-history)
                   (define-key eshell-mode-map (kbd "C-c d") 'counsel-esh-dir-history)
                   (define-key eshell-mode-map (kbd "C-c l") 'eshell/clear)
                   (define-key eshell-mode-map (kbd "C-<return>") 'my/eshell-send-detached-input)
                   ;; When calling dabbrev, hippie-expand uses strings
                   ;; containing words and symbols to:
                   ;;   1) determine the string to expand
                   ;;   2) determine what to expand it with
                   ;; (see hippie-expand-dabbrev-as-symbol)
                   ;; so for instance if I'm typing "curl foo/bar" on
                   ;; an eshell buffer, as "/" is a symbol in eshell
                   ;; mode, it will use "foo/bar" as string to
                   ;; expand. In some cases this is undesirable, for
                   ;; instance when completing URLs, as it's more
                   ;; likely that I'll want to expand the current
                   ;; component ("bar"), not the whole URL. Moving "/"
                   ;; to a non-symbol syntax class works around
                   ;; this. I can't just set
                   ;; hippie-expand-dabbrev-as-symbol to false because
                   ;; if I did h-e wouldn't expand FQDNs, i.e. "bar"
                   ;; would be expanded to "barhost" and not
                   ;; "barhost.example.org"
                   (modify-syntax-entry ?/ "|")))
  :config
  (defun my/eshell-kill-ring-save-outputs ()
    "Add to the kill ring CURRENT-PREFIX-ARG outputs, including prompts.
If no universal argument is passed, assume only one output"
    (interactive)
    (save-excursion
      (let (times)
        (if (or (null current-prefix-arg) (< current-prefix-arg 1))
            (setq times 1)
          (setq times current-prefix-arg))
        (eshell-previous-prompt times)
        (forward-line -1) ; Two lines prompt
        (beginning-of-line)
        (message (format "Shell output added to the kill ring (%d commands)" times))
        (kill-ring-save (point) (eshell-end-of-output)))))

  ;; Inspiration from:
  ;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-eshell.el
  (defun my/eshell-export-last-output ()
    "Produce a buffer with output of the last Eshell command."
    (interactive)
    (let ((eshell-output (buffer-substring-no-properties
                          (eshell-beginning-of-output)
                          (eshell-end-of-output))))
      (with-current-buffer (get-buffer-create "*Exported Eshell output*")
        (erase-buffer)
        (insert eshell-output)
        (switch-to-buffer-other-window (current-buffer)))))
  (defun eshell/tcd (&optional directory)
    "Change the default directory to DIRECTORY but TRAMP-aware.
Like `eshell/cd' but taking into account that the current
directory might be in a remote file system. If that's the case,
the current TRAMP root is prepended to DIRECTORY."
    (let ((tramp-root (file-remote-p default-directory)))
      (if tramp-root
          (eshell/cd (concat tramp-root (or directory "")))
        (eshell/cd directory))))
  (defun my/eshell-send-detached-input (&optional arg)
    "Send the current Eshell input to a compilation buffer.
With universal prefix argument bury the compilation buffer and
send a notification when the process has exited."
    (interactive "p")
    (when-let* ((cmd (buffer-substring
                      eshell-last-output-end (point-max)))
                (cmd-present-p (not (string-empty-p cmd))))
      (let* ((hostname (or
                        (file-remote-p default-directory 'host)
                        (system-name)))
             (compile-command nil)
             (compilation-save-buffers-predicate 'ignore)
             (compilation-scroll-output nil)
             (compilation-buffer
              (compilation-start
               cmd
               nil
               (lambda (major-mode)
                 (format "D#%x %s" (random (expt 2 16)) cmd)))))
        (when (equal arg 4)
          (with-current-buffer compilation-buffer
            (switch-to-prev-buffer (get-buffer-window (current-buffer)))
            (setq-local compilation-finish-functions
                        `((lambda (buffer str)
                            (notifications-notify
                             :body (format "%s # %s" ,hostname ,cmd)
                             :timeout 8000
                             :category "detached_process"
                             :actions '("default" "Switch to buffer")
                             :on-action (lambda (id key) (switch-to-buffer-other-window ,(buffer-name compilation-buffer)))
                             :title (format "Process %s!" (string-chop-newline str))
                             :urgency (if (string-prefix-p "finished" str) 'normal 'critical)))))))
        (eshell-add-input-to-history cmd)
        (eshell-reset))))
  (setenv "EDITOR" "emacsclient")
  (add-to-list 'directory-abbrev-alist '("/home/ibarrien" . "~"))
  (add-to-list 'directory-abbrev-alist '("/afs/cern.ch/user/i/ibarrien" . "~"))
  :custom
  (eshell-banner-message "")
  (eshell-history-size 20000)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all))

(use-package em-hist
  :ensure nil
  :bind (:map eshell-hist-mode-map
              ("<up>" . previous-line)
              ("<down>" . next-line)))

(use-package eshell-prompt-extras
  :after (eshell)
  :config
  (defun my/epe-theme-prompt ()
    (setq eshell-prompt-regexp "^λ ")
    (concat
     (let ((prompt-path (epe-fish-path (tramp-file-local-name (eshell/pwd)))))
       (format
        (epe-colorize-with-face "[%s]" 'epe-remote-face)
        (epe-colorize-with-face
         (if (string-empty-p prompt-path)
             "/"
           prompt-path)
         'epe-dir-face)))
     (if (epe-remote-p)
         (epe-colorize-with-face
          (concat "@" (epe-remote-host))
          'epe-remote-face)
       (epe-colorize-with-face
        (concat "@" (system-name))
        'epe-git-face))
     (if (eshell-exit-success-p)
         (epe-colorize-with-face "\nλ" 'success)
       (epe-colorize-with-face "\nλ" 'error))
     " "))
  (with-eval-after-load "esh-opt"
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'my/epe-theme-prompt)))

;;; Magit and Git
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch)
         ("C-c g" . magit-file-dispatch)
         :map magit-mode-map
         ("x" . magit-reset-hard)
         :map magit-section-mode-map
         ("C-<up>" . magit-section-backward-sibling)
         ("C-<down>" . magit-section-forward-sibling))
  :config
  (add-to-list 'magit-clone-name-alist '("\\(it-puppet-.+\\)" ":@gitlab.cern.ch:8443" "ai"))
  (transient-append-suffix 'magit-push "-n"
    '(1 "-M" "Create MR in Gitlab"
        "--push-option=merge_request.create"))
  (transient-append-suffix 'magit-push "-M"
    '(1 "-U" "Unassign MR in Gitlab"
        "--push-option=merge_request.unassign=22"))
  ;; https://github.com/magit/magit/wiki/Tips-and-Tricks#ask-for-confirmation-before-pushing-to-originmaster
  (define-advice magit-push-current-to-upstream (:before (args) query-yes-or-no)
    "Prompt for confirmation before permitting a push to upstream."
    (when-let ((branch (magit-get-current-branch)))
      (unless (yes-or-no-p
               (format "Push %s branch to %s? "
                       branch
                       (or (magit-get-upstream-branch branch)
                           (magit-get "branch" branch "remote"))))
        (user-error "Push to upstream aborted by user"))))
  :custom
  (magit-blame-time-format "%d/%m/%y %R")
  (magit-save-repository-buffers 'dontask)
  (magit-clone-default-directory "~/dev/")
  (magit-clone-set-remote.pushDefault 'ask)
  (magit-remote-add-set-remote.pushDefault 'ask)
  (magit-list-refs-sortby "-creatordate")
  (magit-diff-refine-hunk t))

(use-package forge
  :after magit
  :config
  (add-to-list 'forge-alist
               '("gitlab.cern.ch" "gitlab.cern.ch/api/v4" "gitlab.cern.ch" forge-gitlab-repository))
  :custom
  (forge-owned-accounts '(("nbarrientos" . nil)
                          ("cernops" . nil))))

(use-package git-link
  :custom
  (git-link-use-commit t))

(use-package ediff-init
  :ensure nil
  :config
  ;; https://github.com/ch11ng/exwm/issues/353
  (advice-add 'ediff-window-display-p :override #'ignore))

;;; Movement and window switching
(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

(use-package window
  :ensure nil
  :bind (("<f8>" . other-window)
         ("C-<f8>" . window-swap-states)
         ("C-x 1" . my/toggle-single-window))
  :init
  (defun my/toggle-single-window ()
  "Toggles between a single window configuration and
the previously multi-windowed one"
  (interactive)
  (if (one-window-p)
    (when (boundp 'my-saved-window-configuration)
      (set-window-configuration my-saved-window-configuration))
    (progn
      (setq my-saved-window-configuration (current-window-configuration))
      (delete-other-windows)))))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1))

(use-package beginend
  :config
  (beginend-global-mode))

(use-package drag-stuff
  :bind
  (("C-M-<up>" . drag-stuff-up)
   ("C-M-<down>" . drag-stuff-down))
  :config
  (drag-stuff-global-mode))

;;; Help
(use-package helpful)

;;; Mu4e
(use-package mu4e
  :ensure nil
  :hook ((mu4e-compose-mode
          . (lambda ()
              (save-excursion
                (replace-regexp "^-- $" "--")))))
  :custom
  (read-mail-command 'mu4e)
  (mu4e-change-filenames-when-moving t)
  (mu4e-confirm-quit t)
  (mu4e-update-interval (* 5 60))
  (mu4e-get-mail-command "true") ; mbsync is run by a systemd timer (only re-index)
  ;; (mu4e-html2text-command "lynx -dump -stdin")
  (mu4e-compose-signature " bye\n Nacho\n http://cern.ch/nacho")
  (mu4e-view-show-addresses t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-hide-index-messages t)
  (mu4e-headers-leave-behavior 'apply)
  (mu4e-split-view nil)
  (mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)
  (mu4e-headers-date-format "%d/%m/%y")
  (mu4e-headers-time-format "%R")
  (mu4e-drafts-folder "/cern/Drafts")
  (mu4e-sent-folder   "/cern/Sent Items")
  (mu4e-trash-folder  "/cern/Trash")
  (mu4e-get-mail-command "/usr/bin/systemctl --user start mbsync-prio-0")
  (mu4e-completing-read-function 'ivy-completing-read)
  (mu4e-headers-fields
   '(( :human-date    .  12)
     ( :flags         .  10)
     ( :from          .  22)
     ( :subject       .  nil)))
  (mu4e-bookmarks
   '(( :name "Unread messages in INBOX"
       :query "flag:unread and maildir:/.*INBOX/"
       :key ?i)
     ( :name  "All unread messages"
       :query "flag:unread and not flag:trashed and not maildir:\"/cern/Spam\""
       :key ?u)
     ( :name "Today's messages"
       :query "date:today..now and not maildir:\"/cern/Spam\""
       :key ?t)
     ( :name "Last 7 days"
       :query "date:7d..now and not maildir:\"/cern/Spam\""
       :key ?w)
     ( :name "Flagged messages"
       :query "flag:flagged"
       :key ?f
       :hide-unread t)))
  :config
  (add-to-list 'mu4e-view-fields :user-agent t)
  (setq user-mail-address "nacho.barrientos@cern.ch")
  (setq user-full-name "Nacho Barrientos")
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq send-mail-function 'smtpmail-send-it)
  (setq smtpmail-smtp-server "cernmail.cern.ch")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-smtp-user "ibarrien")
  (setq mail-user-agent 'mu4e-user-agent)
  ;; These three labels are used in the mode line, prefixing the
  ;; search query. My font does not have the default glyphs being
  ;; displayed and I'd either way prefer not to clutter the mode
  ;; line. Another option would be to set `mu4e-use-fancy-chars' to
  ;; nil but this affects also other views where I like seeing fancy
  ;; chars.
  (setq mu4e-headers-threaded-label   '("T" . ""))
  (setq mu4e-headers-full-label       '("F" . ""))
  (setq mu4e-headers-related-label    '("R" . ""))
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))
  :bind (:map mu4e-headers-mode-map
              ("r" . 'mu4e-headers-mark-for-read)
              ("d" . 'mu4e-headers-mark-for-delete)))

(use-package mu4e-marker-icons
  :custom
  (mu4e-marker-icons-mode 1))

(mu4e t)

;;; Elfeed
(use-package elfeed)

(use-package shr
  :ensure nil
  :custom
  (shr-use-fonts nil))

;;; Multimedia
(use-package eradio
  :ensure t
  :custom
  (eradio-player '("mpv" "--no-video" "--no-terminal"))
  (eradio-channels '(;; French
                     ("Option Musique" . "http://stream.srg-ssr.ch/m/option-musique/aacp_96.m3u")
                     ("Couleur3" . "http://stream.srg-ssr.ch/m/couleur3/aacp_96.m3u")
                     ("La 1ere" . "http://stream.srg-ssr.ch/m/la-1ere/aacp_96.m3u")
                     ;; Spanish
                     ("RNE" . "https://rtvelivestream.akamaized.net/rne_r1_main.m3u8")
                     ("RPA" . "https://cdnlive2.shooowit.net/rtpalive/smil:radio.smil/playlist.m3u8")
                     ;; English
                     ("BBC1" . "http://stream.live.vc.bbcmedia.co.uk/bbc_radio_one")
                     ("BBC2" . "http://stream.live.vc.bbcmedia.co.uk/bbc_radio_two")
                     ("BBC5" . "http://stream.live.vc.bbcmedia.co.uk/bbc_radio_five_live_online_nonuk")
                     ("BBC Scotland" . "http://stream.live.vc.bbcmedia.co.uk/bbc_radio_scotland_fm"))))

;;; Web browsing
(use-package eww
  :ensure nil
  :config
  (setq browse-url-handlers
        '(("." . browse-url-firefox)))
  :custom
  (eww-use-external-browser-for-content-type
   "\\`\\(video/\\|audio/\\|application/ogg\\|application/pdf\\)"))

(use-package engine-mode
  :config
  (defengine duckduckgo
    "https://duckduckgo.com/html?q=%s"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")
  (defengine cern-gitlab
    "https://gitlab.cern.ch/search?search=%s"
    :keybinding "l")
  (defengine google
    "http://www.google.com/search?hl=en&ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine duckduckgo-first
    "https://duckduckgo.com/html?q=\\%s"
    :keybinding "f")
  (defengine google-maps
    "https://www.google.com/maps/search/%s/"
    :keybinding "M")
  (defengine openstreetmap
    "https://www.openstreetmap.org/search?query=%s"
    :keybinding "m")
  (defengine wordreference
    "https://www.wordreference.com/es/translation.asp?tranword=%s"
    :keybinding "r")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")
  (engine/set-keymap-prefix (kbd "C-j"))
  (engine-mode t))

;;; Window manager
(use-package exwm
  :config
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (setq-default my/exwm--do-not-mass-kill nil)
  (defun my/exwm-toggle-or-set-buffer-protection (&optional arg value)
    "Toggle or set EXWM mass-buffer-deletion protection.
When called interactively, toggle. Otherwise set to VALUE."
    (interactive "p")
    (when (derived-mode-p 'exwm-mode)
      (if arg
          (progn
            (if my/exwm--do-not-mass-kill
                (kill-local-variable 'my/exwm--do-not-mass-kill)
              (setq-local my/exwm--do-not-mass-kill t))
            (when arg
              (message "EXWM buffer protection set to %s" my/exwm--do-not-mass-kill)))
        (setq-local my/exwm--do-not-mass-kill value))))
  (defun my/exwm-kill-unprotected-by-prefix (prefix)
    "Kill all EXWM buffers with PREFIX that have `my/exwm--do-not-mass-kill' set to nil."
    (interactive "sPrefix: ")
    (dolist (buf (buffer-list (current-buffer)))
      (with-current-buffer buf
        (when
            (and
             (eq major-mode 'exwm-mode)
             (string-prefix-p (concat prefix "#") (buffer-name))
             (not my/exwm--do-not-mass-kill))
          (kill-buffer)))))

  (defun my/exwm--format-window-title-firefox (title &optional length)
    "Removes noise from and trims Firefox window titles.
Assumes the Add URL to Window Title extension is enabled and
configured to use @ (at symbol) as separator."
    (let* ((length (or length 45))
           (title (concat "F# " (replace-regexp-in-string " [-—] Mozilla Firefox$" "" title)))
           (title-and-hostname (split-string title "@" nil " "))
           (hostname (substring (car (last title-and-hostname)) 0 -1))
           (page-title (string-join (reverse (nthcdr 1 (reverse title-and-hostname))) " "))
           (short-title (reverse (string-truncate-left (reverse page-title) length))))
      (if (length> title-and-hostname 1)
          (concat short-title " @ " hostname)
        (reverse (string-truncate-left (reverse title) length)))))

  (defun my/exwm--format-window-title-urxvt (title)
    "Removes noise from URxvt window titles."
    (concat "U# " (replace-regexp-in-string ":.*$" "" title)))

  (defun my/exwm--format-window-title-* (title)
    "Removes annoying notifications counters."
    (string-trim (replace-regexp-in-string "([[:digit:]]+)" "" title)))

  (defun my/exwm-buffer-name ()
    "Guesses (and formats) the buffer name using the class of the X client."
    (let ((title (my/exwm--format-window-title-* exwm-title))
          (formatter (intern
                      (format "my/exwm--format-window-title-%s"
                              (downcase exwm-class-name)))))
      (if (fboundp formatter)
          (funcall formatter title)
        title)))

  (setq exwm-input-global-keys
        `(
          ([?\s-r]
           . exwm-reset)
          ([?\s-c]
           . exwm-input-toggle-keyboard)
          ([?\s-f]
           . my/toggle-single-window)
          ([?\s-h]
           . split-window-below)
          ([?\s-v]
           . split-window-right)
          ([?\s-k]
           . delete-window)
          ([?\s-d] .
           (lambda ()
             (interactive)
             (counsel-linux-app)))
          ([?\s-1] .
           (lambda ()
             (interactive)
             (my/ivy-switch-buffer-firefox)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" (car i))) .
                        (lambda ()
                          (interactive)
                          (my/switch-to-buffer-if-exists-back-and-forth ,(cdr i)))))
                    '((2 . "Telegram") (3 . "Signal") (6 . "*eshell*")))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda (arg)
                          (interactive "P")
                          (my/bookmark-buffer-or-switch-to-bookmark arg))))
                    '(4 5))
          ([?\s-7]
           . my/ivy-switch-buffer-detached-command)
          ([?\s-8]
           . mu4e-headers-search-bookmark)
          ([?\s-9] .
           (lambda ()
             (interactive)
             (my/ivy-switch-buffer-urxvt)))
          ([?\s-0]
           . erc-track-switch-buffer)
          ([?\s-r] .
           (lambda ()
             (interactive)
             (eradio-toggle)))
          ([?\s-t] .
           (lambda ()
             (interactive)
             (start-process "" nil "/usr/bin/firefox")))
          ([?\s-p]
           . my/exwm-toggle-or-set-buffer-protection)
          ([?\s-u] .
           (lambda ()
             (interactive)
             (start-process "" nil "/usr/bin/urxvt")))
          ([?\s-=]
           . balance-windows)
          ([?\s-+] .
           (lambda ()
             (interactive)
             (exwm-layout-enlarge-window-horizontally 100)))
          ([?\s--] .
           (lambda ()
             (interactive)
             (exwm-layout-shrink-window-horizontally 100)))
          ([?\s-s] .
           (lambda ()
             (interactive)
             (shell-command "import png:- | xclip -selection c -t image/png &>/dev/null")))))

  (setq exwm-input-simulation-keys
        '(
          ([?\C-y] . [?\C-v])))

  (setq exwm-manage-force-tiling t)

  (define-key exwm-mode-map (kbd "C-c") nil)
  ;; Buffer switching
  (add-to-list 'exwm-input-prefix-keys ?\C-b)
  (add-to-list 'exwm-input-prefix-keys ?\M-b)
  ;; Engine mode
  (add-to-list 'exwm-input-prefix-keys ?\C-j)
  ;; Project
  (add-to-list 'exwm-input-prefix-keys ?\C-p)
  ;; Window switching
  (define-key exwm-mode-map (kbd "<f8>") 'other-window)
  (define-key exwm-mode-map (kbd "C-<f8>") 'window-swap-states)

  (exwm-input-set-key (kbd "M-y") #'my/exwm-counsel-yank-pop)

  (defun my/exwm-counsel-yank-pop ()
    "Same as `counsel-yank-pop' and paste into exwm buffer.
Stolen from https://github.com/DamienCassou/gpastel#for-exwmcounsel-users
and adapted to use simulations keys to have a common yank keystroke."
    (interactive)
    (let ((inhibit-read-only t)
          (yank-pop-change-selection t))
      (call-interactively #'counsel-yank-pop))
    (when (derived-mode-p 'exwm-mode)
      ;; https://github.com/ch11ng/exwm/issues/413#issuecomment-386858496
      (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
      (let ((keys (gethash [?\C-y]
                       exwm-input--simulation-keys)))
        (dolist (key keys)
          (exwm-input--fake-key key)))))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer (my/exwm-buffer-name))))

  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (setq-local default-directory (expand-file-name "~/"))
              (when (and exwm-class-name
                         (string= (downcase exwm-class-name) "urxvt"))
                (exwm-input-set-local-simulation-keys
                 (append
                  exwm-input-simulation-keys
                  '(([?\C-y] . [?\C-\S-v])))))
              (when (and exwm-class-name
                         (string= (downcase exwm-class-name) "firefox"))
                (exwm-input-set-local-simulation-keys
                 (append
                  exwm-input-simulation-keys
                  '(([?\C-s] . [?\C-f]) ; Swiper!
                    ([?\C-g] . [escape])
                    ([?\C-t] . nil))))))) ; Prevent accidental tab ; creation

  (add-hook 'exwm-init-hook
            (lambda ()
              (progn
                (split-window-right)
                (start-process-shell-command "xmodmap" nil "xmodmap ~/.Xmodmap")
                (eshell))))

  (exwm-enable))

(use-package time
  :ensure nil
  :after (exwm)
  :custom
  (display-time-default-load-average nil)
  (display-time-format "w%Vq%q %d/%b %H:%M")
  (display-time-use-mail-icon t)
  (display-time-mail-directory nil)
  (display-time-mail-function
   (lambda ()
     (-some-p #'integerp (mapcar
                          (lambda (maildir)
                            (let ((display-time-mail-directory maildir))
                              (display-time-mail-check-directory)))
                          (file-expand-wildcards "~/mail/*/INBOX/new")))))
  :config
  (display-time-mode))

(use-package desktop-environment
  :after (exwm)
  :config
  (exwm-input-set-key (kbd "s-<up>") #'desktop-environment-volume-increment)
  (exwm-input-set-key (kbd "s-<down>") #'desktop-environment-volume-decrement)
  (exwm-input-set-key (kbd "s-m") #'desktop-environment-toggle-mute)
  (exwm-input-set-key (kbd "s-l") #'desktop-environment-lock-screen)
  (exwm-input-set-key (kbd "<XF86AudioPlay>") #'desktop-environment-toggle-music)
  (exwm-input-set-key (kbd "<XF86AudioNext>") #'desktop-environment-music-next)
  ;; (exwm-input-set-key (kbd "s-s") #'desktop-environment-screenshot-part)
  :custom
  (desktop-environment-volume-get-command "pamixer --get-volume")
  (desktop-environment-volume-set-command "pamixer %s")
  (desktop-environment-volume-get-regexp "\\([0-9]+\\)")
  (desktop-environment-volume-normal-increment "-i 5 --allow-boost")
  (desktop-environment-volume-normal-decrement "-d 5")
  (desktop-environment-volume-toggle-command "pamixer -t")
  ;; (desktop-environment-screenshot-partial-command "import png:- | xclip -selection c -t image/png")
  (desktop-environment-screenlock-command "xscreensaver-command -lock"))

;;; Building and compiling
(use-package ansi-color
  :ensure nil)

(use-package compile
  :ensure nil
  :config
  (defun my/colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package multi-compile
  :bind (("C-x b" . recompile)
         ("C-x B" . multi-compile-run))
  :init
  (defun my/multi-compile--bundle-environment (module-origin)
    (let* ((puppet-version "~>6")
           (environment (list (format "PUPPET_VERSION=\"%s\"" puppet-version))))
      (when (eq module-origin 'cern)
        (add-to-list 'environment "BUNDLE_GEMFILE=../ci/Gemfile"))
      (string-join environment " ")))

  ;; sudo ln -s /usr/bin/ruby-2.7 /usr/bin/ruby
  ;; sudo ln -s /usr/bin/bundle-2.7 /usr/bin/bundle
  (defun my/multi-compile--bundle (module-origin cmd &optional args)
    (let* ((ruby-version "2.7")
           (cmdline (list (my/multi-compile--bundle-environment module-origin)
                         (format "bundle-%s %s" ruby-version cmd))))
      (when args
        (add-to-list 'cmdline args t))
      (string-join cmdline " ")))

  (defun my/multi-compile--bundle-rake (module-origin task)
    (let ((rake-task
           (if (eq module-origin 'cern)
               (format "rake --rakefile ../ci/Rakefile %s" task)
             (format "rake %s" task))))
      (my/multi-compile--bundle module-origin "exec" rake-task)))

  (defun my/multi-compile--cern-module-p ()
    (s-contains-p "it-puppet" (or buffer-file-name "")))

  (defun my/multi-compile--find-module-root ()
      (locate-dominating-file buffer-file-name "metadata.json"))
  :custom
  (multi-compile-completion-system 'ivy)
  (multi-compile-default-directory-function #'my/multi-compile--find-module-root)
  (multi-compile-alist
   `(
     ;; All tests and bundle update for all Puppet and Ruby (SPEC) files.
     ((and
       (or (eq 'enh-ruby-mode major-mode) (eq 'puppet-mode major-mode))
       (my/multi-compile--cern-module-p)) .
       (("all-tests" . ,(my/multi-compile--bundle-rake 'cern "test"))
        ("bundle-update" . ,(my/multi-compile--bundle 'cern "update"))))
     ((and
       (or (eq 'enh-ruby-mode major-mode) (eq 'puppet-mode major-mode))
       (not (my/multi-compile--cern-module-p))) .
       (("all-tests" . ,(my/multi-compile--bundle-rake 'upstream "test"))
        ("bundle-update" . ,(my/multi-compile--bundle 'upstream "update"))))
     ;; Single test runs when it's a SPEC file
     ((string-match ".+it-puppet.+_spec\\.rb$" (or buffer-file-name "")) .
      (("single-test" . ,(my/multi-compile--bundle-rake 'cern "spec SPEC=%path"))))
     ((and
       (string-match "_spec\\.rb$" (or buffer-file-name ""))
       (not (my/multi-compile--cern-module-p))) .
       (("single-test" . ,(my/multi-compile--bundle-rake 'upstream "spec SPEC=%path")))))))

(defun my/regenerate-ctags ()
    (interactive)
    (setq default-directory (project-root (project-current)))
    (compile "ctags -e -R --exclude=@/home/nacho/.ctags/exclude --exclude=@.gitignore"))

;;; Org
(use-package org
  :ensure nil
  :bind
  (("C-c o c" . org-capture)
   ("C-c o s" . org-store-link))
  :config
  ;; Inspired by: https://elmord.org/blog/?entry=20180214-exwm-org-capture
  (defun my/exwm-get-firefox-url ()
    "Use EXWM to copy and extract the URL in the address bar"
    (exwm-input--fake-key ?\C-l)
    (sleep-for 0.05)
    (exwm-input--fake-key ?\C-c)
    (sleep-for 0.05)
    (gui-backend-get-selection 'CLIPBOARD 'STRING))

  (defun my/exwm-org-store-link ()
    "Attempt to store an Org link in Firefox windows"
    (when (and (equal major-mode 'exwm-mode)
               (member exwm-class-name '("firefox")))
      (org-store-link-props
       :type "http"
       :link (my/exwm-get-firefox-url)
       :description (replace-regexp-in-string " [-—] Mozilla Firefox$" "" exwm-title))))

  (org-link-set-parameters "firefox" :store 'my/exwm-org-store-link)
  :custom
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-startup-folded 'content))

(use-package org-agenda
  :ensure nil
  :hook
  (org-agenda-finalize . org-agenda-to-appt)
  :config
  (setq calendar-week-start-day 1)
  :custom
  (org-agenda-files '("~/org")))

(use-package org-protocol
  :ensure nil
  :config
  (defun my/org-protocol-eww-handler (props)
    (eww (plist-get props :url))
    nil)
  ;; https://marcowahl.gitlab.io/emacs-blog-mw/2021/20210126.html
  ;; Requires:
  ;; * Adding 'network.protocol-handler.expose.org-protocol'=false to FF
  ;; * Creating a bookmarklet:
  ;; ** javascript:location.href='org-protocol://eww?url='+encodeURIComponent(location.href)
  ;; * Open org-protocol:// links with emacsclient
  (add-to-list 'org-protocol-protocol-alist
               '("eww"
                 :protocol "eww"
                 :function my/org-protocol-eww-handler)))

(use-package org-capture
  :ensure nil
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
           "* TODO %?\n  %u\n  %a")
          ("w" "CERN meeting" entry (file+olp "~/org/calendar.org" "CERN" "Meetings")
           "* %(string-trim-left (buffer-name (plist-get org-capture-plist :original-buffer)) \"^F# \") %?\n  %^{Date and time?}T\n  %a"))))

(use-package org-tree-slide
  :custom
  (org-tree-slide-slide-in-effect nil))

;;; Notifications
(use-package notifications
  :ensure nil)

(use-package appt
  :ensure nil
  :config
  (defun my/appt-display (min-to-app new-time msg)
    (notifications-notify
     :body msg
     :category "appointment"
     :title (format "Appointment in %s minutes!" min-to-app)
     :urgency (if (= 0 (string-to-number min-to-app)) 'critical 'normal)
     :actions '("org-agenda" "Open org-agenda")
     :on-action (lambda (id key) (org-agenda-list))))
  (appt-activate 1)
  (org-agenda-to-appt)
  (run-with-timer (* 60 60) (* 60 60) 'org-agenda-to-appt)
  :custom
  (appt-display-mode-line nil)
  (appt-message-warning-time 15)
  (appt-display-interval 5)
  (appt-display-format 'window)
  (appt-disp-window-function 'my/appt-display))

;;; Host-specific configuration
(when (file-exists-p (format "~/.emacs.d/%s.el" system-name))
  (load-file (format "~/.emacs.d/%s.el" system-name)))

;;; LDAP
(use-package ldap
  :ensure nil
  :commands (ldap-search)
  :custom
  (ldap-host-parameters-alist
   '(("xldap.cern.ch"
      base "OU=Users,OU=Organic Units,DC=cern,DC=ch"
      auth simple
      passwd ""
      scope subtree))))

;;; CERN-specific goodies
(defun my/cern-ldap-user (account)
  "Do an LDAP query returning all attributes for account in a new buffer"
  (interactive "sAccount: ")
  (with-temp-buffer-window
      "*LDAP results*"
      #'temp-buffer-show-function
      nil
    (dolist (e (car (ldap-search
                     (concat "sAMAccountName=" account)
                     "xldap.cern.ch"
                     nil)))
      (princ (format "%s:%s\n" (nth 0 e) (nth 1 e)))))
  (with-current-buffer
      "*LDAP results*"
    (conf-mode)))

(defun my/clone-module (module-name)
  "Clone a Puppet module from gitlab.cern.ch/ai"
  (interactive "sModule name: ")
  (let ((magit-clone-url-format "https://%h/%n.git")
        (magit-clone-set-remote.pushDefault t))
    (magit-clone-internal
     ;; Using an internal here, see  https://github.com/magit/magit/discussions/4335
     (magit-clone--name-to-url (concat "it-puppet-module-" module-name))
     magit-clone-default-directory
     nil)))

(defun my/clone-hostgroup (hostgroup-name)
  "Clone a Puppet top-level hostgroup from gitlab.cern.ch/ai"
  (interactive "sTop-level hostgroup name: ")
  (let ((magit-clone-url-format "https://%h/%n.git")
        (magit-clone-set-remote.pushDefault t))
    (magit-clone-internal
     (magit-clone--name-to-url (concat "it-puppet-hostgroup-" hostgroup-name))
     magit-clone-default-directory
     nil)))

(defun my/os-same-project-as (fqdn)
  "Set the current OpenStack project to the same as FQDN's.
This function is meant to be executed from Eshell in a CWD where
ai-rc is installed. If `tramp-remote-process-environment' has
been successfully updated, then it returns the new project name,
otherwise it returns nil."
  (let ((project-name
         (with-temp-buffer
           (eshell-command (concat "ai-rc --same-project-as " fqdn) t)
           (keep-lines "^export OS_PROJECT_NAME")
           (unless (string-empty-p (buffer-string))
             (replace-string "\"" "")
             (car (last (split-string (substring (buffer-string) 7 -2) "=")))))))
    (unless (null project-name)
      (my/setenv-tramp "OS_PROJECT_NAME" project-name)
      project-name)))

;;; IRC
(use-package erc
  :ensure nil
  :config
  (erc-spelling-mode)
  :custom
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 14)
  (erc-fill-column (- (/ (frame-width) 2) 3))
  (erc-hide-list '("PART" "QUIT"))
  (erc-auto-query 'bury)
  (erc-join-buffer 'bury)
  (erc-kill-server-buffer-on-quit t)
  (erc-kill-queries-on-quit t)
  (erc-disable-ctcp-replies t)
  (erc-prompt (lambda nil (format "%s>" (buffer-name))))
  (erc-part-reason (lambda (&optional s) ""))
  (erc-user-mode "+iRw")
  (erc-nick "nacho")
  (erc-server "irc.libera.chat")
  (erc-email-userid "nacho")
  (erc-user-full-name "Nacho Barrientos"))

(use-package erc-track
  :ensure nil
  :config
  (dolist (msg '("JOIN" "PART" "QUIT" "MODE"))
    (add-to-list 'erc-track-exclude-types msg))
  :custom
  (erc-format-query-as-channel-p nil)
  (erc-track-priority-faces-only 'all)
  (erc-track-faces-priority-list
   '(erc-error-face
     erc-current-nick-face
     erc-keyword-face
     erc-nick-msg-face
     erc-direct-msg-face
     erc-notice-face
     erc-prompt-face)))

(use-package erc-join
  :ensure nil
  :custom
  (erc-autojoin-timing 'ident)
  (erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#erc" "#archlinux" "#theforeman"))))

(use-package erc-services
  :ensure nil
  :config
  (erc-services-mode 1)
  :custom
  ;; The password will be consumed from auth-source. For this the
  ;; secret must expose the following attributes:
  ;;   'host' -> 'irc.libera.chat'
  ;;   'user' -> 'nacho'
  (erc-prompt-for-password nil)
  (erc-prompt-for-nickserv-password nil))

;;; Misc
(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (my/delete-word (- arg)))

;; Stolen from: https://stackoverflow.com/questions/2471557/how-to-undo-fill-paragraph-in-emacs
(defun my/unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun my/getenv-tramp (variable &optional keyvalue)
  "Similar to `getenv' but acting on `tramp-remote-process-environment'.
If KEYVALUE is not nil then VARIABLE=VALUE is returned, otherwise
VALUE. When there's no such a VARIABLE set then nil is returned."
  (let ((current-variable-value
        (seq-find (lambda (x)
                       (s-starts-with-p (concat variable "=") x))
                  tramp-remote-process-environment)))
    (when current-variable-value
      (if keyvalue
          current-variable-value
        (car (last (split-string current-variable-value "=")))))))

(defun my/setenv-tramp (variable &optional value)
  "Like `setenv' but acting on `tramp-remote-process-environment'.
Removes the first occurrence of VARIABLE in
`tramp-remote-process-environment' and then adds VARIABLE=VALUE
if VALUE is not nil."
  (setq
   tramp-remote-process-environment
   (delete (my/getenv-tramp variable t)
           tramp-remote-process-environment))
  (when value
    (let ((key-value-pair (format "%s=%s" variable value)))
      (add-to-list 'tramp-remote-process-environment key-value-pair))))

(defun my/switch-to-buffer-if-exists-back-and-forth (to-buffer-name)
  "Switches to to-buffer-name if it exists. If the current buffer is
to-buffer-name then it switches back to the previous buffer."
  (when (get-buffer to-buffer-name)
    (if (string-equal to-buffer-name (buffer-name))
        (switch-to-prev-buffer)
      (switch-to-buffer to-buffer-name))))

(setq my/bookmark-buffer-or-switch-to-bookmark--bookmarks-alist nil)
(defun my/bookmark-buffer-or-switch-to-bookmark (arg)
  "Switches to the buffer associated to `last-command-event'.
If there's no mapping configured it sets it. With prefix argument
remaps `last-command-event' to the current buffer. The mapping is
stored in
`my/bookmark-buffer-or-switch-to-bookmark--bookmarks-alist'"
  (interactive "P")
  (when arg
    (setq my/bookmark-buffer-or-switch-to-bookmark--bookmarks-alist
          (assq-delete-all last-command-event
                           my/bookmark-buffer-or-switch-to-bookmark--bookmarks-alist)))
  (let ((buffer (cdr
                 (assq last-command-event
                       my/bookmark-buffer-or-switch-to-bookmark--bookmarks-alist))))
    (if buffer
        (if (buffer-live-p buffer)
            (my/switch-to-buffer-if-exists-back-and-forth (buffer-name buffer))
          (ding)
          (message "This buffer has been killed"))
      (add-to-list 'my/bookmark-buffer-or-switch-to-bookmark--bookmarks-alist
                   (cons last-command-event (current-buffer)))
      (with-current-buffer (current-buffer)
        (my/exwm-toggle-or-set-buffer-protection nil t))
      (message (format "Added %s as shortcut for buffer <%s>"
                       (key-description (vector last-command-event))
                       (current-buffer))))))

;;; init.el ends here
