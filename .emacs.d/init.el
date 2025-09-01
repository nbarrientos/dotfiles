;; Copyright (C) 2021-2023 Nacho Barrientos

;; Author: Nacho Barrientos <nacho.barrientos@cern.ch>
;; URL: https://git.sr.ht/~nbarrientos/dotfiles/tree/master/.emacs.d
;; Package-Requires: ((emacs "30.1"))

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
 auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-saves/" t))
 cursor-type 'hbar
 custom-file null-device
 custom-unlispify-tag-names nil
 enable-recursive-minibuffers t
 help-window-select t
 indent-tabs-mode nil
 inhibit-startup-echo-area-message "nacho"
 inhibit-startup-message t
 make-backup-files nil
 native-comp-async-report-warnings-errors nil
 next-screen-context-lines 0
 ring-bell-function 'ignore
 scroll-conservatively 1
 truncate-string-ellipsis "·"
 use-short-answers t
 vc-follow-symlinks t
 visible-bell t)

(server-start)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(mouse-avoidance-mode 'jump)
(minibuffer-depth-indicate-mode)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "C-d") 'mark-word)
(global-set-key (kbd "M-d") 'my/delete-word)
(global-set-key (kbd "M-<backspace>") 'my/backward-delete-word)
(global-set-key (kbd "C-<prior>") 'beginning-of-buffer)
(global-set-key (kbd "C-<next>") 'end-of-buffer)
(global-set-key (kbd "C-x o") 'delete-blank-lines)
(global-set-key (kbd "C-x C-o") 'delete-indentation)
(global-set-key (kbd "C-x C-d") 'duplicate-dwim)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(add-to-list 'yank-excluded-properties 'face)

(put 'narrow-to-region 'disabled nil)

;;;; Fonts
(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Iosevka"
                      :height 120))

(set-face-attribute 'fixed-pitch nil
                    :family (face-attribute 'default :family))

(when (member "FreeSans" (font-family-list))
  (set-face-attribute 'variable-pitch nil
                      :family "FreeSans"
                      :weight 'regular
                      :height 1.1))

(when (member "JoyPixels" (font-family-list))
  (mapc (lambda (characters)
          (set-fontset-font t characters "JoyPixels"))
        '(symbol emoji)))

;;;; Remedies for to-be-reeducated muscle memory
(global-unset-key (kbd "C-x C-f"))
(global-unset-key (kbd "C-x s"))
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "C-x k"))

;;; Use-package

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t)
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
                    #'outline-minor-faces-mode))

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)))

;;;; Grep
(use-package rg
  :bind (:map rg-mode-map
              ("C-<down>" . rg-next-file)
              ("C-<up>" . rg-prev-file))
  :config
  (add-to-list 'rg-custom-type-aliases '("texi" . "*.texi"))
  :custom
  (rg-group-result t)
  (rg-buffer-name (lambda nil
                    (format "*ripgrep (%s)*" (cdr (project-current))))))

;;; Navigation
(use-package goto-chg
  :bind (("C-z" . goto-last-change)))

;;;; Xref
(use-package xref
  :ensure nil
  :bind (("M-/" . xref-find-references)))

;;;; Project management
(use-package project
  :ensure nil
  :bind-keymap ("C-p" . project-prefix-map)
  :bind ((:map project-prefix-map
               ("<home>" . consult-project-buffer)
               ("<end>" . project-kill-buffers)
               ("b" . nil)
               ("C" . magit-clone)
               ("f" . consult-project-extra-find)
               ("F" . consult-project-extra-find-other-window)
               ("g" . rg-project)
               ("G" . rg-dwim-project-dir)
               ("H" . my/clone-hostgroup)
               ("k" . nil)
               ("m" . magit-project-status)
               ("M" . my/clone-module)
               ("s" . project-eshell)
               ("t" . my/regenerate-ctags)))
  :config
  (global-unset-key (kbd "C-x p"))
  (advice-add 'project-remember-project
              :filter-args
              (lambda (args)
                "Abbreviates project paths."
                (when (listp (car args))
                  (pcase-let ((`(,type ,backend ,path) (car args)))
                    (setcar args (list type backend (abbreviate-file-name path)))))
                args))
  :custom
  (project-switch-commands
   '((consult-project-extra-find "Find file")
     (rg-project "Ripgrep")
     (project-find-dir "Find directory")
     (project-eshell "Eshell")
     (consult-project-buffer "Buffers")
     (project-kill-buffers "Kill buffers")
     (magit-project-status "Magit"))))

;;;; Imenu
(use-package imenu)

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
(use-package delsel
  :ensure nil
  :custom
  (delete-selection-mode 1))

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
  :bind (("C-x /" . undo-tree-visualize))
  :config
  (unbind-key "C-b" undo-tree-visualizer-mode-map)
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-auto-save-history nil)
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
  :hook ((prog-mode . auto-fill-mode)
         (org-mode . auto-fill-mode)))

(use-package expand-region
  :bind ("C-f" . er/expand-region)
  :custom
  (expand-region-show-usage-message nil))

(use-package crux
  :bind ("C-o" . crux-smart-open-line-above))

(use-package substitute
  :bind-keymap ("C-x s" . substitute-prefix-map)
  :bind ((:map substitute-prefix-map
               ("b" . substitute-target-in-buffer)
               ("d" . substitute-target-in-defun)
               ("a" . substitute-target-above-point)
               ("l" . substitute-target-below-point)))
  :hook ((substitute-post-replace . substitute-report-operation))
  :config
  (setq substitute-prefix-map (make-sparse-keymap)))

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
;; "(user@host) Your 2nd factor (user):" is typical prompt for
;; 2FA tokens at CERN
(add-to-list 'password-word-equivalents "Your 2nd factor")
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/opt/puppetlabs/bin" t)
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

(use-package tramp-cmds
  :ensure nil
  :init
  (add-hook 'tramp-cleanup-connection-hook
            (lambda (vector)
              (let ((host (tramp-file-name-host vector))
                    (default-directory (expand-file-name "~/")))
                (call-process-shell-command (concat "/usr/bin/ssh -O exit " host) nil nil)))))

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

(use-package sudo-edit)

(use-package sops
  :bind-keymap ("C-c p" . sops-prefix-map)
  :bind (:map sops-prefix-map
         ("e" . sops-edit-file)
         ("s" . sops-save-file)
         ("k" . sops-cancel))
  :config
  (setq sops-prefix-map (make-sparse-keymap))
  :init
  (global-sops-mode 1))

;;; Dired
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-NGalhv --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
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

(use-package disk-usage)

;;; Auto completion

(use-package emacs
  :bind
  (("C-x f" . my/find-file-no-tramp))
  :config
  (defun my/find-file-no-tramp (&optional initial-input initial-directory)
    (interactive)
    (if (string-prefix-p "/ssh:" default-directory)
        (let ((default-directory "~/"))
          (call-interactively #'find-file))
      (call-interactively #'find-file))))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
         ("DEL" . vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (vertico-multiform-mode)
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-resize nil)
  (vertico-multiform-commands
   '((Info-menu (vertico-sort-function . nil)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode)
  (add-to-list 'marginalia-annotator-registry
               '(file none))
  (add-to-list 'marginalia-annotator-registry
               '(project-file none))
  (add-to-list 'marginalia-prompt-categories
               '("\\<channel\\>" . radiostation))
  (add-to-list 'marginalia-command-categories
               '(mu4e-search-maildir . maildir))
  (advice-add 'marginalia--buffer-file :filter-return
              (lambda (buffer-file)
                (string-trim-left buffer-file "(compilation\\(<.+>\\)? run) "))))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode))

(use-package consult
  :bind
  (([home] . consult-buffer)
   ("M-<home>" . consult-buffer-other-window)
   ([end] . kill-current-buffer)
   ("C-s" . consult-line)
   ("M-y" . consult-yank-pop)
   ("C-x i" . consult-imenu)
   :map minibuffer-local-map
   ("C-s" . consult-history)
   ("C-<up>" . previous-history-element)
   ("C-<down>" . next-history-element)
   ("<next>" . nil)
   ("<prior>" . nil))
  :custom
  (consult-async-input-throttle 0.1)
  (consult-async-refresh-delay 0.1)
  (consult-buffer-sources
   '(consult--source-hidden-buffer
     consult--source-modified-buffer
     consult--source-buffer
     consult--source-recent-file
     consult--source-project-buffer))
  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (consult-customize
   my/consult-buffer-firefox
   my/consult-buffer-ansi-term
   my/consult-buffer-detached-command
   consult-buffer consult-buffer-other-window consult-project-buffer
   :preview-key nil)
  (consult-customize
   consult-line
   :initial (when (use-region-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end))))
  :init
  (defun consult-esh-dir-history ()
    (interactive)
    (eshell/cd (consult--read
                (ring-elements eshell-last-dir-ring)
                :prompt "Directory to change to: ")))
  (defun my/consult-xstarter ()
    (interactive)
    (let* ((candidates (split-string
                        (shell-command-to-string "xstarter -P")
                        "\n"
                        t))
           (application-path (consult--read
                              candidates
                              :prompt "Application to launch: ")))
      (start-process "" nil application-path)))
  (defun my/consult-buffer-by-prefix (prefix caller show-preview)
    "Use consult to select a buffer prefixed by PREFIX#.

Show buffer previews if SHOW-PREVIEW is not nil."
    (let* ((consult--customize-alist
            (if show-preview
                (remove (list caller :preview-key nil) consult--customize-alist)
              consult--customize-alist))
           (my/consult--source-buffer-prefixed
            `(:name ,(format "Buffers (%s)" prefix)
                    :category buffer
                    :face consult-buffer
                    :history buffer-name-history
                    :state ,#'consult--buffer-state
                    :default t
                    :items
                    ,(lambda ()
                       (consult--buffer-query
                        :sort 'visibility
                        :include (concat "^" prefix "#")
                        :as #'buffer-name))))
           (consult-buffer-sources (list my/consult--source-buffer-prefixed)))
      (consult-buffer)))
  (defun my/consult-buffer-firefox (arg)
    "Use consult to select a Firefox buffer."
    (interactive "P")
    (my/consult-buffer-by-prefix "F" this-command arg))
  (defun my/consult-buffer-ansi-term (arg)
    "Use consult to select an ansi-term buffer."
    (interactive "P")
    (my/consult-buffer-by-prefix "U" this-command arg))
  (defun my/consult-buffer-detached-command (arg)
    "Use consult to select a compilation buffer."
    (interactive "P")
    (my/consult-buffer-by-prefix "D" this-command arg)))

(use-package consult-flycheck)

(use-package consult-project-extra
  :custom
  (consult-project-extra-sources
   '(consult-project-extra--source-file)))

(use-package embark
  :bind
  (("C-b" . embark-act)
   :map embark-symbol-map
   ("h" . helpful-symbol)
   :map embark-file-map
   ("s" . sudo-edit-find-file)
   ("b" . browse-url))
  :custom
  (embark-indicators '(embark-minimal-indicator))
  (embark-quit-after-action '((kill-buffer . nil) (t . t)))
  :config
  (assoc-delete-all 'kill-buffer embark-pre-action-hooks))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package nerd-icons
  :config
  (assoc-delete-all "^\\." nerd-icons-regexp-icon-alist)
  (add-to-list 'nerd-icons-extension-icon-alist
               '("epp" nerd-icons-sucicon "nf-custom-puppet"
                 :face nerd-icons-blue))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("erb" nerd-icons-sucicon "nf-custom-puppet"
                 :face nerd-icons-orange))
  (add-to-list 'nerd-icons-mode-icon-alist
               '(exwm-mode
                 nerd-icons-codicon "nf-cod-browser"
                 :face nerd-icons-purple)))

(use-package nerd-icons-completion
  :after (nerd-icons)
  :init
  (nerd-icons-completion-mode)
  :hook
  ((marginalia-mode . #'nerd-icons-completion-marginalia-setup))
  :config
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql radiostation)))
    "Return the icon for the candidate CAND of completion category radiostation."
    (concat (nerd-icons-octicon
             "nf-oct-broadcast"
             :face 'nerd-icons-blue)
            " "))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql project-file)))
    "Return the icon for the candidate CAND of completion category project."
    (concat (nerd-icons-octicon
             "nf-oct-git_branch"
             :face 'nerd-icons-blue)
            " "))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql maildir)))
    "Return the icon for the candidate CAND of completion category maildir."
    (concat (nerd-icons-octicon
             "nf-oct-inbox")
            " ")))

(use-package avy
  :bind (("C-v" . avy-goto-char-timer))
  :custom
  (avy-keys '(?a ?s ?d ?f ?h ?j ?l))
  (avy-dispatch-alist
   '((?k . avy-action-kill-move)
     (?K . avy-action-kill-stay)
     (?t . avy-action-teleport)
     (?m . avy-action-mark)
     (?w . avy-action-copy)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-line)
     (?z . avy-action-zap-to-char)))
  (avy-single-candidate-jump t)
  (avy-all-windows nil))

;;;; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet))

;;;; Expand
(use-package minibuffer
  :ensure nil
  :bind
  (("C-M-n" . completion-at-point)))

(use-package hippie-expand
  :ensure nil
  :bind
  (("C-n" . hippie-expand))
  :config
  (global-unset-key (kbd "M-/"))
  (defun my/try-expand-by-dict (old)
    (when (derived-mode-p 'text-mode)
      (let ((ispell-complete-word-dict
             (concat "/usr/share/dict/" ispell-current-dictionary)))
        (unless old
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     (ispell-lookup-words
                      (buffer-substring-no-properties (he-lisp-symbol-beg) (point))))))
        (if (null he-expand-list)
            (if old (he-reset-string))
          (he-substitute-string (car he-expand-list))
          (setq he-expand-list (cdr he-expand-list))
          t))))
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
     try-complete-lisp-symbol
     my/try-expand-by-dict)))

;;; Look and feel
(use-package modus-themes
  :after (mu4e doom-modeline)
  :config
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-mixed-fonts nil)
  (setq modus-themes-common-palette-overrides
        `((bg-paren-match bg-magenta-subtle)
          (bg-region bg-lavender)
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (cursor blue-intense)
          (fg-region unspecified)
          (fringe unspecified)
          (underline-link unspecified)
          (underline-link-symbolic unspecified)
          (underline-link-visited unspecified)
          ,@modus-themes-preset-overrides-intense))
  (setq modus-vivendi-palette-overrides
        `((bg-mode-line-active bg-blue-nuanced)
          (bg-mode-line-inactive bg-dim)))
  (setq modus-themes-to-toggle '(modus-vivendi modus-vivendi-tinted))
  ;; https://christiantietze.de/posts/2023/01/modus-themes-v4-changes/
  (defun my/modus-themes-customize-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       `(consult-file ((,c :weight normal)))
       `(doom-modeline-buffer-file ((,c :foreground ,blue-faint :inherit nil)))
       `(doom-modeline-buffer-major-mode ((,c :foreground ,blue-warmer)))
       `(doom-modeline-time ((,c :foreground ,fg-dim)))
       `(forge-pullreq-merged ((,c :inherit default)))
       `(forge-pullreq-open ((,c :inherit bold)))
       `(forge-pullreq-rejected ((,c :inherit default)))
       `(mode-line ((,c :height 110)))
       `(mode-line-inactive ((,c :height 110)))
       `(mu4e-header-highlight-face ((,c :weight normal :underline nil)))
       `(mu4e-unread-face ((,c :weight normal)))
       `(rpm-spec-dir-face ((,c :foreground ,green)))
       `(rpm-spec-doc-face ((,c :inherit font-lock-doc-face)))
       `(rpm-spec-ghost-face ((,c :foreground ,red)))
       `(rpm-spec-macro-face ((,c :inherit font-lock-type-face)))
       `(rpm-spec-obsolete-tag-face ((,c :inherit modus-themes-intense-red)))
       `(rpm-spec-package-face ((,c :foreground ,red)))
       `(rpm-spec-section-face ((,c :inherit font-lock-builtin-face)))
       `(rpm-spec-tag-face ((,c :inherit font-lock-keyword-face)))
       `(rpm-spec-var-face ((,c :inherit font-lock-variable-name-face))))))
  (add-hook 'modus-themes-after-load-theme-hook #'my/modus-themes-customize-faces)
  (modus-themes-load-theme 'modus-vivendi)
  (define-key global-map (kbd "<f1>") #'modus-themes-toggle))

(use-package doom-modeline
  :config
  (setq-default mode-line-buffer-identification "%b")
  (setq doom-modeline-mode-alist nil)
  (doom-modeline-def-modeline 'my-modeline
    '(bar matches buffer-info remote-host buffer-position)
    '(misc-info time irc debug input-method major-mode process check))
  (add-hook 'doom-modeline-mode-hook
            (lambda nil
              (doom-modeline-set-modeline 'my-modeline 'default)))
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-highlight-modified-buffer-name nil)
  (doom-modeline-irc-buffers t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-time-icon nil))

(use-package frame
  :ensure nil
  :custom
  (window-divider-default-right-width 7)
  :config
  (window-divider-mode 1))

(use-package info-colors
  :hook ((Info-selection . info-colors-fontify-node)))

(use-package rainbow-mode)

;;; Modes for coding
;;;; Syntax checking
(use-package flycheck
  :bind
  ("<f12>" . flycheck-mode)
  :custom
  (flycheck-highlighting-mode . nil))

(use-package flycheck-package
  :config
  (flycheck-package-setup))

;;;; Programming languages
(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode))
  :bind (:map lispy-mode-map-lispy
         ("C-j" . nil)
         :map lispy-mode-map-special
         ("v" . special-lispy-ace-paren)))

(use-package clojure-mode)

(use-package ruby-mode
  :ensure nil
  :hook ((ruby-mode . (lambda ()
                        (setq
                         flycheck-ruby-rubocop-executable
                         "~/.local/bin/rubocop")))))

(use-package python-mode
  :custom
  (py-split-windows-on-execute-function 'split-window-horizontally)
  (py-split-window-on-execute-threshold 2)
  (py-outline-minor-mode-p nil))

(use-package python-pytest
  :after python-mode
  :bind (:map python-mode-map
              ("C-c t" . python-pytest-dispatch)))

(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-minor-mode)))

(use-package puppet-mode
  :hook ((puppet-mode . (lambda ()
                          (setq
                           flycheck-puppet-lint-executable
                           "~/.local/bin/puppet-lint")
                          (setq
                           flycheck-puppet-parser-executable
                           "~/.local/bin/puppet"))))
  :bind (:map puppet-mode-map
              ("C-c C-t" . my/puppet-rspec-find-spec-file-for))
  :config
  (defun my/puppet-rspec-find-spec-file-for (&optional a-file-name)
    "Find spec for A-FILE-NAME. If nil, use the current buffer's file.
It just guesses as the filename for the spec is rather arbitrary."
    (interactive)
    (unless a-file-name
      (setq a-file-name (buffer-file-name)))
    (and-let* ((spec-parent
                (expand-file-name
                 (locate-dominating-file a-file-name "spec")))
               (relative-file-name
                (string-remove-prefix
                 spec-parent
                 a-file-name))
               (code-path
                (string-trim-left relative-file-name ".+?/"))
               (almost-spec-file
                (format "%s/spec/%%s/%s_spec.rb"
                        spec-parent
                        (file-name-sans-extension code-path))))
      (dolist (test-type '("classes" "defines" "type_aliases" "acceptance"))
        (let ((spec-file (format almost-spec-file test-type)))
          (when (file-regular-p spec-file)
            (find-file-other-window (format spec-file test-type))))))))

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

(use-package yaml-pro
  :bind (:map yaml-pro-ts-mode-map
              ("C-c C-y" . my/yaml-pro-kill-yaml-path))
  :custom
  (yaml-pro-ts-path-element-separator ?·)
  :config
  (defun my/yaml-pro-kill-yaml-path ()
    (interactive)
    (kill-new (substring-no-properties (yaml-pro-ts-eldoc)))))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook 'yaml-pro-ts-mode 100))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-asymmetric-header t)
  (markdown-command "markdown_py -x fenced_code -x footnotes -x tables")
  (markdown-css-paths '("~/dev/github-markdown-css/github-markdown-dark.css"))
  (markdown-xhtml-body-preamble "<article class=\"markdown-body\">")
  (markdown-xhtml-body-epilogue "</article>")
  (markdown-xhtml-header-content "<style>
    .markdown-body {
      box-sizing: border-box;
      min-width: 200px; max-width: 980px;
      margin: 0 auto; padding: 45px;
    }
   </style>"))

(use-package rpm-spec-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.spec" . rpm-spec-mode)))

(use-package archive-rpm)

(use-package systemd)

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))

(use-package web-mode
  :custom
  (web-mode-enable-auto-indentation nil)
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.epp\\'" . web-mode))
  (add-to-list 'safe-local-variable-values '(web-mode-enable-auto-indentation . nil))
  (setq web-mode-engines-alist
        '(("erb" . "\\.epp\\'"))))

(use-package csv-mode)

(use-package jq-mode)

(use-package sql
  :ensure nil
  :bind (:map sql-mode-map
              ("C-c C-f" . sqlformat))
  :hook ((sql-interactive-mode . sql-rename-buffer))
  :custom
  (sql-mysql-options '("--prompt=mysql> "))
  :config
  (add-to-list 'sql-postgres-login-params 'port t)
  (add-to-list 'sql-mysql-login-params 'port t))

(use-package sqlformat
  :custom
  (sqlformat-command 'pgformatter))

(use-package nhexl-mode)

(use-package dockerfile-mode)

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

(use-package powerthesaurus)

(use-package package-lint)

(use-package spdx
  :custom
  (spdx-copyright-prefix "SPDX-FileCopyrightText: "))

(use-package license-snippets
  :after yasnippet
  :init
  (license-snippets-init))

(use-package kubel
  :defer t)

;;; Term
(use-package term
  :ensure nil
  :config
  (defun my/term-toggle-line-and-char-mode ()
    "Toggles term between line char and line mode."
    (interactive)
    (if (derived-mode-p 'term-mode)
        (if (term-in-line-mode)
            (term-char-mode)
          (term-line-mode))
      (user-error "This is not a term, maybe s-C?")))
  (defun my/remote-or-local-term (fqdn)
    "Invokes an ansi-term on FQDN.

If the region is active the contents are the FQDN. If no FQDN is
specified then localhost is used."
    (interactive
     (let ((default-value (if (use-region-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))
                            "localhost")))
       (list
        (read-string (format "Fully-qualified domain name (default: %s): " default-value)
                     nil nil default-value))))
    (let* ((default-directory "~/")
           (local-p (string= "localhost" fqdn))
           (term-ansi-buffer-name (concat "U# " fqdn))
           (term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
           (program (if local-p "bash" "ssh"))
           (switches (unless (string= "localhost" fqdn) (list fqdn)))
           (term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name program nil switches)))
      (set-buffer term-ansi-buffer-name)
      (term-mode)
      (term-char-mode)
      (with-current-buffer term-ansi-buffer-name
        (setq-local kill-buffer-query-functions nil)
        (rename-buffer (substring (buffer-name) 1 -1)))
      (switch-to-buffer term-ansi-buffer-name))))

;;; Eshell
(use-package eshell-bookmark
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

(use-package eshell
  :ensure nil
  :init
  (require 'esh-mode) ;; eshell-mode-map
  (require 'em-hist) ;; eshell-hist-mode-map
  :bind
  (:map eshell-mode-map
        ("M-<up>" . eshell-previous-prompt)
        ("M-<down>" . eshell-next-prompt)
        ("C-c C-o" . my/eshell-kill-ring-save-outputs)
        ("C-c o" . my/eshell-export-last-output)
        ("C-c r" . consult-history)
        ("C-c d" . consult-esh-dir-history)
        ("C-c l" . eshell/clear)
        ("C-<return>" . my/eshell-send-detached-input)
   :map eshell-hist-mode-map
        ("<up>" . previous-line)
        ("<down>" . next-line))
  :hook
  (eshell-mode . (lambda ()
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
               (lambda (name-of-mode)
                 (if (eq major-mode 'compilation-mode)
                     (buffer-name)
                     (generate-new-buffer-name (concat "D# " cmd)))))))
        (with-current-buffer compilation-buffer
          (setq list-buffers-directory default-directory)
          (when (equal arg 4)
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
                             :urgency (if (string-prefix-p "finished" str) 'normal 'critical))))))))
      (eshell-add-input-to-history cmd)
      (eshell-reset)))
  (setenv "EDITOR" "emacsclient")
  :custom
  (eshell-banner-message "")
  (eshell-history-size 20000)
  (eshell-history-append t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all))

(use-package eshell-prompt-extras
  :after (eshell)
  :config
  (setq eshell-prompt-regexp "^λ ")
  (defun my/epe-theme-prompt ()
    (let ((prompt-path (epe-fish-path (tramp-file-local-name (eshell/pwd))))
          (machine-face (if (epe-remote-p)
                            'epe-remote-face
                          'epe-symbol-face)))
      (concat
       (format
        (epe-colorize-with-face "[%s]" machine-face)
        (epe-colorize-with-face
         (if (string-empty-p prompt-path)
             "/"
           prompt-path)
         'epe-dir-face))
       (epe-colorize-with-face
        (concat "@" (if (epe-remote-p)
                        (epe-remote-host)
                      (system-name)))
        machine-face)
       (if (eshell-exit-success-p)
           (epe-colorize-with-face "\nλ" 'success)
         (epe-colorize-with-face "\nλ" 'error))
       " ")))
  (with-eval-after-load "esh-opt"
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'my/epe-theme-prompt)))

;;; Comint
(use-package comint
  :ensure nil
  :bind (:map comint-mode-map
         ("M-<up>" . comint-previous-prompt)
         ("M-<down>" . comint-next-prompt)
         ("C-c l" . comint-clear-buffer)
         ("C-c C-o" . my/comint-kill-ring-save-outputs))
  :config
  (defun my/comint-kill-ring-save-outputs ()
    "Add to the kill ring CURRENT-PREFIX-ARG outputs, including prompts.
If no universal argument is passed, assume only one output"
    (interactive)
    (save-excursion
      (let (times)
        (if (or (null current-prefix-arg) (< current-prefix-arg 1))
            (setq times 1)
          (setq times current-prefix-arg))
        (comint-previous-prompt times)
        (forward-line -1)
        (forward-line)
        (message (format "Comint output added to the kill ring (%d commands)" times))
        (kill-ring-save (point) (car comint-last-prompt))))))

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
  :hook
  ((magit-post-clone
    . (lambda ()
        (project-remember-project (list 'vc 'Git default-directory)))))
  :config
  (add-to-list 'magit-clone-name-alist
               '("\\`\\(?:cgl:\\)\\([^:]+\\)\\'" "gitlab.cern.ch" "ai"))
  (add-to-list 'magit-clone-url-format
               '("gitlab.cern.ch" . "https://:@%h:8443/%n.git"))
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
  (unbind-key "C-j" magit-diff-section-base-map)
  (unbind-key "C-j" magit-diff-section-map)
  (defun my/magit-am-apply-maildir-buffer (buffer &optional args)
    "Apply the patch in BUFFER as maildir/mbox."
    (interactive (list (read-buffer "Apply contents of buffer: " "*Article*")
                       (magit-am-arguments)))
    (let ((patch
           (with-current-buffer buffer
             (buffer-substring-no-properties (point-min) (point-max)))))
      (with-temp-buffer
        (insert patch)
        (magit-run-git-with-input
         "am" args "-"))
      (magit-refresh)))
  (transient-append-suffix 'magit-am "m"
       '("b" "maildir from buffer" my/magit-am-apply-maildir-buffer))
  (add-to-list 'magit-ellipsis `(margin (nil . ,truncate-string-ellipsis)))
  :custom
  (magit-blame-time-format "%d/%m/%y %R")
  (magit-save-repository-buffers 'dontask)
  (magit-clone-default-directory "~/dev/")
  (magit-clone-set-remote.pushDefault 'ask)
  (magit-remote-add-set-remote.pushDefault 'ask)
  (magit-list-refs-sortby "-creatordate")
  (magit-diff-extra-stat-arguments #'magit-diff-use-window-width-as-stat-width)
  (magit-diff-refine-hunk t))

(use-package orgit)

(use-package forge
  :after magit
  :bind (:map forge-topic-mode-map
              ("C-c C-a" . forge-topic-set-assignees))
  :config
  (add-to-list 'forge-alist
               '("gitlab.cern.ch" "gitlab.cern.ch/api/v4" "gitlab.cern.ch" forge-gitlab-repository))
  (defun my/forge-browse-pullreq-diff-at-file nil
    "Open in a browser the ediffed file in the pullreq diff view."
    (interactive)
    (let* ((filepath
            (car (split-string (buffer-name ediff-buffer-A) ".~")))
           (pr-url
            (forge-get-url (forge--pullreq-from-rev (magit-get-current-branch))))
           (diff-line-url
            (pcase (eieio-object-class (forge-get-repository nil))
              ('forge-github-repository
               (format "%s/files#diff-%s" pr-url (secure-hash 'sha256 filepath)))
              ('forge-gitlab-repository
               (format "%s/diffs#%s" pr-url (secure-hash 'sha1 filepath)))
              (_
               (user-error "Unsupported forge type")))))
      (with-selected-window (get-buffer-window ediff-buffer-A)
        (browse-url diff-line-url))))
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
  (advice-add 'ediff-window-display-p :override #'ignore)
  (defun my/y-or-n-p-ignore (ediff-quit-f &rest args)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
      (apply ediff-quit-f args)))
  (advice-add 'ediff-quit :around #'my/y-or-n-p-ignore)
  :hook
  ((ediff-startup . ediff-next-difference))
  ((ediff-prepare-buffer . outline-show-all))
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package git-modes)

(use-package gitlab-pipeline
  :after forge
  :custom
  (gitlab-pipeline-ghub-auth-token 'forge)
  :config
  (transient-insert-suffix 'forge-dispatch "v i" '("v P" "pipeline" gitlab-pipeline-show-sha)))

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
    "Toggle single window configuration and the previously multi-windowed one."
    (interactive)
    (if (one-window-p)
        (when (boundp 'my-saved-window-configuration)
          (set-window-configuration my-saved-window-configuration))
      (progn
        (setq my-saved-window-configuration (current-window-configuration))
        (delete-other-windows))))
  :custom
  (switch-to-buffer-preserve-window-point nil)
  (split-width-threshold nil)
  (split-height-threshold nil))

(use-package transpose-frame
  :bind (("M-<f8>" . rotate-frame-clockwise)))

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
(use-package helpful
  :bind
  (("C-h o" . helpful-symbol)
   ("C-h v" . helpful-variable)
   ("C-h f" . helpful-function)
   ("C-h k" . helpful-key)))

;;; Mu4e
(use-package mu4e
  :ensure nil
  :hook ((mu4e-compose-mode
          . (lambda ()
              (save-excursion
                (replace-regexp "^-- $" "--")))))
  :hook (mu4e-compose-mode
         . (lambda ()
             (when (eq mu4e-compose-type 'edit)
               (message-replace-header "Date" (message-make-date) "Subject"))))
  :custom
  (read-mail-command 'mu4e)
  (mu4e-change-filenames-when-moving t)
  (mu4e-modeline-show-global nil)
  (mu4e-confirm-quit t)
  (mu4e-update-interval (* 5 60))
  ;; (mu4e-html2text-command "lynx -dump -stdin")
  (message-signature " bye\n Nacho\n http://cern.ch/nacho")
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-maildir-initial-input "")
  (mu4e-hide-index-messages t)
  (mu4e-headers-leave-behavior 'apply)
  (mu4e-split-view nil)
  (mu4e-headers-precise-alignment t)
  (mu4e-headers-date-format "%d/%m/%y")
  (mu4e-headers-time-format "%R")
  (mu4e-drafts-folder "/cern/Drafts")
  (mu4e-sent-folder   "/cern/Sent Items")
  (mu4e-trash-folder  "/cern/Trash")
  (mu4e-get-mail-command "/usr/bin/systemctl --user start mbsync-prio-0")
  (mu4e-completing-read-function 'completing-read)
  (mu4e-use-fancy-chars t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-headers-fields
   '(( :human-date    .  12)
     ( :flags         .  8)
     ( :from          .  30)
     ( :subject       .  nil)))
  (mu4e-bookmarks
   '(( :name "Unread messages in INBOX"
       :query "maildir:/.*INBOX/ and flag:unread"
       :favorite t
       :key ?i)
     ( :name  "All unread messages"
       :query "flag:unread and not flag:trashed and not maildir:\"/cern/Junk E-Mail\""
       :key ?u)
     ( :name "Today's messages"
       :query "date:today..now and not maildir:\"/cern/Junk E-Mail\""
       :key ?t)
     ( :name "Last 7 days"
       :query "date:7d..now and not maildir:\"/cern/Junk E-Mail\""
       :key ?w)
     ( :name "Flagged messages"
       :query "flag:flagged"
       :key ?f
       :hide-unread t)))
  (mu4e-contact-process-function
   (lambda (contact)
     (unless (string-match-p "\.gmai\.com$" contact)
       contact)))
  :config
  (setq gnus-visible-headers
        (concat gnus-visible-headers "\\|^User-Agent:\\|^X-Mailer:\\|^Message-ID:"))
  (setq gnus-inhibit-images t)
  (setq mu4e-headers-attach-mark '("a" . "📎"))
  (setq mu4e-headers-replied-mark '("R" . "↳"))
  (setq mu4e-headers-passed-mark '("P" . "→"))
  (setq mu4e-headers-unread-mark '("u" . "☘"))
  (setq mu4e-headers-list-mark '("s" . ""))
  (setq mu4e-headers-personal-mark '("p" . ""))
  (setq user-mail-address "nacho.barrientos@cern.ch")
  (setq user-full-name "Nacho Barrientos")
  (setq message-send-mail-function 'sendmail-send-it)
  (setq message-citation-line-function
        'message-insert-formatted-citation-line)
  (setq message-citation-line-format
        "On %d/%m/%y, %N said:\n")
  (setq send-mail-function 'smtpmail-send-it)
  (setq mail-user-agent 'mu4e-user-agent)
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))
  (mu4e t)
  :bind (:map mu4e-headers-mode-map
              ("r" . 'mu4e-headers-mark-for-read)
              ("d" . 'mu4e-headers-mark-for-delete)))

(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

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
                     ("Option Musique" . "http://stream.srg-ssr.ch/srgssr/option-musique/mp3/128")
                     ("Couleur3" . "http://stream.srg-ssr.ch/srgssr/couleur3/mp3/128")
                     ("La 1ere" . "http://stream.srg-ssr.ch/srgssr/la-1ere/mp3/128")
                     ("Radio Lac" . "https://radiolac.ice.infomaniak.ch/radiolac-high.mp3")
                     ;; Spanish
                     ("RNE" . "https://rtvelivestream.akamaized.net/rtvesec/rne/rne_r1_main.m3u8")
                     ("RPA" . "https://cdnlive2.shooowit.net/rtpalive/smil:radio.smil/playlist.m3u8")
                     ;; English
                     ("WRS" . "https://uksouth.streaming.broadcast.radio:10290/wrs")
                     ("BBC1" . "http://stream.live.vc.bbcmedia.co.uk/bbc_radio_one")
                     ("BBC2" . "http://stream.live.vc.bbcmedia.co.uk/bbc_radio_two")
                     ("BBC5" . "http://stream.live.vc.bbcmedia.co.uk/bbc_radio_five_live_online_nonuk")
                     ("BBC Scotland" . "http://stream.live.vc.bbcmedia.co.uk/bbc_radio_scotland_fm"))))

(use-package pipewire)

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
  (defun my/engine-mode-exact-phrase-transform (search-term)
    (if current-prefix-arg
        (concat "\"" search-term "\"")
      search-term))
  (defengine archwiki
    "https://wiki.archlinux.org/index.php?search=%s"
    :keybinding "a")
  (defengine duckduckgo
    "https://duckduckgo.com/html?q=%s"
    :keybinding "d"
    :term-transformation-hook my/engine-mode-exact-phrase-transform)
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h")
  (defengine cern-gitlab
    "https://gitlab.cern.ch/search?search=%s"
    :keybinding "l")
  (defengine google
    "http://www.google.com/search?hl=en&ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g"
    :term-transformation-hook my/engine-mode-exact-phrase-transform)
  (defengine duckduckgo-first
    "https://duckduckgo.com/html?q=! %s"
    :keybinding "f")
  (defengine google-maps
    "https://www.google.com/maps/search/%s/"
    :keybinding "M")
  (defengine larousse
    "https://www.larousse.fr/dictionnaires/francais/%s"
    :keybinding "F")
  (defengine openstreetmap
    "https://www.openstreetmap.org/search?query=%s"
    :keybinding "m")
  (defengine wordreference
    "https://www.wordreference.com/es/translation.asp?tranword=%s"
    :keybinding "r")
  (defengine twitter
    "https://twitter.com/search?q=%s"
    :keybinding "t")
  (defengine twitter-latest
    "https://twitter.com/search?q=%s&f=live"
    :keybinding "T")
  (defengine urbandictionary
    "https://www.urbandictionary.com/define.php?term=%s"
    :keybinding "u")
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
  (exwm-systemtray-mode 1)

  (setq-default my/exwm--do-not-mass-kill nil)
  (defun my/exwm-toggle-or-set-buffer-protection (&optional value)
    "Toggle or set EXWM mass-buffer-deletion protection.
When called interactively, toggle. Otherwise set to VALUE."
    (interactive)
    (when (derived-mode-p 'exwm-mode)
      (if (called-interactively-p)
          (progn
            (if my/exwm--do-not-mass-kill
                (kill-local-variable 'my/exwm--do-not-mass-kill)
              (setq-local my/exwm--do-not-mass-kill t))
            (message "EXWM buffer protection set to %s" my/exwm--do-not-mass-kill))
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
    (let* ((length (or length 65))
           (title (concat "F# " (replace-regexp-in-string " [-—] Mozilla Firefox$" "" title)))
           (title-and-hostname (split-string title "@" nil " "))
           (hostname (substring (car (last title-and-hostname)) 0 -1))
           (page-title (string-join (reverse (nthcdr 1 (reverse title-and-hostname))) " "))
           (short-title (reverse (string-truncate-left (reverse page-title) length))))
      (if (length> title-and-hostname 1)
          (concat short-title " @ " hostname)
        (reverse (string-truncate-left (reverse title) length)))))

  (defun my/exwm--format-window-title-* (title)
    "Removes annoying notifications and FPS counters."
    (dolist (regexp '("([[:digit:]]+)" "FPS :[[:digit:]]+"))
      (setq title (replace-regexp-in-string regexp "" title)))
    (string-trim title))

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
          ([?\s-C]
           . exwm-input-toggle-keyboard)
          ([?\s-c]
           . my/term-toggle-line-and-char-mode)
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
             (my/consult-xstarter)))
          ([?\s-1] .
           my/consult-buffer-firefox)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" (car i))) .
                        (lambda ()
                          (interactive)
                          (my/switch-to-buffer-if-exists-back-and-forth ,(cdr i)))))
                    '((2 . "Telegram") (6 . "*eshell*")))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda (arg)
                          (interactive "P")
                          (my/bookmark-buffer-or-switch-to-bookmark arg))))
                    '(3 4 5))
          ([?\s-7]
           . my/consult-buffer-detached-command)
          ([?\s-8]
           . mu4e-search-bookmark)
          ([?\s-9]
           . my/consult-buffer-ansi-term)
          ([?\s-0]
           . erc-track-switch-buffer)
          ([?\s-r]
           . eradio-toggle)
          ([?\s-R]
           . eradio-play)
          ([?\s-t] .
           (lambda ()
             (interactive)
             (start-process "" nil "/usr/bin/firefox")))
          ([?\s-p]
           . my/exwm-toggle-or-set-buffer-protection)
          ([?\s-u]
           . my/remote-or-local-term)
          ([?\s-=]
           . balance-windows)
          (,(kbd "s-<up>")
           . desktop-environment-volume-increment)
          (,(kbd "s-<down>")
           . desktop-environment-volume-decrement)
          (,(kbd "s-m")
           . desktop-environment-toggle-mute)
          (,(kbd "s-l")
           . desktop-environment-lock-screen)
          (,(kbd "s-s")
           . desktop-environment-screenshot-part)
          ([?\s-+] .
           (lambda ()
             (interactive)
             (exwm-layout-enlarge-window-horizontally 100)))
          ([?\s--] .
           (lambda ()
             (interactive)
             (exwm-layout-shrink-window-horizontally 100)))))

  (setq exwm-input-simulation-keys
        '(
          ([?\C-y] . [?\C-v])
          ([?\C-w] . [?\C-c])
          ([C-prior] . [home])
          ([C-next] . [end])))

  (setq exwm-manage-force-tiling t)

  (add-to-list 'exwm-input-prefix-keys ?\C-c)
  ;; Engine mode
  (add-to-list 'exwm-input-prefix-keys ?\C-j)
  ;; Project
  (add-to-list 'exwm-input-prefix-keys ?\C-p)

  ;; Window switching
  (define-key exwm-mode-map (kbd "<f8>") 'other-window)
  (define-key exwm-mode-map (kbd "C-<f8>") 'window-swap-states)
  (define-key exwm-mode-map (kbd "M-<f8>") 'rotate-frame-clockwise)

  ;; Buffer switching
  (define-key exwm-mode-map (kbd "<home>") 'consult-buffer)
  (define-key exwm-mode-map (kbd "<end>") 'kill-current-buffer)
  (define-key exwm-mode-map (kbd "C-c C-c") 'exwm-input-send-next-key)

  (exwm-input-set-key (kbd "M-y") #'my/exwm-consult-yank-pop)

  (defun my/exwm-consult-yank-pop ()
    "Same as `consult-yank-pop' and paste into exwm buffer.
Stolen from https://github.com/DamienCassou/gpastel#for-exwmcounsel-users
and adapted to use simulations keys to have a common yank keystroke."
    (interactive)
    (let ((inhibit-read-only t)
          (yank-pop-change-selection t))
      (call-interactively #'consult-yank-pop))
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
                         (string= (downcase exwm-class-name) "firefox"))
                (exwm-input-set-local-simulation-keys
                 (append
                  exwm-input-simulation-keys
                  '(([?\C-s] . [?\C-f]) ; Swiper!
                    ([?\C-g] . [escape])
                    ([?\C-m] . [f6]); focus/unfocus address bar
                    ([?\C-i] . [f3]); next link in quick find
                    ([?\C-b] . [S-f3]); previous link in quick find
                    ([?\C-t] . nil))))))) ; Prevent accidental tab ; creation

  (add-hook 'exwm-init-hook
            (lambda ()
              (progn
                (split-window-right)
                (start-process "keepassxc" nil "keepassxc")
                (start-process "pasystray" nil "pasystray")
                (start-process "dunst" nil "dunst")
                (eshell))))

  (setq xcb:connection-timeout 10)
  (exwm-enable))

(use-package time
  :ensure nil
  :after (exwm)
  :custom
  (display-time-default-load-average nil)
  (display-time-format "[w%Vq%q %d/%b %H:%M]")
  (display-time-use-mail-icon nil)
  (display-time-mail-string "📫")
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
  :custom
  (desktop-environment-volume-get-command "pamixer --get-volume")
  (desktop-environment-volume-set-command "pamixer %s")
  (desktop-environment-volume-toggle-regexp nil)
  (desktop-environment-volume-get-regexp "\\([0-9]+\\)")
  (desktop-environment-volume-normal-increment "-i 5 --allow-boost")
  (desktop-environment-volume-normal-decrement "-d 5")
  (desktop-environment-volume-toggle-command "pamixer -t")
  (desktop-environment-screenshot-directory "/tmp")
  (desktop-environment-screenshot-command "scrot -s")
  (desktop-environment-screenshot-delay-argument nil)
  (desktop-environment-screenshot-partial-command "import png:- | xclip -selection c -t image/png -verbose")
  (desktop-environment-screenlock-command "xscreensaver-command -lock"))

(use-package bluetooth
  :custom
  (bluetooth-battery-warning-level 15))

;;; Building and compiling
(use-package ielm
  :ensure nil
  :bind (:map ielm-map
              ("C-<return>" . ielm-return))
  :custom
  (ielm-prompt "λ> "))

(use-package ansi-color
  :ensure nil)

(use-package compile
  :ensure nil
  :config
  (defun my/colorize-compilation-buffer ()
    (read-only-mode 'toggle)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode 'toggle))
  (add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package multi-compile
  :bind (("C-x b" . recompile)
         ("C-x B" . multi-compile-run))
  :init
  (defun my/multi-compile--bundle-environment (module-origin)
    (let* ((puppet-version "~>7")
           (environment (list (format "PUPPET_GEM_VERSION=\"%s\"" puppet-version))))
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
    (if buffer-file-name
        (or (locate-dominating-file buffer-file-name "metadata.json")
            (locate-dominating-file buffer-file-name "Chart.yaml")
            (project-root (project-current)))
      (project-root (project-current))))
  :custom
  (multi-compile-completion-system 'auto)
  (multi-compile-default-directory-function #'my/multi-compile--find-module-root)
  (multi-compile-alist
   `(
     ;; All tests and bundle update for all Puppet and Ruby (SPEC) files.
     ((and
       (or (eq 'ruby-mode major-mode) (eq 'puppet-mode major-mode))
       (my/multi-compile--cern-module-p)) .
       (("all-tests" . ,(my/multi-compile--bundle-rake 'cern "test"))
        ("generate-reference" . ,(my/multi-compile--bundle-rake 'cern "strings:generate:reference"))
        ("rubocop" . ,(my/multi-compile--bundle-rake 'cern "rubocop"))
        ("bundle-update" . ,(my/multi-compile--bundle 'cern "update"))))
     ((and
       (or (eq 'ruby-mode major-mode) (eq 'puppet-mode major-mode))
       (not (my/multi-compile--cern-module-p))) .
       (("all-tests" . ,(my/multi-compile--bundle-rake 'upstream "test"))
        ("generate-reference" . ,(my/multi-compile--bundle-rake 'upstream "strings:generate:reference"))
        ("rubocop" . ,(my/multi-compile--bundle-rake 'upstream "rubocop"))
        ("bundle-update" . ,(my/multi-compile--bundle 'upstream "update"))))
     ;; Single test runs when it's a SPEC file
     ((string-match ".+it-puppet.+_spec\\.rb$" (or buffer-file-name "")) .
      (("single-test" . ,(my/multi-compile--bundle-rake 'cern "spec SPEC=%path"))))
     ((and
       (string-match "_spec\\.rb$" (or buffer-file-name ""))
       (not (my/multi-compile--cern-module-p))) .
       (("single-test" . ,(my/multi-compile--bundle-rake 'upstream "spec SPEC=%path"))))
     ((eq 'yaml-mode major-mode) .
      (("helm-render-this" . "helm template . -s templates/%file-name")
       ("helm-render-all" . "helm template .")
       ("helm-unit-test-all" . "helm unittest -f 'tests/**/*.yaml' .")
       ("helm-docs" . "helm-docs")))
     )))

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
  (org-display-custom-times t)
  (org-time-stamp-custom-formats '("<%a %B %e %Y>" . "<%a %B %e %Y %H:%M>"))
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-startup-folded 'content))

(use-package org-agenda
  :ensure nil
  :bind (("C-c o a" . org-agenda))
  :hook
  (org-agenda-finalize . org-agenda-to-appt)
  :config
  (setq calendar-week-start-day 1)
  :custom
  (org-agenda-files '("~/org/calendar.org")))

(use-package org-protocol
  :ensure nil
  :after org
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
  :after org
  :config
  (defun my/org-capture-region-or-buffer-name ()
    "Return the active region if active, otherwise the buffer name."
    (let ((original-buffer (plist-get org-capture-plist :original-buffer)))
      (with-current-buffer original-buffer
        (if (region-active-p)
            (buffer-substring-no-properties (region-beginning) (region-end))
          (string-trim-left (buffer-name original-buffer) "^F# ")))))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
           "* TODO %?\n  %u\n  %a")
          ("w" "CERN meeting" entry (file+olp "~/org/calendar.org" "CERN" "Meetings")
           "* %(my/org-capture-region-or-buffer-name)%?\n  %^{Date and time?}T\n  %a"))))

(use-package org-tree-slide
  :after org
  :custom
  (org-tree-slide-slide-in-effect nil))

(use-package ox-gfm
  :after org)

(use-package ob-eshell
  :after org
  :ensure nil)

;;; Home Assistant
(use-package hass
  :custom
  (hass-insecure t)
  (hass-apikey (lambda ()
                 (funcall
                  (plist-get (car (auth-source-search :type 'secrets :host hass-host))
                             :secret))))
  :config
  (defun my/desk-lamp-brightness-up ()
    (interactive)
    (my/desk-lamp-brightness-step-pct 5))
  (defun my/desk-lamp-brightness-down ()
    (interactive)
    (my/desk-lamp-brightness-step-pct -5))
  (defun my/desk-lamp-off ()
    (interactive)
    (my/desk-lamp-brightness-step-pct -100))
  (defun my/desk-lamp-brightness-step-pct (level &optional zone)
    (let ((zone (or zone "mezzanine")))
      (hass-call-service-with-payload
       "light.turn_on"
       (format "{\"area_id\": \"%s\", \"brightness_step_pct\": %d}" zone level))
      (message "Brightness level in '%s' adjusted by %+d%%" zone level)))
  (exwm-input-set-key (kbd "s-(") #'my/desk-lamp-brightness-down)
  (exwm-input-set-key (kbd "s-)") #'my/desk-lamp-brightness-up)
  (exwm-input-set-key (kbd "s-*") #'my/desk-lamp-off))

;;; Notifications
(use-package notifications
  :ensure nil)

(use-package appt
  :ensure nil
  :after org
  :config
  (defun my/appt-display (min-to-app new-time msg)
    (notifications-notify
     :body msg
     :category "appointment"
     :title (format "Appointment in %s minutes!" min-to-app)
     :urgency (if (= 0 (string-to-number min-to-app)) 'normal 'low)
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

;;; Networking
(use-package net-utils
  :ensure nil
  :custom
  (whois-reverse-lookup-server "whois.ripe.net"))

;;; CERN-specific goodies
(use-package cern-ldap
  :custom
  (cern-ldap-server-url
   (cond
    ((and (boundp 'my/in-cern-net-p) my/in-cern-net-p)
     "ldap://xldap.cern.ch:389")
    (t
     "ldap://localhost:1389")))
  (cern-ldap-user-full-name-matching-type 'relaxed)
  (cern-ldap-user-group-membership-filter "CN=cern-status\\|CN=nationality")
  :config
  (add-to-list 'cern-ldap-user-displayed-attributes "mail"))

(defun my/clone-module (module-name)
  "Clone a Puppet module from gitlab.cern.ch/ai"
  (interactive "sModule name: ")
  (my/--clone-puppet-entity
   module-name
   'module))

(defun my/clone-hostgroup (hostgroup-name)
  "Clone a Puppet top-level hostgroup from gitlab.cern.ch/ai"
  (interactive "sTop-level hostgroup name: ")
  (my/--clone-puppet-entity
   hostgroup-name
   'hostgroup))

(defun my/--clone-puppet-entity (entity-name entity-type)
  "Clone it-puppet-ENTITY-TYPE-ENTITY-NAME from gitlab.cern.ch/ai.
The repository will be cloned into
`magit-clone-default-directory' if the repository is not already
cloned."
  (let* ((magit-clone-set-remote.pushDefault t)
         (repo-name (format
                     "it-puppet-%s-%s"
                     entity-type
                     entity-name))
         (service-and-repo-name (concat "cgl:" repo-name))
         (destination (concat magit-clone-default-directory repo-name)))
    (if (file-directory-p destination)
        (user-error "%s already cloned" repo-name)
      (magit-clone-internal
       (magit-clone--name-to-url service-and-repo-name)
       destination
       nil))))

(defun my/os-same-project-as (fqdn)
  "Set the current OpenStack project to the same as FQDN's.
This function is meant to be executed from Eshell in a CWD where
ai-rc is installed. If `tramp-remote-process-environment' has
been successfully updated, then it returns the new project name,
otherwise it returns nil."
  (and-let* ((project-name
              (with-temp-buffer
                (eshell-command (concat "ai-rc --same-project-as " fqdn) t)
                (keep-lines "^export OS_PROJECT_NAME")
                (unless (string-empty-p (buffer-string))
                  (replace-string "\"" "")
                  (car (last (split-string (substring (buffer-string) 7 -2) "=")))))))
    (my/setenv-tramp "OS_PROJECT_NAME" project-name)
    project-name))

;;; Transient
(use-package transient
  :bind (("C-x C-c" . my/cern-dispatch))
  :config
  (transient-define-prefix my/cern-dispatch ()
    "Dispatch a CERN-specific command."
    [["LDAP user (by login)"
      ("U" "Dwim" cern-ldap-user-by-login-dwim)
      ("u" "Ask" cern-ldap-user-by-login)]
     ["LDAP user (by full name)"
      ("F" "Dwim" cern-ldap-user-by-full-name-dwim)
      ("f" "Ask" cern-ldap-user-by-full-name)]
     ["LDAP user (by location)"
      ("L" "Dwim" cern-ldap-user-by-location-dwim)
      ("l" "Ask" cern-ldap-user-by-location)]
     ["LDAP group"
      ("G" "Dwim" cern-ldap-group-dwim)
      ("g" "Ask" cern-ldap-group)]]))

;;; Social
;;;; IRC
(use-package erc
  :ensure nil
  :bind (:map erc-mode-map
              ("C-<up>" . erc-previous-command)
              ("C-<down>" . erc-next-command)
              ("<home>" . consult-buffer))
  :config
  (erc-spelling-mode)
  ;; Needs `my/monkeys--exwm-pr-900' (exwm-input.el) applied.
  (erc-scrolltobottom-mode)
  :custom
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 19)
  (erc-fill-column 138) ;(- (/ (frame-width) 2) 3))
  (erc-hide-list '("PART" "QUIT" "JOIN"))
  (erc-auto-query 'bury)
  (erc-join-buffer 'bury)
  (erc-kill-server-buffer-on-quit t)
  (erc-kill-queries-on-quit t)
  (erc-disable-ctcp-replies t)
  (erc-prompt (lambda nil
                (concat
                 (make-string (+ 7 (- erc-fill-static-center (+ 2 (length (buffer-name))))) ? )
                 (buffer-name)
                 ">")))
  (erc-part-reason (lambda (&optional s) ""))
  (erc-insert-timestamp-function #'erc-insert-timestamp-left)
  (erc-timestamp-only-if-changed-flag nil)
  (erc-user-mode "+iRw")
  (erc-nick "nacho")
  (erc-server "irc.libera.chat")
  (erc-email-userid "nacho")
  (erc-user-full-name "Nacho Barrientos"))

(use-package erc-track
  :ensure nil
  :config
  (dolist (msg '("JOIN" "PART" "QUIT" "MODE" "KICK"))
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
  (erc-autojoin-channels-alist
   '(("libera.chat"
      "#emacs" "#erc" "#archlinux" "#sr.ht" "#almalinux"))))

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

(use-package erc-hl-nicks
  :custom
  (erc-hl-nicks-skip-nicks '("nacho")))

;;;; Mastodon
(use-package mastodon
  :bind (:map mastodon-mode-map
              ("g" . mastodon-tl--update)
              ("M-<up>" . mastodon-tl--goto-prev-toot)
              ("M-<down>" . mastodon-tl--goto-next-toot))
  :custom
  (mastodon-instance-url "https://emacs.ch")
  (mastodon-active-user "nachobarrientos"))

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
          (user-error "This buffer has been killed"))
      (add-to-list 'my/bookmark-buffer-or-switch-to-bookmark--bookmarks-alist
                   (cons last-command-event (current-buffer)))
      (with-current-buffer (current-buffer)
        (my/exwm-toggle-or-set-buffer-protection t))
      (message (format "Added %s as shortcut for buffer <%s>"
                       (key-description (vector last-command-event))
                       (current-buffer))))))

(defun my/postgres-id-headroom (current-max datatype)
  "Calculate how much space for an id of type DATATYPE is still available.
CURRENT-MAX is the value of the id for the row with highest id.

When called interactively, CURRENT-MAX is taken from the region
and the DATATYPE is prompted for."
  (interactive
   (list (string-to-number
          (buffer-substring-no-properties (region-beginning) (region-end)))
         (intern
          (completing-read
           "Data type: "
           '("smallint" "integer" "bigint")
           nil nil nil nil
           "integer"))))
  (let* ((datawidth (cond
                     ((eq datatype 'smallint)
                      2)
                     ((eq datatype 'integer)
                      4)
                     ((eq datatype 'bigint)
                      8)))
         (range-max (- (expt 2 (- (* datawidth 8) 1)) 1))
         (current-max (min range-max current-max))
         (%-available (*
                       100
                       (/
                        (- range-max current-max)
                        (float range-max)))))
    (if (interactive-p)
        (message "%.2f%% is still available" %-available)
      %-available)))

;;; init.el ends here
