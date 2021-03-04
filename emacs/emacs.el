(setq-default
 inhibit-startup-message t
 visible-bell t
 vc-follow-symlinks t
 indent-tabs-mode nil
 auth-source-save-behavior nil
 custom-file null-device
 make-backup-files nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(server-start)

(set-face-attribute 'default nil :font "Hack" :height 110) ; deb: fonts-hack

(setq show-trailing-whitespace t)
(add-hook 'shell-mode-hook (lambda ()
                            (setq show-trailing-whitespace nil)))

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'conf-mode-hook 'linum-mode)

(define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

(global-whitespace-mode)
(add-function :filter-return whitespace-enable-predicate
   (lambda (ret) (and ret (not (derived-mode-p 'magit-mode)))))
;; Emacs28 only, see https://debbugs.gnu.org/db/40/40481.html
;;(setq whitespace-global-modes '(not magit-mode))
(setq whitespace-style '(face trailing tabs empty big-indent))

;; Option (1-2): is a typical prompt for 2FA tokens at CERN
(add-to-list 'password-word-equivalents "Option")

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

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

(use-package whole-line-or-region
  :init
  (whole-line-or-region-global-mode))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-NGalhv --group-directories-first"))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ([escape] . minibuffer-keyboard-quit)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers 'recentf)
  (ivy-virtual-abbreviate 'abbreviate))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (ivy-rich-mode 1)
  (ivy-rich-project-root-cache-mode)
  (ivy-rich-modify-columns
   'ivy-switch-buffer
   '((ivy-rich-switch-buffer-project (:width 30))
     (ivy-rich-switch-buffer-major-mode (:width 20)))))

(use-package smex) ; adds last used cmds to counsel-M-x

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x b" . ivy-switch-buffer)
         ("C-b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x f" . counsel-find-file)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x r b" . counsel-bookmark)))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-material t)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package puppet-mode)
(use-package rspec-mode)
(use-package rake)
(use-package yaml-mode)
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
(use-package rpm-spec-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.spec" . rpm-spec-mode)))
(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

(use-package gitignore-templates)

(use-package highlight-parentheses)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-relative-timestamps t))

(use-package eshell-bookmark
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)
         ("C-x G" . magit-dispatch))
  :config
  (add-to-list 'magit-clone-name-alist '("\\(it-puppet-.+\\)" ":@gitlab.cern.ch:8443" "ai"))
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-clone-default-directory "~/dev/")
  (magit-clone-url-format "https://%h/%n.git")
  (magit-clone-set-remote.pushDefault t))

(defun my/clone-module (module-name)
  "Clone a Puppet module from gitlab.cern.ch/ai"
  (interactive "sModule name: ")
  (magit-clone-internal
   ;; Using an internal here, see  https://github.com/magit/magit/discussions/4335
   (magit-clone--name-to-url (concat "it-puppet-module-" module-name))
   magit-clone-default-directory
   nil))

(defun my/clone-hostgroup (hostgroup-name)
  "Clone a Puppet top-level hostgroup from gitlab.cern.ch/ai"
  (interactive "sTop-level hostgroup name: ")
  (magit-clone-internal
   (magit-clone--name-to-url (concat "it-puppet-hostgroup-" hostgroup-name))
   magit-clone-default-directory
   nil))

(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . ace-window)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 5))

;; Highlight URLs and kill them instead of opening them
(require 'url-util)
(defun my/kill-url-at-point ()
  "Kill the url at point."
  (interactive)
  (kill-new (url-get-url-at-point)))
(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
          (prog-mode . goto-address-prog-mode)
          (magit-mode . goto-address-mode)
          (mu4e-view-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("C-c RET" . my/kill-url-at-point)
              ("<mouse-2>" . my/kill-url-at-point))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package mu4e
  :ensure nil
  :bind ("<f8>" . mu4e)
  :custom
  (mu4e-change-filenames-when-moving t)
  (mu4e-confirm-quit nil)
  (mu4e-update-interval (* 5 60))
  (mu4e-get-mail-command "true") ; mbsync is run by a systemd timer (only re-index)
  (mu4e-html2text-command "lynx -dump -stdin")
  (mu4e-compose-signature " bye\n Nacho\n http://cern.ch/nacho")
  (mu4e-view-show-addresses t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-hide-index-messages nil)
  (mu4e-headers-date-format "%d/%m/%y")
  (mu4e-maildir "~/Mail")
  (mu4e-drafts-folder "/cern/Drafts")
  (mu4e-sent-folder   "/cern/Sent Items")
  (mu4e-trash-folder  "/cern/Trash")
  (mu4e-bookmarks
   '(( :name "Unread messages in INBOX"
       :query "flag:unread and maildir:\"/cern/INBOX\""
       :key ?i)
     ( :name  "All unread messages"
       :query "flag:unread and not flag:trashed"
       :key ?u)
     ( :name "Today's messages"
       :query "date:today..now"
       :key ?t)
     ( :name "Last 7 days"
       :query "date:7d..now"
       :key ?w)))
  :config
  (add-to-list 'mu4e-view-fields :user-agent t)
  (setq user-mail-address "nacho.barrientos@cern.ch")
  (setq user-full-name "Nacho Barrientos")
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq send-mail-function 'smtpmail-send-it)
  (setq smtpmail-smtp-server "smtp.cern.ch")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-smtp-user "ibarrien"))

(use-package mu4e-alert
  :custom
  (mu4e-alert-interesting-mail-query "flag:unread AND maildir:\"/cern/INBOX\"")
  :config
  (mu4e-alert-enable-mode-line-display))

(mu4e t)

;; Spelling
(setq ispell-dictionary "british")
(dolist (hook '(text-mode-hook mu4e-compose-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

;; Add color support to compilation buffers
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'first-error)

(setq my/mc-env-pv "PUPPET_VERSION=\"~>6\"")
(setq my/mc-root '(locate-dominating-file buffer-file-name "metadata.json"))
;; CERN environment
(setq my/mc-c-env (concat "BUNDLE_GEMFILE=../ci/Gemfile " my/mc-env-pv))
(setq my/mc-c-bundle (format "schroot -- bash -c '%s bundle %%s'" my/mc-c-env))
(setq my/mc-c-rake (format my/mc-c-bundle "exec rake --rakefile ../ci/Rakefile %s"))
;; Standard Puppet module
(setq my/mc-env my/mc-env-pv)
(setq my/mc-bundle (format "schroot -- bash -c '%s bundle %%s'" my/mc-env))
(setq my/mc-rake (format my/mc-bundle "exec rake %s"))

(use-package multi-compile
  :bind (("C-x m" . multi-compile-run))
  :custom
  (multi-compile-alist
   `((ruby-mode . (("cern-p-rubocop" ,(format my/mc-c-rake "rubocop") ,my/mc-root)
                   ("cern-p-rubocop-autocorrect" ,(format my/mc-c-rake "rubocop -a") ,my/mc-root)
                   ("cern-p-all-tests" ,(format my/mc-c-rake "test") ,my/mc-root)
                   ("cern-p-bundle-update" ,(format my/mc-c-bundle "update") ,my/mc-root)
                   ;; Standard Puppet module
                   ("p-rubocop" ,(format my/mc-rake "rubocop") ,my/mc-root)
                   ("p-rubocop-autocorrect" ,(format my/mc-rake "rubocop -a") ,my/mc-root)
                   ("p-all-tests" ,(format my/mc-rake "test") ,my/mc-root)
                   ("p-bundle-update" ,(format my/mc-bundle "update") ,my/mc-root)
                   ))
     ("_spec\\.rb\\'" . (("cern-p-single-test" ,(format my/mc-c-rake "spec SPEC=%path") ,my/mc-root)
                         ;; Standard Puppet module
                         ("p-single-test" ,(format my/mc-rake "spec SPEC=%path") ,my/mc-root)
                         ))
     )))
