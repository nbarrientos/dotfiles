;;; Basic configuration of built-in features
(setq-default
 inhibit-startup-message t
 visible-bell t
 vc-follow-symlinks t
 indent-tabs-mode nil
 auth-source-save-behavior nil
 custom-file null-device
 make-backup-files nil
 help-window-select t
 confirm-kill-emacs 'y-or-n-p
 auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-saves/" t)))

(server-start)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(mouse-avoidance-mode 'jump)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;;;; Fonts
(set-face-attribute 'default nil :font "Hack" :height 110) ; deb: fonts-hack

;;;; Remedies for to-be-reeducated muscle memory
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-f"))

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
  (whitespace-style '(face trailing tabs empty big-indent)))
(add-function :filter-return whitespace-enable-predicate
   (lambda (ret) (and ret (not (derived-mode-p 'magit-mode)))))
;; Emacs28 only, see https://debbugs.gnu.org/db/40/40481.html
;;(setq whitespace-global-modes '(not magit-mode))

;;;; Sexp delimiters highlighting
(use-package paren
  :ensure nil
  :config
  (show-paren-mode)
  :custom
  (show-paren-style 'mixed))

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

;;;; Whiteroom
(use-package writeroom-mode
  :bind
  ("<f11>" . writeroom-mode)
  :custom
  (writeroom-restore-window-config t)
  (writeroom-width 100)
  :hook
  ((writeroom-mode-enable .
                          (lambda ()
                            (when (executable-find "dunstctl")
                              (shell-command "dunstctl set-paused true"))))
   (writeroom-mode-disable .
                           (lambda ()
                             (when (executable-find "dunstctl")
                               (shell-command "dunstctl set-paused false"))))))
;;;; URLs
(require 'url-util)
(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
          (prog-mode . goto-address-prog-mode)
          (magit-mode . goto-address-mode)
          (mu4e-view-mode . goto-address-mode))
  :commands (goto-address-prog-mode
             goto-address-mode))

;;;; Grep
(use-package rg
  :bind
  (("C-x p" . rg)
   :map rg-mode-map
   ("C-<down>" . rg-next-file)
   ("C-<up>" . rg-prev-file))
  :config
  (add-to-list 'rg-custom-type-aliases '("texi" . "*.texi"))
  :custom
  (rg-group-result t)
  (rg-buffer-name "ripgrep"))

;;; Killing, Yanking and Undo
(use-package whole-line-or-region
  :init
  (whole-line-or-region-global-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-relative-timestamps t))

;;; Spelling and grammar
(setq ispell-dictionary "british")
(dolist (hook '(text-mode-hook mu4e-compose-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

;;; TRAMP
;; Option (1-2): is a typical prompt for 2FA tokens at CERN
(add-to-list 'password-word-equivalents "Option")
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :custom
  (tramp-default-method "ssh"))

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
  (dired-auto-revert-buffer t))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ([tab] . dired-subtree-toggle)
              ([C-tab] . dired-subtree-cycle))
  :custom
  (dired-subtree-line-prefix "  |-")
  (dired-subtree-use-backgrounds nil))

;;; Auto completion
;;;; Ivy-Counsel-Swiper
(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers 'recentf)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

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
  :after (helpful)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x b" . ivy-switch-buffer)
         ("C-b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x f" . counsel-find-file)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-h k" . helpful-key)
         ("C-x r b" . counsel-bookmark))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package swiper
  :bind (("C-s" . swiper)))

;;;; Snippets
(use-package yasnippet
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (delete 'try-expand-line hippie-expand-try-functions-list)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet))

;;;; Tabbing
(use-package smart-tab
  :config
  (global-smart-tab-mode 1)
  (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
  :custom
  (smart-tab-using-hippie-expand t))

;;; Look and feel
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30))

(use-package frame
  :ensure nil
  :custom
  (window-divider-default-right-width 10)
  :config
  (window-divider-mode 1))

;;; Modes for coding
;;;; Programming languages
(use-package enh-ruby-mode
  :mode (("\\.rb\\'" . enh-ruby-mode)))
(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-minor-mode)))
(use-package puppet-mode)
(use-package rspec-mode)
(use-package rubocop
  :custom
  (rubocop-run-in-chroot t))
;; Pending https://github.com/rubocop/rubocop-emacs/pull/36
(defun rubocop-ensure-installed ()
  "Check if RuboCop is installed."
  (unless (or (executable-find "rubocop") (rubocop-bundled-p) rubocop-run-in-chroot)
    (error "RuboCop is not installed")))

;;;; Markup, scripting and conf
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
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'safe-local-variable-values '(web-mode-enable-auto-indentation . nil)))

(use-package csv-mode)
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
                   (define-key eshell-mode-map (kbd "<up>") 'previous-line)
                   (define-key eshell-mode-map (kbd "<down>") 'next-line)
                   (define-key eshell-mode-map (kbd "M-<up>") 'eshell-previous-prompt)
                   (define-key eshell-mode-map (kbd "M-<down>") 'eshell-next-prompt)
                   (define-key eshell-mode-map (kbd "C-c C-o") 'my/eshell-kill-output)
                   (define-key eshell-mode-map (kbd "C-c o") 'my/eshell-export-output)
                   (define-key eshell-mode-map (kbd "C-c r") 'counsel-esh-history)))
  :config
  ;; I'm puzzled about the fact that a function that has the word kill
  ;; in its name does not actually kill the text in question. The original
  ;; one does 'delete-region' instead. This cannot be a bug. Is it?
  (defun my/eshell-kill-output ()
    "Kill all output from interpreter since last input.
    Does not delete the prompt."
    (interactive)
    (save-excursion
      (goto-char (eshell-beginning-of-output))
      (insert "*** output flushed ***\n")
      (kill-region (point) (eshell-end-of-output))))

  ;; Inspiration from:
  ;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-eshell.el
  (defun my/eshell-export-output ()
    "Produce a buffer with output of the last Eshell command."
    (interactive)
    (let ((eshell-output (buffer-substring-no-properties
                          (eshell-beginning-of-output)
                          (eshell-end-of-output))))
      (with-current-buffer (get-buffer-create "*Exported Eshell output*")
        (erase-buffer)
        (insert eshell-output)
        (switch-to-buffer-other-window (current-buffer)))))
  :custom
  (eshell-banner-message "")
  (eshell-history-size 20000)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all))

(use-package eshell-prompt-extras
  :after (eshell)
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-dakrone "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-dakrone))
  (defun epe-git-p () nil) ; no git info on the prompt
  :custom
  (epe-show-python-info nil)
  :custom-face
  (epe-symbol-face ((t (:inherit eshell-ls-missing)))))

;;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch))
  :config
  (add-to-list 'magit-clone-name-alist '("\\(it-puppet-.+\\)" ":@gitlab.cern.ch:8443" "ai"))
  (transient-append-suffix 'magit-push "-n"
    '(1 "-M" "Create MR in Gitlab" "--push-option=merge_request.create"))
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-clone-default-directory "~/dev/")
  (magit-clone-url-format "https://%h/%n.git")
  (magit-clone-set-remote.pushDefault t))

;;; Movement and window switching
(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("<f8>" . ace-window)
         ("C-x 1" . my/toggle-single-window))
  :config
  (defun my/toggle-single-window ()
  "Toggles between a single window configuration and
the previously multi-windowed one"
  (interactive)
  (if (one-window-p)
    (when (boundp 'my-saved-window-configuration)
      (set-window-configuration my-saved-window-configuration))
    (progn
      (setq my-saved-window-configuration (current-window-configuration))
      (delete-other-windows))))
  :custom
  (aw-scope 'frame))

;;; Help
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 5))

(use-package helpful)

;;; Mu4e
(use-package mu4e
  :ensure nil
  :custom
  (mu4e-change-filenames-when-moving t)
  (mu4e-confirm-quit t)
  (mu4e-update-interval (* 5 60))
  (mu4e-get-mail-command "true") ; mbsync is run by a systemd timer (only re-index)
  ;; (mu4e-html2text-command "lynx -dump -stdin")
  (mu4e-view-use-gnus t)
  (mu4e-compose-signature " bye\n Nacho\n http://cern.ch/nacho")
  (mu4e-view-show-addresses t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-hide-index-messages t)
  (mu4e-headers-leave-behavior 'apply)
  (mu4e-split-view nil)
  (mu4e-headers-date-format "%d/%m/%y")
  (mu4e-maildir "~/Mail")
  (mu4e-drafts-folder "/cern/Drafts")
  (mu4e-sent-folder   "/cern/Sent Items")
  (mu4e-trash-folder  "/cern/Trash")
  (mu4e-headers-fields
   '(( :human-date    .  12)
     ( :flags         .  10)
     ( :from          .  22)
     ( :subject       .  nil)))
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
  (setq smtpmail-smtp-server "smtp.cern.ch")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-smtp-user "ibarrien"))

(use-package mu4e-alert
  :custom
  (mu4e-alert-interesting-mail-query "flag:unread AND maildir:\"/cern/INBOX\"")
  :config
  (mu4e-alert-enable-mode-line-display))

(use-package mu4e-marker-icons
  :custom
  (mu4e-marker-icons-mode 1))

(mu4e t)

;;; Elfeed
(use-package elfeed)

;;; Web browsing
(use-package eww
  :ensure nil
  :config
  (setq browse-url-browser-function
        '(("youtube\\.com" . browse-url-firefox)
          ("github\\.com" . browse-url-firefox)
          ("gitlab\\.cern\\.ch" . browse-url-firefox)
          ("twitter\\.com" . browse-url-firefox)
          ("docs\\.google\\.com" . browse-url-firefox)
          ("google\\.com/maps" . browse-url-firefox)
          ("openstreetmap\\.org" . browse-url-firefox)
          ("." . eww-browse-url))))

(use-package engine-mode
  :config
  (defengine duckduckgo
    "https://duckduckgo.com/html?q=%s"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")
  (defengine google
    "http://www.google.com/search?hl=en&ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine google-maps
    "https://www.google.com/maps/search/%s/")
  (defengine openstreetmap
    "https://www.openstreetmap.org/search?query=%s"
    :keybinding "m")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
  (engine/set-keymap-prefix (kbd "C-c s"))
  (engine-mode t))

;;; Window manager
(use-package exwm
  :config
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (start-process-shell-command "xmodmap" nil "xmodmap ~/.Xmodmap")

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (defun my/switch-to-buffer-if-exists-back-and-forth (to-buffer-name)
    "Switches to to-buffer-name if it exists. If the current buffer is
to-buffer-name then it switches back to the previous buffer."
    (when (get-buffer to-buffer-name)
      (if (string-equal to-buffer-name (buffer-name))
          (switch-to-prev-buffer)
        (switch-to-buffer to-buffer-name))))

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
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" (car i))) .
                        (lambda ()
                          (interactive)
                          (my/switch-to-buffer-if-exists-back-and-forth ,(cdr i)))))
                    '((1 . "firefox") (2 . "TelegramDesktop") (3 . "Signal") (5 . "*eww*") (6 . "*eshell*")))
          ([?\s-7]
           . mu4e-headers-search-bookmark)
          ([?\s-8]
           . mu4e)
          ([?\s-s] .
           (lambda ()
             (interactive)
             (shell-command "import png:- | xclip -selection c -t image/png &>/dev/null")))))

  (setq exwm-input-simulation-keys
        '(
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))

  (add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "firefox"))
              (exwm-input-set-local-simulation-keys
               '(([?\C-s] . [?\C-f])))))) ; Swiper!

  (define-key exwm-mode-map (kbd "C-c") nil)
  ;; Buffer switching
  (add-to-list 'exwm-input-prefix-keys ?\C-b)
  ;; Window switching
  (define-key exwm-mode-map (kbd "<f8>") 'ace-window)

  (setq display-time-default-load-average nil)
  (setq display-time-format "%d/%b %H:%M")
  (display-time-mode)

  (add-hook 'exwm-init-hook
            (lambda ()
              (progn
                (split-window-right)
                (eshell))))

  (exwm-enable))

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
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'first-error)

(setq my/mc-env-pv "PUPPET_VERSION=\"~>6\" ")
(setq my/mc-root '(locate-dominating-file buffer-file-name "metadata.json"))
(setq my/mc-bundle-cmd "%s schroot -p -- bundle %%s")
;; CERN environment
(setq my/mc-c-env (concat "BUNDLE_GEMFILE=../ci/Gemfile " my/mc-env-pv))
(setq my/mc-c-bundle (format my/mc-bundle-cmd my/mc-c-env))
(setq my/mc-c-rake (format my/mc-c-bundle "exec rake --rakefile ../ci/Rakefile %s"))
;; Standard Puppet module
(setq my/mc-env my/mc-env-pv)
(setq my/mc-bundle (format my/mc-bundle-cmd my/mc-env))
(setq my/mc-rake (format my/mc-bundle "exec rake %s"))

(use-package multi-compile
  :bind (("C-x b" . recompile)
         ("C-x B" . multi-compile-run))
  :custom
  (multi-compile-alist
   `(
     ;; Rubocop tasks for all Ruby (SPEC) files.
     (enh-ruby-mode .
      (("cern-p-rubocop" ,(format my/mc-c-bundle "exec rubocop --format emacs") ,my/mc-root)
       ("cern-p-rubocop-autocorrect" ,(format my/mc-c-bundle "exec rubocop -a --format emacs") ,my/mc-root)))
     ;; All tests and bundle update for all Puppet and Ruby (SPEC) files.
     ((or (eq 'enh-ruby-mode major-mode) (eq 'puppet-mode major-mode)) .
      (("cern-p-all-tests" ,(format my/mc-c-rake "test") ,my/mc-root)
       ("cern-p-bundle-update" ,(format my/mc-c-bundle "update") ,my/mc-root)
       ("p-all-tests" ,(format my/mc-rake "test") ,my/mc-root)
       ("p-bundle-update" ,(format my/mc-bundle "update") ,my/mc-root)))
     ;; Single test runs when it's a SPEC file
     ("_spec\\.rb\\'" .
      (("cern-p-single-test" ,(format my/mc-c-rake "spec SPEC=%path") ,my/mc-root)
       ("p-single-test" ,(format my/mc-rake "spec SPEC=%path") ,my/mc-root))))))

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
       :description exwm-title)))

  (org-link-set-parameters "firefox" :store 'my/exwm-org-store-link)
  :custom
  (org-default-notes-file (concat org-directory "/notes.org")))

(use-package org-agenda
  :ensure nil
  :hook
  (org-agenda-finalize . org-agenda-to-appt)
  :config
  (setq calendar-week-start-day 1)
  :custom
  (org-agenda-files '("~/org")))

;;; Notifications
(use-package appt
  :ensure nil
  :config
  (defun my/appt-display (min-to-app new-time msg)
    (notifications-notify
     :body msg
     :category "appointment"
     :title (format "Appointment in %s minutes!" min-to-app)
     :urgency 'critical
     :actions '("org-agenda" "Open org-agenda")
     ;; Dunst: middle-click to trigger the action
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

(defun my/gimme-url (filename)
  "Copy a file to the bucket a put the URL in the kill ring"
  (interactive "fFile Path:")
  (let* ((hash
          (with-temp-buffer
           (insert-file-contents filename)
           (sha1 (buffer-string))))
         (new-filename
          (concat hash (url-file-extension filename))))
    (progn
      (copy-file filename (concat "~/afs/www/bucket/" new-filename) t)
      (kill-new (concat "https://cern.ch/nacho/bucket/" new-filename)))))

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