(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Set up the visible bell
(setq visible-bell t)

(setq show-trailing-whitespace t)
(add-hook 'shell-mode-hook (lambda ()
                            (setq show-trailing-whitespace nil)))

(global-display-line-numbers-mode t)
(add-hook 'vterm-mode-hook (lambda () (linum-mode 0)))

(load-theme 'wombat)

(define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit)

(defalias 'yes-or-no-p 'y-or-n-p)


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

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
	 ([escape] . minibuffer-keyboard-quit)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
	 ("C-x f" . counsel-find-file)))

(use-package swiper)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package puppet-mode)
(use-package rspec-mode)
(use-package rake)
(use-package yaml-mode)

(use-package highlight-parentheses)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . ace-window)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package vterm)

;; Mail
(server-start)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; Spelling
(setq ispell-dictionary "british")
(dolist (hook '(text-mode-hook mail-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode 1))))

;; Add color support to compilation buffers
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq compilation-scroll-output 'first-error)

(use-package multi-compile)
(global-set-key (kbd (concat "C-x " "m")) 'multi-compile-run)
(setq multi-compile-alist '(
			    (ruby-mode . (("cern-p6-rubocop" "schroot -- bash -c 'BUNDLE_GEMFILE=../ci/Gemfile PUPPET_VERSION=\"~>6\" bundle exec rake --rakefile ../ci/Rakefile rubocop'"
					   (locate-dominating-file buffer-file-name "metadata.json"))
					  ("cern-p6-all-tests" "schroot -- bash -c 'BUNDLE_GEMFILE=../ci/Gemfile PUPPET_VERSION=\"~>6\" bundle exec rake --rakefile ../ci/Rakefile test'"
					   (locate-dominating-file buffer-file-name "metadata.json"))
					  ("cern-p6-bundle-update" "schroot -- bash -c 'BUNDLE_GEMFILE=../ci/Gemfile PUPPET_VERSION=\"~>6\" bundle update'"
					   (locate-dominating-file buffer-file-name "metadata.json"))
					  ;; Standard Puppet module
					  ("p6-rubocop" "schroot -- bash -c 'PUPPET_VERSION=\"~>6\" bundle exec rake rubocop'"
					   (locate-dominating-file buffer-file-name "metadata.json"))
					  ("p6-all-tests" "schroot -- bash -c 'PUPPET_VERSION=\"~>6\" bundle exec rake test'"
					   (locate-dominating-file buffer-file-name "metadata.json"))
					  ("p6-bundle-update" "schroot -- bash -c 'PUPPET_VERSION=\"~>6\" bundle update'"
					   (locate-dominating-file buffer-file-name "metadata.json"))
					  ))
			    ("_spec\\.rb\\'" . (("cern-p6-single-test" "schroot -- bash -c 'BUNDLE_GEMFILE=../ci/Gemfile PUPPET_VERSION=\"~>6\" bundle exec rake --rakefile ../ci/Rakefile spec SPEC=%path'"
						 (locate-dominating-file buffer-file-name "metadata.json"))
						;; Standard Puppet module
						("p6-single-test" "schroot -- bash -c 'PUPPET_VERSION=\"~>6\" bundle exec rake spec SPEC=%path'"
						 (locate-dominating-file buffer-file-name "metadata.json"))
						))
			    ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ivy-rich counsel yaml-mode multi-compile vterm which-key ace-window rake rspec-mode magit highlight-parentheses use-package swiper puppet-mode doom-modeline command-log-mode))
 '(send-mail-function 'mailclient-send-it))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
