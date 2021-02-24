(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq visible-bell t)

(setq vc-follow-symlinks t)

(setq show-trailing-whitespace t)
(add-hook 'shell-mode-hook (lambda ()
                            (setq show-trailing-whitespace nil)))

(global-display-line-numbers-mode t)
(add-hook 'vterm-mode-hook (lambda () (linum-mode 0)))

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

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
	 ([escape] . minibuffer-keyboard-quit)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-b" . counsel-switch-buffer)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
	 ("C-x f" . counsel-find-file)))

(use-package swiper)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-material t)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package puppet-mode)
(use-package rspec-mode)
(use-package rake)
(use-package yaml-mode)
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package highlight-parentheses)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  :custom
  (magit-save-repository-buffers 'dontask))

(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . ace-window)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package vterm)

;; Highlight URLs and kill them instead of opening them
(require 'url-util)
(defun my/kill-url-at-point ()
  "Kill the url at point."
  (interactive)
  (kill-new (url-get-url-at-point)))
(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
          (prog-mode . goto-address-prog-mode)
          (magit-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("C-c RET" . my/kill-url-at-point)
	      ("<mouse-2>" . my/kill-url-at-point))
  :commands (goto-address-prog-mode
             goto-address-mode))

;; Mail
(server-start)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook #'auto-fill-mode)

;; Spelling
(setq ispell-dictionary "british")
(dolist (hook '(text-mode-hook mail-mode-hook))
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

(setq nb-mc-env-pv "PUPPET_VERSION=\"~>6\"")
(setq nb-mc-root '(locate-dominating-file buffer-file-name "metadata.json"))
;; CERN environment
(setq nb-mc-c-env (concat "BUNDLE_GEMFILE=../ci/Gemfile " nb-mc-env-pv))
(setq nb-mc-c-bundle (format "schroot -- bash -c '%s bundle %%s'" nb-mc-c-env))
(setq nb-mc-c-rake (format nb-mc-c-bundle "exec rake --rakefile ../ci/Rakefile %s"))
;; Standard Puppet module
(setq nb-mc-env nb-mc-env-pv)
(setq nb-mc-bundle (format "schroot -- bash -c '%s bundle %%s'" nb-mc-env))
(setq nb-mc-rake (format nb-mc-bundle "exec rake %s"))

(use-package multi-compile
  :bind (("C-x m" . multi-compile-run)))
(setq multi-compile-alist
      `((ruby-mode . (("cern-p-rubocop" ,(format nb-mc-c-rake "rubocop") ,nb-mc-root)
		      ("cern-p-all-tests" ,(format nb-mc-c-rake "test") ,nb-mc-root)
		      ("cern-p-bundle-update" ,(format nb-mc-c-bundle "update") ,nb-mc-root)
		      ;; Standard Puppet module
		      ("p-rubocop" ,(format nb-mc-rake "rubocop") ,nb-mc-root)
		      ("p-all-tests" ,(format nb-mc-rake "test") ,nb-mc-root)
		      ("p-bundle-update" ,(format nb-mc-bundle "update") ,nb-mc-root)
		      ))
	("_spec\\.rb\\'" . (("cern-p-single-test" ,(format nb-mc-c-rake "spec SPEC=%path") ,nb-mc-root)
			    ;; Standard Puppet module
			    ("p-single-test" ,(format nb-mc-rake "spec SPEC=%path") ,nb-mc-root)
			    ))
	))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(goto-address thing-edit url-util markdown-mode doom-themes ivy-rich counsel yaml-mode multi-compile vterm which-key ace-window rake rspec-mode magit highlight-parentheses use-package swiper puppet-mode doom-modeline command-log-mode))
 '(send-mail-function 'mailclient-send-it))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
