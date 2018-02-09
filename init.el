;;; package --- Summary
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       BASE                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
		    ("elpy" . "https://jorgenschaefer.github.io/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; UTF-8 settings
(set-language-environment               "UTF-8")
(set-charset-priority			'unicode)
(setq locale-coding-system		'utf-8)
(set-terminal-coding-system		'utf-8)
(set-keyboard-coding-system		'utf-8)
(set-selection-coding-system		'utf-8)
(prefer-coding-system			'utf-8)
(setq default-process-coding-system	'(utf-8-unix . utf-8-unix))

;; (show-paren-mode 1)
;; (setq show-paren-delay 0)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq select-enable-primary t)

;; Line numbers in programming mode
(add-hook 'prog-mode-hook 'linum-mode)

;; Electric pair mode in programming mode
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Paredit for lisp/schemes
(use-package paredit
  :ensure t
  :config
    (add-hook 'clojure-mode-hook #'paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
    (add-hook 'racket-mode-hook #'paredit-mode)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    ;; Disable electric-pair when using paredit
    (add-hook 'paredit-mode-hook (lambda ()
				   (electric-pair-mode -1))))

;; Move lines in visual mode up/down with shift-j/k
;; Selected words left and right with shift-h/l
(use-package drag-stuff
  :ensure t)

;; For Debugging
(use-package esup
  :ensure t)

;; For trying packages
(use-package try
  :ensure t)

(use-package multi-term
  :ensure t)

(global-set-key (kbd "C-x i") 'indent-for-tab-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       AESTHETICS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Theme
;; (use-package dracula-theme
;;   :ensure t)

(use-package nord-theme
  :ensure t
  :init
  (defvar nord-comment-brightness 15))


(if (display-graphic-p)
    (enable-theme 'nord)
  (load-theme 'tsdh-dark))

(set-frame-font "Fira Code Retina 18")
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
;;(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

(use-package powerline
  :ensure t
  :init
  (powerline-default-theme))

(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      FUNCTIONALITY                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (defvar company-dabbrev-downcase nil)
  (setq company-idle-delay 0
	company-minimum-prefix-length 1
	company-show-numbers t
	company-tooltip-align-annotations t
	company-tooltip-limit 10)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map [tab] #'company-complete-selection))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "What is thy bidding, my Master?")
  (setq dashboard-items
	'((agenda . 5)
	  (recents . 5)
	  (projects . 5)))
  (dashboard-setup-startup-hook))

(use-package ediff
  :ensure t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (define-key help-map "\C-h" 'which-key-C-h-dispatch))

(use-package define-word
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "KIEX_HOME")
  (exec-path-from-shell-copy-env "MIX_ARCHIVES")
  (exec-path-from-shell-copy-env "kotlinc")
  (exec-path-from-shell-copy-env "RUST_SRC_PATH"))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :bind
  ("C-s"	.	swiper)
  ("C-x C-r"	.	ivy-resume)
  ("C-l"	.	ivy-alt-done)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1))

;; Helm - Yikes!
;; (use-package helm
;;   :ensure t
;;   :diminish helm-mode
;;   :init
;;   (setq helm-autoresize-max-height 30
;;     helm-display-header-line nil
;;     helm-always-two-windows t
;;     helm-split-window-inside-p t
;;     helm-move-to-line-cycle-in-source t
;;     helm-ff-search-library-in-sexp t
;;     helm-ff-file-name-history-use-recentf t
;;     helm-comp-read-mode-line ""
;;     helm-read-file-name-mode-line-string ""
;;     helm-mode-line-string "")
;;   ;; enable fuzzy matching
;;   (setq helm-buffers-fuzzy-matching t
;;     helm-completion-in-region-fuzzy-match t
;;     helm-M-x-fuzzy-match t
;;     helm-apropos-fuzzy-match t
;;     helm-imenu-fuzzy-match t
;;     helm-lisp-fuzzy-completion t
;;     helm-locate-fuzzy-match t
;;     helm-mode-fuzzy-match t
;;     helm-recentf-fuzzy-match t
;;     helm-semantic-fuzzy-match t)
;;   :config
;;   (require 'helm-config)
;;   (helm-mode 1)
;;   (helm-autoresize-mode 1))

;; ;; Fuzzy Matcher for Helm
;; (use-package helm-flx
;;   :ensure t
;;   :after helm
;;   :config
;; (helm-flx-mode +1))

;; Ace Window
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :config
  ;; to use when 2 or less windows
  ;; (setq aw-dispatch always t)
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))


;; Project management
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  :init
  (projectile-global-mode))

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode))

(use-package ag
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (global-set-key [f1] 'neotree-toggle)
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow 'nerd)))
(use-package all-the-icons
  :ensure t)

;; Version Control
(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-x g" . magit-status))

(use-package git-gutter-fringe
  :ensure t)

;; (use-package magithub
  ;; :after magit
  ;; :ensure t
  ;; :config (magithub-feature-autoinject t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          EVIL                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun alex/open-init ()
  "Open my init file."
  (interactive)
  (find-file user-init-file))

(defun alex/insert-line-above ()
  "Insert a line above current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))

(defun alex/insert-line-below ()
  "Insert a line below current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-global-set-key 'normal ",s" 'save-buffer)
  (evil-global-set-key 'normal ",q" 'save-buffers-kill-terminal)
  (evil-global-set-key 'normal ",n" 'evil-ex-nohighlight)
  (evil-global-set-key 'normal ",e" 'dired)
  (evil-global-set-key 'normal ",t" 'multi-term-dedicated-toggle)
  (evil-global-set-key 'normal ",f" 'counsel-find-file)
  (evil-global-set-key 'normal ",b" 'switch-to-buffer)
  (evil-global-set-key 'normal "]q" 'flycheck-next-error)
  (evil-global-set-key 'normal "[q" 'flycheck-previous-error)
  (evil-global-set-key 'normal (kbd "[ <SPC>") 'alex/insert-line-above)
  (evil-global-set-key 'normal (kbd "] <SPC>") 'alex/insert-line-below)
  (evil-global-set-key 'visual (kbd "H") 'drag-stuff-left)
  (evil-global-set-key 'visual (kbd "J") 'drag-stuff-down)
  (evil-global-set-key 'visual (kbd "K") 'drag-stuff-up)
  (evil-global-set-key 'visual (kbd "L") 'drag-stuff-right)
  (evil-global-set-key 'normal ",vr" 'alex/open-init))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  :init
  (evil-escape-mode t))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-magit
  :ensure t)

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

;;Specific conflict with Neotree & Evil
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       LANGUAGES                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Elixir

(defun elixir/fancify-symbols (mode)
  (font-lock-add-keywords mode
			  '(("\\(|>\\)[\[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "⦊"))))
			    ("\\(++\\)[\[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "⧺"))))
			    ("\\(=>\\)[\[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "⇒"))))
			    ("\\(<>\\)[\[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "⃟"))))
			    ("\\(||\\)[\:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "∥")))))))

(use-package elixir-mode
  :ensure t
  :config
  (add-to-list 'elixir-mode-hook 'alchemist-mode)
  ;; (elixir/fancify-symbols 'elixir-mode)
  )

(use-package s
  :ensure t) ;; required by alchemist
(use-package alchemist
  :ensure t
  :commands alchemist-mode
  :config
  (setq alchemist-iex-program-name "/home/asqrd/.kiex/elixirs/elixir-1.6.1/bin/iex")
  (setq alchemist-execute-command "/home/asqrd/.kiex/elixirs/elixir-1.6.1/bin/elixir")
  (setq alchemist-compile-command "/home/asqrd/.kiex/elixirs/elixir-1.6.1/bin/elixirc")
  (setq alchemist-mix-command "/home/asqrd/.kiex/elixirs/elixir-1.6.1/bin/mix")
  ;; Bind some Alchemist commands to more commonly used keys.
  (bind-keys :map alchemist-mode-map
             ("C-c C-l" . (lambda () (interactive)
                            (save-buffer)
                            (alchemist-iex-compile-this-buffer))))
  (bind-keys :map alchemist-mode-map
             ("C-x C-e" . alchemist-iex-send-current-line)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Ruby

(defvar ruby-indent-level 2)

(use-package inf-ruby
  :ensure t)

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

(use-package projectile-rails
  :ensure t
  :config
  (projectile-rails-global-mode))

(use-package rspec-mode
  :ensure t
  :config
  '(rspec-install-snippets))

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  "Select correct ruby version."
  (rvm-activate-corresponding-ruby))

(eval-after-load 'company
  '(push 'company-robe company-backends))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Python
(use-package elpy
  :ensure t
  :config
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (elpy-enable))

(use-package company-jedi
  :ensure t)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-jedi))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Java

(use-package meghanada
  :ensure t
  :config
  (add-hook 'java-mode-hook
	    (lambda ()
	      (meghanada-mode t)
	      (defvar c-basic-offset 2)
	      (highlight-symbol-mode t)
	      (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))))

;; (use-package java-snippets
;;   :ensure t)

(use-package groovy-mode
  :ensure t)
(use-package gradle-mode
  :ensure t)
(use-package kotlin-mode
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       CPP
(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

;; (use-package rtags
;;   :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Rust
(use-package rust-mode
  :ensure t)

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Clojure

;; This was shamelessly stolen from spacemacs- Thanks :)
(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords mode
			  `(("(\\(fn\\)[\[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "λ"))))
			    ("(\\(partial\\)[\[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "Ƥ"))))
			    ("(\\(comp\\)[\[[:space:]]"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "∘"))))
			    ("\\(#\\)("
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "ƒ"))))
			    ("\\(#\\){"
			     (0 (progn (compose-region (match-beginning 1)
						       (match-end 1) "∈")))))))

;; Lein
(add-to-list 'exec-path "/home/asqrd/.sdkman/candidates/leiningen/current/bin/lein")

(use-package clojure-mode
  :ensure t)

;; (use-package clj-refactor
;;   :ensure t
;;   :config
;;   (cljr-add-keybindings-with-prefix "C-c C-m")
;;   (add-hook 'clojure-mode-hook #'clj-refactor-mode)
;;   (add-hook 'clojure-mode-hook #'yas-minor-mode))

(use-package cider
  :ensure t
  :config
  (define-key cider-mode-map (kbd "C-c M-l") 'cider-inspect-last-result)
  (define-key cider-mode-map (kbd "C-c M-v") 'cider-find-var)
  (define-key cider-mode-map (kbd "C-c M-f") 'cider-find-var)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (clojure/fancify-symbols 'cider-repl-mode)
  (clojure/fancify-symbols 'clojure-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Web
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq emmet-move-cursor-between-quotes t)
  (defvar emmet-self-closing-tag-styles " /")
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line))

(use-package restclient
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       DO NOT TOUCH!                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (drag-stuff sudoku typit typing ag emmet-mode multi-term try irony-eldoc irony kotlin-mode gradle-mode groovy-mode meghanada tabbar evil-tabs powerline evil-commentary evil counsel-projectile projectile ace-window ivy expand-region exec-path-from-shell dashboard flycheck company dracula-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
