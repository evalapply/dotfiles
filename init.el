;; package --- Summary
;;; Commentary:
;;; Code:
;; Custom Functions
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

;;(global-set-key (kbd "C-j") 'vi-open-line-above)
;;(global-set-key (kbd "C-o") 'vi-open-line-below)

;; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)
;; Start

(setq inhibit-startup-screen t)
;;Bootstrap use-package
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
		    ("elpy" . "https://jorgenschaefer.github.io/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))
;; Backup saves directory
(setq backup-directory-alist `(("." . "~/.saves")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; Path From Shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;;To try packages without installing
(use-package try
  :ensure t)

;; To show key combinations
(use-package which-key
  :ensure t
  :config (which-key-mode))

;;Org-mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
			     (org-bullets-mode 1)
			     (org-babel-do-load-languages
			      'org-babel-load-languages
			      '((python . t)))
			     )))
;; Babel

;; Markdown
(eval-after-load "org"
  '(require 'ox-md nil t))

;;Ace Window Management
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    ))

;; Swiper/Ivy search
;; And Counsel -- dependency
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  )

(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    ))
;;Avy Goto Char
(use-package avy
  :ensure t
  :bind (("M-g c" . avy-goto-char)
	 ("M-g w" . avy-goto-word-1))
  )

;; YASnippet
(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode)

  :init
  (progn
    (add-hook 'prog-mode-hook #'yas-minor-mode))
  :config
  (progn
    (yas-reload-all)))

;;Company-mode auto-complete
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 2
	company-show-numbers t
	company-tooltip-limit 10
	company-dabbrev-downcase nil
	;;company-backends '((company-elixir company-java company-go))
	)
  )
;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Web
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))
(unbind-key "C-j" emmet-mode-keymap)
(define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)
(with-eval-after-load 'web-mode
  (add-hook 'javascript-mode 'web-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

;; Css
(setq css-indent-offset 2)

;; Templates
(use-package web-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.moustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.[s]css\\'" .web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;;(setq web-mode-enable-auto-pairing t)

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))


(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-engines-alist
      '(("django"  . "\\.jinja\\'")
        ("django"  . "\\.djhtml\\'")
        ("django"  . "\\.html\\'")
        ("erb"     . "\\.erb\\'")
	("thymeleaf" . "'\\.html\\'")
	("blade" . "\\.blade\\.")))
;;php
(use-package php-mode
  :ensure t
  :defer t)

(use-package company-php
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-php)))

;; Javascript
(setq js-indent-level 2)

;; Java
(use-package meghanada
  :ensure t)
(add-hook 'java-mode-hook
          (lambda ()
            (meghanada-mode t)
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

(use-package java-snippets
  :ensure t)

(use-package groovy-mode
  :ensure t)
(use-package gradle-mode
  :ensure t)
(use-package kotlin-mode
  :ensure t)

;; Ruby / Rails
(use-package inf-ruby
  :ensure t)

;;(use-package robe
;;  :ensure t
;;  :config
;;  (add-hook 'ruby-mode-hook 'robe-mode))

(add-hook 'ruby-mode-hook 'electric-pair-mode)

;;(eval-after-load 'company
;;  '(push 'company-robe company-backends))

;; Set up the basic Elixir mode.
(use-package elixir-mode
  :ensure t
  :commands elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

;; Alchemist offers integration with the Mix tool.
(use-package alchemist
  :ensure t
  :commands alchemist-mode
  :config
  ;; Bind some Alchemist commands to more commonly used keys.
  (bind-keys :map alchemist-mode-map
             ("C-c C-l" . (lambda () (interactive)
                            (save-buffer)
                            (alchemist-iex-compile-this-buffer))))
  (bind-keys :map alchemist-mode-map
             ("C-x C-e" . alchemist-iex-send-current-line)))

;;Golang
(setenv "GOPATH" "/home/asqrd/Work/code/go")
(add-to-list 'exec-path "/home/asqrd/Work/code/go/bin")

(use-package company-go
  :ensure t)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))

(use-package go-mode
  :ensure t
  :init
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (bind-key [remap find-tag] #'godef-jump))
  :config
  (add-hook 'go-mode-hook 'electric-pair-mode))

(use-package flycheck-gometalinter
  :ensure t
  :config
  (progn
    (flycheck-gometalinter-setup)))

(use-package go-eldoc
  :ensure t
  :defer
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; Python
;;(use-package elpy
;;  :ensure t)
;;(elpy-enable)
(use-package company-jedi
  :ensure t)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-jedi))
;;Flycheck Syntax lint
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  ;; Only Check on Save
  :config (setq flycheck-check-syntax-automatically '(mode-enabled save)))
;;Projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode))

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-on))

;;Neotree
(use-package neotree
  :ensure t)
(global-set-key [f1] 'neotree-toggle)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow 'nerd))
(use-package all-the-icons
  :ensure t)
;; Evil
(use-package evil
  :ensure t)
(evil-mode t)
(use-package evil-escape
  :ensure t)
(evil-escape-mode t)
;;(setq-default evil-escape-key-sequence "C-;")
(global-set-key (kbd "C-l") 'evil-escape)
(setq-default evil-escape-delay 0.2)
;;Specific conflict with Neotree & Evil
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
;; Line numbers
(add-hook 'prog-mode-hook 'linum-mode)
;;Theme and behaviour
(use-package dracula-theme
  :ensure t)
;;(use-package material-theme
;;  :ensure t)
(set-frame-font "Fira Mono 14")

(setq ring-bell-function 'ignore)

(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
 (add-to-list 'default-frame-alist '(alpha . (90 . 50)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6")))
 '(ivy-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (flycheck-gometalinter ggo-mode java-snippets rjsx-mode esup dracula-theme kotlin-mode enh-ruby-mode robe robe-mode projectile-rails magit emmet-mode jedi elpy counsel-projectile projectile key-chord evil-surround evil-leader evil-indent-textobject evil evil-escape all-the-icons neotree mvn exec-path-from-shell ensime f gradle-mode groovy-mode company-go go-eldoc go-mode alchemist meghanada material-theme atom-one-dark-theme spacemacs-theme company counsel swiper ace-window org-bullets which-key try use-package)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

