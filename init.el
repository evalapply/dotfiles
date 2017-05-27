;;; package --- Summary
;;; Commentary:
;;; Code:
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

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; Path From Shell
(use-package exec-path-from-shell
  :ensure t)
(exec-path-from-shell-initialize)
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
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

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
;; Web
(use-package emmet-mode
  :ensure t)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
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
(setq web-mode-engines-alist
      '(("blade" . "\\.blade\\.")))
;;php
(use-package php-mode
  :ensure t)
(use-package company-php
  :ensure t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-php)))

(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            (meghanada-mode t)
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

(use-package groovy-mode
  :ensure t)
(use-package gradle-mode
  :ensure t)

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
(use-package company-go
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(use-package go-mode
  :ensure t
  :init
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (bind-key [remap find-tag] #'godef-jump))
  :config
  (add-hook 'go-mode-hook 'electric-pair-mode))

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
  :config
  (projectile-global-mode +1))

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
;;(use-package evil
;;  :ensure t)
;;(evil-mode t)
;;(use-package evil-escape
;;  :ensure t)
;;(evil-escape-mode t)
;;(setq-default evil-escape-key-sequence "jk")
;;(setq-default evil-escape-delay 0.2)
;;;; Specific conflict with Neotree & Evil
;;(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
;;(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
;;(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
;;(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
;; Line numbers
(add-hook 'prog-mode-hook 'linum-mode)
;;Theme and behaviour
(use-package material-theme
  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes nil)
 '(ivy-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (emmet-mode jedi elpy counsel-projectile projectile key-chord evil-surround evil-leader evil-indent-textobject evil evil-escape all-the-icons neotree mvn exec-path-from-shell ensime f gradle-mode groovy-mode company-go go-eldoc go-mode alchemist meghanada material-theme atom-one-dark-theme spacemacs-theme company counsel swiper ace-window org-bullets which-key try use-package)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

