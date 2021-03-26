;; package -- Init File
;;; Commentary:
;;; This is the init file for Emacs configuration.
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       BASE                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 5)
			      ("melpa" . 10)
			      ("org" . 12)
			      ("gnu" . 1)))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; For MAC
;; (when (eq system-type 'darwin)
  ;; (setq mac-option-modifier 'meta)
  ;; (setq mac-command-modifier '(:ordinary super :button 2)))

;; UTF-8 settings
(set-language-environment               "UTF-8")
(set-charset-priority			'unicode)
(setq locale-coding-system		'utf-8)
(set-terminal-coding-system		'utf-8)
(set-keyboard-coding-system		'utf-8)
(set-selection-coding-system		'utf-8)
(prefer-coding-system			'utf-8)
(setq default-process-coding-system	'(utf-8-unix . utf-8-unix))

;; autosaves and backups
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Mac Specific settings
;; (if (eq system-type 'darwin)
    ;; (progn
      ;; (setq mac-option-key-is-meta nil)
      ;; (setq mac-command-key-is-meta t)
      ;; (setq mac-command-modifier 'meta)
      ;; (setq mac-option-modifier 'alt)))

;; Delete trailing space on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; After copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq select-enable-clipboard t)

;; After mouse selection in X11, you can paste by `yank' in emacs
(setq select-enable-primary t)

;; Line numbers in programming mode
(add-hook 'prog-mode-hook 'linum-mode)

;; Evil Mode
;; (use-package evil
  ;; :ensure t
  ;; :config
  ;; (setq evil-default-state 'emacs)
;; (evil-mode))

(use-package nimbus-theme
  :ensure
  :defer)

;; (use-package nord-theme
  ;; :ensure
  ;; :defer)

;; (use-package apropospriate-theme
  ;; :ensure
  ;; :defer)

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("8:00" . leuven)
			   ("17:00" . nimbus)))
  ;; Low-contrast themes
  ;; (setq circadian-themes '((:sunrise . apropospriate-light)
			   ;; (:sunset . nord)))
  (circadian-setup))



(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smartparens
  :ensure t)

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
    ;; Disable smartparens when using paredit
    (add-hook 'paredit-mode-hook (lambda ()
				   (smartparens-mode -1))))
;; Move lines in visual mode up/down with shift-j/k
;; Selected words left and right with shift-h/l
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

;; For Debugging Emacs init (this file) and startup.
(use-package esup
	     :ensure t)

;; For trying packages
(use-package try
  :ensure t)

;; Quick terminals
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-dedicated-select-after-open-p t)
  (global-set-key (kbd "C-x t") 'multi-term-dedicated-toggle))

;; Eshell
(global-unset-key (kbd "C-x e")) ; unset kmacro binding
(global-set-key (kbd "C-x e") 'eshell)
(add-hook 'eshell-mode-hook (lambda ()
			      (eshell/alias "e" "find-file $1")
			      (eshell/alias "d" "dired $1")
			      (eshell/alias "g" 'magit-status)))

;; Fix to indent with TAB key.
(global-set-key (kbd "C-x i") 'indent-for-tab-command)

;; RealGUD debugger for debugging many languages
(use-package realgud
	     :ensure t)

;; Lookup definition of word without leaving emacs
(use-package define-word
  :ensure t
  :config
  (global-set-key (kbd "C-x C-y") 'define-word-at-point))

;; Displays which keys are available, and what they do.
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (define-key help-map "\C-h" 'which-key-C-h-dispatch))

;; Get Path information from shell
(use-package exec-path-from-shell
	     :ensure t
	     :config
	     (when (memq window-system '(mac ns x))
	       (exec-path-from-shell-initialize))
	     ;; Can copy specific exports as follows:
	     ;; (exec-path-from-shell-copy-env "RUST_SRC_PATH")
	     )

;; A better expand-region
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      FUNCTIONALITY                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto Completion
(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (defvar company-dabbrev-downcase nil)
  (setq company-idle-delay 0
	company-minimum-prefix-length 1
	company-show-numbers t
	company-tooltip-align-annotations t
	company-tooltip-limit 10
	company-global-modes '(not eshell-mode term-mode shell-mode))
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map [tab] #'company-complete-selection)
  )

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; Language Server Protocol (LSP)
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; LSP Debugger
(when (display-graphic-p)
  (progn
    (use-package lsp-ui
      :ensure t
      :hook ((lsp-mode . lsp-ui-mode))
      :commands lsp-ui-mode)

   (use-package dap-mode
     :ensure t
     :after lsp-mode
     :config
     (dap-mode t)
     (dap-ui-mode t)
     (require 'dap-elixir))

   (dap-ui-mode 1)

   ))


;; Linter/Syntax checker
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Snippets
;; run: M-x package-install RET yasnippet-snippets
;; to install snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; Quick Calculator
(global-set-key (kbd "M-# q") 'quick-calc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           MISC                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Editing Dockerfiles
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; For managing docker
(use-package docker
  :ensure t
  :bind ("C-x w" . docker))

;; YAML files
(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ORG                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org updated
(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-pretty-entities t)


  (setq org-agenda-files (list "~/org/tasks"))

  (setq org-agenda-file-regexp "\\`[^.].*\\.org\\'\\|\\`[0-9]+\\'")

  (setq org-return-follows-link t)

  (defvar org-capture-templates '(("t" "Task [inbox]" entry
				   (file+headline "~/org/inbox.org" "Tasks")
				   "*** TODO %?")
				  ("n" "Notebook [inbox]" entry
				   (file+headline "~/org/notebooks/inbox.org" "Notebooks Inbox")
				   "* %? Notebook \n** Tasks\n** Resources\n** Projects\n** Notes")
				  ("i" "Idea [inbox]" entry
				   (file+headline "~/org/inbox.org" "Ideas")
				   "*** Idea - %?")
				  ("r" "Resource [inbox]" entry
				   (file+headline "~/org/inbox.org" "Resources")
				   "** TOREAD:  %?\t\t%^G"))))

;; Org Capture from anywhere
(with-eval-after-load "counsel"
  (global-set-key (kbd "C-x c") 'counsel-org-capture))

;; Org bullets that look nice
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
			     (visual-line-mode)
			     (org-indent-mode)
			     (org-bullets-mode 1)
			     (turn-off-drag-stuff-mode)
			     ;; (flyspell-mode)
			     (ispell-minor-mode))))

;; Journal entries
(use-package org-journal
  :ensure t
  :bind
  ("C-x j" . org-journal-new-entry)
  :config
  (setq org-journal-dir "~/org/journal")
  (global-unset-key (kbd "C-c C-j")))

;; Org src elixir support
(use-package ob-elixir
  :ensure t)

;; Org Eval Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (java . t)
   (ruby . t)
   (scheme . t)
   (lisp . t)
   (emacs-lisp . t)
   (elixir . t)
   (C . t)
   (python . t)
   (js . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       AESTHETICS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(set-frame-font "Fira Code 14")
(setq default-frame-alist '((font . "Fira Code 14")))
(setq initial-frame-alist '((top . 0) (left . 0) (width . 140) (height . 40)))
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Search and Management                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package ivy
  :ensure t
  :bind
  ("C-s"	.	swiper)
  ("C-x C-r"	.	ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-extra-directories nil)
  (avy-setup-default)
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1))

(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g c") 'avy-goto-char)

;; Ace Window
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :config
  ;; to use when 2 or less windows
  (setq aw-dispatch-always nil)
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Project Management                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  :bind
  ("C-x p" . projectile-command-map)
  :init
  (projectile-mode +1))

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

(use-package imenu-list
  :ensure t
  :bind
  ("C-x m" . imenu-list-smart-toggle))

(use-package treemacs
  :ensure t
  :bind
  ("C-x l" . treemacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Version Control                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-x g" . magit-status))

(use-package git-gutter-fringe
  :ensure t)

(use-package ediff
  :ensure t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Emacs Lisp                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Keybindings
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(define-key emacs-lisp-mode-map
			  "\C-c\C-e" 'pp-eval-last-sexp)
			(define-key emacs-lisp-mode-map
			  "\C-c\C-s" 'eval-print-last-sexp)
			(define-key emacs-lisp-mode-map
			  "\C-c\C-d" 'eval-defun)
			(define-key emacs-lisp-mode-map
			  "\C-c\C-z" 'ielm)
			(define-key emacs-lisp-mode-map
			  "\C-c\C-k" 'eval-current-buffer)
			(define-key emacs-lisp-mode-map
			  "\C-c\C-a" 'pp-eval-expression)
			(define-key emacs-lisp-mode-map
			  "\C-c\C-b" 'edebug-defun)))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; Package that suggest elisp functions based on inputs and outputs.
(use-package suggest
  :ensure t)

;; Package that defines elisp functions based on keyboard macros.
(use-package elmacro
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Front End                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
			     ;; Disable smartparens in html for mustache syntax bug.
			     (when (string-equal "html" (file-name-extension buffer-file-name))
			       (setq smartparens-mode nil))))
  ;; (add-hook 'web-mode-hook (lambda ()
			     ;; Use Tide in JSX files.
  			     ;; (when (string-equal "jsx" (file-name-extension buffer-file-name))
  			       ;; (setup-tide-mode))))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-engines-alist
	'(("django" . "\\.html\\'")
	  ("blade" . "\\.blade\\."))))

;; Emmet mode, Ctrl , for expansion
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'js2-jsx-mode-hook 'emmet-mode)
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-self-closing-tag-style " /")
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)
  (add-hook 'emmet-mode-hook (lambda ()
			       (setq emmet-indentation 2))))

;; Use to test APIs, similar to Postman
(use-package restclient
  :ensure t)

;; Enhanced mode for markdown
(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "/usr/bin/pandoc"))

;; For my website/blog
(use-package easy-jekyll
  :ensure t
  :init
  (setq easy-jekyll-basedir "~/work/alexafshar.com")
  (setq easy-jekyll-url "http://alexafshar.com")
  (setq easy-jekyll-sshdomain "alexafshar.com")
  (setq easy-jekyll-root "/var/www/alexafshar.com"))

;; Enhanced JavaScript Mode
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
  (setq js2-basic-offset 2))

;; Async web server
(use-package elnode
  :ensure t)

;; Tide Typescript/JavaScript mode
(use-package tide
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'js2-jsx-mode #'setup-tide-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode))

;; from the Tide README
(defun setup-tide-mode ()
  "Set up Tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; {
;;   "compilerOptions": {
;;     "target": "es2017",
;;     "allowSyntheticDefaultImports": true,
;;     "noEmit": true,
;;     "checkJS": true,
;;     "jsx": "react",
;;     "lib": ["dom", "es2017"]
;;   }
;; }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Python                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; At the moment Python LSP is very slow,
;; need to review later.

;; Python IDE like mode

(use-package anaconda-mode
  :ensure t
  :hook
   (python-mode . anaconda-mode)
   (python-mode . anaconda-eldoc-mode))

;; Python Auto Completion
(use-package company-anaconda
  :ensure t
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

(use-package poetry
  :ensure t)

;; (use-package pipenv
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))

(use-package pipenv
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Rust                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic Rust Support
(use-package rust-mode
  :ensure t)

;; Cargo command support
(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode #'flycheck-rust-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Golang                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         C/C++                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Scheme                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(chicken)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Common-Lisp                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package slime
;;   :ensure t
;;   :config
;;   (setq inferior-lisp-program "sbcl")
;;   (load "/Users/asqrd/quicklisp/clhs-use-local.el" t)
;;   (slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-company)))

;; (use-package slime-company
;;   :ensure t)

;; Company mode built in to sly
(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Clojure                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cider
  :ensure t)

(use-package clj-refactor
  :ensure t)

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
(clojure.tools.namespace.repl/refresh)"))



;;; (provide 'init)
;;; init.el ends here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      End Of Init                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-box lsp-ivy lsp-ui clj-refactor geiser sly go-mode cargo rust-mode company-anaconda anaconda-mode dap-mode company expand-region exec-path-from-shell which-key define-word realgud multi-term try esup drag-stuff paredit rainbow-delimiters circadian nimbus-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
