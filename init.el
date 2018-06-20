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
 package-archive-priorities '(("melpa-stable" . 10)
			      ("melpa" . 5)
			      ("gnu" . 1)))

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
;; (add-hook 'prog-mode-hook #'electric-pair-mode)

(use-package smartparens
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))

;; (setq-default c-basic-offset 4
		;; tab-width 4
		;; indent-tabs-mode t)
;; required by many packages.
(use-package dash
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

;; Elisp mode keybindings
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
			  "\C-c\C-a" 'pp-eval-expression)))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; (setq initial-major-mode 'emacs-lisp-mode)

;; (setq initial-scratch-message "\
;; Buffer Message.")

;; Move lines in visual mode up/down with shift-j/k
;; Selected words left and right with shift-h/l
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

;; For Debugging
(use-package esup
  :ensure t)

;; For trying packages
(use-package try
  :ensure t)

(use-package multi-term
  :ensure t
  :config
  (global-set-key (kbd "C-x t") 'multi-term-dedicated-toggle))

(global-set-key (kbd "C-x i") 'indent-for-tab-command)


;; Log my coding with Wakatime
(use-package wakatime-mode
  :ensure t
  :config
  (global-wakatime-mode))

;; autosaves and backups
(setq auto-save-default nil)
(setq make-backup-files nil)

;; RealGUD debugger
(use-package realgud
  :ensure t)

;; Lookup definition of word without leaving emacs
(use-package define-word
  :ensure t
  :config
  (global-set-key (kbd "C-x C-y") 'define-word-at-point))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Custom Functions                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun alex/open-init ()
  "Open my init file."
  (interactive)
  (find-file user-init-file))

(defun alex/clear-buffers ()
  "Clear all recent buffers."
  (interactive)
  (setq recentf-list '()))

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

(defun alex/copy-line ()
  "Copy current line."
  (interactive)
  (save-excursion
	(back-to-indentation)
	(kill-ring-save
	 (point)
	 (line-end-position)))
  (message "1 line copied."))

(defun alex/paste-below ()
  (interactive)
  (save-excursion
	(end-of-line)
	(newline-and-indent)
	(yank))
  (message "line pasted below."))

(defun alex/paste-above ()
  (interactive)
  (save-excursion
	(end-of-line)
	(previous-line)
	(indent-for-tab-command)
	(yank))
  (message "line pasted above."))

(defun alex/jump-to-line-above ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun alex/jump-to-line-below ()
  "Jump to next line and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun alex/jump-out-of-function ()
  "Jump down two lines and indent."
  (interactive)
  (next-line)
  (end-of-line)
  (newline-and-indent))

;; Required for `forward-to-word` function.
(require 'misc)

(defun alex/move-forward (arg)
  "Move forward to word, like vim.  Takes in ARG."
  (interactive "^p")
  (forward-to-word arg))


;; Moves past last letter, that way can delete last character,
;; which is often the reason for use.
(defun alex/move-to-end-of-word (arg)
  "Move to end of next word.  Takes in ARG."
  (interactive "p")
  (forward-word))

;; Still needs work, parens work but not quotes yet.
(defun alex/goto-match-paren (arg)
  "Move to matching parenthesis.  Takes in ARG."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
		((looking-at "\\s\)") (forward-char 1) (backward-list 1))
		;;((looking-at "\\s\"\\s\-") (backward-sexp) (backward-char 1))
		;;((looking-at "\\s\"") (forward-sexp) (backward-char 1))
		(t (self-inset-command (or arg 1)))))

(defun alex/jump-forward ()
  "Jumps to definition like vim Ctrl-]"
  (interactive)
  (evil-emacs-state)
  (call-interactively (key-binding (kbd "M-.")))
  (evil-change-to-previous-state (other-buffer))
  (evil-change-to-previous-state (current-buffer)))

(defun alex/jump-backward ()
  "Jumps back like vim Ctrl-o"
  (interactive)
  (evil-emacs-state)
  (call-interactively (key-binding (kbd "M-,")))
  (evil-change-to-previous-state (other-buffer))
  (evil-change-to-previous-state (current-buffer)))


;; Use before calling setup-tide-mode.
;; (defun alex/make-jsconfig ()
;;   "Create a jsconfig file."
;;   (f-write-text "{\n\"compilerOptions\": {\n\t\"target" 'utf-8 "jsconfig.json"))


;; Java experiments

;; (create-file-buffer "blah.org")
;; (f-write-text "Blah from emacs" 'utf-8 "blah.org")
;; (defun alex/read-string ()
;;   (interactive)
;;   (message "String is %s" (read-string "Enter string: ")))

;; (defun alex/java/create-class ()
;;   (interactive)
;;   (message "String is %s.java" (read-string "Enter class name: ")))

;; (defun alex/java/create-project ()
;;   (interactive)
;;   (message "String is %s" (read-directory-name "Enter project directory name: ")))

;; read-file-name, read-directory-name


;; Keymapings of custom functions

(global-set-key (kbd "C-x ,") 'recenter-top-bottom)
(global-set-key (kbd "C-.") 'alex/paste-below)
(global-set-key (kbd "C->") 'alex/paste-above)
(global-set-key (kbd "C-l") 'alex/copy-line)
(global-set-key (kbd "C-S-o") 'alex/insert-line-below)
(global-set-key (kbd "C-o") 'alex/insert-line-above)
(global-set-key (kbd "C-;") 'alex/jump-to-line-above)
(global-set-key (kbd "C-:") 'alex/jump-to-line-below)
(global-set-key (kbd "M-f") 'alex/move-forward)
(global-set-key (kbd "C-%") 'alex/goto-match-paren)
(global-set-key (kbd "M-'") 'alex/move-to-end-of-word)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       AESTHETICS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Theme
(use-package dracula-theme
  :ensure t)

;; (use-package nord-theme
  ;; :ensure t)

;; (with-eval-after-load 'nord-theme
;;   (setq nord-comment-brightness 20)
;;   ;; Highlight can be frost or snowstorm
;;   (setq nord-region-highlight "snowstorm")
;;   (setq nord-uniform-mode-lines t))


(set-frame-font "Fira Code Retina 18")
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
;;(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

;; (use-package powerline
;;   :ensure t
;;   :init
;;   (powerline-default-theme))

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
	  (projects . 5)
	  (recents . 5)))
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
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-extra-directories nil)
  ;; (setq ivy-re-builders-alist
  ;; 	'((t . ivy--regex-fuzzy)))
  (avy-setup-default)
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1))

(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g c") 'avy-goto-char)

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
  (setq aw-dispatch-always nil)
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

(use-package imenu-list
  :ensure t
  :bind
  ("C-x m" . imenu-list-smart-toggle))

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


;; (use-package evil
;;   :ensure t
;;   :config
;;   (evil-mode 1)
;;   (evil-global-set-key 'normal (kbd "<SPC> s") 'save-buffer)
;;   (evil-global-set-key 'normal (kbd "<SPC> q") 'save-buffers-kill-terminal)
;;   (evil-global-set-key 'normal (kbd "<SPC> n") 'evil-ex-nohighlight)
;;   (evil-global-set-key 'normal (kbd "<SPC> e") 'dired)
;;   (evil-global-set-key 'normal (kbd "<SPC> t") 'multi-term-dedicated-toggle)
;;   (evil-global-set-key 'normal (kbd "<SPC> f") 'counsel-find-file)
;;   (evil-global-set-key 'normal (kbd "<SPC> b") 'switch-to-buffer)
;;   (evil-global-set-key 'normal (kbd "] q") 'flycheck-next-error)
;;   (evil-global-set-key 'normal (kbd "[ q") 'flycheck-previous-error)
;;   (evil-global-set-key 'normal (kbd "[ <SPC>") 'alex/insert-line-above)
;;   (evil-global-set-key 'normal (kbd "] <SPC>") 'alex/insert-line-below)
;;   (evil-global-set-key 'visual (kbd "H") 'drag-stuff-left)
;;   (evil-global-set-key 'visual (kbd "J") 'drag-stuff-down)
;;   (evil-global-set-key 'visual (kbd "K") 'drag-stuff-up)
;;   (evil-global-set-key 'visual (kbd "L") 'drag-stuff-right)
;;   (evil-global-set-key 'normal (kbd "C-]") 'alex/jump-forward)
;;   (evil-global-set-key 'normal (kbd "C-o") 'alex/jump-backward)
;;   (evil-global-set-key 'normal (kbd "<SPC> d") 'alex/open-daily)
;;   (evil-global-set-key 'normal (kbd "<SPC> vr") 'alex/open-init))


;; (use-package evil-escape
;;   :ensure t
;;   :config
;;   (setq-default evil-escape-key-sequence "jk")
;;   (setq-default evil-escape-delay 0.2)
;;   :init
;;   (evil-escape-mode t))

;; (use-package evil-commentary
;;   :ensure t
;;   :config
;;   (evil-commentary-mode))

;; (use-package evil-magit
;;   :ensure t)

;; (use-package evil-lion
;;   :ensure t
;;   :config
;;   (evil-lion-mode))

;; ;; ;; Specific conflict with Neotree & Evil
;; (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
;; (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
;; (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
;; (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Orgmode                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-plus-contrib
  :ensure t)

(setq org-agenda-files (list "~/org/daily.org"
			     "~/org/journal"))

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
				 "** TOREAD:  %?\t\t%^G")))

(global-set-key (kbd "C-x c") 'counsel-org-capture)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
			     (org-bullets-mode 1))))

(use-package org-journal
  :ensure t
  :bind
  ("C-x j" . org-journal-new-entry)
  :config
  (setq org-journal-dir "~/org/journal")
  (global-unset-key (kbd "C-c C-j")))

(with-eval-after-load 'org-journal

  )


;; (use-package org-brain
;;   :ensure t
;;   :init
;;   (setq org-brain-path "~/org/brain")
;;   (eval-after-load 'evil
;;     (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;;   :config
;;   (setq org-id-track-globally t)
;;   (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
;;   (push '("b" "Brain" plain (function org-brain-goto-end)
;; 	  "* %i%?" :empty-lines 1)
;; 	org-capture-templates)
;;   (setq org-brain-visualize-default-choices 'all)
;;   (setq org-brain-title-max-length 12))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "~/work/plantuml/plantuml.jar"))


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
  ;; (add-to-list 'elixir-mode-hook 'alchemist-mode)
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
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  ;; Bind some Alchemist commands to more commonly used keys.
  (bind-keys :map alchemist-mode-map
             ("C-c C-l" . (lambda () (interactive)
                            (save-buffer)
                            (alchemist-iex-compile-this-buffer))))
  (bind-keys :map alchemist-mode-map
             ("C-x C-e" . alchemist-iex-send-current-line)))

;; (eval-after-load 'company
;;   '(push 'alchemist-company 'company-backends))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Ruby

(use-package inf-ruby
  :ensure t)

(use-package robe
  :ensure t
  :commands robe-mode
  :after inf-ruby company
  :config
  (define-key ruby-mode-map (kbd "C-c C-c") 'robe-start)
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

(use-package ruby-end
  :ensure t)

(use-package seeing-is-believing
  :ensure t
  :config
  (setq seeing-is-believing-max-length 150
	seeing-is-believing-max-results 10
	seeing-is-believing-timeout 10.5
	seeing-is-believing-alignment 'file)
  (add-hook 'ruby-mode-hook 'seeing-is-believing))

;;(defvar ruby-indent-level 2)

;; (eval-after-load 'company
;;   '(push 'company-robe company-backends))

;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;   "Select correct ruby version."
;;   (rvm-activate-corresponding-ruby))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Python
(use-package elpy
  :ensure t)


(elpy-enable)

(setq elpy-rpc-python-command "python3")

;; Ipython has issues
(setq python-shell-interpreter "python3")

;; (use-package jedi
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t))

;; (use-package company-jedi
  ;; :ensure t)

;; (with-eval-after-load 'company
  ;; (add-to-list 'company-backends 'company-jedi))

;; Requires simple-httpd
;; (use-package ein
  ;; :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Java

(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(use-package meghanada
  :ensure t
  :defer t
  :init
  (add-hook 'java-mode-hook
			(lambda ()
			  (google-set-c-style)
			  (google-make-newline-indent)
			  (meghanada-mode t)
			  (smartparens-mode t)
			  (rainbow-delimiters-mode t)
			  (highlight-symbol-mode t)
			  (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  :config
  ;; (use-package realgud
  ;; 	:ensure t)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  :bind
  (:map meghanada-mode-map
		("C-S-t" . meghanada-switch-testcase)
		("M-RET" . meghanada-local-variable)
		("C-M-." . imenu)
		("M-r" . meghanada-reference)
		("M-t" . meghanada-typeinfo)
		("C-z" . hydra-meghanada/body))
  :commands
  (meghanada-mode))

(defhydra hydra-meghanada (:hint nil :exit t)
;;"
;; ^Edit^                           ^Tast or Task^
;; ^^^^^^-------------------------------------------------------
;; _f_: meghanada-compile-file      _m_: meghanada-restart
;; _c_: meghanada-compile-project   _t_: meghanada-run-task
;; _o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
;; _s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
;; _v_: meghanada-local-variable    _R_: meghanada-run-junit-recent
;; _i_: meghanada-import-all        _r_: meghanada-reference
;; _g_: magit-status                _T_: meghanada-typeinfo
;; _l_: helm-ls-git-ls
;; _q_: exit
;; "
  ("f" meghanada-compile-file)
  ("m" meghanada-restart)

  ("c" meghanada-compile-project)
  ("o" meghanada-optimize-import)
  ("s" meghanada-switch-test-case)
  ("v" meghanada-local-variable)
  ("i" meghanada-import-all)

  ("g" magit-status)
  ;; ("l" helm-ls-git-ls)

  ("t" meghanada-run-task)
  ("T" meghanada-typeinfo)
  ("j" meghanada-run-junit-test-case)
  ("J" meghanada-run-junit-class)
  ("R" meghanada-run-junit-recent)
  ("r" meghanada-reference)

  ("q" exit)
  ("z" nil "leave"))


  ;; :config
  ;; (add-hook 'java-mode-hook
  ;; 	    (lambda ()
  ;; 	      (meghanada-mode t)
  ;; 	      (defvar c-basic-offset 2)
  ;; 	      (highlight-symbol-mode t)
  ;; 	      (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))))

(use-package java-snippets
  :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package gradle-mode
  :ensure t)

(use-package kotlin-mode
  :ensure t)



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

;; Problem with emacs 26.1...May not be needed with cider.
;; (use-package clj-refactor
  ;; :ensure t
  ;; :config
  ;; (cljr-add-keybindings-with-prefix "C-c C-m")
  ;; (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  ;; (add-hook 'clojure-mode-hook #'yas-minor-mode))

(use-package cider
  :config
  (define-key cider-mode-map (kbd "C-c M-l") 'cider-inspect-last-result)
  (define-key cider-mode-map (kbd "C-c M-v") 'cider-find-var)
  (define-key cider-mode-map (kbd "C-c M-f") 'cider-find-var)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-repl-use-pretty-printing t)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (clojure/fancify-symbols 'cider-repl-mode)
  (clojure/fancify-symbols 'clojure-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Common Lisp
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "ros run")
  (setq slime-contribs '(slime-fancy)))

;; set C-c Meta-j run 'slime'
(define-key lisp-mode-map (kbd "C-c M-j") 'slime)

;; Local HyperSpec
(setq common-lisp-hyperspec-root (expand-file-name "~/.emacs.d/hyper_spec/HyperSpec/"))


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
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
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
	'(("django" . "\\.html\\'"))))
;; ("jsx" . "\\.js[x]?\\'")

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-self-closing-tag-style " /")
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)
  (add-hook 'emmet-mode-hook (lambda ()
			       (setq emmet-indentation 2))))

(use-package restclient
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "/usr/bin/pandoc"))

(use-package easy-jekyll
  :ensure t
  :init
  (setq easy-jekyll-basedir "~/work/alexafshar/alexafshar.com")
  (setq easy-jekyll-url "http://alexafshar.com")
  (setq easy-jekyll-sshdomain "alexafshar.com")
  (setq easy-jekyll-root "/var/www/alexafshar.com"))

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq js2-basic-offset 2))

;; Async web server
(use-package elnode
  :ensure t)

(use-package tide
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
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


;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; Skewer dependency
(use-package simple-httpd
  :ensure t)

;; (use-package skewer-mode
;;   :ensure t
;;   :config
;;   (skewer-setup))

;; (use-package indium
;;   :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook #'indium-interaction-mode))

;; (use-package tern
;;   :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook (lambda ()
;; 			     (tern-mode t))))

;; (use-package company-tern
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'company
;;     (add-to-list 'company-backends 'company-tern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       CPP
(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook
			(lambda ()
			  (local-set-key (kbd "<F5>") 'recompile)))
  (add-hook 'c-mode-hook
			(lambda ()
			  (local-set-key (kbd "<F5>") 'recompile))))

(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

;; (use-package company-irony
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'company '(add-to-list
;; 				   'company-backends 'company-irony)))

;; (use-package flycheck-irony
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'flycheck '(add-hook
;; 			       'flycheck-mode-hook #'flycheck-irony-setup)))

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
;;                      Scheme
(use-package geiser
  :ensure t)

;; (with-eval-after-load 'geiser
  ;; (define-key geiser-mode-map (kbd "C-c C-c") 'scheme-send-last-sexp))


(setq scheme-program-name "csi -:c")

(require 'cmuscheme)

;; Lovely function that calls chicken-doc to look up symbol at point
;; Shamelessly stolen from wiki.call-cc.org/eggref/4/chicken-doc#emacs
(defun chicken-doc (&optional obtain-function)
  (interactive)
  (let ((func (funcall (or obtain-function 'current-word))))
	(when func
	  (process-send-string (scheme-proc)
						   (format "(require-library chicken-doc) ,doc %S\n" func))
	  (save-selected-window
		(select-window (display-buffer (get-buffer scheme-buffer) t))
		(goto-char (point-max))))))

;; Binds chicken doc to C-c d
(with-eval-after-load 'cmuscheme
  (define-key scheme-mode-map "\C-cd" 'chicken-doc)
  (define-key scheme-mode-map "\C-cx" 'scheme-send-last-sexp))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Golang
(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook (lambda ()
			    (set (make-local-variable 'company-backends) '(company-go))
			    (company-mode))))

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Social                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq emacs-secrets-file "/home/asqrd/.emacs-secrets.el")

;; (defun get-freenode-password (server)
;;   (with-temp-buffer
;;     (insert-file-contents-literally emacs-secrets-file)
;;     (plist-get (read (buffer-string)) :freenode-password)))

;; (use-package circe
;;   :ensure t
;;   :config
;;   (setq circe-network-options
;; 	'(("Freenode"
;; 	   :tls t
;; 	   :nick "asqrd_"
;; 	   :sasl-username "asqrd_"
;; 	   :sasl-password get-freenode-password
;; 	   :channels ("#emacs"
;; 		      "#vim"
;; 		      "#emacs-elixir"
;; 		      "#clojure-emacs"
;; 		      "#Solus"
;; 		      "#Solus-Chat"
;; 		      "#Solus-Dev"
;; 		      "#clojure"
;; 		      "#clojure-beginners"
;; 		      "#elixir-lang"
;; 		      "#ruby"
;; 		      "##java")))))



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
 '(cider-boot-parameters "cider repl -s wait")
 '(counsel-mode t)
 '(custom-safe-themes
   (quote
    ("617341f1be9e584692e4f01821716a0b6326baaec1749e15d88f6cc11c288ec6" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "d6db7498e2615025c419364764d5e9b09438dfe25b044b44e1f336501acd4f5b" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "b97a01622103266c1a26a032567e02d920b2c697ff69d40b7d9956821ab666cc" "40da996f3246a3e99a2dff2c6b78e65307382f23db161b8316a5440b037eb72c" default)))
 '(geiser-default-implementation (quote chicken))
 '(geiser-implementations-alist
   (quote
    (((regexp "\\.scm$")
      chicken)
     ((regexp "\\.ss$")
      racket)
     ((regexp "\\.rkt$")
      racket)
     ((regexp "\\.scm$")
      guile)
     ((regexp "\\.release-info$")
      chicken)
     ((regexp "\\.meta$")
      chicken)
     ((regexp "\\.setup$")
      chicken)
     ((regexp "\\.ss$")
      chez)
     ((regexp "\\.def$")
      chez)
     ((regexp "\\.scm$")
      mit)
     ((regexp "\\.pkg$")
      mit)
     ((regexp "\\.scm$")
      chibi)
     ((regexp "\\.sld$")
      chibi))))
 '(package-selected-packages
   (quote
    (alect-themes racket-mode skewer-mode impatient-mode simple-httpd clj-refactor cider alchemist elixir-mode magit define-word lorem-ipsum elnode seeing-is-believing imenu-list geiser org-plus-contrib go-eldoc company-go go-mode slime ruby-end smartparens highlight-indent-guides org-journal markdown-mode android-mode drag-stuff sudoku typit typing ag emmet-mode multi-term try irony-eldoc irony kotlin-mode gradle-mode groovy-mode meghanada tabbar evil-tabs powerline evil-commentary evil counsel-projectile projectile ace-window ivy expand-region exec-path-from-shell dashboard flycheck company dracula-theme use-package)))
 '(safe-local-variable-values
   (quote
    ((cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend"))))
 '(wakatime-cli-path "/usr/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
