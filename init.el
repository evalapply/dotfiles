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

(if (eq system-type 'darwin)
    (progn
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'alt)))

;; Delete trailing space on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq select-enable-primary t)

;; Line numbers in programming mode
(add-hook 'prog-mode-hook 'linum-mode)

(use-package smartparens
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


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

;; For Debugging
(use-package esup
  :ensure t)

;; For trying packages
(use-package try
  :ensure t)

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-dedicated-select-after-open-p t)
  (global-set-key (kbd "C-x t") 'multi-term-dedicated-toggle))

(global-set-key (kbd "C-x i") 'indent-for-tab-command)

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


(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (define-key help-map "\C-h" 'which-key-C-h-dispatch))


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "KIEX_HOME")
  (exec-path-from-shell-copy-env "MIX_ARCHIVES")
  (exec-path-from-shell-copy-env "kotlinc")
  (exec-path-from-shell-copy-env "RUST_SRC_PATH")
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Emacs Lisp                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;                     Custom Functions                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun asqrd/open-init ()
  "Open my init file."
  (interactive)
  (find-file user-init-file))

(defun asqrd/clear-buffers ()
  "Clear all recent buffers."
  (interactive)
  (setq recentf-list '()))

(defun asqrd/insert-line-above ()
  "Insert a line above current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))

(defun asqrd/insert-line-below ()
  "Insert a line below current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun asqrd/copy-line ()
  "Copy current line."
  (interactive)
  (save-excursion
	(back-to-indentation)
	(kill-ring-save
	 (point)
	 (line-end-position)))
  (message "1 line copied."))

(defun asqrd/paste-below ()
  (interactive)
  (save-excursion
	(end-of-line)
	(newline-and-indent)
	(yank))
  (message "line pasted below."))

(defun asqrd/paste-above ()
  (interactive)
  (save-excursion
	(end-of-line)
	(previous-line)
	(indent-for-tab-command)
	(yank))
  (message "line pasted above."))

(defun asqrd/jump-to-line-above ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun asqrd/jump-to-line-below ()
  "Jump to next line and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun asqrd/jump-out-of-function ()
  "Jump down two lines and indent."
  (interactive)
  (next-line)
  (end-of-line)
  (newline-and-indent))

;; Required for `forward-to-word` function.
(require 'misc)

(defun asqrd/move-forward (arg)
  "Move forward to word, like vim.  Takes in ARG."
  (interactive "^p")
  (forward-to-word arg))


;; Moves past last letter, that way can delete last character,
;; which is often the reason for use.
(defun asqrd/move-to-end-of-word (arg)
  "Move to end of next word.  Takes in ARG."
  (interactive "p")
  (forward-word))

;; Still needs work, parens work but not quotes yet.
(defun asqrd/goto-match-paren (arg)
  "Move to matching parenthesis.  Takes in ARG."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
		((looking-at "\\s\)") (forward-char 1) (backward-list 1))
		;;((looking-at "\\s\"\\s\-") (backward-sexp) (backward-char 1))
		;;((looking-at "\\s\"") (forward-sexp) (backward-char 1))
		(t (self-inset-command (or arg 1)))))

(defun asqrd/jump-forward ()
  "Jumps to definition like vim Ctrl-]"
  (interactive)
  (evil-emacs-state)
  (call-interactively (key-binding (kbd "M-.")))
  (evil-change-to-previous-state (other-buffer))
  (evil-change-to-previous-state (current-buffer)))

(defun asqrd/jump-backward ()
  "Jumps back like vim Ctrl-o"
  (interactive)
  (evil-emacs-state)
  (call-interactively (key-binding (kbd "M-,")))
  (evil-change-to-previous-state (other-buffer))
  (evil-change-to-previous-state (current-buffer)))

(defun asqrd/company-show-doc-buffer ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available"))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (display-buffer doc-buffer t)))

;; Found here: https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
;; TODO: name buffers ->
;; (let* (buf-name (generate-buffer-name "*indirect*"))
;;       (buf (clone-indirect-buffer-other-window buf-name t))...
(defun asqrd/indirect-region-below (start end)
  "Open buffer with selected region from START to END in window below."
  (interactive "r")
  (let ((buf (clone-indirect-buffer-other-window nil t)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(defun asqrd/indirect-region-side (start end)
  "Open buffer with selected region from START to END in window to the sidek."
  (interactive "r")
  (split-window-horizontally)
  (other-window 1)
  (let ((buf (clone-indirect-buffer-other-window nil t)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))


;; Keymapings of custom functions

;; (global-set-key (kbd "C-x ,") 'recenter-top-bottom)
(global-set-key (kbd "C-.") 'asqrd/paste-below)
(global-set-key (kbd "C->") 'asqrd/paste-above)
(global-set-key (kbd "C-x ,") 'asqrd/copy-line)
(global-set-key (kbd "C-S-o") 'asqrd/insert-line-below)
(global-set-key (kbd "C-o") 'asqrd/insert-line-above)
(global-set-key (kbd "C-;") 'asqrd/jump-to-line-above)
(global-set-key (kbd "C-:") 'asqrd/jump-to-line-below)
(global-set-key (kbd "M-f") 'asqrd/move-forward)
(global-set-key (kbd "C-%") 'asqrd/goto-match-paren)
(global-set-key (kbd "M-'") 'asqrd/move-to-end-of-word)
;; (define-key company-active-map (kbd "C-c d") #'asqrd/company-show-doc-buffer)

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


(set-frame-font "Fira Code 18")
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)

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
	company-tooltip-limit 10
	company-global-modes '(not eshell-mode term-mode shell-mode))
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map [tab] #'company-complete-selection))

(use-package pos-tip
  :ensure t)

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode)
  (with-eval-after-load 'company
    '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))


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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Search and Management                         ;;
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
  ;; (setq ivy-re-builders-alist
  ;; 	'((t . ivy--regex-fuzzy)))
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


;; (use-package flx
;; :ensure t)

;; Project management
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Version Control                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package ediff
  :ensure t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Orgmode                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure org-plus-contrib)
(setq org-pretty-entities t)

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

(global-set-key (kbd "M-# q") 'quick-calc)

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

(use-package ob-elixir
  :ensure t)

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
;;                        Elixir                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (setq alchemist-iex-program-name "/home/asqrd/.asdf/shims/iex")
  (setq alchemist-execute-command "/home/asqrd/.asdf/shims/elixir")
  (setq alchemist-compile-command "/home/asqrd/.asdf/shims/elixirc")
  (setq alchemist-mix-command "/home/asqrd/.asdf/shims/mix")
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Ruby                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Python                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;; (use-package elpy
;;   :ensure t
;;   :config
;;   ;; (elpy-enable)
;;   (add-hook 'python-mode-hook 'elpy-mode)
;;   (remove-hook 'elpy-modules 'elpy-module-flymake)
;;   (add-hook 'elpy-mode-hook 'flycheck-mode)
;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;;   (setq python-shell-interpreter "ipython3" python-shell-interpreter-args "--simple-prompt --pprint"))

;; (use-package py-autopep8
;;   :ensure t
;;   :config
;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; (use-package ein
;;   :ensure t)



;; (add-hook 'elpy-mode-hook
	  ;; (lambda ()
	    ;; (set (make-local-variable 'company-backends)
		 ;; '((company-dabbrev-code company-yasnippet elpy-company-backend)))))


;; Requires simple-httpd
;; (use-package ein
  ;; :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Java                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package hydra
  :ensure t)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Clojure                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :ensure t
  :config
  (define-key cider-mode-map (kbd "C-c M-l") 'cider-inspect-last-result)
  (define-key cider-mode-map (kbd "C-c M-v") 'cider-find-var)
  (define-key cider-mode-map (kbd "C-c M-f") 'cider-find-var)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  ;; (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-use-clojure-font-lock t)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (clojure/fancify-symbols 'cider-repl-mode)
  (clojure/fancify-symbols 'clojure-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Common Lisp                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "ros run")
  ;; (setq inferior-lisp-program "clisp")
  (setq slime-contribs '(slime-fancy)))

;; set C-c Meta-j run 'slime'
(define-key lisp-mode-map (kbd "C-c M-j") 'slime)
;; set C-c C-p eval and print result in buffer.
(define-key lisp-mode-map (kbd "C-c C-p") 'slime-eval-print-last-expression)

;; Using CLHS, no need for local hyper_spec
;; use C-c C-d h *name-of-function* to launch hyperspec
;; Local HyperSpec
;; (setq common-lisp-hyperspec-root (expand-file-name "~/.emacs.d/hyper_spec/HyperSpec/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Web                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	'(("django" . "\\.html\\'"))))

;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

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

(use-package restclient
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "/usr/bin/pandoc"))

(use-package easy-jekyll
  :ensure t
  :init
  (setq easy-jekyll-basedir "~/work/alexafshar.com")
  (setq easy-jekyll-url "http://alexafshar.com")
  (setq easy-jekyll-sshdomain "alexafshar.com")
  (setq easy-jekyll-root "/var/www/alexafshar.com"))

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
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



;; Skewer dependency
(use-package simple-httpd
  :ensure t)

;; (use-package skewer-mode
  ;; :ensure t
  ;; :config
  ;; (skewer-setup))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         C-CPP                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook
			(lambda ()
			  (local-set-key (kbd "<f5>") 'recompile)))
  (add-hook 'c-mode-hook
	    (lambda ()
	      (local-set-key (kbd "<f5>") 'recompile))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Rust                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Scheme                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package geiser
  :ensure t)

;; (with-eval-after-load 'geiser
  ;; (define-key geiser-mode-map (kbd "C-c C-c") 'scheme-send-last-sexp))


;; (setq scheme-program-name "csi -:c")
(setq scheme-program-name "guile")
(setq geiser-scheme-implementation "guile")

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Golang                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package company-go
  :ensure t)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-go))


;; (company-mode))))

;; (require 'company-go)
;; (add-hook 'go-mode-hook (lambda ()
			  ;; (shell-copy-environment-variable "GOPATH")
			  ;; (set (make-local-variable 'company-backends) '(company-go))))

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;;; (provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-quickhelp pos-tip which-key web-mode use-package try tide suggest smartparens slime seeing-is-believing rvm ruby-end rspec-mode robe restclient realgud rainbow-delimiters racer python-environment py-autopep8 projectile-rails paredit org-plus-contrib org-journal org-bullets ob-elixir neotree multi-term meghanada markdown-mode magit kotlin-mode java-snippets irony-eldoc imenu-list hydra groovy-mode gradle-mode google-c-style go-eldoc git-gutter-fringe geiser flycheck-rust flx expand-region exec-path-from-shell esup epc emmet-mode elpy elnode elmacro ein easy-jekyll drag-stuff dracula-theme diminish define-word debbugs dashboard counsel-projectile company-go company-anaconda cider cargo autodisass-java-bytecode all-the-icons alchemist ag ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
