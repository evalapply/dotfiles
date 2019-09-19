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

;; LSP Debugger
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (require 'dap-elixir))

;; (when (display-graphic-p)
  ;; (progn
    ;; Show information (errors, function args, docs, etc) in buffer
    ;; (use-package lsp-ui
      ;; :ensure t
      ;; :config
      ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode))

    ;; Error list Tree view
    ;; (use-package lsp-treemacs
      ;; :ensure t)

    ;; Enable DAP UI
    ;; (dap-ui-mode 1)
    ;; )
  ;; )

;; LSP (Language Server Protocol)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Notes:
;;
;; For ruby:
;;    'gem install solargraph', lsp will auto detect
;; For elixir must build elixir-ls and add to exec-path
;; For JavaScript/Typescript:
;;    'npm i -g typescript-language-server; npm i -g typescript', lsp will auto detect
;; For Rust:
;;    'rustup update'
;;    'rustup component add rls rust-analysis rust-src', lsp will auto detect

;; Really minimal alternative to lsp-mode
;; (use-package eglot
;; :ensure t)

(use-package lsp-java
  :ensure t
  :after lsp
  :config
  (add-hook 'java-mode-hook 'lsp)
  (require 'lsp-java-boot)
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  )

(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :config
  (require 'lsp-mode)
  (require 'lsp-clients)
  ;; This essentially turns off lsp-ui-mode.
  ;; In Fullscreen mode it is causing error on mac,
  ;; and is in general annoying.
  (with-eval-after-load 'lsp-ui
      (setq lsp-ui-doc-enable nil
	lsp-ui-peek-enable nil
	lsp-ui-sideline-enable nil
	lsp-ui-imenu-enable nil
	lsp-ui-flycheck-enable t))
  :hook
  (elixir-mode . lsp)
  (ruby-mode . lsp)
  (rust-mode . lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
  (go-mode . lsp)
  (php-mode . lsp)
  :init
  ;; Elixir Language Server
  (add-to-list 'exec-path "~/work/thirdparty/elixir-ls/release")
  (add-to-list 'exec-path "~/work/code/go/bin"))

;;Company LSP enhancements
(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

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


(set-frame-font "Fira Code 18")
(setq default-frame-alist '((font . "Fira Code 18")))
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

(global-set-key (kbd "C-x ;") 'comment-line) ; This is needed in terminal
;; (global-set-key (kbd "C-x ,") 'recenter-top-bottom)
;;(global-set-key (kbd "C-.") 'asqrd/paste-below)
;;(global-set-key (kbd "C->") 'asqrd/paste-above)
;;(global-set-key (kbd "C-x ,") 'asqrd/copy-line)
;;(global-set-key (kbd "C-S-o") 'asqrd/insert-line-below)
;;(global-set-key (kbd "C-o") 'asqrd/insert-line-above)
;;(global-set-key (kbd "C-;") 'asqrd/jump-to-line-above)
;;(global-set-key (kbd "C-:") 'asqrd/jump-to-line-below)
;;(global-set-key (kbd "M-f") 'asqrd/move-forward)
;;(global-set-key (kbd "C-%") 'asqrd/goto-match-paren)
;;(global-set-key (kbd "M-'") 'asqrd/move-to-end-of-word)
;; (define-key company-active-map (kbd "C-c d") #'asqrd/company-show-doc-buffer)

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
;;                        Elixir                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elixir-mode
  :ensure t
  :config)

;; (use-package dap-elixir
  ;; :ensure t)


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
;;                          PHP                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package php-mode
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

(use-package sly
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Clojure                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#454545" "#d65946" "#6aaf50" "#baba36" "#598bc1" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   (quote
    (slime sly cider go-mode apropospriate-theme circadian flycheck-irony irony-eldoc irony-mode company-irony cquery flycheck-rust nord-theme nimbus-theme nimbus leuven-theme dracula-theme yaml-mode docker dockerfile-mode company-anaconda anaconda-mode company-lsp yasnippet-snippets yasnippet lsp-mode magit elmacro suggest git-gutter-fringe imenu-list all-the-icons neotree ag counsel-projectile projectile ace-window ivy flycheck company-quickhelp pos-tip company expand-region exec-path-from-shell which-key define-word realgud multi-term try esup drag-stuff paredit rainbow-delimiters smartparens use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
