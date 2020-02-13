;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Alex Afshar"
      user-mail-address "contact@alexafshar.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Setup Scratch buffer for emacs-lisp experimentation.
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message "\
;; This is your Scratch buffer.
;; Write some Elisp here:
")

;; Add some aliases to eshell
;; TODO: I think that these are now included in doom. Should check the repo.
(add-hook 'eshell-mode-hook (lambda ()
                              (map! "C-l" "clear")
                              (eshell/alias "e" "find-file $1")
                              (eshell/alias "d" "dired $1")
                              (eshell/alias "g" 'magit-status)))

;; Add some extra filetypes to web-mode.
(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.svelte?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode)))

(add-hook 'before-save-hook 'gofmt-before-save)

;; Use EWW by default, this can be overriden with:
;; & (eww-browse-with-external-browser) when entering url.
(setq browse-url-browser-function 'eww-browse-url)

;; Map emmet expand to something other than tab, or it becomes annoying.
(after! emmet-mode
  (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line))

;; Common LISP Hyperspec local documentation, instead of browser.
(load! "/Users/evalapply/quicklisp/clhs-use-local.el" t)

(after! eshell-git-prompt
  (eshell-git-prompt-use-theme 'robbyrussell))

(add-hook! sly-mode
           #'lispyville-mode)
