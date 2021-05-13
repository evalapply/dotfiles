;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Afshar"
      user-mail-address "contact@alexafshar.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Fira Code" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Split windows right and bottom
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Prompt for buffer on window split
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

;; Show buffer preview
(setq +ivy-buffer-preview t)

;; Default mode elisp
(setq-default major-mode 'emacs-lisp-mode)

(use-package! paredit
              :hook (clojure-mode . paredit-mode))

;; (setq-hook! clojure-mode paredit-mode)

;; Clojure mode opens in emacs modee
(add-to-list 'evil-emacs-state-modes 'clojure-mode)

(after! rustic
  setq lsp-rust-server 'rust-analyzer)

;; LiveView Sigil L syntax highlighting
(require 'mmm-mode)
(require 'web-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-parse-when-idle 't)
(setq mmm-set-file-name-for-modes '(web-mode))
(custom-set-faces '(mmm-default-submode-face ((t (:background nil)))))
(let ((class 'elixir-eex)
    (submode 'web-mode)
    (front "^[ ]+~L\"\"\"")
    (back "^[ ]+\"\"\""))
  (mmm-add-classes (list (list class :submode submode :front front :back back)))
  (mmm-add-mode-ext-class 'elixir-mode nil class))

(define-advice web-mode-guess-engine-and-content-type (:around (f &rest r) guess-engine-by-extension)
  (if (and buffer-file-name (equal "ex" (file-name-extension buffer-file-name)))
      (progn (setq web-mode-content-type "html")
         (setq web-mode-engine "elixir")
         (web-mode-on-engine-setted))
    (apply f r)))
