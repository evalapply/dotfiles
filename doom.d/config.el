;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Fira Code" :size 14))

(setq-default c-basic-offset tab-width)

(add-hook 'php-mode-hook 'lsp)

(add-hook 'eshell-mode-hook (lambda ()
                              (eshell/alias "e" "find-file $1")
                              (eshell/alias "d" "dired $1")
                              (eshell/alias "g" 'magit-status)))

;; (global-set-key (kbd "C-x e") 'eshell)
