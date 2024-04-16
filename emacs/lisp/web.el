(use-package web-mode
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
(use-package restclient)

;; Enhanced mode for markdown
(use-package markdown-mode
  :config
  (setq markdown-command "/usr/local/bin/pandoc"))

;; For my website/blog
;; (use-package easy-jekyll
;;   :init
;;   (setq easy-jekyll-basedir "~/work/alexafshar.com")
;;   (setq easy-jekyll-url "http://alexafshar.com")
;;   (setq easy-jekyll-sshdomain "alexafshar.com")
;;   (setq easy-jekyll-root "/var/www/alexafshar.com"))

;; Async web server
(use-package elnode)

(provide 'web)
