(use-package nimbus-theme)

(use-package dracula-theme)

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (load-theme 'doom-one t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;; (use-package apropospriate-theme
  ;; :defer)

;; (use-package circadian
;;   :config
;;   ;; (setq circadian-themes '(("8:00" . doom-nord-light)
;;   ;; 			   ("17:00" . doom-city-lights)))

;;   ;; (setq circadian-themes '(("8:00" . doom-opera-light)
;;   ;; 			   ("17:00" . doom-one)))

;;   ;; (setq circadian-themes '(("8:00" . doom-acario-light)
;;   ;; 			   ("17:00" . doom-vibrant)))

;;   ;; Low-contrast themes
;;   ;; (setq circadian-themes '((:sunrise . apropospriate-light) ; or doom-nord-light
;; 			   ;; (:sunset . doom-nord)))
;;   (circadian-setup))

;;(use-package doom-themes
;;  :ensure t
;;  :config
;;  ;; Global settings (defaults)
;;  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;        doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;  (load-theme 'doom-one t)
;;
;;  ;; Enable flashing mode-line on errors
;;  (doom-themes-visual-bell-config)
;;  ;; Enable custom neotree theme (all-the-icons must be installed!)
;;  (doom-themes-neotree-config)
;;  ;; or for treemacs users
;;  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;  (doom-themes-treemacs-config)
;;  ;; Corrects (and improves) org-mode's native fontification.
;;  (doom-themes-org-config))

(provide 'theme)
