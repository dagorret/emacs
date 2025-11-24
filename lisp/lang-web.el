;;; lang-web.el --- Configuración para desarrollo web -*- lexical-binding: t; -*-

;; HTML, CSS, JS, TS, JSON, Vue

;; ============================================================
;; WEB-MODE (plantillas, HTML, JSX, TSX)
;; ============================================================

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.jsx?\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset   2
        web-mode-css-indent-offset    2))

;; ============================================================
;; CSS / SCSS
;; ============================================================

(use-package css-mode
  :ensure nil
  :mode ("\\.css\\'" . css-mode)
  :hook (css-mode . lsp-deferred))

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :hook (scss-mode . lsp-deferred)
  :config
  (setq scss-compile-at-save nil))

;; ============================================================
;; HTML
;; ============================================================

(add-hook 'html-mode-hook #'lsp-deferred)

;; ============================================================
;; JS / TS
;; ============================================================

(use-package js-mode
  :ensure nil
  :mode ("\\.js\\'" . js-mode)
  :hook (js-mode . lsp-deferred))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; ============================================================
;; JSON
;; ============================================================

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :hook (json-mode . lsp-deferred))

;; ============================================================
;; VUE
;; ============================================================

(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode)
  :hook (vue-mode . lsp-deferred))

(provide 'lang-web)
;;; lang-web.el ends here

