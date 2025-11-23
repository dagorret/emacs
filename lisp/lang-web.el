;;; lang-web.el --- Web development: JS, TS, CSS, HTML, Vue -*- lexical-binding: t; -*-

;; ============================================================
;; JAVASCRIPT & TYPESCRIPT
;; ============================================================

;; JS clásico
(use-package js
  :mode ("\\.js\\'" . js-mode))

;; TypeScript
(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode))

;; JSX / TSX (React, etc.) con web-mode
(use-package web-mode
  :mode (("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.jsx\\'")
          ("tsx" . "\\.tsx\\'"))))

;; JSX / TSX (React, etc.) + HTML con web-mode
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)   ;; <- HTML y HTM
         ("\\.jsx\\'"   . web-mode)
         ("\\.tsx\\'"   . web-mode))
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.jsx\\'")
          ("tsx" . "\\.tsx\\'"))))

;; ============================================================
;; CSS / SCSS
;; ============================================================

(use-package css-mode
  :mode ("\\.css\\'" . css-mode)
  :hook (css-mode . lsp-deferred))

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :hook (scss-mode . lsp-deferred))

;; ============================================================
;; HTML
;; ============================================================

(add-hook 'html-mode-hook #'lsp-deferred)

;; ============================================================
;; JSON
;; ============================================================

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :hook (json-mode . lsp-deferred))

;; ============================================================
;; VUE (Volar LSP)
;; ============================================================

(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode)
  :hook (vue-mode . lsp-deferred))

;; ============================================================
;; ALPINE.JS
;; ============================================================
;; Alpine no tiene LSP propio.
;; Se analiza con el LSP de JS/TS automáticamente en HTML/JS/TS/TSX.

(provide 'lang-web)
;;; lang-web.el ends here

