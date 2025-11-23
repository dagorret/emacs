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

;; LSP para JS/TS/JSX/TSX
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'web-mode-hook
          (lambda ()
            (when (or (and buffer-file-name (string-suffix-p ".tsx" buffer-file-name))
                      (and buffer-file-name (string-suffix-p ".jsx" buffer-file-name)))
              (lsp-deferred))))

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

