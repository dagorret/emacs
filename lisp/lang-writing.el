;;; lang-writing.el --- Markdown y LaTeX -*- lexical-binding: t; -*-

;; ------------------------------
;; Markdown
;; ------------------------------
(use-package markdown-mode
  :mode (("\\.md\\'"       . gfm-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  ;; Si tenés pandoc instalado, se usa para previsualizar/conversión
  (setq markdown-command "pandoc"))

;; ------------------------------
;; LaTeX con AUCTeX
;; ------------------------------
(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t))

;; ------------------------------
;; PDF-tools (opcional pero útil)
;; ------------------------------
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(provide 'lang-writing)
;;; lang-writing.el ends here

