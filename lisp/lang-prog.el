;;; lang-prog.el --- Lenguajes de programación -*- lexical-binding: t; -*-

;; ============================================================
;; PYTHON
;; ============================================================
;; Recomendado en el sistema:
;;   sudo apt install python3-venv python3-pip
;;   npm install -g pyright
;;
;; Pyright: servidor LSP para Python, integrado vía lsp-pyright.

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3"))

(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; ============================================================
;; RUST
;; ============================================================
;; Ya tenés rustc y cargo. Faltaría:
;;   rustup component add rust-analyzer
;;
;; rustic integra muy bien con lsp-mode y rust-analyzer.
;;; ======================
;;; RUST
;;; ======================

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook ((rustic-mode . lsp-deferred))
  :config
  ;; Formatear automáticamente con rustfmt al guardar
  (setq rustic-format-on-save t))

;; Para tree-sitter si tu Emacs 29+ ya lo trae
(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust")))


;; ============================================================
;; C / C++
;; ============================================================
;; Recomendado en el sistema:
;;   sudo apt install clangd
;;
;; lsp-mode se encarga de hablar con clangd.

(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

;; ============================================================
;; PHP
;; ============================================================
;; Recomendado en el sistema:
;;   sudo apt install php-cli npm
;;   npm install -g intelephense
;;
;; lsp-mode puede usar intelephense si está en el PATH.

(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . lsp-deferred))

;; ============================================================
;; BASH / SHELL
;; ============================================================
;; Si querés LSP para Bash:
;;   npm install -g bash-language-server

(add-hook 'sh-mode-hook #'lsp-deferred)

(provide 'lang-prog)
;;; lang-prog.el ends here

