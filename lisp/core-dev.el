;;; core-dev.el --- Configuración general de desarrollo -*- lexical-binding: t; -*-

;; LSP, Flycheck, Magit, Projectile, etc.
;; Este módulo complementa lang-prog.el y lang-web.el

;; ============================================================
;; LSP MODE
;; ============================================================

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp-deferred)
         (c-mode      . lsp-deferred)
         (c++-mode    . lsp-deferred)
         (rust-mode   . lsp-deferred)
         (rustic-mode . lsp-deferred)
         (php-mode    . lsp-deferred)
         (sh-mode     . lsp-deferred))
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t))

;; ============================================================
;; FLYCHECK
;; ============================================================

(use-package flycheck
  :init (global-flycheck-mode))

;; ============================================================
;; PROYECTOS Y GIT
;; ============================================================

(use-package projectile
  :init (projectile-mode +1)
  :custom (projectile-project-search-path '("~/proyectos/")))

(use-package magit
  :commands magit-status)

;; ============================================================
;; COMPLETADO
;; ============================================================

(use-package corfu
  :init (global-corfu-mode))

(use-package which-key
  :init (which-key-mode))

(provide 'core-dev)
;;; core-dev.el ends here

