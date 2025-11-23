;;; core-dev.el --- Herramientas de desarrollo -*- lexical-binding: t; -*-

;; Magit: interfaz Git dentro de Emacs
(use-package magit
  :commands (magit-status)
  :bind ("C-x g" . magit-status))

;; Projectile: gestión de proyectos
(use-package projectile
  :diminish
  :config
  (projectile-mode 1)
  ;; Ajustá esta ruta si usás otra carpeta para tus proyectos
  (setq projectile-project-search-path '("~/proyectos/"))
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; LSP para modo “IDE”
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (c-mode      . lsp-deferred)
         (c++-mode    . lsp-deferred)
         (rust-mode   . lsp-deferred)
         (rustic-mode . lsp-deferred)
         (php-mode    . lsp-deferred)
         (sh-mode     . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

;; Flycheck: chequeo de sintaxis en tiempo real
(use-package flycheck
  :init (global-flycheck-mode))

;; Ajustes de árboles de sintaxis (tree-sitter en Emacs 29)
(setq treesit-font-lock-level 3)

(provide 'core-dev)
;;; core-dev.el ends here

