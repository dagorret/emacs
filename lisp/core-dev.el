;;; core-dev.el --- Herramientas de desarrollo -*- lexical-binding: t; -*-

;; ============================================================
;; MAGIT: interfaz Git dentro de Emacs
;; ============================================================

(use-package magit
  :commands (magit-status)
  :bind ("C-x g" . magit-status))

;; ============================================================
;; PROJECTILE: gestión de proyectos
;; ============================================================

(use-package projectile
  :diminish
  :init
  ;; Carpetas donde Projectile busca proyectos
  (setq projectile-project-search-path '("~/proyectos/"))
  :config
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; ============================================================
;; LSP-MODE: “IDE” para varios lenguajes
;; ============================================================

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
  ;; Prefijo de teclas para LSP
  (setq lsp-keymap-prefix "C-c l")
  ;; 🔽 Estos ajustes reducen prompts molestos al crear archivos nuevos
  (setq lsp-enable-file-watchers nil      ; no vigiles el FS agresivamente
        lsp-file-watch-threshold 20000    ; por si se vuelve a activar
        lsp-auto-guess-root t             ; intenta detectar raíz del proyecto
        lsp-log-io nil)                   ; no llenar el log con ruido
  :config
  (lsp-enable-which-key-integration t))

;; Interfaz gráfica extra para LSP (hover, doc, etc.)
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

;; ============================================================
;; FLYCHECK: chequeo de sintaxis en tiempo real
;; ============================================================

(use-package flycheck
  :init
  (global-flycheck-mode))

;; ============================================================
;; TREE-SITTER (Emacs 29+)
;; ============================================================

(setq treesit-font-lock-level 3)

(provide 'core-dev)
;;; core-dev.el ends here

