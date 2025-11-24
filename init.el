;;; init.el --- Config principal Emacs -*- lexical-binding: t; -*-

;; ------------------------------------------------------------
;; Repositorios de paquetes + use-package
;; ------------------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-check-signature nil)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; ------------------------------------------------------------
;; Carpeta lisp/ donde están los módulos propios
;; ------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; ------------------------------------------------------------
;; Cargar módulos principales (tu estructura actual)
;; ------------------------------------------------------------
(require 'core-base)
(require 'core-files)
(require 'core-ui)
(require 'core-dev)
(require 'lang-org)
(require 'core-notes)
(require 'lang-writing)
(require 'lang-prog)
(require 'lang-web)
(require 'lang-web-extras)

;; ------------------------------------------------------------
;; Plantillas de captura personales
;; ------------------------------------------------------------
(require 'org-capture-templates)

;; ------------------------------------------------------------
;; Personalizaciones automáticas de Emacs
;; ------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; Your init file should contain only one such instance.
 '(package-selected-packages
   '(php-mode rustic lsp-pyright python-mode yasnippet-snippets which-key
              vertico undo-tree projectile pdf-tools org-bullets
              orderless marginalia magit lsp-ui langtool flycheck
              corfu consult auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 )

(provide 'init)
;;; init.el ends here

