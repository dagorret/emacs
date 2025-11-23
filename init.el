;;; init.el --- Config principal Emacs -*- lexical-binding: t; -*-

;; ------------------------------------------------------------
;; Repos de paquetes + use-package
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

;; Carpeta lisp/ donde están los módulos
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Cargar módulos (los crearemos en los próximos pasos)
(require 'core-base)
(require 'core-ui)
(require 'core-dev)
(require 'lang-org)
(require 'lang-writing)
(require 'lang-prog)

(provide 'init)
;;; init.el ends here

