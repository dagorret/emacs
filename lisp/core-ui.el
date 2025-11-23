;;; core-ui.el --- Interfaz y sistema de completado -*- lexical-binding: t; -*-

;; which-key: muestra sugerencias de atajos en la minibarra
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; undo-tree: historial de deshacer/rehacer más potente
(use-package undo-tree
  :init
  (global-undo-tree-mode))

;; ------------------------------
;; Completado “estilo Doom”
;; ------------------------------
;; vertico + orderless + marginalia + consult + corfu

;; Vertico: interfaz de selección en el minibuffer
(use-package vertico
  :init
  (vertico-mode))

;; Guardar historial de minibuffer
(use-package savehist
  :init (savehist-mode))

;; Orderless: permite escribir palabras en cualquier orden
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

;; Marginalia: información adicional en las listas de completado
(use-package marginalia
  :init (marginalia-mode))

;; Consult: búsquedas y comandos mejorados
(use-package consult
  :bind (("C-s" . consult-line)      ;; buscar en el buffer actual
         ("C-x b" . consult-buffer)  ;; cambiar de buffer
         ("M-y" . consult-yank-pop))) ;; historial de copiado

;; Corfu: completado en el buffer (como el popup de Doom)
(use-package corfu
  :init
  (global-corfu-mode))

(provide 'core-ui)
;;; core-ui.el ends here

