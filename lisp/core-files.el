;;; core-files.el --- Gestión de directorios y archivos -*- lexical-binding: t; -*-

;; ============================================================
;; DIRED: gestor de archivos dentro de Emacs
;; ============================================================

(use-package dired
  :ensure nil                      ;; dired viene con Emacs
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)) ;; ir al directorio del archivo actual
  :config
  ;; Listado más cómodo: tamaños humanos, etc.
  (setq dired-listing-switches "-alh"
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-kill-when-opening-new-dired-buffer t)

  ;; ------------------------------------------------------------
  ;; Función: crear directorio con un prompt simple
  ;; ------------------------------------------------------------
  (defun my/dired-create-directory ()
    "Crear un nuevo directorio en el Dired actual, con un prompt claro."
    (interactive)
    (let ((name (read-from-minibuffer "Nuevo directorio: ")))
      (dired-create-directory name)
      (revert-buffer))) ;; refrescar listado

  ;; ------------------------------------------------------------
  ;; Función: crear archivo vacío con un prompt simple
  ;; ------------------------------------------------------------
  (defun my/dired-create-empty-file ()
    "Crear un archivo vacío en el Dired actual."
    (interactive)
    (let ((name (read-from-minibuffer "Nuevo archivo: ")))
      (dired-create-empty-file name)
      (revert-buffer)))

  ;; ------------------------------------------------------------
  ;; Atajos simples dentro de Dired
  ;; ------------------------------------------------------------
  ;; F6: nuevo archivo
  ;; F7: nuevo directorio
  (define-key dired-mode-map (kbd "<f6>") #'my/dired-create-empty-file)
  (define-key dired-mode-map (kbd "<f7>") #'my/dired-create-directory))

;; ============================================================
;; TREEMACS: explorador de archivos moderno
;; ============================================================

(use-package treemacs
  :bind
  (("<f8>"        . treemacs)          ;; abrir/cerrar
   ("C-c n n"     . treemacs))         ;; alternativa
  :config
  (setq treemacs-width 35))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after treemacs
  :hook (dired-mode . treemacs-icons-dired-mode))

(provide 'core-files)
;;; core-files.el ends here

