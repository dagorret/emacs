;;; lang-org.el --- Configuración de Org-mode -*- lexical-binding: t; -*-

;; Directorio principal de archivos ORG
(setq org-directory "~/org/")
(unless (file-exists-p org-directory)
  (make-directory org-directory t))

;; Archivos principales
(setq org-agenda-files (list (expand-file-name "agenda.org" org-directory))
      org-default-notes-file (expand-file-name "notas.org" org-directory))

;; Atajos útiles
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)

;; Ajustes visuales y comportamiento
(setq org-hide-emphasis-markers t      ;; *negrita* -> negrita sin mostrar *
      org-log-done 'time)              ;; timestamp cuando marcas DONE

;; Paquete extra opcional
(use-package org-contrib :after org)

;; Plantillas de captura
(setq org-capture-templates
      '(("t" "Tarea" entry
         (file+headline "agenda.org" "Tareas")
         "* TODO %?\n  %U\n")
        ("n" "Nota rápida" entry
         (file+headline "notas.org" "Notas")
         "* %?\n  %U\n")))

;; Bullets más lindos
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(provide 'lang-org)
;;; lang-org.el ends here

