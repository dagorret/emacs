;;; lang-org.el --- Configuración de Org-mode -*- lexical-binding: t; -*-
;; 
;; Este archivo configura:
;;   - Directorio base: ~/org/
;;   - Archivos: agenda, notas, inbox, proyectos
;;   - Capturas rápidas (tareas, notas, eventos)
;;   - Agenda (calendario + TODOs)
;;   - Soporte visual mejorado (org-bullets)
;;   - Exportación a PDF/LaTeX
;;
;; Nota: org-roam se configura en core-notes.el

;; ============================================================
;; DIRECTORIO ORG PRINCIPAL
;; ============================================================

(setq my/org-directory (expand-file-name "~/org/"))
(unless (file-directory-p my/org-directory)
  (make-directory my/org-directory t))

;; Archivos principales
(setq my/org-inbox-file     (expand-file-name "inbox.org"     my/org-directory))
(setq my/org-agenda-file    (expand-file-name "agenda.org"    my/org-directory))
(setq my/org-notes-file     (expand-file-name "notas.org"     my/org-directory))
(setq my/org-projects-file  (expand-file-name "proyectos.org" my/org-directory))

;; Crear si no existen
(dolist (file (list my/org-inbox-file my/org-agenda-file my/org-notes-file my/org-projects-file))
  (unless (file-exists-p file)
    (with-temp-buffer (write-file file))))

;; ============================================================
;; CONFIG ORG-MODE BASE
;; ============================================================

(use-package org
  :ensure nil   ;; Viene incluido en Emacs
  :hook ((org-mode . visual-line-mode))
  :config
  ;; Archivos que la agenda mira
  (setq org-agenda-files
        (list my/org-inbox-file
              my/org-agenda-file
              my/org-notes-file
              my/org-projects-file))

  ;; Keywords de tareas
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Visuales
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)

  ;; Log de tareas completadas
  (setq org-log-done 'time)

  ;; Exportación a LaTeX/PDF
  (require 'ox-latex)
  (setq org-latex-compiler "pdflatex"))

;; ============================================================
;; CAPTURAS RÁPIDAS (C-c c)
;; ============================================================

(setq org-default-notes-file my/org-inbox-file)

(setq org-capture-templates
      `(
        ("i" "Inbox (nota rápida)" entry
         (file ,my/org-inbox-file)
         "* %?\n  %U\n")

        ("t" "Tarea" entry
         (file ,my/org-agenda-file)
         "* TODO %?\n  %U\n")

        ("p" "Proyecto" entry
         (file ,my/org-projects-file)
         "* %^{Nombre del proyecto}\n%?\n  %U\n")

        ("e" "Evento (calendario)" entry
         (file ,my/org-agenda-file)
         "* %?\n  SCHEDULED: %^T\n  %U\n  :PROPERTIES:\n  :LOCATION: %^{Lugar}\n  :END:\n")
        ))

;; ============================================================
;; MEJORAS VISUALES
;; ============================================================

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; ============================================================
;; ATAJOS GLOBALES
;; ============================================================

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(provide 'lang-org)
;;; lang-org.el ends here

