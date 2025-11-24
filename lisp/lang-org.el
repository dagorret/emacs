;;; lang-org.el --- Configuración de Org-mode -*- lexical-binding: t; -*-
;;
;; Este archivo configura:
;;   - Directorio base: ~/org/
;;   - Archivos: agenda, notas, inbox, proyectos
;;   - Agenda (calendario + TODOs)
;;   - Exportación a PDF/LaTeX, HTML, Markdown, ODT
;;   - Funciones para exportar HTML temporal y HTML con CSS
;;   - Soporte visual (org-bullets)
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
  :ensure nil   ;; Viene con Emacs
  :hook ((org-mode . visual-line-mode))
  :config
  ;; Archivos que mira la agenda
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

  ;; ========================================================
  ;; EXPORTACIÓN: LaTeX / PDF
  ;; ========================================================
  (require 'ox-latex)
  (setq org-latex-compiler "pdflatex")

  ;; ========================================================
  ;; EXPORTACIÓN: Markdown, HTML, ODT, (opcional Pandoc)
  ;; ========================================================
  (require 'ox-md)    ;; C-c C-e m m  → Markdown
  (require 'ox-html)  ;; C-c C-e h h  → HTML
  (require 'ox-odt)   ;; C-c C-e o o  → ODT

  ;; Si tenés ox-pandoc instalado, también activamos exportación vía Pandoc
  (when (locate-library "ox-pandoc")
    (require 'ox-pandoc))

  ;; ========================================================
  ;; HTML + CSS: configuración general
  ;; ========================================================

  ;; Archivo CSS por defecto para exportación HTML.
  ;; Podés crear este archivo en ~/org/org.css y poner ahí tus estilos.
  (defvar my/org-html-css-file
    (expand-file-name "org.css" my/org-directory)
    "Ruta al CSS que se usará para exportar Org a HTML.")

  ;; Función: exportar a HTML en la misma carpeta que el .org, usando CSS si existe.
  (defun my/org-export-html-to-file-with-css ()
    "Exportar el buffer Org actual a HTML en el mismo directorio, enlazando org.css si existe.
El HTML se genera con `org-html-export-to-html`."
    (interactive)
    (let* ((org-html-head-include-default-style nil)
           (org-html-head
            (when (file-exists-p my/org-html-css-file)
              (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"file:%s\" />"
                      my/org-html-css-file))))
      (org-html-export-to-html)))

  ;; Función: exportar a un HTML temporal y abrir en el navegador.
  (defun my/org-export-view-temp-html ()
    "Exportar el buffer Org actual a un HTML temporal y abrirlo en el navegador."
    (interactive)
    (let* ((tmp-file (make-temp-file "org-export-" nil ".html"))
           ;; Exportar usando el backend HTML
           (org-export-show-temporary-export-buffer nil))
      (org-export-to-file 'html tmp-file
        nil nil nil nil nil)
      (browse-url-of-file tmp-file))))

;; ============================================================
;; MEJORAS VISUALES
;; ============================================================

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; ============================================================
;; ATAJOS GLOBALES
;; ============================================================

;; Agenda y Capture
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Exportar HTML con CSS (archivo al lado del .org)
(global-set-key (kbd "C-c e h") #'my/org-export-html-to-file-with-css)

;; Ver HTML temporal en el navegador
(global-set-key (kbd "C-c e v") #'my/org-export-view-temp-html)

(provide 'lang-org)
;;; lang-org.el ends here

