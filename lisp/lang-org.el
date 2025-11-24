;;; lang-org.el --- Configuración de Org-mode -*- lexical-binding: t; -*-
;;
;; Configura:
;;   - Directorio ~/org
;;   - Archivos: inbox, agenda, notas, proyectos
;;   - Agenda + TODO
;;   - Exportación: HTML, Markdown, PDF/LaTeX, ODT, Pandoc (si está)
;;   - Selector de CSS (default, dark, light, academic, print)
;;   - HTML a archivo (C-c e h) y HTML temporal en navegador (C-c e v)
;;   - org-bullets

;; ============================================================
;; DIRECTORIO ORG
;; ============================================================

(setq my/org-directory (expand-file-name "~/org/"))
(unless (file-directory-p my/org-directory)
  (make-directory my/org-directory t))

(setq my/org-inbox-file     (expand-file-name "inbox.org"     my/org-directory))
(setq my/org-agenda-file    (expand-file-name "agenda.org"    my/org-directory))
(setq my/org-notes-file     (expand-file-name "notas.org"     my/org-directory))
(setq my/org-projects-file  (expand-file-name "proyectos.org" my/org-directory))

(dolist (file (list my/org-inbox-file my/org-agenda-file my/org-notes-file my/org-projects-file))
  (unless (file-exists-p file)
    (with-temp-buffer (write-file file))))

;; ============================================================
;; ORG-MODE BASE + EXPORTACIÓN
;; ============================================================

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode))
  :config

  ;; Archivos de agenda
  (setq org-agenda-files
        (list my/org-inbox-file
              my/org-agenda-file
              my/org-notes-file
              my/org-projects-file))

  ;; Keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Visual
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-log-done 'time)

  ;; Export PDF
  (require 'ox-latex)
  (setq org-latex-compiler "pdflatex")

  ;; Export Markdown / HTML / ODT
  (require 'ox-md)
  (require 'ox-html)
  (require 'ox-odt)

  ;; Pandoc si está
  (when (locate-library "ox-pandoc")
    (require 'ox-pandoc))

  ;; ========================================================
  ;; SELECTOR DE CSS PARA EXPORTACIÓN HTML
  ;; ========================================================

  (defvar my/org-html-css-styles
    '(("default"  . "org.css")
      ("dark"     . "org-dark.css")
      ("light"    . "org-light.css")
      ("academic" . "org-academic.css")
      ("print"    . "org-print.css"))
    "Lista de estilos CSS disponibles para exportar a HTML.")

  (defvar my/org-html-css-last-style "academic"
    "Último estilo CSS seleccionado al exportar HTML.")

  (defun my/org-read-css-style ()
    "Elegir estilo CSS desde el minibuffer."
    (let* ((names (mapcar #'car my/org-html-css-styles))
           (choice (completing-read
                    "CSS style: "
                    names nil t nil nil my/org-html-css-last-style))
           (entry (assoc choice my/org-html-css-styles)))
      (setq my/org-html-css-last-style choice)
      (cdr entry)))

  ;; ========================================================
  ;; EXPORTAR HTML A ARCHIVO CON CSS
  ;; ========================================================

  (defun my/org-export-html-to-file-with-css ()
    "Exportar el Org actual a HTML en la misma carpeta, con CSS elegido."
    (interactive)
    (let* ((css-file (my/org-read-css-style))
           (css-path (and css-file (expand-file-name css-file my/org-directory)))
           (org-html-head-include-default-style nil)
           (org-html-head
            (when (and css-path (file-exists-p css-path))
              (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"file:%s\" />"
                      css-path))))
      (org-html-export-to-html)))

  ;; ========================================================
  ;; EXPORTAR HTML TEMPORAL + ABRIR EN NAVEGADOR
  ;; ========================================================

  (defun my/org-export-view-temp-html ()
    "Exportar el Org actual a un HTML 'temporal' en ~/org/_html-tmp/ y abrirlo en el navegador."
    (interactive)
    (let* ((css-file (my/org-read-css-style))
           (css-path (and css-file (expand-file-name css-file my/org-directory)))
           (org-html-head-include-default-style nil)
           (org-html-head
            (when (and css-path (file-exists-p css-path))
              (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"file:%s\" />"
                      css-path)))
           ;; directorio 'temporal' pero dentro de tu HOME
           (tmp-dir (expand-file-name "_html-tmp" my/org-directory)))
      ;; Crear carpeta si no existe
      (unless (file-directory-p tmp-dir)
        (make-directory tmp-dir t))
      ;; Nombre de archivo “temporal” pero legible
      (let* ((tmp-file (expand-file-name
                        (format "org-tmp-%s.html"
                                (format-time-string "%Y%m%d-%H%M%S"))
                        tmp-dir)))
        ;; Exportar
        (org-export-to-file 'html tmp-file
          nil nil nil nil nil)
        ;; Abrir en navegador
        (browse-url-of-file tmp-file)
        (message "HTML temporal generado: %s" tmp-file)))))

;; ============================================================
;; MEJORAS VISUALES
;; ============================================================

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; ============================================================
;; ATAJOS
;; ============================================================

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; HTML a archivo con CSS
(global-set-key (kbd "C-c e h") #'my/org-export-html-to-file-with-css)

;; HTML temporal en navegador
(global-set-key (kbd "C-c e v") #'my/org-export-view-temp-html)

(provide 'lang-org)
;;; lang-org.el ends here

