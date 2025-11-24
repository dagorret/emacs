;;; lang-org.el --- Configuración de Org-mode -*- lexical-binding: t; -*-

;; ... comentarios ...

;; ============================================================
;; DIRECTORIO ORG PRINCIPAL
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
;; CONFIG ORG-MODE BASE
;; ============================================================

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode))
  :config
  (setq org-agenda-files
        (list my/org-inbox-file
              my/org-agenda-file
              my/org-notes-file
              my/org-projects-file))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-log-done 'time)

  (require 'ox-latex)
  (setq org-latex-compiler "pdflatex"))

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

