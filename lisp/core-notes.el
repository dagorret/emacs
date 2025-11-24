;;; core-notes.el --- Sistema de notas con Org-roam y diario -*- lexical-binding: t; -*-
;; 
;; Este módulo complementa lang-org.el y configura:
;;   - Org-roam (Second Brain estilo Obsidian)
;;   - Directorio ~/notas/roam
;;   - Diario (daily notes)
;;   - Capturas para notas, ideas, proyectos
;;   - Búsqueda rápida consult-ripgrep
;;   - Teclas globales C-c n ...
;;
;; Todo está pensado para ser 100% compatible con tu agenda y tu flujo de estudio.

;; ============================================================
;; DIRECTORIO PRINCIPAL DE ORG-ROAM
;; ============================================================

(setq my/org-roam-directory (file-truename "~/notas/roam/"))

(unless (file-directory-p my/org-roam-directory)
  (make-directory my/org-roam-directory t))

;; ============================================================
;; CONFIGURACIÓN ORG-ROAM
;; ============================================================

(use-package org-roam
  :init
  ;; Evita el mensaje “acknowledge v2”
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory my/org-roam-directory)
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)   ;; Backlinks panel
         ("C-c n f" . org-roam-node-find)       ;; Buscar nota
         ("C-c n i" . org-roam-node-insert)     ;; Insertar enlace
         ("C-c n c" . org-roam-capture)         ;; Capturar nota nueva
         ("C-c n g" . org-roam-graph))          ;; Grafo visual

  :config
  ;; Base de datos siempre al día
  (org-roam-db-autosync-mode)

  ;; ========================================================
  ;; TEMPLATES PARA CAPTURE (Org-roam)
  ;; ========================================================

  (setq org-roam-capture-templates
        '(("n" "Nota general" plain
           "* %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n\n")
           :unnarrowed t)

          ("i" "Idea puntual" plain
           "* Idea\n%?\n\n* Contexto\n"
           :if-new (file+head "ideas/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: Idea — ${title}\n#+date: %U\n\n")
           :unnarrowed t)

          ("p" "Proyecto" plain
           "* Objetivo\n%?\n\n* Estado\n- TODO\n\n* Notas\n\n"
           :if-new (file+head "proyectos/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: Proyecto — ${title}\n#+category: proyecto\n#+date: %U\n\n")
           :unnarrowed t)))

  ;; ========================================================
  ;; DAILY NOTES (DIARIO)
  ;; ========================================================

  (setq org-roam-dailies-directory "diario/")

  ;; Plantilla para el día
  (setq org-roam-dailies-capture-templates
        '(("d" "Diario" entry
           "* Hoy\n\n- %?\n\n* Notas\n\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: Diario %<%Y-%m-%d>\n#+date: %U\n\n"))))

  ;; Atajos del diario
  (global-set-key (kbd "C-c n d") #'org-roam-dailies-goto-today)
  (global-set-key (kbd "C-c n j") #'org-roam-dailies-capture-today))

;; ============================================================
;; BUSQUEDA GLOBAL EN NOTAS
;; ============================================================

(use-package consult
  :bind (("C-c s r" . consult-ripgrep))) ;; Buscar en todas las notas

(provide 'core-notes)
;;; core-notes.el ends here

