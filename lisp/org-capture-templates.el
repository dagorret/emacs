;;; org-capture-templates.el --- Plantillas de captura de Carlos -*- lexical-binding: t; -*-

;; Este archivo asume que lang-org.el ya definió:
;;   my/org-inbox-file, my/org-agenda-file, my/org-notes-file, my/org-projects-file

(setq org-default-notes-file my/org-inbox-file)

(setq org-capture-templates
      '(("c" "Captura rápida (Inbox)" entry
         (file+headline my/org-inbox-file "Capturas")
         "* %?\n:CREATED: %U\n")

        ("a" "Artículo (Inbox)" entry
         (file+headline my/org-inbox-file "Artículos")
         "* Artículo: %^{Título}\n:SOURCE: %^{Fuente/URL}\n:CREATED: %U\n\n%?\n")

        ("n" "Nota (Inbox)" entry
         (file+headline my/org-inbox-file "Notas")
         "* %^{Título}\n:CREATED: %U\n\n%?\n")

        ("o" "Programación (Inbox)" entry
         (file+headline my/org-inbox-file "Programación")
         "* %^{Lenguaje|C|C++|Python|Elisp|Otro} %^{Título}\n:CREATED: %U\n\n%?\n")

        ;; Tareas y proyectos
        ("t" "Tarea" entry
         (file+headline my/org-agenda-file "Tareas")
         "* TODO %^{Descripción}\n:CREATED: %U\n\n%?")

        ("p" "Proyecto")
        ("pi" "Proyecto → Idea" entry
         (file+headline my/org-projects-file "Ideas")
         "* IDEA %^{Nombre}\n:CREATED: %U\n\n%?\n")

        ("pr" "Proyecto → Investigación" entry
         (file+headline my/org-projects-file "Investigación")
         "* INVESTIGACIÓN %^{Tema}\n:CREATED: %U\n\n%?\n")

        ("pp" "Proyecto (general)" entry
         (file+headline my/org-projects-file "Proyectos")
         "* PROYECTO %^{Nombre}\n:CREATED: %U\n\n%?\n")

        ;; Borradores y escritos
        ("b" "Borrador" entry
         (file+headline my/org-notes-file "Borradores")
         "* Borrador: %^{Título}\n:CREATED: %U\n\n%?\n")

        ("e" "Escrito / Artículo largo" entry
         (file+headline my/org-notes-file "Escritos")
         "* Escrito: %^{Título}\n:CREATED: %U\n\n%?\n")

        ;; Listas (URL, música, direcciones)
        ("l" "Listas")
        ("lu" "Lista → URL" entry
         (file+headline my/org-notes-file "URLs")
         "* %^{Etiqueta}\n:URL: %^{URL}\n:CREATED: %U\n\n%?\n")

        ("lm" "Lista → Música" entry
         (file+headline my/org-notes-file "Música")
         "* %^{Tema}\n:ARTISTA: %^{Artista}\n:CREATED: %U\n\n%?\n")

        ("ld" "Lista → Dirección" entry
         (file+headline my/org-notes-file "Direcciones")
         "* %^{Etiqueta}\n:DIRECCIÓN: %^{Dirección}\n:CREATED: %U\n\n%?\n")))

(provide 'org-capture-templates)
;;; org-capture-templates.el ends here

