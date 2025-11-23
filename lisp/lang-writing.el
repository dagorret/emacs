;;; lang-writing.el --- Markdown, LaTeX y herramientas de escritura -*- lexical-binding: t; -*-

;; ============================================================
;; MARKDOWN
;; ============================================================

(use-package markdown-mode
  :mode (("\\.md\\'"       . gfm-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  ;; Usar pandoc para convertir/preview
  (setq markdown-command "pandoc"))

;; ============================================================
;; LATEX (AUCTeX)
;; ============================================================

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t))

;; ============================================================
;; PDF-TOOLS (VER PDF DENTRO DE EMACS)
;; ============================================================

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

;; ============================================================
;; ORTOGRAFÍA (HUNSPELL) + FLYSPELL (ES_AR + EN_US)
;; ============================================================

;; Programa externo
(setq ispell-program-name "hunspell")

;; Diccionario por defecto: español (Argentina)
(setq ispell-dictionary "es_AR")

;; Cambiar diccionario a ES y EN de forma simple
(defun my/dict-spanish ()
  "Cambiar diccionario a español (es_AR)."
  (interactive)
  (ispell-change-dictionary "es_AR")
  (message "Diccionario cambiado a español (es_AR)"))

(defun my/dict-english ()
  "Cambiar diccionario a inglés (en_US)."
  (interactive)
  (ispell-change-dictionary "en_US")
  (message "Dictionary changed to English (en_US)"))

(global-set-key (kbd "C-c d s") #'my/dict-spanish)
(global-set-key (kbd "C-c d e") #'my/dict-english)

;; Flyspell en modos de texto y en comentarios de código
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :init
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil))

;; ============================================================
;; GRAMÁTICA (LANGUAGETOOL)
;; ============================================================

(use-package langtool
  :init
  (setq langtool-language-tool-jar "/opt/languagetool/languagetool-commandline.jar"
        langtool-default-language "es"
        langtool-mother-tongue "es")
  :bind (("C-c l c" . langtool-check)
         ("C-c l C" . langtool-check-done)
         ("C-c l n" . langtool-goto-next-error)
         ("C-c l p" . langtool-goto-previous-error)
         ("C-c l s" . langtool-show-message-at-point)))

;; ============================================================
;; YASNIPPET (SNIPPETS PARA LATEX, MARKDOWN, ETC.)
;; ============================================================

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; ============================================================
;; ATAJOS PARA MARKDOWN
;; ============================================================

(defun my/markdown-wrap (left right)
  "Rodear región o punto con LEFT y RIGHT."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert right)
        (goto-char beg)
        (insert left))
    (insert left right)
    (backward-char (length right))))

(defun my/markdown-bold ()
  "Negrita **texto**."
  (interactive)
  (my/markdown-wrap "**" "**"))

(defun my/markdown-italic ()
  "Cursiva *texto*."
  (interactive)
  (my/markdown-wrap "*" "*"))

(defun my/markdown-inline-code ()
  "Código en línea `code`."
  (interactive)
  (my/markdown-wrap "`" "`"))

(defun my/markdown-link ()
  "Insertar enlace [texto](url)."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end))
            (url (read-string "URL: ")))
        (let ((text (buffer-substring-no-properties beg end)))
          (delete-region beg end)
          (insert (format "[%s](%s)" text url))))
    (let ((text (read-string "Texto: "))
          (url (read-string "URL: ")))
      (insert (format "[%s](%s)" text url)))))

(defun my/markdown-setup-keys ()
  "Atajos de teclado para markdown-mode."
  (local-set-key (kbd "C-c b") #'my/markdown-bold)
  (local-set-key (kbd "C-c i") #'my/markdown-italic)
  (local-set-key (kbd "C-c `") #'my/markdown-inline-code)
  (local-set-key (kbd "C-c l") #'my/markdown-link))

(add-hook 'markdown-mode-hook #'my/markdown-setup-keys)

;; ============================================================
;; ATAJOS PARA LATEX
;; ============================================================

(defun my/latex-insert-environment (name)
  "Insertar entorno LaTeX \\begin{name} ... \\end{name}."
  (interactive "sEntorno: ")
  (insert (format "\\begin{%s}\n" name))
  (save-excursion
    (insert (format "\n\\end{%s}\n" name))))

(defun my/latex-equation ()
  (interactive)
  (my/latex-insert-environment "equation"))

(defun my/latex-itemize ()
  (interactive)
  (my/latex-insert-environment "itemize"))

(defun my/latex-figure ()
  (interactive)
  (my/latex-insert-environment "figure"))

(defun my/latex-table ()
  (interactive)
  (my/latex-insert-environment "table"))

(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c e") #'my/latex-equation)
  (define-key LaTeX-mode-map (kbd "C-c f") #'my/latex-figure)
  (define-key LaTeX-mode-map (kbd "C-c t") #'my/latex-table)
  (define-key LaTeX-mode-map (kbd "C-c i") #'my/latex-itemize))

;; ============================================================
;; EXPORTAR A DOCX CON PANDOC
;; ============================================================

(defun my/export-buffer-to-docx ()
  "Exportar el archivo actual a DOCX usando pandoc.
Funciona bien para Org y Markdown."
  (interactive)
  (unless buffer-file-name
    (user-error "El buffer no está asociado a un archivo"))
  (unless (executable-find "pandoc")
    (user-error "pandoc no está instalado"))
  (save-buffer)
  (let* ((in  (buffer-file-name))
         (out (concat (file-name-sans-extension in) ".docx")))
    (call-process "pandoc" nil "*Pandoc Output*" t in "-o" out)
    (message "Exportado a %s" out)))

(global-set-key (kbd "C-c e w") #'my/export-buffer-to-docx)

(provide 'lang-writing)
;;; lang-writing.el ends here

