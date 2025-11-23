;;; lang-web-extras.el --- Herramientas extra para desarrollo web -*- lexical-binding: t; -*-

;; ============================================================
;; TAILWIND CSS LSP
;; ============================================================
;; Requiere: npm install -g @tailwindcss/language-server

(use-package lsp-tailwindcss
  :after lsp-mode
  :hook ((html-mode web-mode css-mode typescript-mode js-mode vue-mode)
         . lsp-tailwindcss-mode)
  :config
  (setq lsp-tailwindcss-add-on-mode t))

;; ============================================================
;; PRETTIER (AUTOFORMATO)
;; ============================================================
;; Requiere: npm install -g prettier

(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (scss-mode . prettier-js-mode)
         (vue-mode . prettier-js-mode))
  :config
  (setq prettier-js-command "prettier"))

;; ============================================================
;; ESLINT / STYLELINT (integrados con Flycheck)
;; ============================================================
;; Requiere:
;;   npm install -g eslint
;;   npm install -g stylelint

(with-eval-after-load 'flycheck
  ;; ESLint para JS/TS
  (when (executable-find "eslint")
    (setq-default flycheck-javascript-eslint-executable "eslint")
    (flycheck-add-mode 'javascript-eslint 'js-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-mode))

  ;; Stylelint para CSS/SCSS
  (when (executable-find "stylelint")
    (flycheck-define-checker stylelint
      "A CSS/Sass checker using stylelint."
      :command ("stylelint" "--formatter" "json" source)
      :error-parser flycheck-parse-stylelint
      :modes (css-mode scss-mode less-css-mode))
    (add-to-list 'flycheck-checkers 'stylelint)))

;; ============================================================
;; EMMET (EXPANSIÓN RÁPIDA HTML/CSS)
;; ============================================================
;; Escribir: div.container>ul>li*3 y expandir.

(use-package emmet-mode
  :hook ((html-mode . emmet-mode)
         (css-mode . emmet-mode)
         (web-mode . emmet-mode)
         (vue-mode . emmet-mode))
  :config
  (setq emmet-self-closing-tag-style " /"))

;; ============================================================
;; HELPERS PARA VUE Y ALPINE
;; ============================================================

(defun my/vue-insert-sfc ()
  "Insertar skeleton básico de Single File Component Vue 3."
  (interactive)
  (insert "<template>\n  <div class=\"\">\n  </div>\n</template>\n\n")
  (insert "<script setup>\n\n</script>\n\n")
  (insert "<style scoped>\n\n</style>\n")
  (message "Vue SFC insertado."))

(defun my/alpine-insert-component ()
  "Insertar un bloque básico Alpine.js x-data."
  (interactive)
  (insert "<div x-data=\"{ open: false }\">\n")
  (insert "  <button @click=\"open = !open\">Toggle</button>\n")
  (insert "  <div x-show=\"open\">\n")
  (insert "    Contenido...\n")
  (insert "  </div>\n")
  (insert "</div>\n")
  (message "Componente básico Alpine insertado."))

(defun my/web-setup-framework-keys ()
  "Keybindings para Vue/Alpine en modos web."
  (local-set-key (kbd "C-c v c") #'my/vue-insert-sfc)
  (local-set-key (kbd "C-c a c") #'my/alpine-insert-component))

(add-hook 'web-mode-hook #'my/web-setup-framework-keys)
(add-hook 'html-mode-hook #'my/web-setup-framework-keys)
(add-hook 'vue-mode-hook #'my/web-setup-framework-keys)

(provide 'lang-web-extras)
;;; lang-web-extras.el ends here
