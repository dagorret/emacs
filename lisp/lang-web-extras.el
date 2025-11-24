;;; lang-web-extras.el --- Herramientas extra para desarrollo web -*- lexical-binding: t; -*-

;; Prettier, ESLint integrados con Flycheck

;; ============================================================
;; PRETTIER (AUTOFORMATEO)
;; ============================================================

(when (locate-library "prettier-js")
  (use-package prettier-js
    :hook ((js-mode         . prettier-js-mode)
           (typescript-mode . prettier-js-mode)
           (web-mode        . prettier-js-mode)
           (json-mode       . prettier-js-mode)
           (scss-mode       . prettier-js-mode)
           (css-mode        . prettier-js-mode)
           (vue-mode        . prettier-js-mode))
    :config
    (setq prettier-js-command "prettier")))

;; ============================================================
;; ESLINT PARA FLYCHECK
;; ============================================================

(when (locate-library "flycheck")
  (eval-after-load 'flycheck
    '(progn
       (when (executable-find "eslint")
         (setq-default flycheck-javascript-eslint-executable "eslint")))))

;; Stylelint lo dejaríamos para uso manual desde terminal o
;; más adelante con un checker personalizado, para no romper nada.

(provide 'lang-web-extras)
;;; lang-web-extras.el ends here

