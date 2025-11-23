;;; core-base.el --- Ajustes básicos de Emacs -*- lexical-binding: t; -*-

;;;Ocultar Warning
;;(setq native-comp-async-report-warnings-errors nil)

;; Sin pantalla de inicio ni mensajes molestos
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Silenciar campana
(setq ring-bell-function 'ignore)

;; UI más limpia
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Fuente un poco más grande (110 = 11pt aprox)
(set-face-attribute 'default nil :height 110)

;; Números de línea relativos
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Mostrar emparejamiento de paréntesis
(show-paren-mode 1)

;; Backups en carpeta separada, sin autosave por todos lados
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-default nil)

;; Usar espacios en vez de tabs
(setq-default indent-tabs-mode nil)

;; Autocerrar paréntesis, comillas, etc.
(electric-pair-mode 1)

;; ==========================================
;; CUA mode (Ctrl+C / Ctrl+V / Ctrl+X / Shift+flechas)
;; ==========================================
;(cua-mode 1)
;(setq cua-enable-cua-keys t)

(provide 'core-base)
;;; core-base.el ends here

