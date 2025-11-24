;;; lang-prog.el --- Lenguajes de programación -*- lexical-binding: t; -*-
;;; 
;;; Este archivo configura los lenguajes “de código” que uso en Emacs:
;;; - Python  : LSP con Pyright
;;; - Rust    : rustic + rust-analyzer + tree-sitter
;;; - PHP     : php-mode + LSP (phpactor/intelephense)
;;; - C / C++ : clangd + clang-format + atajo para compilar/ejecutar
;;; - Shell   : bash con LSP
;;;
;;; Nota para mi yo del futuro:
;;; - Toda la parte “IDE” (LSP, UI, etc.) está en core-dev/core-ui.
;;; - Este archivo sólo define por-lenguaje: modo mayor, hooks, formato, etc.

;; ============================================================
;; PYTHON
;; ============================================================
;; Requisitos en el sistema:
;;   sudo apt install python3-venv python3-pip
;;   npm install -g pyright
;;
;; Pyright funciona como servidor LSP y se integra vía lsp-pyright.

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3"))

(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; ============================================================
;; RUST
;; ============================================================
;; Requisitos en el sistema:
;;   - Instalar rustup:
;;       curl https://sh.rustup.rs -sSf | sh
;;   - Agregar rust-analyzer:
;;       rustup component add rust-analyzer
;;
;; rustic integra rust-analyzer con lsp-mode y añade helpers.
;; Además, activamos tree-sitter para Rust (Emacs 29+).

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp-deferred)
  :config
  ;; Formatear automáticamente con rustfmt al guardar (cargo fmt).
  (setq rustic-format-on-save t))

;; Para tree-sitter si Emacs 29+ ya lo trae (no rompe en versiones anteriores).
(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust")))

;; ============================================================
;; PHP
;; ============================================================
;; Requisitos en el sistema:
;;   - PHP CLI (apt install php-cli)
;;   - Composer (apt install composer)
;;   - Algún servidor LSP:
;;       * phpactor  (recomendado): composer global require phpactor/phpactor
;;       * ó intelephense: npm install -g intelephense
;;
;; lsp-mode detecta phpactor o intelephense si están en el PATH.

(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . lsp-deferred))

;; ============================================================
;; C / C++
;; ============================================================
;; Requisitos en el sistema:
;;   sudo apt install build-essential clang clangd clang-format cmake pkg-config
;;
;; - clangd: servidor LSP
;; - clang-format: formateo automático al guardar
;; - my/compile-and-run: atajo C-c r para compilar con g++ y ejecutar
;;   en la terminal integrada (vterm). Asegurarse de tener vterm configurado.

;; LSP con clangd
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(use-package clang-format
  :commands clang-format-buffer
  :hook ((c-mode . my/c-enabled)
         (c++-mode . my/c-enabled)))

(defun my/c-enabled ()
  "Hook para C/C++: agrega clang-format al guardar en este buffer."
  (add-hook 'before-save-hook #'clang-format-buffer nil t))

(defun my/compile-and-run ()
  "Compila y ejecuta el archivo C/C++ actual usando g++.
Compila a un binario con el mismo nombre (sin extensión) y lo ejecuta
en una ventana con vterm."
  (interactive)
  (unless buffer-file-name
    (user-error "Este buffer no está asociado a un archivo"))
  (save-buffer)
  (let* ((src (buffer-file-name))
         (out (concat (file-name-sans-extension src))))
    ;; Compilar en el buffer *compilation*
    (compile (format "g++ -std=c++20 -O2 %s -o %s" src out))
    ;; Abrir vterm en otra ventana y ejecutar el binario
    (other-window 1)
    (vterm)
    (insert (concat out "\n"))
    (vterm-send-return)))

;; Atajo global para compilar y correr C/C++
(global-set-key (kbd "C-c r") #'my/compile-and-run)

;; ============================================================
;; BASH / SHELL
;; ============================================================
;; Requisitos opcionales:
;;   npm install -g bash-language-server
;;
;; lsp-mode detecta bash-language-server automáticamente.

(add-hook 'sh-mode-hook #'lsp-deferred)

(provide 'lang-prog)
;;; lang-prog.el ends here

