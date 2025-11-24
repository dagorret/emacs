# Emacs IDE Pack

Este paquete contiene una configuración de Emacs lista para usar como IDE:

- Programación: Python, Rust, PHP, C/C++, Web (JS/TS/HTML/CSS/Vue)
- Escritura: Org, Markdown, LaTeX, PDF
- Notas: Org-roam (second brain) + diario
- Flujo tipo IDE: Treemacs + vterm + LSP

## Contenido

- `.emacs.d/` con toda la configuración
- `org/` con archivos base: inbox, agenda, notas, proyectos
- `notas/roam/` con estructura para Org-roam:
  - diario/
  - ideas/
  - proyectos/
  - referencias/

## Instalación rápida

```bash
chmod +x install-emacs-config.sh
./install-emacs-config.sh
```

Luego abrí Emacs normalmente.

## Atajos importantes

### Globales

- `C-x C-f` : abrir archivo
- `C-x C-s` : guardar
- `C-x C-c` : salir
- `M-x`     : ejecutar comando

### Archivos / proyecto

- `F8`      : Treemacs (árbol de archivos)
- `C-x C-j` : Dired en el directorio del archivo actual

### Layout tipo IDE

- `F9`      : Treemacs izquierda, código arriba derecha, vterm abajo derecha
- `C-c t`   : abrir vterm en la ventana actual

### LSP

- `C-c l`   : prefijo LSP (lsp-mode)
- `M-.`     : ir a definición
- `M-,`     : volver atrás

### C / C++

- `C-c r`   : compilar y ejecutar el archivo actual con g++

### Org / Agenda

- `C-c a`   : agenda (calendario + tareas)
- `C-c c`   : capturas (tareas, eventos, notas, proyectos)
  - `i` : inbox
  - `t` : tarea
  - `p` : proyecto
  - `e` : evento

### Org-roam / notas

- `C-c n f` : buscar nota Org-roam
- `C-c n i` : insertar enlace
- `C-c n l` : panel de backlinks
- `C-c n c` : crear nota nueva

### Diario

- `C-c n d` : ir al diario de hoy
- `C-c n j` : nueva entrada en el diario de hoy

### Búsqueda en notas

- `C-c s r` : consult-ripgrep (búsqueda en notas / proyectos)

## Requisitos recomendados (apt)

```bash
sudo apt update
sudo apt install \
  build-essential git curl \
  ripgrep fd-find \
  python3 python3-venv python3-pip \
  php-cli composer \
  nodejs npm \
  pandoc texlive-latex-base texlive-latex-extra \
  hunspell hunspell-es \
  default-jre \
  sqlite3 \
  clang clangd clang-format cmake pkg-config
```

Y LSPs opcionales:

- `npm install -g pyright bash-language-server prettier @tailwindcss/language-server`
- `composer global require phpactor/phpactor`
- `curl https://sh.rustup.rs -sSf | sh` y luego `rustup component add rust-analyzer`

Disfrutá tu Emacs como IDE completo :)
