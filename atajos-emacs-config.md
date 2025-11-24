# Atajos de teclado de Emacs en tu configuración

Este documento resume:

1. Los atajos **básicos por defecto** de Emacs (los que “trae de fábrica”).

2. Los atajos **personalizados** que aparecen en tus archivos de configuración:
   
   - `/mnt/data/core-base.el`
   - `/mnt/data/core-dev.el`
   - `/mnt/data/core-files.el`
   - `/mnt/data/core-notes.el`
   - `/mnt/data/core-ui.el`
   - `/mnt/data/lang-org.el`
   - `/mnt/data/lang-prog.el`
   - `/mnt/data/lang-writing.el`
   - `/mnt/data/lang-web.el`
   - `/mnt/data/lang-web-extras.el`

> Nota: Emacs trae cientos de comandos. Aquí se listan los **principales para
> mover el cursor, manejar ventanas, archivos, cortar/pegar, Org, Dired**, etc.,
> más **todos** los atajos extra definidos explícitamente en tu configuración.

---

## 0. Notación de teclas

- `C-`  = tecla **Control**
- `M-`  = tecla **Meta** (Alt o Esc)
- `S-`  = tecla **Shift**
- Ejemplos:
  - `C-x C-f` = Control + x, luego Control + f
  - `M-x` = Meta (Alt) + x
  - `<f6>` = tecla de función F6

---

## 1. Comandos básicos de edición y movimiento (por defecto de Emacs)

### 1.1 Movimiento del cursor

| Tecla | Acción                              |
| ----- | ----------------------------------- |
| `C-f` | Mover un carácter **adelante**      |
| `C-b` | Mover un carácter **atrás**         |
| `C-n` | Línea siguiente (abajo)             |
| `C-p` | Línea anterior (arriba)             |
| `M-f` | Saltar una **palabra adelante**     |
| `M-b` | Saltar una **palabra atrás**        |
| `C-a` | Ir al **inicio de línea**           |
| `C-e` | Ir al **final de línea**            |
| `M-a` | Ir al inicio de oración             |
| `M-e` | Ir al final de oración              |
| `M-<` | Ir al **principio** del buffer      |
| `M->` | Ir al **final** del buffer          |
| `C-v` | Avanzar una “pantalla” (page down)  |
| `M-v` | Retroceder una “pantalla” (page up) |

### 1.2 Selección, cortar, copiar, pegar

En Emacs, la selección se llama **región**.

| Tecla   | Acción                                   |
| ------- | ---------------------------------------- |
| `C-SPC` | Empezar a marcar región (activar marca)  |
| `C-g`   | Cancelar selección/comando               |
| `C-w`   | Cortar (kill) la región                  |
| `M-w`   | Copiar la región sin borrarla            |
| `C-y`   | Pegar (yank) lo último cortado/copiado   |
| `M-y`   | Moverse atrás en el historial de “kills” |

### 1.3 Deshacer / rehacer básico

| Tecla | Acción                                  |
| ----- | --------------------------------------- |
| `C-/` | Deshacer                                |
| `C-_` | Deshacer (igual)                        |
| `M-/` | Completar texto (no es rehacer clásico) |

(Dependiendo de paquetes, la combinación para rehacer puede variar; por defecto
Emacs no tiene rehacer “clásico” como otros editores).

---

## 2. Archivos y buffers (abrir, guardar, cerrar)

### 2.1 Archivos

| Tecla     | Acción                                         |
| --------- | ---------------------------------------------- |
| `C-x C-f` | Abrir archivo (o crear si no existe)           |
| `C-x C-s` | Guardar archivo                                |
| `C-x s`   | Guardar varios buffers (consulta por cada uno) |
| `C-x C-w` | Guardar como… (write file)                     |
| `C-x C-v` | Reemplazar el buffer actual por otro archivo   |

### 2.2 Buffers

| Tecla         | Acción                                       |
| ------------- | -------------------------------------------- |
| `C-x b`       | Cambiar de buffer (o crear uno nuevo)        |
| `C-x C-b`     | Listar buffers                               |
| `C-x k`       | Cerrar (kill) el buffer actual               |
| `C-x <left>`  | Ir al buffer anterior (si está configurado)  |
| `C-x <right>` | Ir al buffer siguiente (si está configurado) |

---

## 3. Ventanas (splits) y “movimiento de ventanas”

### 3.1 Crear/cerrar ventanas

| Tecla   | Acción                                             |
| ------- | -------------------------------------------------- |
| `C-x 1` | Dejar **sólo** la ventana actual                   |
| `C-x 2` | Dividir ventana **horizontalmente** (dos ventanas) |
| `C-x 3` | Dividir ventana **verticalmente**                  |
| `C-x 0` | Cerrar ventana actual                              |

### 3.2 Moverse entre ventanas

| Tecla   | Acción                   |
| ------- | ------------------------ |
| `C-x o` | Ir a la **otra** ventana |

(Con paquetes extra se pueden usar `S-<cursor>` para moverse, pero en tu
configuración no se ve nada especial para eso, así que vale el comando estándar
`C-x o`.)

---

## 4. Búsqueda y reemplazo

| Tecla   | Acción                                  |
| ------- | --------------------------------------- |
| `C-s`   | Búsqueda incremental hacia **adelante** |
| `C-r`   | Búsqueda incremental hacia **atrás**    |
| `M-%`   | Buscar y reemplazar (query-replace)     |
| `C-M-%` | Buscar y reemplazar con regex           |

---

## 5. Ayuda (muy importante en Emacs)

| Tecla   | Acción                                           |
| ------- | ------------------------------------------------ |
| `C-h k` | Describir qué hace una tecla concreta            |
| `C-h f` | Describir una función                            |
| `C-h v` | Describir una variable                           |
| `C-h b` | Ver todos los atajos activos en el buffer actual |
| `C-h m` | Mostrar ayuda de los *modos* activos             |
| `C-h t` | Tutorial interactivo de Emacs                    |

---

## 6. Dired (gestor de archivos dentro de Emacs)

**Comandos por defecto de Dired** (además de tus atajos personalizados):

| Tecla | Acción                                      |
| ----- | ------------------------------------------- |
| `RET` | Abrir archivo o entrar en el directorio     |
| `^`   | Subir al directorio padre                   |
| `+`   | Crear directorio                            |
| `% m` | Marcar archivos por patrón                  |
| `m`   | Marcar archivo                              |
| `u`   | Desmarcar archivo                           |
| `d`   | Marcar archivo para borrar                  |
| `x`   | Ejecutar las operaciones marcadas (borrado) |
| `g`   | Refrescar listado                           |

---

## 7. Org mode (básico por defecto)

### 7.1 Estructura

| Tecla       | Acción                       |
| ----------- | ---------------------------- |
| `TAB`       | Expandir/colapsar subtítulo  |
| `S-TAB`     | Ciclar visibilidad global    |
| `M-RET`     | Nueva entrada al mismo nivel |
| `M-<up>`    | Subir encabezado             |
| `M-<down>`  | Bajar encabezado             |
| `M-<left>`  | Promover encabezado          |
| `M-<right>` | Degradar encabezado          |

### 7.2 TODOs y agenda

| Tecla     | Acción                     |
| --------- | -------------------------- |
| `C-c C-t` | Cambiar estado TODO / DONE |
| `C-c .`   | Insertar fecha con agenda  |
| `C-c C-s` | Programar (SCHEDULED)      |
| `C-c C-d` | Deadline (DEADLINE)        |

Además, en tu configuración:

- `C-c a` → `org-agenda` (agenda de Org)
- `C-c c` → `org-capture` (capturas rápidas)

Esto viene de:

```elisp
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
```

## 8. Atajos **personalizados** de tu configuración

Escaneando tus archivos `.el`, estos son los atajos adicionales que se  
definen explícitamente:

| Modo / Contexto | Tecla     | Comando Emacs                    | Descripción                                                                 |
| --------------- | --------- | -------------------------------- | --------------------------------------------------------------------------- |
| Global          | `C-c t`   | `vterm`                          | Abrir terminal integrada vterm en la ventana actual.                        |
| Global          | `<f9>`    | `my/dev-layout`                  | Abrir layout de desarrollo: Treemacs izquierda, código arriba, vterm abajo. |
| Global          | `C-c d s` | `my/dict-spanish`                | Cambiar diccionario de ortografía a español (es_AR).                        |
| Global          | `C-c d e` | `my/dict-english`                | Cambiar diccionario de ortografía a inglés (en_US).                         |
| LaTeX-mode      | `C-c e`   | `my/latex-equation`              | Insertar entorno LaTeX \begin{equation}...\end{equation}.                   |
| LaTeX-mode      | `C-c f`   | `my/latex-figure`                | Insertar entorno LaTeX \begin{figure}...\end{figure}.                       |
| LaTeX-mode      | `C-c t`   | `my/latex-table`                 | Insertar entorno LaTeX \begin{table}...\end{table}.                         |
| LaTeX-mode      | `C-c i`   | `my/latex-itemize`               | Insertar entorno LaTeX \begin{itemize}...\end{itemize}.                     |
| Global          | `C-c e w` | `my/export-buffer-to-docx`       | Exportar el archivo actual a DOCX usando pandoc.                            |
| Dired           | `<f6>`    | `my/dired-create-empty-file`     | Crear archivo vacío en el directorio actual y refrescar Dired.              |
| Dired           | `<f7>`    | `my/dired-create-directory`      | Crear directorio nuevo en el directorio actual y refrescar Dired.           |
| Global          | `C-c n d` | `org-roam-dailies-goto-today`    | Ir a la nota diaria de hoy (org-roam dailies).                              |
| Global          | `C-c n j` | `org-roam-dailies-capture-today` | Capturar una entrada en la nota diaria de hoy.                              |
| Global          | `C-c r`   | `my/compile-and-run`             | Compilar y ejecutar el archivo C/C++ actual en vterm.                       |
| Global          | `C-c a`   | `org-agenda`                     | Abrir agenda de Org.                                                        |
| Global          | `C-c c`   | `org-capture`                    | Abrir menú de capturas rápidas de Org.                                      |



### 8.1 Descripción breve de algunos comandos `my/*`

A partir de las definiciones en tus `.el`:

- `my/dev-layout`  
  Abre un layout de desarrollo: Treemacs a la izquierda, la ventana de la  
  derecha dividida en dos (arriba código actual, abajo `vterm`).

- `my/dict-spanish`  
  Cambia el diccionario de ortografía a `es_AR` (Hunspell).

- `my/dict-english`  
  Cambia el diccionario de ortografía a `en_US`.

- `my/latex-insert-environment`  
  Inserta:

```
\begin{nombre}

\end{nombre}

```

- y las funciones `my/latex-equation`, `my/latex-figure`, `my/latex-table`,  
  `my/latex-itemize` usan esto para cada entorno.

- `my/dired-create-empty-file`  
  Pide un nombre, crea un archivo vacío en el Dired actual y refresca el  
  listado.

- `my/dired-create-directory`  
  Pide un nombre, crea un directorio en el Dired actual y refresca el  
  listado.

- `my/export-buffer-to-docx`  
  Usa `pandoc` para exportar el archivo actual a DOCX (para Org o Markdown).  
  Requiere tener `pandoc` instalado.

- `my/compile-and-run`  
  Compila el archivo C/C++ actual con `g++ -std=c++20 -O2` a un binario con el  
  mismo nombre (sin extensión) y lo ejecuta en una ventana con `vterm`. Útil  
  para pruebas rápidas de código.

---

## 9. Cómo encontrar más comandos desde Emacs

Aunque aquí tienes un resumen, recuerda:

- `C-h b` → lista **todos** los atajos activos en el buffer actual.

- `C-h m` → muestra la ayuda de los modos activos, con sus atajos.

- `C-h k` seguido de una tecla → te dice qué comando ejecuta esa tecla.

- Si en el futuro agregás más `global-set-key` o `define-key` en tu  
  configuración, aparecerán aquí mismo si vuelves a generar una lista similar.
  
  

---

## 10. Sugerencia de uso

Puedes:

1. Guardar este archivo como `atajos-emacs-config.md` o `atajos-emacs.org`.

2. Abrirlo en Emacs y tenerlo siempre a mano (incluso en una ventana lateral).

3. Ir subrayando o marcando las secciones que más usás.
