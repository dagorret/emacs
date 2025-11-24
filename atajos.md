
# &Iacute;ndice

1.  [Introducción](#orgbeff4e7)
2.  [Convenciones de teclas](#org65cc545)
3.  [Movimiento, edición, cortar y pegar (por defecto)](#orgaea51a9)
    1.  [Movimiento del cursor](#org174a972)
    2.  [Selección, cortar, copiar, pegar](#orgbd1c9a3)
    3.  [Deshacer](#orgc8ef952)
4.  [Archivos y buffers](#orgec91330)
    1.  [Archivos](#org83abbde)
    2.  [Buffers](#orgf4fcf19)
5.  [Ventanas (splits)](#orgfe1bae1)
6.  [Búsqueda y reemplazo](#org5fe0eb2)
7.  [Ayuda en Emacs](#org9debae8)
8.  [Dired (gestor de archivos)](#org9189605)
9.  [Org mode básico](#org8c49f56)
10. [Atajos personalizados globales](#orgb928505)
    1.  [Terminal integrada](#org8fde83c)
    2.  [Layout de desarrollo](#orgfe17d97)
    3.  [Org-roam (notas diarias)](#org700137b)
    4.  [Org agenda y capture](#org03432da)
    5.  [Compilar y ejecutar C/C++](#org688d03a)
    6.  [Diccionarios de ortografía](#org60727de)
    7.  [Exportar a DOCX](#org4cd8837)
11. [Atajos personalizados en Dired](#orgff5de3c)
12. [Atajos personalizados en LaTeX](#orgbe97225)
13. [Funciones `my/...` y código fuente](#org1c8d38c)
    1.  [`core-dev.el` :: layout de desarrollo](#orge4fd868)
    2.  [`core-files.el` :: utilidades para Dired](#org92fc422)
    3.  [`lang-prog.el` :: programación (C/C++)](#org62995e1)
    4.  [`lang-web-extras.el` :: helpers para Vue / Alpine, etc.](#orgb8f69ee)
    5.  [`lang-writing.el` :: escritura, Markdown, LaTeX, ortografía, exportación](#orgbdd93f5)



<a id="orgbeff4e7"></a>

# Introducción

Este archivo documenta:

-   Los atajos de teclado **básicos** de Emacs (los más usados para movimiento,
    cortar/pegar, ventanas, archivos, etc.).
-   Todos los atajos **personalizados** definidos en mi configuración:
    -   `core-base.el`
    -   `core-files.el`
    -   `core-ui.el`
    -   `core-dev.el`
    -   `core-notes.el`
    -   `lang-org.el`
    -   `lang-writing.el`
    -   `lang-prog.el`
    -   `lang-web.el`
    -   `lang-web-extras.el`
-   Las funciones `my/...` que agregan comportamiento nuevo, con su código
    fuente en bloques `emacs-lisp`.

Puedo abrir este archivo dentro de Emacs y usarlo como manual rápido.


<a id="org65cc545"></a>

# Convenciones de teclas

-   **`C-`:** tecla Control
-   **`M-`:** tecla Meta (Alt o `ESC`)
-   **`S-`:** tecla Shift
-   Ejemplos:
    -   `C-x C-f`  → Control + x, luego Control + f
    -   `M-x`      → Meta (Alt) + x
    -   `<f6>`     → tecla de función F6


<a id="orgaea51a9"></a>

# Movimiento, edición, cortar y pegar (por defecto)


<a id="org174a972"></a>

## Movimiento del cursor

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-f</td>
<td class="org-left">Carácter adelante</td>
</tr>


<tr>
<td class="org-left">C-b</td>
<td class="org-left">Carácter atrás</td>
</tr>


<tr>
<td class="org-left">C-n</td>
<td class="org-left">Línea siguiente</td>
</tr>


<tr>
<td class="org-left">C-p</td>
<td class="org-left">Línea anterior</td>
</tr>


<tr>
<td class="org-left">M-f</td>
<td class="org-left">Palabra adelante</td>
</tr>


<tr>
<td class="org-left">M-b</td>
<td class="org-left">Palabra atrás</td>
</tr>


<tr>
<td class="org-left">C-a</td>
<td class="org-left">Inicio de línea</td>
</tr>


<tr>
<td class="org-left">C-e</td>
<td class="org-left">Fin de línea</td>
</tr>


<tr>
<td class="org-left">M-a</td>
<td class="org-left">Inicio de oración</td>
</tr>


<tr>
<td class="org-left">M-e</td>
<td class="org-left">Fin de oración</td>
</tr>


<tr>
<td class="org-left">M-&lt;</td>
<td class="org-left">Principio del buffer</td>
</tr>


<tr>
<td class="org-left">M-&gt;</td>
<td class="org-left">Final del buffer</td>
</tr>


<tr>
<td class="org-left">C-v</td>
<td class="org-left">Página abajo</td>
</tr>


<tr>
<td class="org-left">M-v</td>
<td class="org-left">Página arriba</td>
</tr>
</tbody>
</table>


<a id="orgbd1c9a3"></a>

## Selección, cortar, copiar, pegar

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-SPC</td>
<td class="org-left">Empezar a marcar región</td>
</tr>


<tr>
<td class="org-left">C-g</td>
<td class="org-left">Cancelar región/comando</td>
</tr>


<tr>
<td class="org-left">C-w</td>
<td class="org-left">Cortar (kill) región</td>
</tr>


<tr>
<td class="org-left">M-w</td>
<td class="org-left">Copiar región</td>
</tr>


<tr>
<td class="org-left">C-y</td>
<td class="org-left">Pegar (yank)</td>
</tr>


<tr>
<td class="org-left">M-y</td>
<td class="org-left">Navegar por el kill-ring</td>
</tr>
</tbody>
</table>


<a id="orgc8ef952"></a>

## Deshacer

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-/</td>
<td class="org-left">Deshacer</td>
</tr>


<tr>
<td class="org-left">C-_</td>
<td class="org-left">Deshacer</td>
</tr>
</tbody>
</table>


<a id="orgec91330"></a>

# Archivos y buffers


<a id="org83abbde"></a>

## Archivos

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-x C-f</td>
<td class="org-left">Abrir archivo</td>
</tr>


<tr>
<td class="org-left">C-x C-s</td>
<td class="org-left">Guardar archivo</td>
</tr>


<tr>
<td class="org-left">C-x C-w</td>
<td class="org-left">Guardar como…</td>
</tr>


<tr>
<td class="org-left">C-x C-v</td>
<td class="org-left">Reemplazar buffer por otro</td>
</tr>
</tbody>
</table>


<a id="orgf4fcf19"></a>

## Buffers

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-x b</td>
<td class="org-left">Cambiar de buffer</td>
</tr>


<tr>
<td class="org-left">C-x C-b</td>
<td class="org-left">Lista de buffers</td>
</tr>


<tr>
<td class="org-left">C-x k</td>
<td class="org-left">Cerrar (kill) buffer actual</td>
</tr>
</tbody>
</table>


<a id="orgfe1bae1"></a>

# Ventanas (splits)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-x 1</td>
<td class="org-left">Dejar solo la ventana actual</td>
</tr>


<tr>
<td class="org-left">C-x 2</td>
<td class="org-left">Dividir horizontalmente (una arriba/otra)</td>
</tr>


<tr>
<td class="org-left">C-x 3</td>
<td class="org-left">Dividir verticalmente (izquierda/derecha)</td>
</tr>


<tr>
<td class="org-left">C-x 0</td>
<td class="org-left">Cerrar ventana actual</td>
</tr>


<tr>
<td class="org-left">C-x o</td>
<td class="org-left">Ir a la otra ventana</td>
</tr>
</tbody>
</table>


<a id="org5fe0eb2"></a>

# Búsqueda y reemplazo

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-s</td>
<td class="org-left">Búsqueda incremental adelante</td>
</tr>


<tr>
<td class="org-left">C-r</td>
<td class="org-left">Búsqueda incremental atrás</td>
</tr>


<tr>
<td class="org-left">M-%</td>
<td class="org-left">Buscar y reemplazar (query-replace)</td>
</tr>


<tr>
<td class="org-left">C-M-%</td>
<td class="org-left">Buscar y reemplazar con regexp</td>
</tr>
</tbody>
</table>


<a id="org9debae8"></a>

# Ayuda en Emacs

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-h k</td>
<td class="org-left">Describir qué hace una tecla</td>
</tr>


<tr>
<td class="org-left">C-h f</td>
<td class="org-left">Describir función</td>
</tr>


<tr>
<td class="org-left">C-h v</td>
<td class="org-left">Describir variable</td>
</tr>


<tr>
<td class="org-left">C-h b</td>
<td class="org-left">Ver todos los atajos activos en el buffer</td>
</tr>


<tr>
<td class="org-left">C-h m</td>
<td class="org-left">Ayuda de los modos activos</td>
</tr>


<tr>
<td class="org-left">C-h t</td>
<td class="org-left">Tutorial interactivo de Emacs</td>
</tr>
</tbody>
</table>


<a id="org9189605"></a>

# Dired (gestor de archivos)

En `Dired` se agregan atajos personalizados, pero primero los básicos:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">RET</td>
<td class="org-left">Abrir archivo / entrar directorio</td>
</tr>


<tr>
<td class="org-left">^</td>
<td class="org-left">Subir al directorio padre</td>
</tr>


<tr>
<td class="org-left">+</td>
<td class="org-left">Crear directorio</td>
</tr>


<tr>
<td class="org-left">m</td>
<td class="org-left">Marcar archivo</td>
</tr>


<tr>
<td class="org-left">u</td>
<td class="org-left">Desmarcar archivo</td>
</tr>


<tr>
<td class="org-left">d</td>
<td class="org-left">Marcar para borrar</td>
</tr>


<tr>
<td class="org-left">x</td>
<td class="org-left">Ejecutar borrados marcados</td>
</tr>


<tr>
<td class="org-left">g</td>
<td class="org-left">Refrescar listado</td>
</tr>
</tbody>
</table>

Los atajos extra definidos en mi configuración están documentados más abajo.


<a id="org8c49f56"></a>

# Org mode básico

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">TAB</td>
<td class="org-left">Expandir/colapsar bloque</td>
</tr>


<tr>
<td class="org-left">S-TAB</td>
<td class="org-left">Ciclar visibilidad global</td>
</tr>


<tr>
<td class="org-left">M-RET</td>
<td class="org-left">Nuevo ítem al mismo nivel</td>
</tr>


<tr>
<td class="org-left">M-&lt;up&gt;</td>
<td class="org-left">Subir encabezado</td>
</tr>


<tr>
<td class="org-left">M-&lt;down&gt;</td>
<td class="org-left">Bajar encabezado</td>
</tr>


<tr>
<td class="org-left">M-&lt;left&gt;</td>
<td class="org-left">Promover encabezado</td>
</tr>


<tr>
<td class="org-left">M-&lt;right&gt;</td>
<td class="org-left">Degradar encabezado</td>
</tr>
</tbody>
</table>

TODOs y agenda:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Acción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-t</td>
<td class="org-left">Cambiar TODO/DONE</td>
</tr>


<tr>
<td class="org-left">C-c .</td>
<td class="org-left">Insertar fecha</td>
</tr>


<tr>
<td class="org-left">C-c C-s</td>
<td class="org-left">SCHEDULED</td>
</tr>


<tr>
<td class="org-left">C-c C-d</td>
<td class="org-left">DEADLINE</td>
</tr>
</tbody>
</table>


<a id="orgb928505"></a>

# Atajos personalizados globales

Los siguientes atajos se definen en mis módulos `core-*` y `lang-*`.


<a id="org8fde83c"></a>

## Terminal integrada

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Comando</th>
<th scope="col" class="org-left">Descripción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c t</td>
<td class="org-left"><code>vterm</code></td>
<td class="org-left">Abrir terminal <code>vterm</code></td>
</tr>
</tbody>
</table>


<a id="orgfe17d97"></a>

## Layout de desarrollo

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Comando</th>
<th scope="col" class="org-left">Descripción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">&lt;f9&gt;</td>
<td class="org-left"><code>my/dev-layout</code></td>
<td class="org-left">Layout con Treemacs a la izquierda, código y vterm</td>
</tr>
</tbody>
</table>


<a id="org700137b"></a>

## Org-roam (notas diarias)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Comando</th>
<th scope="col" class="org-left">Descripción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c n d</td>
<td class="org-left"><code>org-roam-dailies-goto-today</code></td>
<td class="org-left">Ir a la nota diaria de hoy</td>
</tr>


<tr>
<td class="org-left">C-c n j</td>
<td class="org-left"><code>org-roam-dailies-capture-today</code></td>
<td class="org-left">Capturar entrada en el diario</td>
</tr>
</tbody>
</table>


<a id="org03432da"></a>

## Org agenda y capture

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Comando</th>
<th scope="col" class="org-left">Descripción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c a</td>
<td class="org-left"><code>org-agenda</code></td>
<td class="org-left">Agenda de Org</td>
</tr>


<tr>
<td class="org-left">C-c c</td>
<td class="org-left"><code>org-capture</code></td>
<td class="org-left">Capturas rápidas de Org</td>
</tr>
</tbody>
</table>


<a id="org688d03a"></a>

## Compilar y ejecutar C/C++

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Comando</th>
<th scope="col" class="org-left">Descripción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c r</td>
<td class="org-left"><code>my/compile-and-run</code></td>
<td class="org-left">Compilar y ejecutar archivo actual</td>
</tr>
</tbody>
</table>


<a id="org60727de"></a>

## Diccionarios de ortografía

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Comando</th>
<th scope="col" class="org-left">Descripción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c d s</td>
<td class="org-left"><code>my/dict-spanish</code></td>
<td class="org-left">Diccionario español (es<sub>AR</sub>)</td>
</tr>


<tr>
<td class="org-left">C-c d e</td>
<td class="org-left"><code>my/dict-english</code></td>
<td class="org-left">Diccionario inglés (en<sub>US</sub>)</td>
</tr>
</tbody>
</table>


<a id="org4cd8837"></a>

## Exportar a DOCX

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Comando</th>
<th scope="col" class="org-left">Descripción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c e w</td>
<td class="org-left"><code>my/export-buffer-to-docx</code></td>
<td class="org-left">Exportar buffer actual a DOCX usando pandoc</td>
</tr>
</tbody>
</table>


<a id="orgff5de3c"></a>

# Atajos personalizados en Dired

En `core-files.el` se agregan atajos para crear directorios y archivos
directamente desde Dired:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Modo</th>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Comando</th>
<th scope="col" class="org-left">Descripción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Dired</td>
<td class="org-left">&lt;f6&gt;</td>
<td class="org-left"><code>my/dired-create-empty-file</code></td>
<td class="org-left">Crear archivo vacío en Dired</td>
</tr>


<tr>
<td class="org-left">Dired</td>
<td class="org-left">&lt;f7&gt;</td>
<td class="org-left"><code>my/dired-create-directory</code></td>
<td class="org-left">Crear nuevo directorio en Dired</td>
</tr>
</tbody>
</table>


<a id="orgbe97225"></a>

# Atajos personalizados en LaTeX

En `lang-writing.el` (AUCTeX / LaTeX):

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Modo</th>
<th scope="col" class="org-left">Tecla</th>
<th scope="col" class="org-left">Comando</th>
<th scope="col" class="org-left">Descripción</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">LaTeX-mode</td>
<td class="org-left">C-c e</td>
<td class="org-left"><code>my/latex-equation</code></td>
<td class="org-left">Insertar entorno <code>equation</code></td>
</tr>


<tr>
<td class="org-left">LaTeX-mode</td>
<td class="org-left">C-c f</td>
<td class="org-left"><code>my/latex-figure</code></td>
<td class="org-left">Insertar entorno <code>figure</code></td>
</tr>


<tr>
<td class="org-left">LaTeX-mode</td>
<td class="org-left">C-c t</td>
<td class="org-left"><code>my/latex-table</code></td>
<td class="org-left">Insertar entorno <code>table</code></td>
</tr>


<tr>
<td class="org-left">LaTeX-mode</td>
<td class="org-left">C-c i</td>
<td class="org-left"><code>my/latex-itemize</code></td>
<td class="org-left">Insertar entorno <code>itemize</code></td>
</tr>
</tbody>
</table>


<a id="org1c8d38c"></a>

# Funciones `my/...` y código fuente

A continuación, el código fuente de todas las funciones `my/...` definidas en
los módulos. Sirve como referencia y también como ejemplo para futuras
modificaciones.


<a id="orge4fd868"></a>

## `core-dev.el` :: layout de desarrollo

    (defun my/dev-layout ()
      "Abrir layout con Treemacs a la izquierda, código arriba y vterm abajo."
      (interactive)
      (delete-other-windows)
      ;; Panel izquierdo: Treemacs
      (treemacs)
      ;; Nos movemos a la ventana de la derecha para código
      (select-window (next-window))
      ;; Partimos la derecha en dos (arriba código, abajo terminal)
      (split-window-below)
      ;; Ventana superior derecha: se queda para el buffer actual
      (other-window 1)
      ;; Ventana inferior derecha: vterm
      (vterm)
      ;; Volver a la ventana de código
      (other-window -1))


<a id="org92fc422"></a>

## `core-files.el` :: utilidades para Dired

    (defun my/dired-create-directory ()
      "Crear un nuevo directorio en el Dired actual, con un prompt claro."
      (interactive)
      (let* ((dir (dired-current-directory))
             (name (read-string (format "Nombre del nuevo directorio en %s: " dir))))
        (when (and name (not (string-empty-p name)))
          (let ((full (expand-file-name name dir)))
            (make-directory full t)
            (revert-buffer)
            (message "Directorio creado: %s" full)))))
    
    (defun my/dired-create-empty-file ()
      "Crear un nuevo archivo vacío en el Dired actual, con un prompt claro."
      (interactive)
      (let* ((dir (dired-current-directory))
             (name (read-string (format "Nombre del nuevo archivo en %s: " dir))))
        (when (and name (not (string-empty-p name)))
          (let ((full (expand-file-name name dir)))
            (with-temp-buffer
              (write-file full))
            (revert-buffer)
            (message "Archivo creado: %s" full)))))
    
    (with-eval-after-load 'dired
      (define-key dired-mode-map (kbd "<f6>") #'my/dired-create-empty-file)
      (define-key dired-mode-map (kbd "<f7>") #'my/dired-create-directory))


<a id="org62995e1"></a>

## `lang-prog.el` :: programación (C/C++)

    (defun my/c-enabled ()
      "Config básica para C/C++: estilo, sangría, etc."
      (c-set-style "linux")
      (setq c-basic-offset 4
            indent-tabs-mode nil))
    
    (add-hook 'c-mode-hook #'my/c-enabled)
    (add-hook 'c++-mode-hook #'my/c-enabled)
    
    (defun my/compile-and-run ()
      "Compilar y ejecutar el archivo C/C++ actual en una ventana vterm.
    Compila con g++ -std=c++20 -O2 y ejecuta el binario resultante."
      (interactive)
      (unless buffer-file-name
        (user-error "El buffer no está asociado a un archivo"))
      (save-buffer)
      (let* ((src buffer-file-name)
             (exe (file-name-sans-extension src))
             (cmd (format "g++ -std=c++20 -O2 -o %s %s && %s\n" exe src exe)))
        (unless (executable-find "g++")
          (user-error "g++ no está instalado"))
        (let ((vterm-buf (get-buffer "*vterm-compilacion*")))
          (if vterm-buf
              (pop-to-buffer vterm-buf)
            (setq vterm-buf (vterm "*vterm-compilacion*")))
          (vterm-send-string cmd)
          (vterm-send-return))))


<a id="orgb8f69ee"></a>

## `lang-web-extras.el` :: helpers para Vue / Alpine, etc.

    (defun my/vue-insert-sfc ()
      "Insertar skeleton básico de Single File Component Vue 3."
      (interactive)
      (insert "<template>\n  <div class=\"\">\n  </div>\n</template>\n\n")
      (insert "<script setup>\n\n</script>\n\n")
      (insert "<style scoped>\n\n</style>\n")
      (message "Vue SFC insertado."))
    
    (defun my/alpine-insert-component ()
      "Insertar un snippet básico para un componente con Alpine.js."
      (interactive)
      (insert "<div x-data=\"{ open: false }\">\n")
      (insert "  <button @click=\"open = !open\">Toggle</button>\n")
      (insert "  <div x-show=\"open\">\n")
      (insert "    Contenido...\n")
      (insert "  </div>\n")
      (insert "</div>\n")
      (message "Snippet Alpine.js insertado."))
    
    (defun my/tailwind-insert-container ()
      "Insertar un contenedor básico con clases de Tailwind CSS."
      (interactive)
      (insert "<div class=\"max-w-4xl mx-auto px-4 sm:px-6 lg:px-8\">\n")
      (insert "  <!-- Contenido -->\n")
      (insert "</div>\n")
      (message "Contenedor Tailwind CSS insertado."))


<a id="orgbdd93f5"></a>

## `lang-writing.el` :: escritura, Markdown, LaTeX, ortografía, exportación

Incluye helpers para:

-   Cambiar diccionarios (español/inglés).
-   Helpers de Markdown (negrita, itálica, código, enlaces, etc.).
-   Helpers de LaTeX (entornos `equation`, `figure`, `table`, `itemize`).
-   Exportar el archivo actual a DOCX con `pandoc`.

Código completo:

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
    
    ;; Helpers Markdown
    (defun my/markdown-wrap (left right)
      "Envolver la región activa (o palabra actual) entre LEFT y RIGHT."
      (if (use-region-p)
          (let ((beg (region-beginning))
                (end (region-end)))
            (goto-char end)
            (insert right)
            (goto-char beg)
            (insert left))
        (let ((bounds (bounds-of-thing-at-point 'word)))
          (when bounds
            (goto-char (cdr bounds))
            (insert right)
            (goto-char (car bounds))
            (insert left)))))
    
    (defun my/markdown-bold ()
      "Negrita Markdown para región o palabra actual."
      (interactive)
      (my/markdown-wrap "**" "**"))
    
    (defun my/markdown-italic ()
      "Itálica Markdown para región o palabra actual."
      (interactive)
      (my/markdown-wrap "*" "*"))
    
    (defun my/markdown-inline-code ()
      "Código en línea Markdown para región o palabra actual."
      (interactive)
      (my/markdown-wrap "`" "`"))
    
    (defun my/markdown-link ()
      "Insertar un enlace Markdown. Si hay región, usarla como texto."
      (interactive)
      (let* ((url (read-string "URL: "))
             (text (if (use-region-p)
                       (buffer-substring-no-properties (region-beginning)
                                                       (region-end))
                     (read-string "Texto del enlace: "))))
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert (format "[%s](%s)" text url))))
    
    (defun my/markdown-setup-keys ()
      "Configurar atajos para helpers de Markdown."
      (local-set-key (kbd "C-c m b") #'my/markdown-bold)
      (local-set-key (kbd "C-c m i") #'my/markdown-italic)
      (local-set-key (kbd "C-c m c") #'my/markdown-inline-code)
      (local-set-key (kbd "C-c m l") #'my/markdown-link))
    
    (add-hook 'markdown-mode-hook #'my/markdown-setup-keys)
    
    ;; Helpers LaTeX
    (defun my/latex-insert-environment (name)
      "Insertar un entorno LaTeX \\begin{NAME} ... \\end{NAME}, con punto de inserción en medio."
      (interactive "sNombre del entorno: ")
      (insert (format "\\begin{%s}\n" name))
      (save-excursion
        (insert (format "\n\\end{%s}\n" name))))
    
    (defun my/latex-equation ()
      "Insertar entorno equation."
      (interactive)
      (my/latex-insert-environment "equation"))
    
    (defun my/latex-itemize ()
      "Insertar entorno itemize."
      (interactive)
      (my/latex-insert-environment "itemize"))
    
    (defun my/latex-figure ()
      "Insertar entorno figure."
      (interactive)
      (my/latex-insert-environment "figure"))
    
    (defun my/latex-table ()
      "Insertar entorno table."
      (interactive)
      (my/latex-insert-environment "table"))
    
    (with-eval-after-load 'latex
      (define-key LaTeX-mode-map (kbd "C-c e") #'my/latex-equation)
      (define-key LaTeX-mode-map (kbd "C-c f") #'my/latex-figure)
      (define-key LaTeX-mode-map (kbd "C-c t") #'my/latex-table)
      (define-key LaTeX-mode-map (kbd "C-c i") #'my/latex-itemize))
    
    ;; Exportar a DOCX con pandoc
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

