
# Table of Contents

1.  [Propiedades estándar de Org-mode](#orgc0b3f00)
    1.  [ID](#org1604540)
    2.  [CATEGORY](#org950e255)
    3.  [PRIORITY](#orgf2c3c1e)
    4.  [STYLE](#org6edc821)
    5.  [COOKIE<sub>DATA</sub>](#orge5caa02)
2.  [Propiedades específicas de Org-roam](#org71b520c)
    1.  [ROAM<sub>ALIASES</sub>](#org7e528b2)
    2.  [ROAM<sub>TAGS</sub>](#orga857abc)
    3.  [ROAM<sub>REFS</sub>](#org68a3519)
    4.  [ID](#orgff7eb61)
3.  [Propiedades útiles creadas por el usuario](#orgfe47d91)
    1.  [CREATED](#orgc1d6f19)
    2.  [UPDATED / LAST<sub>MODIFIED</sub>](#org477ab4d)
    3.  [SOURCE](#orga2b1d98)
    4.  [AUTHOR](#org8bf7275)
    5.  [TIPO / TYPE](#orgb7524eb)
    6.  [TOPIC](#org8b47e1b)
    7.  [STATUS](#org07f94d9)
    8.  [CONTEXT](#orgdd3138d)
    9.  [DIFFICULTY](#orgde9da49)
    10. [RATING](#orge79daf9)



<a id="orgc0b3f00"></a>

# Propiedades estándar de Org-mode

Estas propiedades vienen soportadas por Org-mode y son útiles en tareas, proyectos y organización.


<a id="org1604540"></a>

## ID

Identificador único de un encabezado.
Org y Org-roam lo usan para enlaces estables.

:ID: 23e40f1f-a3f9-49cd-b2ad-6c11cbb7a2c4


<a id="org950e255"></a>

## CATEGORY

Categoriza entradas (útil para agenda).

:CATEGORY: Emacs


<a id="orgf2c3c1e"></a>

## PRIORITY

Prioridad de tareas: A, B o C.

:PRIORITY: A


<a id="org6edc821"></a>

## STYLE

Determina el comportamiento de un TODO.
Ejemplo para hábitos:

:STYLE: habit


<a id="orge5caa02"></a>

## COOKIE<sub>DATA</sub>

Configura conteo de subtareas:

:COOKIE<sub>DATA</sub>: todo recursive


<a id="org71b520c"></a>

# Propiedades específicas de Org-roam

Org-roam usa propiedades especiales para manejo de notas enlazadas.


<a id="org7e528b2"></a>

## ROAM<sub>ALIASES</sub>

Nombres alternativos para una nota.

:ROAM<sub>ALIASES</sub>: "Productividad" "Mejorar productividad"


<a id="orga857abc"></a>

## ROAM<sub>TAGS</sub>

Etiquetas de notas (muy útil para agrupar).

:ROAM<sub>TAGS</sub>: productividad foco organizacion


<a id="org68a3519"></a>

## ROAM<sub>REFS</sub>

Para guardar referencias externas:
links, papers, artículos, libros.

:ROAM<sub>REFS</sub>: <https://ejemplo.com/productividad>


<a id="orgff7eb61"></a>

## ID

Org-roam crea uno automáticamente al generar la nota:

:ID: 5b0f1aae-c232-4c19-beb4-e70b2c9a7f8d


<a id="orgfe47d91"></a>

# Propiedades útiles creadas por el usuario

Estas no vienen predefinidas: vos las inventás y son muy útiles.


<a id="orgc1d6f19"></a>

## CREATED

Fecha de creación de la nota.

:CREATED: <span class="timestamp-wrapper"><span class="timestamp">[2025-02-25 mar]</span></span>


<a id="org477ab4d"></a>

## UPDATED / LAST<sub>MODIFIED</sub>

Fecha de última modificación.

:UPDATED: <span class="timestamp-wrapper"><span class="timestamp">[2025-02-25 mar 18:14]</span></span>


<a id="orga2b1d98"></a>

## SOURCE

De dónde viene la información:
URL, libro, título, video, revista.

:SOURCE: <https://ejemplo.com/articulo>


<a id="org8bf7275"></a>

## AUTHOR

Autor del artículo o fuente:

:AUTHOR: Cal Newport


<a id="orgb7524eb"></a>

## TIPO / TYPE

Categorizar tus notas:

:TIPO: Articulo

Ejemplos de valores:

-   Concepto
-   Artículo
-   Idea
-   Proyecto
-   Apunte
-   Investigación


<a id="org8b47e1b"></a>

## TOPIC

Tema general al que pertenece la nota:

:TOPIC: Emacs


<a id="org07f94d9"></a>

## STATUS

Estado de un proyecto, idea o investigación:

:STATUS: en-progreso


<a id="orgdd3138d"></a>

## CONTEXT

Contexto donde aplica la nota:

:CONTEXT: trabajo


<a id="orgde9da49"></a>

## DIFFICULTY

Para notas técnicas:

:DIFFICULTY: media


<a id="orge79daf9"></a>

## RATING

Para libros, películas, música, artículos:

:RATING: 4/5

