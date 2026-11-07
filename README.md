# Apuntes de Probabilidad y Estadística

Libro de apuntes de Álvaro Mattus Donaire para el curso de Estadística de Ingeniería de la Universidad del Desarrollo. Está escrito en LaTeX con la clase [`kaobook`](https://github.com/fmarotta/kaobook) (formato A5, con márgenes para notas y figuras), con gráficos hechos en TikZ/pgfplots y aplicaciones interactivas en R/Shiny.

> **📘 [Descargar el libro en PDF](https://github.com/varodonaire/apuntes_prob_est/releases/latest/download/main.pdf)** — 191 páginas, 12 capítulos.
> Ese enlace entrega siempre la versión más reciente; no hace falta compilar
> nada para leerlo. El resto del repositorio es el fuente, por si quieres
> compilarlo tú o mirar cómo está hecho.

La idea del libro es ir de la intuición al formalismo: cada concepto parte de un ejemplo cotidiano (fichas, dados, naipes, monedas), le da al lector un momento para conjeturar, y recién después llega la definición. El texto en negro es la intuición y la conversación; el texto en azul (`\formal`) son las definiciones y demostraciones.

## Contenido

| Cap. | Título |
|-----:|--------|
| 1 | ¿Qué probabilidad hay? |
| 2 | Probabilidades++ |
| 3 | ¡Resultados! ¡Resultados! ¡Resultados! |
| 4 | El zoológico de las variables aleatorias, parte 1 |
| 5 | El zoológico de las variables aleatorias, parte 2 |
| 6 | ¿Qué dicen los datos? |
| 7 | E pluribus unum… duo, tres… |
| 8 | El arte de estimar |
| 9 | En la confianza está el peligro |
| 10 | Pruebas de hipótesis |
| 11 | Regresión lineal |
| 12 | Epílogo: El capítulo de los senderos que se bifurcan |

## Estructura del repositorio

```
main.tex                 Preámbulo, macros y orden de los capítulos
chapters/                Un archivo .tex por capítulo
images/                  Figuras que no se pueden dibujar en TikZ (datos reales, simulaciones)
app*.R, run_app*.R       Aplicaciones Shiny (varianza muestral, lanzamiento de monedas, TLC)
lanz_monedas.R, ...      Scripts de R para simulaciones y figuras
guia_eda_nacimientos.ipynb
Serie_Nacimientos_2020_2022.csv   Datos del cuaderno de análisis exploratorio
```

## Compilar

Requiere una distribución de TeX Live reciente con `kaobook`, `polyglossia`, `biblatex` y `pgfplots`. Se compila con LuaLaTeX:

```bash
latexmk -lualatex main.tex
```

Para trabajar en un solo capítulo sin compilar todo el libro, descomenta la línea `\includeonly{...}` en `main.tex`.

## Aplicaciones interactivas

Las apps de Shiny necesitan R con los paquetes `shiny`, `ggplot2` y `dplyr`:

```r
shiny::runApp("app.R")                       # estimación de varianza muestral
# versión en línea: https://varodonaire.shinyapps.io/VarianzaMuestral/
source("run_app_tlc.R")                      # teorema del límite central
```

## Estilo

El libro sigue unas pocas convenciones: intuición antes que formalismo, una `pausa` antes de cada revelación, coma decimal con `\num{}`, comillas con `\enquote{}` y los comentarios laterales en `\sidenote{}` o `\marginnote{}`.
