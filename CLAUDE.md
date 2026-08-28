# CLAUDE.md

Instrucciones para trabajar en este repositorio.

## Qué es esto

Blog técnico de Leonardo Hansa, escrito en español y construido con Quarto. Se publica en
<https://lhansa.github.io/technical-notebooks>.

Un post es una carpeta dentro de `posts/` con un único `index.qmd`:

```
posts/2026-08-28-de-donde-sale-el-log-loss/index.qmd
```

El listado de posts lo genera `cuadernos.qmd`; no hay que registrar nada a mano al añadir uno.

## La voz

Esto es lo más importante del fichero. Un post que esté bien de contenido pero suene a LLM no sirve.

Reglas:

- **Tutea.** "Entrenas un clasificador", "fíjate en", "te lo cobra". Nunca "el lector" ni "usted".
- **Empieza en seco.** La primera frase ya está dentro del tema. Nada de "En este artículo vamos a
  explorar" ni de contextualizar la importancia del asunto.
- **Frases cortas. Párrafos de una o dos líneas.** El texto respira; se lee bajando rápido.
- **Primera persona para lo tuyo.** "Yo estuve años así", "los datos no me importan especialmente".
  La experiencia propia y las dudas propias son parte del texto.
- **Negrita para la idea que sostiene el post**, una o dos veces por sección, no más.
- **Cierra con la consecuencia**, no con un resumen. Qué cambia para quien lee, ahora que sabe esto.
- Humor seco y frases de andar por casa cuando encajen. Sin exclamaciones ni entusiasmo impostado.
- Prosa antes que listas. Una lista es para enumerar cosas de verdad, no para trocear un argumento.
- Fuera el relleno: "cabe destacar", "es importante señalar", "en el mundo actual", "profundicemos".
- Matemáticas en LaTeX inline (`$p$`, `$-\log p$`) y en bloque `$$...$$` cuando la fórmula es el
  centro del párrafo. Explica la fórmula en palabras antes o después de escribirla.

Referencia de tono, de `posts/2026-08-28-de-donde-sale-el-log-loss/index.qmd`:

> Yo estuve años así. Sabía usarla, sabía que penalizaba mucho equivocarte con confianza, y hasta
> ahí. La fórmula tenía toda la pinta de que alguien la había elegido porque le funcionaba bien, no
> porque tuviera un motivo.

> Pues resulta que hay un motivo. Y es bastante bonito.

> No es que el log-loss castigue la confianza mal puesta porque alguien decidiera castigarla. Es que
> estar muy seguro y fallar **es** llevarse mucha sorpresa.

Cuando dudes de cómo suena algo, lee un post reciente entero antes de escribir.

## Los dos formatos de post

### Ensayo (por defecto)

Explicaciones y desarrollos de una idea. Solo prosa y fórmulas, sin código ejecutado. Es el formato
por defecto cuando se pide "escribe un post sobre X".

Front matter:

```yaml
---
title: "De dónde sale el log-loss"
description: "La función de pérdida que usas en clasificación no es una fórmula arbitraria: sale de medir sorpresa. Te cuento el camino."
description-meta: "La función de pérdida que usas en clasificación no es una fórmula arbitraria: sale de medir sorpresa. Te cuento el camino."
author: "Leonardo Hansa"
date: "2026-08-28"
categories: [datos]
---
```

Estructura habitual: arranque de dos o tres párrafos que plantea la pregunta, tres o cuatro
secciones `##` que la desarrollan, y una última sección tipo "Por qué esto te importa".

### Cuaderno con código

Experimentos, simulaciones y comprobaciones. El código se ve y se ejecuta al renderizar.

Front matter:

```yaml
---
title: "Cuánto te afecta la semilla al resultado final"
description: "..."
description-meta: "..."
author: "Leonardo Hansa"
date: "2025-04-12"
categories: [exploraciones]
execute:
  echo: true
  eval: true
  message: false
  warning: false
freeze: true
---
```

El texto entre bloques sigue mandando: explica qué se busca antes de cada bloque y qué ha salido
después. Un cuaderno no es una sucesión de celdas con un comentario encima.

## Front matter y convenciones

- Campos obligatorios: `title`, `description`, `description-meta`, `author: "Leonardo Hansa"`,
  `date`, `categories`.
- `description-meta` es **idéntica** a `description`. Una o dos frases, en la voz del blog, que
  digan qué te llevas del post.
- Categorías permitidas, no inventar otras:
  - `[datos]` — estadística y machine learning explicados.
  - `[exploraciones]` — experimentos y simulaciones con código.
  - `[lecturas]` — notas de libros.
- Slug de la carpeta: `posts/YYYY-MM-DD-titulo-en-kebab-case/`, sin acentos ni eñes. La `date` del
  front matter coincide con la fecha del slug, en formato `"YYYY-MM-DD"`.
- Los títulos son afirmaciones o preguntas concretas ("Cómo un modelo erróneo predice mejor que uno
  correcto"), no etiquetas de temario.

## Autoría cuando escribe Claude

Todo post que redacte Claude tiene que dejarlo dicho. Elige una de estas dos formas:

- Pon `author: "Claude"` en el front matter, en vez de `"Leonardo Hansa"`.
- Deja `author: "Leonardo Hansa"` y añade, justo debajo del front matter, antes del primer párrafo,
  la línea `*Escrito por Claude.*`.

No hace falta combinar las dos. Cualquiera de ellas es suficiente, pero una de ellas es obligatoria.

## Código

- **Python por defecto.** El repo tiene posts antiguos en R; se quedan como están. No escribas R
  nuevo salvo que se pida explícitamente.
- Librerías disponibles, con las versiones fijadas en `requirements.txt`: `numpy` 1.24.2,
  `pandas` 1.5.3, `matplotlib` 3.7.0, `statsmodels` 0.14.2. Si un post necesita otra cosa
  (`scikit-learn`, `seaborn`), hay que añadirla a `requirements.txt` en el mismo PR.
- Numpy es la herramienta principal; pandas solo si los datos lo piden de verdad.
- `np.random.seed(...)` siempre que haya aleatoriedad: el post tiene que dar el mismo resultado en
  cada render.
- Etiqueta los bloques: `#| label: libs`, `#| label: datos`, `#| label: modelo-accion`.
- Bloques cortos, que quepan en pantalla. Un bloque hace una cosa.
- Los gráficos, con matplotlib y sin florituras: histograma, línea, `axvline` para marcar la media.

## Renderizar en local

```bash
pip install -r requirements.txt        # dependencias de Python
quarto preview                         # sitio completo, con recarga
quarto render posts/<slug>/index.qmd   # solo un post
```

Para los posts en R hace falta además `renv::restore()`.

`_quarto.yml` tiene `execute: freeze: auto` y los resultados congelados se versionan en `_freeze/`.
Si un post ejecuta código, su carpeta de `_freeze/` entra en el commit: es lo que permite que el
workflow de publicación no tenga que recalcularlo todo.

`_site/` es salida de build y está en `.gitignore`. No lo toques.

El push a `main` dispara `.github/workflows/publish.yml`, que renderiza y publica en `gh-pages`.

## Antes de dar un post por terminado

- [ ] `quarto render posts/<slug>/index.qmd` termina sin errores.
- [ ] `description` y `description-meta` rellenas e iguales.
- [ ] Si lo ha escrito Claude, autoría marcada (`author: "Claude"` o nota `*Escrito por Claude.*`).
- [ ] Categoría de la lista permitida.
- [ ] Fecha del front matter igual a la del slug.
- [ ] Si ejecuta código, `_freeze/` actualizado y añadido al commit.
- [ ] Léelo en voz alta: si suena a manual, reescríbelo.
