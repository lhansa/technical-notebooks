# CLAUDE.md

Instrucciones para trabajar en este repositorio.

## Qué es esto

Blog técnico de Leonardo Hansa, construido con Quarto. Se publica en
<https://lhansa.github.io/technical-notebooks>.

Un post es una carpeta dentro de `posts/` con un único `index.qmd`:

```
posts/2026-08-28-de-donde-sale-el-log-loss/index.qmd
```

## Idioma

**Los posts nuevos se escriben en inglés**: título, `description`, cuerpo, comentarios del código y
slug. Desde octubre de 2026. Los posts anteriores están en español y se quedan así; no se traducen.

Todo lo que no se publica sigue en español: estas instrucciones, las piezas de `.claude/`, las fichas
de `temas` y `angulo` (salvo su título y su arranque, que ya van en inglés), los issues, los commits y
los PR. Si un tema o un título llega en español, se traduce al escribir el post.

El inglés, claro y de nivel C1, con algo de C2 si cae natural. Sin modismos rebuscados ni frases
hechas que solo entiende un nativo. Tiene que sonar escrito en inglés por una persona, no traducido
del español: si una frase arrastra el orden o los giros del castellano, se rehace.

El listado de posts lo genera `cuadernos.qmd`; no hay que registrar nada a mano al añadir uno.

## El sistema

Para escribir un post, `/post <tema>`. Son cinco fases y una parada.

La skill `angulo` propone dos o tres enfoques con su gancho escrito y su ejemplo concreto, y **para
ahí** hasta que elijas uno. Después, el agente `laboratorio` escribe y ejecuta el código si el post
es un cuaderno, y `hilos` busca enlaces a posts viejos y saca temas para los siguientes. La
redacción no se delega: la hace el hilo principal. Ya escrito, `rigor` comprueba en frío si lo que
dice es verdad y `oido` si suena a manual. La skill `publicar` cierra: render, `_freeze/`, rama,
commit, PR y los issues de los temas derivados.

Las piezas están en `.claude/`. Contienen procedimiento, no reglas: las reglas del blog viven en
este fichero y solo aquí.

## La voz

Esto es lo más importante del fichero. Un post que esté bien de contenido pero suene a LLM no sirve.

Las reglas valen en inglés. Los ejemplos de cada una están en el idioma en que se escribe ahora.

Reglas:

- **Háblale a quien lee, en segunda persona.** "You train a classifier", "look at this", "it costs
  you". Nunca "the reader", "one" ni "we" de manual.
- **Empieza en seco.** La primera frase ya está dentro del tema. Nada de "In this post we'll
  explore", "Let's dive in" ni de contextualizar la importancia del asunto.
- **Frases cortas. Párrafos de una o dos líneas.** El texto respira; se lee bajando rápido.
- **Sujeto, verbo y complementos.** Una idea por frase. Antes tres frases seguidas que una oración
  con dos subordinadas dentro. Si una frase lleva un "que" y dos comas, pártela.
- **Nada de "it's not X, it's Y".** Las construcciones de contraste ("it's not a computational
  trick, it's the only way", "not just X but Y") se gastan enseguida y acaban sonando a tic. Di lo
  que la cosa es y sigue.
- **Primera persona para lo tuyo.** "I spent years like that", "I don't particularly care about the
  data".
  La experiencia propia y las dudas propias son parte del texto.
- **Negrita para la idea que sostiene el post**, una o dos veces por sección, no más.
- **Cierra con la consecuencia**, no con un resumen. Qué cambia para quien lee, ahora que sabe esto.
- Humor seco y frases de andar por casa cuando encajen. Sin exclamaciones ni entusiasmo impostado.
- Prosa antes que listas. Una lista es para enumerar cosas de verdad, no para trocear un argumento.
- Fuera el relleno: "it's worth noting", "it's important to note", "in today's world", "delve",
  "let's unpack", "crucial", "game-changer", "Here's the thing".
- Matemáticas en LaTeX inline (`$p$`, `$-\log p$`) y en bloque `$$...$$` cuando la fórmula es el
  centro del párrafo. Explica la fórmula en palabras antes o después de escribirla.

Referencia de tono, de `posts/2026-08-28-de-donde-sale-el-log-loss/index.qmd`, y cómo suena en
inglés:

> Yo estuve años así. Sabía usarla. Sabía que penalizaba mucho equivocarte con confianza. Y hasta
> ahí llegaba. La fórmula tenía pinta de capricho: alguien la eligió porque le funcionaba bien y ya
> está.

> I spent years like that. I knew how to use it. I knew it punished you hard for being wrong with
> confidence. And that was about it. The formula looked arbitrary: someone picked it because it
> worked, end of story.

> Pues resulta que hay un motivo detrás. Y es bastante bonito.

> Turns out there's a reason behind it. A pretty nice one.

> Estar muy seguro y fallar **es** llevarse mucha sorpresa. El castigo sale de ahí, de la propia
> definición. Nadie lo puso a mano.

> Being very sure and wrong **is** getting a big surprise. The penalty comes from there, from the
> definition itself. Nobody put it in by hand.

Cuando dudes de cómo suena algo, lee un post reciente entero antes de escribir. El ritmo y la actitud
se cogen igual de un post en español; las palabras, no.

## Los dos formatos de post

### Ensayo (por defecto)

Explicaciones y desarrollos de una idea. Solo prosa y fórmulas, sin código ejecutado. Es el formato
por defecto cuando se pide "escribe un post sobre X".

Front matter:

```yaml
---
title: "Where log-loss comes from"
description: "The loss function you use for classification comes from measuring surprise. Here's the path."
description-meta: "The loss function you use for classification comes from measuring surprise. Here's the path."
author: "Leonardo Hansa"
date: "2026-08-28"
seccion: "estadística"
categories: [predictive models]
---
```

Estructura habitual: arranque de dos o tres párrafos que plantea la pregunta, tres o cuatro
secciones `##` que la desarrollan, y una última sección tipo "Why this matters to you".

### Cuaderno con código

Experimentos, simulaciones y comprobaciones. El código se ve y se ejecuta al renderizar.

Front matter:

```yaml
---
title: "How much your random seed moves the final result"
description: "..."
description-meta: "..."
author: "Leonardo Hansa"
date: "2025-04-12"
seccion: "estadística"
categories: [predictive models, simulation, python]
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
  `date`, `seccion`, `categories`.
- `description-meta` es **idéntica** a `description`. Una o dos frases, en la voz del blog, que
  digan qué te llevas del post.

## Sección y tags

Cada post lleva una sección y varios tags. Son cosas distintas y ninguna depende del formato: un
post de estadística es de estadística tenga código o no.

**Sección** (`seccion:`, una y solo una). Es lo que agrupa las tablas de `cuadernos.qmd`. Es una
clave interna que no se ve en la web, así que se escribe tal cual, en español, también en los posts
en inglés:

- `"estadística"` — una idea de estadística o de modelado explicada, con o sin simulación.
- `"exploraciones"` — un conjunto de datos real que se explora.
- `"herramientas"` — programación, R/Python, rendimiento, formatos, limpieza, gráficos como oficio.
- `"lecturas"` — notas de libros.

**Tags** (`categories:`, el campo que Quarto usa para filtrar). Se ven en la web, así que van en
inglés. De esta lista, no inventar otros:

- Tema: `bayes`, `regression`, `causality`, `sampling`, `probability`, `predictive models`,
  `visualization`, `statistical rethinking`, `taleb`.
- Método u oficio: `simulation`, `performance`, `cleaning`, `rlang`, `ine`.
- Lenguaje: `r` o `python`, en todo post que ejecute código.

Los posts anteriores a octubre de 2026 llevan todavía los tags en español (`regresión`,
`simulación`...). Están pendientes de migrar; no los copies a un post nuevo.

Entre uno y cuatro tags por post. No repitas la sección como tag. Un tag nuevo solo entra en la
lista si ya hay tres posts que lo llevarían; si se añade, se añade aquí.

## Otras convenciones

- Slug de la carpeta: `posts/YYYY-MM-DD-title-in-kebab-case/`, en inglés como el título. La `date` del
  front matter coincide con la fecha del slug, en formato `"YYYY-MM-DD"`.
- Los títulos son afirmaciones o preguntas concretas ("How a wrong model predicts better than the
  right one"), no etiquetas de temario. En inglés, en sentence case: solo la primera palabra y los
  nombres propios con mayúscula.

## Autoría cuando escribe Claude

Todo post que redacte Claude tiene que dejarlo dicho. Elige una de estas dos formas:

- Pon `author: "Claude"` en el front matter, en vez de `"Leonardo Hansa"`.
- Deja `author: "Leonardo Hansa"` y añade, justo debajo del front matter, antes del primer párrafo,
  la línea `*Written by Claude.*`. Los posts viejos en español llevan `*Escrito por Claude.*`.

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
- [ ] Título, `description`, cuerpo, comentarios del código y slug en inglés.
- [ ] `description` y `description-meta` rellenas e iguales.
- [ ] Si lo ha escrito Claude, autoría marcada (`author: "Claude"` o nota `*Written by Claude.*`).
- [ ] Una `seccion` de las cuatro y tags de la lista, en inglés.
- [ ] Fecha del front matter igual a la del slug.
- [ ] Si ejecuta código, `_freeze/` actualizado y añadido al commit.
- [ ] Léelo en voz alta: si suena a manual, reescríbelo.
