---
description: Escribe un post del blog sobre un tema, de la elección del enfoque hasta el pull request.
argument-hint: <tema>
model: inherit
---

Tema: $ARGUMENTS

Las reglas del blog están en @CLAUDE.md y mandan sobre todo lo que sigue.

Vas a escribir un post en cinco fases. Antes de empezar, una cosa que decide el resto: **el post lo
escribes tú, en este hilo.** No delegues la redacción a un subagente. Escribir en esta voz necesita
tener a la vez delante las reglas, los posts de referencia, la conversación del ángulo y los
resultados del código. Un subagente lo haría con contexto pobre y saldría plano.

Los subagentes son para lo que gana con mirada fresca o con búsqueda amplia. Nada más.

## Fase 1 — El enfoque

Usa la skill `angulo` con el tema.

Te dará dos o tres fichas, cada una con su gancho escrito y su ejemplo concreto.

**Para aquí y espera a que elija.** No escribas el post. No sigas a la fase 2. Esta parada es el
punto del pipeline donde él decide, y saltársela lo convierte en una máquina de sacar posts
correctos y olvidables.

La única excepción es que no haya nadie delante, porque esto corra desde un issue por GitHub
Actions. Entonces aplica lo que dice `angulo` para ese caso: las fichas van al PR como comentario y
sigues con la mejor.

## Fase 2 — El material

Con la ficha ya elegida, lanza en paralelo:

- **`laboratorio`**, solo si la ficha dice cuaderno. Le pasas la ficha entera y qué quieres que
  demuestre la simulación. Te devuelve bloques ejecutados y números reales.
- **`hilos`**, siempre. Le pasas la ficha. Te devuelve entre cero y dos enlaces internos y dos o tres
  temas derivados.

Si la ficha dice ensayo, `laboratorio` no se usa. No metas código decorativo.

## Fase 3 — Escribir

Ahora tú.

Antes de teclear, lee entero `posts/2026-08-28-de-donde-sale-el-log-loss/index.qmd` si es ensayo, o
`posts/2026-09-15-collider/index.qmd` si es cuaderno. `CLAUDE.md` lo pide y tiene razón: la voz se
coge leyendo, no repasando una lista de reglas.

El arranque ya está escrito: son las tres primeras frases de la ficha. Empieza por ahí.

Los números que afirmes salen de lo que devolvió `laboratorio`. Ninguno de otro sitio.

Crea la carpeta y el fichero: `posts/YYYY-MM-DD-titulo-en-kebab-case/index.qmd`.

## Fase 4 — La revisión en frío

Lanza en paralelo, pasándoles la ruta del fichero:

- **`rigor`**: comprueba si lo que dice es verdad.
- **`oido`**: comprueba si suena a él o suena a manual.

Los dos leen del disco. No les cuentes lo que pretendías: leerlo sin contexto es justamente para lo
que sirven.

**Una sola ronda.** Los hallazgos de `rigor` sobre algo falso se corrigen todos. Los de `oido` los
filtras: coge los que delatan de verdad y deja pasar lo defendible. Aplicar catorce correcciones
seguidas produce texto sobre-editado y plano, que es lo mismo que suena a manual por otro camino.

Si rechazas un hallazgo, di por qué en una línea.

## Fase 5 — Cerrar

Usa la skill `publicar`. Se encarga del checklist, el render, `_freeze/`, la rama, el commit, el PR y
los issues de los temas derivados de `hilos`.

Al terminar, dile en tres líneas: dónde está el PR, qué enfoque se eligió y qué temas quedaron
abiertos como issues.
