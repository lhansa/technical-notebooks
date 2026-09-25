---
name: lunes
description: Recoge los tres temas que él ha elegido en el issue semanal, sean fichas de `temas` o temas suyos, pasa los suyos por hilos sin bloquear y deja listo el cuerpo de cada sub-issue. Úsala cuando él haya contestado en el semanal. No elige, no filtra y no abre issues.
allowed-tools: Read, Grep, Glob, Agent, mcp__github__list_issues, mcp__github__issue_read, mcp__github__search_issues
---

Lee `CLAUDE.md` antes de nada, y luego `.claude/skills/temas/SKILL.md`: los campos de la ficha son
los que vas a mover.

El lunes `temas` propone cinco fichas y van tal cual al issue `[semana]`. Nadie las filtra. Él lee,
elige tres y contesta con un comentario. A veces uno de los tres no es de las fichas: es un tema
que se le ha ocurrido a él.

Tu trabajo empieza cuando ha contestado. Recoges lo que ha dicho y lo dejas listo para los
sub-issues. No eliges nada ni opinas sobre lo que ha elegido.

## 1. La entrada

El `[semana]` de esta semana, con `list_issues` por la etiqueta `semana`. Del cuerpo sacas las
cinco fichas. De los comentarios, con `issue_read` y `get_comments`, **el último del dueño del
repo**. Los comentarios de cualquier otra persona no cuentan.

Si todavía no ha contestado, no haces nada. Lo dices en una línea y paras.

## 2. Cómo contesta

Tres líneas, en el orden de publicación: martes, jueves y sábado. Cada línea es un número de ficha
o un tema suyo, en texto libre:

```
2
Por qué la media de los ratios no es el ratio de las medias
5
```

Las líneas del tipo "la 4 ya lo sabía" no son elección. Son el feedback que lee `temas` la semana
siguiente, y aquí no las tocas.

Si hay menos de tres, salen los que haya y lo dices. Si hay más, cuentan los tres primeros.

Si una línea no se entiende, no la adivines: un número que no está entre las fichas, o una frase
que no sabes si es un tema o un comentario. La dejas fuera y dices por qué. Es mejor un sub-issue
de menos que uno con el tema equivocado.

## 3. Las fichas

Un número lleva su ficha tal cual, con todos sus campos. No se vuelve a verificar nada: la
referencia la comprobó `temas`, y lo que diga el post lo comprobará `rigor` sobre el borrador.

## 4. Los temas suyos

Cada tema suyo va a `hilos`. Si hay más de uno, lánzalos en paralelo, en un solo mensaje.

Le pasas el texto tal cual y le pides tres cosas: un veredicto de solape con el corpus (**ninguno**,
**cercano pero distinto** o **solapa**), los posts que lo justifican con su ruta, y los enlaces que
ese post sostendría, entre cero y dos. No le pidas temas derivados. Si te los da, ignóralos.

**`hilos` no bloquea.** Aunque diga "solapa", el tema entra. El aviso va al sub-issue y lo lee
quien escriba. Él sabe lo que hay en su blog; si lo ha elegido, tendrá sus motivos.

Tampoco le pidas referencia. La regla de la referencia primaria es de `temas`, no de sus ideas.

## 5. Lo que entregas

Un bloque por día, en orden. Esto va tal cual al cuerpo de cada sub-issue, así que el formato es
fijo:

```
### <Título>
Publicación: <martes | jueves | sábado> AAAA-MM-DD
Origen: <ficha N de `temas` | tema propio>

<La ficha entera, con sus campos.>
  o bien
<Su texto tal cual.>
Hilos: <ninguno | cercano pero distinto | solapa>: <posts con su ruta; enlaces, si hay>
```

El título de una ficha es el suyo. El de un tema propio es su texto, sin retocar: el ángulo ya lo
buscará `angulo` cuando toque escribir.

La fecha sale de la semana del `[semana]`: el martes, el jueves y el sábado de esa semana.

Debajo de los bloques, una línea por cada cosa que no ha entrado y por qué: una línea ambigua, un
número que no existe, o que eligió menos de tres.

## Lo que no haces

No eliges, no sustituyes y no completas lo que falta. No pasas nada por `rigor`. No abres ni editas
issues: los sub-issues los crea otro paso, con tu salida delante.
