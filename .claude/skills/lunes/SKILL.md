---
name: lunes
description: Filtra las cinco fichas de `temas` - lanza rigor y hilos en paralelo, comprueba los "ya lo sabía" y elige tres, con una línea por cada descartado. Úsala el lunes, justo después de `temas`. No abre el issue semanal ni escribe posts.
allowed-tools: Read, Grep, Glob, Bash, Agent, mcp__github__list_issues, mcp__github__issue_read, mcp__github__search_issues
---

Lee `CLAUDE.md` antes de nada, y luego `.claude/skills/temas/SKILL.md`: los campos de la ficha son
los que vas a consumir.

Tienes cinco fichas. Salen tres. Este es el sitio donde un tema bonito con la cita inventada se
queda fuera, y donde se evita escribir tres semanas seguidas de machine learning.

Tú no juzgas las referencias ni el solape. Para eso están `rigor` y `hilos`. Tú eliges con lo que
ellos devuelven.

## 1. La entrada

Las fichas de `temas`, tal cual, con sus campos: Título, Campo, La idea, Referencia, Qué afirma la
referencia, Por qué es probable que no lo sepa, El ejemplo concreto, Formato y categoría, Solape.

Si a una ficha le falta la **Referencia** o **Qué afirma la referencia**, ya está fuera. No hace
falta mandársela a nadie.

## 2. Los "ya lo sabía"

`temas` ya mira esto, pero lo compruebas tú también. Es un criterio de la elección y no puede
depender de que otro se acordara.

Con `list_issues`, los issues con la etiqueta `semana`, abiertos y cerrados. De cada uno, los
comentarios con `issue_read` y `get_comments`. Apunta cada "ya lo sabía" con su **zona**, no solo
con su título: si dijo que ya conocía a James-Stein, el shrinkage de Efron y Morris cae con él.

Si todavía no hay ningún `[semana]`, dilo en una línea y sigue.

## 3. Rigor y hilos, en paralelo

Lánzalos en un solo mensaje, como en la fase 4 de `/post`. No les cuentes qué esperas que salga.

- **`rigor`**, con las fichas enteras y copiadas tal cual. Dile que son fichas de `temas`, no un
  borrador, para que use su sección de fichas. Te devuelve un veredicto por ficha: confirmada,
  imprecisa con su corrección, o no confirmada.
- **`hilos`**, con las mismas fichas. Aquí no buscas temas derivados, así que pídele otra cosa: por
  cada ficha, un veredicto de solape con el corpus (**ninguno**, **cercano pero distinto** o
  **solapa**), los posts que lo justifican con su ruta, y los enlaces que el post sostendría, entre
  cero y dos. Si te da temas derivados igualmente, ignóralos.

## 4. La elección

Tres criterios, en este orden. Un tema que cae en uno no llega al siguiente.

1. **Referencia confirmada por `rigor`.** Confirmada pasa. Imprecisa pasa con la corrección metida
   en la ficha: la cita que llega al issue es la buena. No confirmada queda fuera, sin discusión y
   sin segunda ronda.
2. **Novedad.** Fuera si `hilos` dice "solapa", o si el tema cae en una zona "ya lo sabía". "Cercano
   pero distinto" pasa: a menudo es justo el enlace que el post necesita.
3. **Variedad.** Los tres no pueden ser del mismo Campo. Si con los que quedan se pueden cubrir tres
   campos distintos, se cubren.

Lo normal es que sobrevivan más de tres. Entonces ordenas a los supervivientes con los mismos
criterios, de más a menos limpio, y te quedas con los tres primeros que respeten la variedad:

- confirmada va antes que imprecisa;
- a igual veredicto de `rigor`, solape "ninguno" antes que "cercano pero distinto";
- si aún empatan, el que ya traiga números comprobados en el ejemplo.

Es mecánico a propósito. Si te gusta más uno de los de abajo, dilo en las notas; la decisión es de
él, que puede cambiarlo en el issue.

Por cada descartado, **una línea** que diga en qué criterio cayó y por qué. "Rigor: no confirmada,
el DOI lleva a otro artículo" sirve. "No encaja" no.

## 5. Si pasan menos de tres

Pide más a `temas` **una sola vez**, con las cinco de antes contadas como ya propuestas. Pasa solo
las nuevas por los pasos 2 a 4, igual que las primeras.

Si aun así no llegan a tres, sigue con las que haya. No bajes el listón para rellenar. Y déjalo
dicho en la salida: cuántas pasaron y por qué se quedó corto.

## 6. Lo que entregas

Esto va tal cual al cuerpo del issue `[semana]`, así que el formato es fijo.

**Las cinco.** Una entrada por ficha, en el orden en que llegaron:

```
### N. <Título>
- Campo: <campo> · Formato: <ensayo o cuaderno>, <categoría>
- Referencia: <cita completa, ya corregida si rigor la dio por imprecisa>
- Rigor: <confirmada | imprecisa | no confirmada> — <la corrección o lo que falló, en una línea>
- Hilos: <ninguno | cercano pero distinto | solapa> — <posts con su ruta; enlaces propuestos, si hay>
```

**Los elegidos.** Los tres, con el número de ficha, en el orden en que propones publicarlos
(martes, jueves, sábado). Una línea por cada uno con lo que lo sostiene.

**Los descartados.** Una línea por cada uno, con el criterio en que cayó.

**Notas.** Los "ya lo sabía" que se aplicaron, o que no había ninguno. Si se pidió una segunda
tanda a `temas`. Si salieron menos de tres.

## Lo que no haces

No abres el issue `[semana]` ni los sub-issues: eso es otro paso, con tu salida delante. No
escribes posts. No verificas referencias por tu cuenta ni relanzas `rigor` para regatearle un
veredicto: si dice no confirmada, es no confirmada.
