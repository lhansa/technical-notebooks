---
name: semana
description: Lleva el estado semanal en issues - abre el `[semana]` con la salida de `temas` y, cuando `lunes` ha recogido su elección, cuelga de él un sub-issue por post. También cierra el semanal cuando se cierran sus sub-issues. Úsala justo después de `temas` o de `lunes`. No elige temas, no escribe posts y no abre PR.
allowed-tools: Bash, mcp__github__list_issues, mcp__github__issue_read, mcp__github__issue_write, mcp__github__sub_issue_write, mcp__github__add_issue_comment
---

Lee `CLAUDE.md` antes de nada. Luego mira qué entregan `.claude/skills/temas/SKILL.md` y
`.claude/skills/lunes/SKILL.md`, en sus secciones "Qué entregas" y "Lo que entregas": es lo que
recibes, y lo mueves tal cual.

El estado de la semana vive en issues, no en ficheros del repo. Así él lo ve desde el móvil y
contesta con un comentario.

Hay dos modos. **abrir** va el lunes, detrás de `temas`. **materializar** va cuando él ha
contestado, detrás de `lunes`. En los dos, lo primero es mirar qué existe ya: esta skill se puede
lanzar dos veces seguidas y la segunda no puede duplicar nada.

Tú no redactas. Copias. Si algo de lo que recibes está mal, lo dices, pero no lo arreglas.

## Abrir

### 1. La semana

El título es `[semana] AAAA-Wxx`, con la semana ISO. Si no te dicen otra, es la de hoy:

```bash
date +%G-W%V
```

Ojo con el año: `%G` es el año de la semana ISO, no el del calendario. El 31 de diciembre puede
caer en la semana 1 del año siguiente.

### 2. ¿Ya existe?

Con `list_issues`, etiqueta `semana`, abiertos y cerrados. Si alguno tiene exactamente ese título,
**no abres otro**. Das su número en una línea y paras.

Tampoco lo editas. Si ya está abierto, puede que él haya empezado a leer las fichas, o a contestar.

### 3. El cuerpo

Tres partes, en este orden y sin nada más:

1. **Las cinco fichas de `temas`, tal cual.** Numeradas y con todos sus campos, en el orden de
   `temas`. Los "no confirmado" se quedan: él tiene que verlos antes de elegir. Sin veredictos, sin
   resumen delante y sin comentarios tuyos.
2. **Las dos líneas finales de `temas`**: zonas descartadas y campos cubiertos.
3. **El bloque fijo**, separado por una línea `---` y copiado letra a letra:

```
Contesta con tres líneas, en orden de martes, jueves y sábado.
Cada una es un número de ficha o un tema tuyo en texto libre.
Si alguna ya la sabías, añade "la N ya lo sabía".
Hasta que contestes no se crean sub-issues ni se escribe nada.
```

Si `temas` trae un preámbulo, como el mapa o una nota sobre cómo comprobó las citas, se queda
fuera. Al semanal van las fichas y las dos líneas.

Antes de abrir, cuenta: cinco fichas, y cada una con sus nueve campos. Si falta una ficha o un
campo, no abras. Di qué falta. Un semanal con cuatro fichas es una semana que él elige con menos
material sin saberlo.

### 4. El issue

`issue_write` con `method: create`, el título, el cuerpo y la etiqueta `semana`. Da el número y
el enlace en una línea.

## Materializar

### 1. La entrada

Dos cosas: el número del `[semana]` y la salida de `lunes`, que son sus bloques y, debajo, las
líneas de lo que no entró.

Lee el semanal con `issue_read`. Si está cerrado, **para**: una semana cerrada no recibe
sub-issues. Dilo en una línea.

Si `lunes` dijo que él todavía no ha contestado, no hay bloques. No crees nada: salta al punto 5,
el cierre, y para.

### 2. Los bloques

Cada bloque empieza por una línea `### <Título>` y va hasta el siguiente `###` o hasta las líneas
de lo que no entró. De cada uno sacas dos cosas:

- **El título del sub-issue**: el texto de la línea `###`, sin la almohadilla y sin retocar.
- **El cuerpo**: el bloque entero, tal cual, con su línea `###` incluida.

Cada bloque tiene que llevar su línea `Publicación: <día> AAAA-MM-DD`. Es la que busca la rutina de
cada día para saber qué post le toca. **Sin esa línea el bloque no se crea**: se va al comentario
del punto 4 con el motivo.

### 3. Los sub-issues

Primero mira qué hay. Con `issue_read` y `get_sub_issues` sobre el semanal, anota los títulos y la
línea `Publicación:` de cada sub-issue que ya exista, abierto o cerrado.

Un bloque ya está creado si hay un sub-issue con **el mismo título o la misma línea
`Publicación:`**. Con cualquiera de las dos coincidencias, no lo creas otra vez. Si coincide una y
la otra no, algo ha cambiado entre dos pasadas: no toques el que existe y dilo.

Antes de crear, mira también si el issue existe suelto. Con `list_issues` y la etiqueta
`idea-post`, abiertos, busca el título exacto. Si está pero no cuelga del semanal, es una pasada
anterior que se cortó entre crear y colgar. No abras otro: cuélgalo con `sub_issue_write`,
`method: add`, y el `id` del issue, que no es su número.

Para cada bloque que falte, `issue_write` con:

- `method: create`,
- el título y el cuerpo del punto 2,
- la etiqueta `idea-post`,
- `parent_issue_number`: el número del semanal.

Así se crea y se cuelga en una sola llamada. Créalos en el orden de publicación: martes, jueves,
sábado.

Al terminar, vuelve a leer los sub-issues del semanal y comprueba que salen todos, cada uno con su
`Publicación:`. Si alguno se creó pero no cuelga, cuélgalo con `sub_issue_write`.

### 4. Lo que no entró

Si `lunes` dejó líneas de lo que no entró, van en **un** comentario en el semanal, con
`add_issue_comment`. La primera línea es fija:

```
No entró en los sub-issues:
```

Y debajo, las líneas de `lunes` tal cual, más las tuyas del punto 2 si algún bloque no traía su
`Publicación:`.

Antes de comentar, lee los comentarios del semanal con `get_comments`. Si ya hay uno que empieza
por esa línea y dice lo mismo, no lo repitas.

Si todo entró, no comentes nada. Un "todo bien" en el semanal es ruido.

### 5. El cierre

Con `get_sub_issues`, mira el estado de todos los sub-issues del semanal. Si hay al menos uno y
**todos están cerrados**, cierra el semanal con `issue_write`, `method: update`, `state: closed` y
`state_reason: completed`.

Un semanal sin sub-issues no se cierra: es que él aún no ha contestado.

Este punto se puede lanzar solo, sin salida de `lunes`, para comprobar si una semana ya terminó.
Cada PR de post lleva `Closes #N` y el merge cierra su sub-issue; el semanal no se cierra por su
cuenta.

### 6. Lo que entregas

Una línea por sub-issue, en orden, con su número, su día y su título. Otra por cada cosa que no
hiciste y por qué: un bloque que ya existía, uno sin `Publicación:`, un comentario que ya estaba.
Y si cerraste el semanal, dilo.

## Lo que no haces

No eliges temas ni cambias el orden. No retocas títulos, fichas ni avisos de `hilos`: si un bloque
te parece mal, lo dices. No comentas en nombre de él. No escribes posts ni abres PR. No creas
etiquetas: `semana` e `idea-post` ya existen.
