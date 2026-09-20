---
name: publicar
description: Cierra un post ya escrito y revisado - comprueba el front matter y la autoría, renderiza con Quarto, versiona el freeze, crea la rama, el commit y el pull request, y abre los issues de los temas derivados. Úsala como último paso, cuando el borrador está terminado.
allowed-tools: Read, Edit, Bash, Glob, Grep, mcp__github__create_pull_request, mcp__github__issue_write, mcp__github__get_label, mcp__Claude_Code_Remote__subscribe_pr_activity
---

Lee `CLAUDE.md` antes de nada. El checklist de "Antes de dar un post por terminado" es el que
ejecutas; esto lo ordena y añade lo que va después.

El orden importa: **todo lo que puede fallar, falla antes del commit.**

## 1. El entorno

```bash
command -v quarto || echo "FALTA QUARTO"
```

Si no está, **para aquí** y dilo. No hay atajo: sin render no se actualiza `_freeze/`, y el post o
se publica desde una caché vieja o revienta en el workflow. Instalar Quarto o cambiar de entorno es
decisión suya, no tuya.

## 2. El fichero

Comprueba, y arregla lo que puedas arreglar sin tocar la prosa:

- Carpeta `posts/YYYY-MM-DD-titulo-en-kebab-case/index.qmd`, sin acentos ni eñes en el slug.
- La `date` del front matter **coincide con la fecha del slug**. Hay cuatro posts viejos donde no
  coinciden; son deuda, no precedente.
- Están los seis campos: `title`, `description`, `description-meta`, `author`, `date`, `categories`.
- `description` y `description-meta` son **idénticas**, carácter a carácter.
- La categoría es una de las tres de `CLAUDE.md`. Ninguna otra.
- Si el post ejecuta código, lleva su bloque `execute:` y `freeze: true`.

### La autoría: parada dura

Todo lo que sale de este pipeline lo ha escrito Claude, así que la regla de `CLAUDE.md` aplica
siempre. Comprueba que hay una de las dos formas:

```bash
head -20 posts/<slug>/index.qmd | grep -E 'author: "Claude"|\*Escrito por Claude\.\*'
```

Si no hay ninguna, **no commitees**. Añádela y vuelve a comprobar.

## 3. El render

```bash
quarto render posts/<slug>/index.qmd
```

Tiene que terminar sin errores. Un aviso de Quarto no es un error, pero léelo.

Si el post ejecuta código, mira que `_freeze/posts/<slug>/` se haya actualizado: eso es lo que evita
que el workflow de publicación tenga que recalcularlo todo.

## 4. Rama, commit y PR

```bash
git checkout -b claude/<slug>
git add posts/<slug>/ _freeze/posts/<slug>/
git status --short
```

Revisa el `git status` antes de commitear. `_site/` está en `.gitignore` y no debe aparecer. Si ves
ficheros que no esperabas, para y mira qué son.

Mensaje de commit en la línea de los que ya hay: "Añade el post del collider", "Post nuevo: de dónde
sale el log-loss". Descriptivo y en castellano.

```bash
git push -u origin claude/<slug>
```

El PR rellena `.github/pull_request_template.md` con sus tres secciones —Qué cambia, Por qué,
Comprobaciones— y marca las casillas que de verdad hiciste.

Después, suscríbete a la actividad del PR con `subscribe_pr_activity`.

## 5. Los temas derivados

`hilos` devolvió dos o tres. Abre un issue por cada uno, con el título que propuso, el porqué en el
cuerpo, y un enlace al PR del post del que salieron.

Sobre la etiqueta: mira con `get_label` si existe `idea-post` en el repo.

- Si existe, úsala.
- Si no, abre los issues **sin etiqueta** y con el título prefijado `[idea]`, y dile a él que la cree
  a mano una vez. El servidor de GitHub disponible aquí no crea etiquetas.

## 6. El último punto

El checklist de `CLAUDE.md` termina con "léelo en voz alta: si suena a manual, reescríbelo". Ya pasó
por `oido`, pero el que manda es él. Dile en una línea dónde está el post y qué falta por mirar, si
falta algo.
