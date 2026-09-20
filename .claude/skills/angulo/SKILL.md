---
name: angulo
description: Convierte un tema crudo en dos o tres fichas de enfoque para un post del blog, con su gancho de apertura y su ejemplo concreto, y las discute hasta cerrar una. Úsala como primer paso al escribir un post, antes de redactar nada.
allowed-tools: Read, Grep, Glob, Bash
---

Lee `CLAUDE.md` antes de nada, y en particular la sección **La voz** y la de los dos formatos.

Tienes un tema. Todavía no hay post. Lo que hay que decidir aquí es por dónde se entra, y esa
decisión vale más que todo lo que venga después.

Esto es una conversación, no un informe. Propones, él empuja, cierras.

## Antes de proponer nada

Mira qué hay ya. Son 75 posts y varias series enteras: espurias, multicolinealidad, bayes, muestreo.
El solape es fácil y es un desperdicio.

```bash
for f in posts/*/index.qmd; do echo "== $f"; sed -n '1,8p' "$f"; done
```

Si el tema ya está cubierto, dilo antes que nada. Puede que aun así haya un ángulo nuevo, pero eso se
plantea sabiendo lo que hay.

Y lee entero uno de los dos últimos posts, `posts/2026-09-15-collider/index.qmd` o
`posts/2026-08-28-de-donde-sale-el-log-loss/index.qmd`. No para copiar, para acordarte de la vara.

## Las fichas

Dos o tres. Que sean **de verdad distintas**: tres variantes del mismo enfoque no son tres opciones.
Cada una lleva:

- **Título candidato.** Afirmación o pregunta concreta. "Controlar por más variables no es ser más
  riguroso", no "Introducción a los colliders".
- **La pregunta que el post responde**, en una frase.
- **Las tres primeras frases, escritas.** Literales, no descritas. El arranque en seco es donde se
  gana o se pierde al lector, y es lo único que permite comparar dos enfoques de verdad. Si aquí
  escribes "empezaría hablando de...", la ficha no sirve.
- **El ejemplo concreto.** Esto es obligatorio. La fuerza del post del collider no son las
  matemáticas, son los futbolistas de primera división que no llegan por las dos puertas a la vez.
  Sin ejemplo no hay ficha: hay temario.
- **Qué se deja fuera a propósito.** No es un cliffhanger ni un "continuará". Es dónde se corta
  sabiendo que ahí sigue habiendo tela, de forma que quien lo lea note el borde. Di también qué se
  deja fuera porque sobra, que no es lo mismo.
- **Formato y categoría.** Ensayo o cuaderno, y por qué. Cuaderno solo si la simulación demuestra
  algo que la prosa no puede; si el código es decorativo, es ensayo. Categoría de las tres de
  `CLAUDE.md`.
- **El riesgo.** Por dónde este enfoque concreto puede acabar sonando a manual.

## La parada

Presentas las fichas y **paras**. No escribes el post.

Él puede elegir una, mezclarlas ("la 2 con el ejemplo de la 3"), o mandarte a buscar otro sitio por
donde entrar. Ese ida y vuelta es el trabajo, no una interrupción de él.

### Cuando no hay nadie delante

Si esto corre sin humano —desde un issue, por GitHub Actions—, no te quedes esperando. Escribe las
fichas como primer comentario del PR, sigue con la que te parezca mejor y di en una línea por qué esa.
La parada no desaparece: se mueve al PR, y él contesta ahí si quiere otra.

## El cierre

Cuando haya una elegida, escríbela en seis líneas como brief para quien redacte: la pregunta, el
gancho, el ejemplo, dónde se corta, formato y categoría.

**En frases, no en campos.** Si el brief sale en viñetas, el borrador tiende a viñetas, y `CLAUDE.md`
pide prosa antes que listas.
