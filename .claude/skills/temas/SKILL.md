---
name: temas
description: Propone cinco temas para posts del blog, cada uno con una referencia primaria real, su ejemplo concreto y una línea de por qué es probable que él no lo sepa. Úsala el lunes: las fichas van tal cual al issue semanal y él elige tres. No elige ni escribe posts.
allowed-tools: Read, Grep, Glob, Bash, WebSearch, WebFetch, mcp__github__list_issues, mcp__github__issue_read, mcp__github__search_issues
---

Lee `CLAUDE.md` antes de nada: las categorías, la forma de los títulos y la sección **La voz**.

Tu trabajo es encontrar cinco cosas que él no sepa y que den para un post. Cinco fichas, cada una
atada a una referencia que existe. No eliges cuáles se escriben ni escribes nada: eso viene después.

Lo difícil no es encontrar temas. Es encontrar temas que no sean los de siempre. Un LLM tiende a
proponer el teorema central del límite, la paradoja de Simpson y el sesgo de supervivencia. Él los
sabe desde la carrera.

## 1. El mapa de lo que ya sabe

Antes de buscar nada, hazte el mapa. Son tres fuentes y las tres cuentan.

**El corpus.** Los front matter de los 75 posts, como hacen `angulo` y `hilos`:

```bash
for f in posts/*/index.qmd; do echo "== $f"; sed -n '1,8p' "$f"; done
```

Fíjate en las series: espurias, multicolinealidad, bayes, muestreo, jerárquicos. Un tema que cae
dentro de una serie ya abierta solo vale si dice algo que la serie no dijo.

**Los issues de ideas.** Con `list_issues`, abiertos y cerrados. Entran los que llevan la etiqueta
`idea-post` y también los viejos que no la llevan: títulos que empiezan por `[idea]` o por `Post:`.
Una idea ya apuntada no se vuelve a proponer, aunque la referencia sea otra.

**Los issues `[semana]` anteriores, con sus comentarios.** Etiqueta `semana`, abiertos y cerrados.
Lee el cuerpo y luego los comentarios con `issue_read` y `get_comments`. Cada tema propuesto antes
queda gastado, se eligiera o no.

Y aquí está lo que más importa: **si él comentó "ya lo sabía" sobre un tema, esa zona se cierra.**
No solo ese tema. Si dijo "ya lo sabía" del estimador de James-Stein, no vuelvas con el shrinkage de
Efron y Morris con otro título. Anota la zona, no la ficha.

Si todavía no hay ningún `[semana]`, dilo en una línea y sigue.

## 2. Dónde buscar

Estadística, machine learning, programación y ramas de las matemáticas. Todo orientado a datos: la
teoría de números pura no entra, la aritmética de coma flotante sí, porque te rompe una varianza.

Busca en frío con `WebSearch`. Funciona mejor ir a por autores y revistas que a por conceptos: *The
American Statistician*, *Biometrika*, *JASA*, *Statistical Science*, *Technometrics*, *ACM TOMS*,
*PNAS*, documentación técnica de numpy o de IEEE 754, libros de referencia. Las "paradojas" con
nombre propio y las notas cortas de dos o tres páginas suelen ser buen terreno.

Reparte. Cinco temas de inferencia bayesiana son una semana tirada. Él elige tres, y con cinco del
mismo campo no tiene dónde elegir. Dale material: **al menos tres campos distintos entre las cinco
fichas.**

## 3. La vara de novedad

Un tema no entra si:

- sale en un temario de estadística de la carrera, aunque sea en la última lección;
- ya está en el corpus o en un issue de ideas;
- cae en una zona marcada "ya lo sabía";
- es un tema de IA de moda sin estadística debajo. A él le interesa explicar, no predecir.

Cada ficha lleva una línea de **por qué es probable que no lo sepa**. Tiene que ser concreta:
"es de 1962 y en *Technometrics*, no de un manual" dice algo; "es un tema avanzado" no dice nada.

## 4. La referencia: parada dura

**Sin referencia primaria real no hay tema.** Paper, libro o documentación técnica. Un blog, un hilo
o una entrada de Wikipedia sirven para llegar a ella, no como referencia.

La cita va completa: autores, año, título, publicación, volumen y número, páginas, y DOI o URL.

Compruébala antes de ponerla, no la recuerdes. Con `WebSearch` busca el título exacto y confirma
autores, año, revista y páginas en al menos una fuente que no sea la tuya. `WebFetch` a las
editoriales suele estar bloqueado; si falla, tira de búsquedas contra el título y el DOI.

Y confirma también que **la referencia dice lo que la ficha afirma**. Este es el fallo que se cuela:
la cita existe, pero el paper dice otra cosa, o una cosa parecida con una condición que la ficha se
ha comido. Si no puedes confirmar algún dato, márcalo como "no confirmado" en la ficha. Nadie más
va a comprobar la cita antes de que él elija, así que tiene que verlo al leer la ficha.

Si una ficha se queda sin referencia confirmada, tírala y busca otra. No entregues cuatro buenas y
una coja.

## 5. Las fichas

Cinco, numeradas. Cada una con estos campos, en este orden, porque el issue semanal y los sub-issues
los leen así:

- **Título.** En la forma del blog: afirmación o pregunta concreta. "La fórmula de la varianza del
  libro no aguanta la coma flotante", no "Algoritmos numéricos para la varianza".
- **Campo.** Uno: estadística, machine learning, programación o matemáticas.
- **La idea.** Dos frases. Lo que el post contaría.
- **Referencia.** La cita completa, con DOI o URL.
- **Qué afirma la referencia.** Una o dos frases con lo que el paper dice y la ficha usa. Si el tema
  sale elegido, el post se apoya en esto.
- **Por qué es probable que no lo sepa.** Una línea, concreta.
- **El ejemplo concreto.** Obligatorio. Un caso con números o con una escena, no una descripción
  del tipo "un ejemplo con datos simulados". Sin ejemplo no hay tema: hay temario.
- **Formato y categoría.** Ensayo o cuaderno, y una de las tres categorías de `CLAUDE.md`. Cuaderno
  solo si la simulación demuestra algo que la prosa no puede.
- **Solape.** Los posts o issues más cercanos, con su ruta o número, o "ninguno cercano". Es lo que
  él ve antes de elegir, así que compruébalo leyendo los posts, no solo los títulos.

## La vara, con ejemplos

Estas siete referencias están comprobadas. Sirven para fijar el nivel, no para proponerlas otra vez
sin mirar el mapa.

- Lindley, D. V. (1957). "A statistical paradox". *Biometrika* 44(1/2), 187–192.
  doi:10.1093/biomet/44.1-2.187. Con muchos datos y algo de probabilidad a priori puesta en la
  nula, el p-valor la rechaza y la probabilidad a posteriori la apoya.
- Welford, B. P. (1962). "Note on a method for calculating corrected sums of squares and products".
  *Technometrics* 4(3), 419–420. doi:10.1080/00401706.1962.10490022. La fórmula de libro, suma de
  cuadrados menos corrección, pierde cifras significativas en coma flotante. La recurrencia de
  Welford también es de una pasada y no las pierde.
- Freedman, D. A. (1983). "A note on screening regression equations". *The American Statistician*
  37(2), 152–155. doi:10.1080/00031305.1983.10482729. Seleccionas variables entre ruido puro, reajustas
  y salen R² y F significativos.
- Gelman, A. y Carlin, J. (2014). "Beyond power calculations: Assessing Type S (sign) and Type M
  (magnitude) errors". *Perspectives on Psychological Science* 9(6), 641–651.
  doi:10.1177/1745691614551642. Con poca potencia, lo significativo sale exagerado y a veces con el
  signo cambiado.
- Vitter, J. S. (1985). "Random sampling with a reservoir". *ACM Transactions on Mathematical
  Software* 11(1), 37–57. doi:10.1145/3147.3165. Muestrear un flujo sin saber cuánto mide. El
  algoritmo básico es anterior (Knuth lo atribuye a Waterman); lo de Vitter es hacerlo rápido.
- Belkin, M., Hsu, D., Ma, S. y Mandal, S. (2019). "Reconciling modern machine-learning practice and
  the classical bias–variance trade-off". *PNAS* 116(32), 15849–15854. doi:10.1073/pnas.1903070116.
  Pasado el punto de interpolación, el error de test vuelve a bajar: *double descent*.
- Efron, B. y Morris, C. (1977). "Stein's paradox in statistics". *Scientific American* 236(5),
  119–127. doi:10.1038/scientificamerican0577-119. Con los promedios de bateo de 18 jugadores tras
  sus primeros 45 turnos de 1970, encoger cada uno hacia la media de todos predice mejor el resto de
  la temporada que el promedio de cada jugador por separado.

Fíjate en que varias ya rozan el corpus: Freedman con
`posts/2025-04-17-optimism-correction/`, Stein con
`posts/2026-09-20-el-condado-mas-radiactivo-tiene-dos-casas/`. Eso es justo lo que el campo
**Solape** tiene que decir.

## Qué entregas

Las cinco fichas, y debajo dos líneas: qué zonas descartaste por el mapa (corpus, ideas, "ya lo
sabía") y cuántos campos distintos cubren las cinco.

Si él pide más porque ninguna le convence, repites el proceso con el mapa ampliado:
las cinco de antes cuentan como ya propuestas.

## Lo que no haces

No eliges las tres. No abres el issue `[semana]`. No escribes posts. No comentas en issues. Eso lo
hacen otros, con tu salida delante.
