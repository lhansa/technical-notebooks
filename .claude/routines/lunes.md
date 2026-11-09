# Rutina del lunes

Esto corre sin nadie delante, desde una rutina, los lunes a las 07:00 hora de Madrid. Nadie lee lo
que dices en el hilo: lo que tenga que saber él va a GitHub. La excepción es el mensaje final del
paso 4, que le llega al móvil como notificación.

Lee `CLAUDE.md` antes de nada.

## 1. Los temas

Usa la skill `temas`. Te devuelve la línea del modo, cinco fichas, dos líneas debajo y, en modo
cierre, el backlog de martes y jueves. El modo lo explica `CLAUDE.md` en **Issues de temas y
calendario**.

Aquí no hay nadie a quien preguntar si ninguna convence. Las cinco van tal cual.

## 2. El semanal

Con la salida de `temas` entera, usa la skill `semana` en modo **abrir**, con la semana ISO de hoy:

```bash
date +%G-W%V
```

En modo cierre, `semana` cuelga además los dos issues del backlog y cierra los cubiertos.

Si `semana` dice que el semanal de esta semana ya existe, no hagas nada más. Es que la rutina ha
corrido dos veces.

## 3. Si algo para

Si `temas` no llega a cinco fichas completas, o `semana` se niega a abrir por otro motivo, no
abres nada en GitHub: ni semanal ni issue `[aviso]`. `CLAUDE.md` no deja abrir issues. El motivo
va en el mensaje final.

## 4. El aviso

GitHub no le avisa de los issues que abres, porque salen con su cuenta. El aviso es tu último
mensaje: la tarea programada tiene el push activado y se lo manda al móvil al terminar.

- Si has abierto el semanal en modo normal, termina con una sola línea:
  `Semanal AAAA-Wxx listo: elige tres. <enlace al issue>`
- Si lo has abierto en modo cierre:
  `Semanal AAAA-Wxx listo: elige una para el sábado. Martes #A, jueves #B. Quedan N issues. <enlace>`
- Si no hay semanal, termina con una sola línea:
  `Esta semana no hay semanal: <qué paró, en diez palabras>`
- Si el semanal ya existía, termina en silencio: sin mensaje ni resumen. Es una segunda ejecución y
  el aviso ya salió en la primera.

Nada más en ese mensaje. Sin las fichas, sin resumen de lo que has hecho y sin saludo.

## Lo que no haces

No eliges temas. No creas sub-issues de las fichas: eso pasa cuando él conteste, en la rutina del
día de publicación. No escribes posts. No abres más issues que el semanal.
