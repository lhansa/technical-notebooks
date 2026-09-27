# Rutina de los días de publicación

Esto corre sin nadie delante, desde una rutina, los martes, jueves y sábados a las 07:00 hora de
Madrid.

Ejecuta `/post hoy`.

Todo lo demás lo hace `/post` en modo desatendido: busca el semanal, crea los sub-issues si él ya
ha contestado, lo deja dicho si no, busca el sub-issue de hoy, escribe, revisa, publica y mergea.
No lo repitas aquí ni lo adelantes: si esta rutina y `/post` hicieran lo mismo, acabarían
contradiciéndose.

Dos cosas del entorno, antes de lanzarlo:

- Trabaja sobre `main` actualizado. La rama del post la crea `publicar`.
- No preguntes nada. Si algo te bloquea y `/post` no dice qué hacer, abre un issue
  `[aviso] AAAA-MM-DD: <qué pasó>` como el de la fase 0 de `/post`, con el pie de Claude Code, y
  para.
