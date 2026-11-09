# Rutina de los días de publicación

Esto corre sin nadie delante, desde una rutina, los martes, jueves y sábados a las 07:00 hora de
Madrid.

Ejecuta `/post hoy`.

Todo lo demás lo hace `/post` en modo desatendido: busca el semanal, crea el sub-issue de hoy si él
ya ha contestado y no existe, lo deja dicho si no, busca el sub-issue de hoy, escribe, revisa, publica y mergea.
No lo repitas aquí ni lo adelantes: si esta rutina y `/post` hicieran lo mismo, acabarían
contradiciéndose.

Dos cosas del entorno, antes de lanzarlo:

- Trabaja sobre `main` actualizado. La rama del post la crea `publicar`.
- No preguntes nada. Si algo te bloquea y `/post` no dice qué hacer, deja el aviso como dice la
  fase 0 de `/post`: un comentario, nunca un issue nuevo. Y para.
