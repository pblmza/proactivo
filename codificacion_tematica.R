# Codificación temática manual de las respuestas abiertas (n = 27).
#
# Cada respuesta se leyó y se le asignaron uno o más temas (una misma
# respuesta suele tocar varias ideas a la vez). Este archivo es la fuente
# de verdad de esa codificación: si se agregan encuestas nuevas, hay que
# revisar y extender estas tablas a mano, no inferirlas automáticamente.
#
# id se refiere a la columna `id` de data_wide.csv / data_long.csv.

library(tibble)

# ---- Aspectos positivos ----
temas_positivos <- tribble(
  ~id, ~tema,
  1,  "Proactividad / pesquisa precoz",
  2,  "Calidad clínica / abordaje integral",
  3,  "Disponibilidad y accesibilidad",
  4,  "Proactividad / pesquisa precoz",
  5,  "Disponibilidad y accesibilidad",
  6,  "Disponibilidad y accesibilidad",
  6,  "Equipo multidisciplinario",
  7,  "Presencia y frecuencia (rondas diarias)",
  8,  "Rapidez de respuesta",
  9,  "Compromiso / trato humano",
  9,  "Comunicación y claridad",
  10, "Presencia y frecuencia (rondas diarias)",
  10, "Compromiso / trato humano",
  11, "Proactividad / pesquisa precoz",
  11, "Disponibilidad y accesibilidad",
  11, "Compromiso / trato humano",
  12, "Presencia y frecuencia (rondas diarias)",
  12, "Proactividad / pesquisa precoz",
  12, "Calidad clínica / abordaje integral",
  13, "Equipo multidisciplinario",
  13, "Calidad clínica / abordaje integral",
  14, "Presencia y frecuencia (rondas diarias)",
  15, "Presencia y frecuencia (rondas diarias)",
  15, "Compromiso / trato humano",
  16, "Presencia y frecuencia (rondas diarias)",
  16, "Compromiso / trato humano",
  17, "Proactividad / pesquisa precoz",
  17, "Comunicación y claridad",
  18, "Presencia y frecuencia (rondas diarias)",
  18, "Equipo multidisciplinario",
  19, "Disponibilidad y accesibilidad",
  20, "Presencia y frecuencia (rondas diarias)",
  20, "Comunicación y claridad",
  21, "Compromiso / trato humano",
  22, "Proactividad / pesquisa precoz",
  22, "Comunicación y claridad",
  22, "Compromiso / trato humano",
  22, "Equipo multidisciplinario",
  23, "Calidad clínica / abordaje integral",
  24, "Presencia y frecuencia (rondas diarias)",
  24, "Rapidez de respuesta",
  24, "Calidad clínica / abordaje integral",
  25, "Presencia y frecuencia (rondas diarias)",
  25, "Proactividad / pesquisa precoz",
  26, "Proactividad / pesquisa precoz",
  26, "Rapidez de respuesta",
  27, "Proactividad / pesquisa precoz"
)

# ---- Aspectos a mejorar ----
# ids 6,7,9,16,18,19,20,21,23,25 son respuestas vacías/"nada"/no interpretables
# ("Nada", "No lo sé", ".", "Siento") y se codifican como "Sin comentarios /
# conforme" en vez de omitirse, para que el gráfico refleje que una fracción
# relevante de encuestados no identifica nada que mejorar.
temas_mejorar <- tribble(
  ~id, ~tema,
  1,  "Comunicación/relación con equipo tratante",
  2,  "Continuidad y seguimiento ambulatorio",
  3,  "Continuidad y seguimiento ambulatorio",
  4,  "Mayor rol resolutivo (no solo sugerir)",
  5,  "Mayor rol resolutivo (no solo sugerir)",
  6,  "Sin comentarios / conforme",
  7,  "Sin comentarios / conforme",
  8,  "Mayor rol resolutivo (no solo sugerir)",
  9,  "Sin comentarios / conforme",
  10, "Continuidad y seguimiento ambulatorio",
  11, "Dotación de personal insuficiente",
  11, "Grupos grandes interrumpen la dinámica clínica",
  11, "Mayor rol resolutivo (no solo sugerir)",
  12, "Ampliar cobertura a otros servicios",
  13, "Claridad sobre alcance y competencias",
  13, "Mayor rol resolutivo (no solo sugerir)",
  14, "Claridad sobre alcance y competencias",
  15, "Mayor rol resolutivo (no solo sugerir)",
  15, "Continuidad y seguimiento ambulatorio",
  16, "Sin comentarios / conforme",
  17, "Grupos grandes interrumpen la dinámica clínica",
  18, "Sin comentarios / conforme",
  19, "Sin comentarios / conforme",
  20, "Sin comentarios / conforme",
  21, "Sin comentarios / conforme",
  22, "Dotación de personal insuficiente",
  23, "Sin comentarios / conforme",
  24, "Continuidad y seguimiento ambulatorio",
  25, "Sin comentarios / conforme",
  26, "Comunicación/relación con equipo tratante",
  26, "Continuidad y seguimiento ambulatorio",
  27, "Claridad sobre alcance y competencias"
)
