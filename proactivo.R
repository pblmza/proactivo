library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(gtsummary)
library(ggplot2)

data_raw <- read_csv("data.csv")

# Nombres cortos y en snake_case para trabajar cómodo en R.
# El enunciado completo de cada pregunta queda documentado aquí como referencia.
nombres_cortos <- c(
  marca_temporal            = "Marca temporal",
  consentimiento             = "✅ Declaración de consentimiento:\n\nAl marcar la siguiente opción, declaro que he leído y comprendido la información entregada anteriormente y acepto participar de manera voluntaria en este estudio.",
  anios_experiencia          = "Años de experiencia",
  anios_trabajo_hospital     = "Años de trabajo en el hospital",
  cargo                      = "Cargo",
  sala                       = "Sala de hospital donde trabaja actualmente",
  interacciones_mes          = "Nº estimado de interacciones con psiquiatría en el último mes",
  q_responde_oportuno        = "El equipo de psiquiatría de enlace responde de forma oportuna a las necesidades clínicas.",
  q_facil_contactar          = "Es fácil contactar y coordinar con el equipo de psiquiatría de enlace cuando se requiere su intervención.",
  q_recomendaciones_claras   = "Las recomendaciones del equipo son claras, útiles y aplicables a la práctica médica.",
  q_buena_comunicacion       = "Existe una buena comunicación entre el equipo tratante y el equipo de psiquiatría de enlace.",
  q_continuidad_cuidados     = "El equipo de psiquiatría de enlace participa activamente en la continuidad de cuidados tras el alta hospitalaria (ej. interconsultas, derivaciones a salud mental, coordinación con la familia, etc.)",
  q_mejora_calidad_atencion  = "La presencia del equipo de psiquiatría de enlace mejora la calidad de la atención brindada a los pacientes.",
  q_apoyo_manejo_pacientes   = "Me siento apoyado/a por el equipo de psiquiatría de enlace en el manejo de pacientes con alteraciones del ánimo, conducta o sospecha de delirium",
  q_perspectiva_biopsicosocial = "El equipo de psiquiatría aporta una perspectiva biopsicosocial valiosa en el manejo clínico.",
  q_reduce_carga_trabajo     = "El trabajo conjunto con psiquiatría de enlace ha contribuido a reducir mi carga de trabajo o el estrés clínico.",
  q_disminuye_conflictos     = "Considero que su intervención ha disminuido conflictos o dificultades con pacientes con alteraciones conductuales.",
  q_satisfaccion_general     = "Estoy satisfecho/a con el funcionamiento general del equipo de psiquiatría de enlace.",
  q_mantener_extender_modelo = "Considero que este modelo debiera mantenerse y/o extenderse a otras unidades del hospital.",
  aspectos_positivos         = "¿Qué aspectos del modelo actual de psiquiatría de enlace considera más positivos?",
  aspectos_mejorar           = "¿Qué aspectos considera que podrían mejorarse?"
)

data <- data_raw %>%
  rename(!!!nombres_cortos) %>%
  mutate(id = row_number(), .before = 1)

# Columnas tipo Likert (escala de acuerdo/desacuerdo)
cols_likert <- c(
  "q_responde_oportuno", "q_facil_contactar", "q_recomendaciones_claras",
  "q_buena_comunicacion", "q_continuidad_cuidados", "q_mejora_calidad_atencion",
  "q_apoyo_manejo_pacientes", "q_perspectiva_biopsicosocial", "q_reduce_carga_trabajo",
  "q_disminuye_conflictos", "q_satisfaccion_general", "q_mantener_extender_modelo"
)

niveles_likert <- c(
  "Muy en desacuerdo", "En desacuerdo", "Ni deacuerdo ni en desacuerdo",
  "De acuerdo", "Muy de acuerdo"
)

# ---- Formato WIDE: una fila por encuestado, una columna por pregunta ----
data_wide <- data %>%
  mutate(across(all_of(cols_likert),
                ~ factor(.x, levels = niveles_likert, ordered = TRUE)))

write_csv(data_wide, "data_wide.csv")

# ---- Formato LONG: una fila por encuestado x pregunta Likert ----
data_long <- data %>%
  select(id, marca_temporal, anios_experiencia, anios_trabajo_hospital,
         cargo, sala, interacciones_mes, all_of(cols_likert)) %>%
  pivot_longer(
    cols = all_of(cols_likert),
    names_to = "pregunta",
    values_to = "respuesta"
  ) %>%
  mutate(
    respuesta = factor(respuesta, levels = niveles_likert, ordered = TRUE),
    respuesta_num = as.integer(respuesta)
  )

write_csv(data_long, "data_long.csv")

# ---- Tabla de características de la población ----
# "Años de experiencia" y "años de trabajo en el hospital" se respondieron
# como texto libre en algunos casos ("2 meses", "<1", "5 años", etc.).
# Se convierten a años (numérico) para poder resumirlas junto con las demás.
anios_a_numero <- function(x) {
  case_when(
    x == "Interno de medicina (6º año)" ~ NA_real_,  # indica año de carrera, no años de experiencia
    x == "<1" ~ 0.5,
    str_detect(x, "^\\d+(\\.\\d+)?$") ~ as.numeric(x),
    str_detect(x, "^\\d+\\s*mes") ~ as.numeric(str_extract(x, "^\\d+")) / 12,
    str_detect(x, "^\\d+\\s*años?") ~ as.numeric(str_extract(x, "^\\d+")),
    TRUE ~ NA_real_
  )
}

caracteristicas_poblacion <- data_wide %>%
  mutate(
    anios_experiencia = anios_a_numero(anios_experiencia),
    anios_trabajo_hospital = anios_a_numero(anios_trabajo_hospital)
  ) %>%
  select(cargo, sala, anios_experiencia, anios_trabajo_hospital, interacciones_mes) %>%
  tbl_summary(
    label = list(
      cargo ~ "Cargo",
      sala ~ "Sala",
      anios_experiencia ~ "Años de experiencia",
      anios_trabajo_hospital ~ "Años trabajando en el hospital",
      interacciones_mes ~ "Interacciones con psiquiatría (último mes)"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing_text = "Sin dato"
  ) %>%
  modify_header(label = "**Característica**", stat_0 = "**N = {N}**") %>%
  bold_labels()

caracteristicas_poblacion %>%
  as_gt() %>%
  gt::gtsave("tabla_caracteristicas.html")

# ---- Barras apiladas divergentes (Likert) ----
# La categoría neutra se reparte en dos mitades iguales, una a cada lado del
# cero, para que "0" quede en el límite entre desacuerdo y acuerdo (técnica
# estándar para graficar escalas Likert).

etiquetas_preguntas <- c(
  q_responde_oportuno          = "Responde oportunamente",
  q_facil_contactar             = "Fácil de contactar",
  q_recomendaciones_claras      = "Recomendaciones claras y aplicables",
  q_buena_comunicacion          = "Buena comunicación con el equipo",
  q_continuidad_cuidados        = "Continuidad de cuidados tras el alta",
  q_mejora_calidad_atencion     = "Mejora la calidad de atención",
  q_apoyo_manejo_pacientes      = "Apoyo en el manejo de pacientes",
  q_perspectiva_biopsicosocial  = "Perspectiva biopsicosocial valiosa",
  q_reduce_carga_trabajo        = "Reduce la carga de trabajo/estrés",
  q_disminuye_conflictos        = "Disminuye conflictos con pacientes",
  q_satisfaccion_general        = "Satisfacción general",
  q_mantener_extender_modelo    = "Mantener/extender el modelo"
)

resumen_likert <- data_long %>%
  filter(!is.na(respuesta)) %>%
  count(pregunta, respuesta) %>%
  group_by(pregunta) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(pregunta_label = etiquetas_preguntas[pregunta]) %>%
  select(pregunta, pregunta_label, respuesta, pct) %>%
  pivot_wider(names_from = respuesta, values_from = pct, values_fill = 0)

base_segmentos <- resumen_likert %>%
  transmute(
    pregunta, pregunta_label,
    muy_desacuerdo = `Muy en desacuerdo`,
    desacuerdo     = `En desacuerdo`,
    neutral_mitad  = `Ni deacuerdo ni en desacuerdo` / 2,
    acuerdo        = `De acuerdo`,
    muy_acuerdo    = `Muy de acuerdo`
  )

datos_diverging <- bind_rows(
  base_segmentos %>% transmute(pregunta, pregunta_label,
    categoria = "Muy en desacuerdo", pct = muy_desacuerdo,
    xmax = -(desacuerdo + neutral_mitad), xmin = xmax - muy_desacuerdo),
  base_segmentos %>% transmute(pregunta, pregunta_label,
    categoria = "En desacuerdo", pct = desacuerdo,
    xmax = -neutral_mitad, xmin = xmax - desacuerdo),
  base_segmentos %>% transmute(pregunta, pregunta_label,
    categoria = "Ni deacuerdo ni en desacuerdo", pct = neutral_mitad,
    xmin = -neutral_mitad, xmax = 0),
  base_segmentos %>% transmute(pregunta, pregunta_label,
    categoria = "Ni deacuerdo ni en desacuerdo", pct = neutral_mitad,
    xmin = 0, xmax = neutral_mitad),
  base_segmentos %>% transmute(pregunta, pregunta_label,
    categoria = "De acuerdo", pct = acuerdo,
    xmin = neutral_mitad, xmax = neutral_mitad + acuerdo),
  base_segmentos %>% transmute(pregunta, pregunta_label,
    categoria = "Muy de acuerdo", pct = muy_acuerdo,
    xmin = neutral_mitad + acuerdo, xmax = neutral_mitad + acuerdo + muy_acuerdo)
) %>%
  mutate(categoria = factor(categoria, levels = niveles_likert, ordered = TRUE))

# Ordenar las preguntas de menor a mayor % favorable (acuerdo + muy de acuerdo)
orden_preguntas <- base_segmentos %>%
  mutate(favorable = acuerdo + muy_acuerdo) %>%
  arrange(favorable) %>%
  pull(pregunta_label)

datos_diverging <- datos_diverging %>%
  mutate(pregunta_label = factor(pregunta_label, levels = orden_preguntas))

# Par divergente rojo/azul con gris neutro al centro; texto claro sobre los
# tonos oscuros y texto oscuro sobre los tonos claros para mantener contraste.
colores_likert <- c(
  "Muy en desacuerdo"             = "#b23434",
  "En desacuerdo"                 = "#e88a89",
  "Ni deacuerdo ni en desacuerdo" = "#c9c8c0",
  "De acuerdo"                    = "#86b6ef",
  "Muy de acuerdo"                = "#184f95"
)
color_texto_segmento <- c(
  "Muy en desacuerdo"             = "#fcfcfb",
  "En desacuerdo"                 = "#0b0b0b",
  "Ni deacuerdo ni en desacuerdo" = "#0b0b0b",
  "De acuerdo"                    = "#0b0b0b",
  "Muy de acuerdo"                = "#fcfcfb"
)

grafico_diverging <- ggplot(datos_diverging) +
  geom_rect(aes(ymin = as.numeric(pregunta_label) - 0.4,
                ymax = as.numeric(pregunta_label) + 0.4,
                xmin = xmin, xmax = xmax, fill = categoria),
            color = "#fcfcfb", linewidth = 0.6) +
  geom_text(aes(x = (xmin + xmax) / 2, y = as.numeric(pregunta_label),
                label = ifelse(pct >= 5, paste0(round(pct), "%"), ""),
                color = categoria),
            size = 3.2, fontface = "plain") +
  geom_vline(xintercept = 0, color = "#898781", linewidth = 0.4) +
  scale_fill_manual(values = colores_likert, breaks = niveles_likert, name = NULL) +
  scale_color_manual(values = color_texto_segmento, guide = "none") +
  scale_x_continuous(breaks = seq(-100, 100, 25),
                      labels = function(x) paste0(abs(x), "%")) +
  scale_y_continuous(breaks = seq_along(orden_preguntas), labels = orden_preguntas,
                      expand = expansion(add = 0.7)) +
  labs(
    title = "Percepción del equipo tratante sobre psiquiatría de enlace",
    subtitle = paste0("n = ", nrow(data_wide), " encuestados"),
    x = "Porcentaje de respuestas", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot"
  )

ggsave("grafico_diverging_likert.png", grafico_diverging, width = 10, height = 7, dpi = 150)


