library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(gtsummary)
library(ggplot2)
library(psych)

# ---- Paleta y tema compartidos por todos los gráficos ----
color_azul  <- "#184f95"
color_rojo  <- "#b23434"
color_gris  <- "#898781"
color_claro <- "#fcfcfb"
color_oscuro <- "#0b0b0b"

tema_base <- theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot")

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

# ---- Variante "sin Internos" ----
# Excluye a los 5 encuestados con cargo Interno/a (análisis de sensibilidad;
# queda solo Becado/a y Staff). El id se asigna ANTES de filtrar para que se
# mantenga alineado con `codificacion_tematica.R`, que referencia ids del set
# completo de 27 respuestas.
data <- data_raw %>%
  rename(!!!nombres_cortos) %>%
  mutate(id = row_number(), .before = 1) %>%
  filter(cargo != "Interno/a")

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

write_csv(data_wide, "data_wide_sin_internos.csv")

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

write_csv(data_long, "data_long_sin_internos.csv")

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
  gt::gtsave("tabla_caracteristicas_sin_internos.html")

caracteristicas_poblacion %>%
  as_gt() %>%
  gt::gtsave("tabla_caracteristicas_sin_internos.png", vwidth = 900, zoom = 3)

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
  "Muy en desacuerdo"             = color_rojo,
  "En desacuerdo"                 = "#e88a89",
  "Ni deacuerdo ni en desacuerdo" = "#c9c8c0",
  "De acuerdo"                    = "#86b6ef",
  "Muy de acuerdo"                = color_azul
)
color_texto_segmento <- c(
  "Muy en desacuerdo"             = color_claro,
  "En desacuerdo"                 = color_oscuro,
  "Ni deacuerdo ni en desacuerdo" = color_oscuro,
  "De acuerdo"                    = color_oscuro,
  "Muy de acuerdo"                = color_claro
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
    title = "Percepción del equipo tratante sobre psiquiatría de enlace (sin Internos)",
    subtitle = paste0("n = ", nrow(data_wide), " encuestados"),
    x = "Porcentaje de respuestas", y = NULL
  ) +
  tema_base +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave("grafico_diverging_likert_sin_internos.png", grafico_diverging, width = 10, height = 7, dpi = 150)


# =============================================================
# ANÁLISIS ADICIONALES
# =============================================================

# ---- 1. % favorable (De acuerdo + Muy de acuerdo) por cargo ----
# Se desglosa por cargo (Becado/a, Interno/a, Staff) porque cada grupo tiene
# un n razonable (15, 5, 7). No se desglosa por sala: hay salas con solo
# 2-3 respuestas y el % por celda no sería confiable.

favorable_por_grupo <- function(datos_long, var_grupo) {
  datos_long %>%
    filter(!is.na(respuesta)) %>%
    mutate(favorable = respuesta_num >= 4) %>%
    group_by(pregunta, grupo = .data[[var_grupo]]) %>%
    summarise(pct_favorable = 100 * mean(favorable), n = n(), .groups = "drop") %>%
    mutate(
      pregunta_label = factor(etiquetas_preguntas[pregunta], levels = orden_preguntas),
      color_texto = ifelse(pct_favorable > 55, "claro", "oscuro")
    )
}

favorable_cargo <- favorable_por_grupo(data_long, "cargo")

grafico_favorable_cargo <- ggplot(favorable_cargo, aes(x = grupo, y = pregunta_label)) +
  geom_tile(aes(fill = pct_favorable), color = color_claro, linewidth = 0.6) +
  geom_text(aes(label = paste0(round(pct_favorable), "%"), color = color_texto), size = 3.2) +
  scale_fill_gradient(low = "#e88a89", high = color_azul, limits = c(0, 100), name = "% favorable") +
  scale_color_manual(values = c(claro = color_claro, oscuro = color_oscuro), guide = "none") +
  labs(
    title = "Porcentaje de acuerdo/muy de acuerdo, por cargo",
    subtitle = "% de respuestas 'De acuerdo' o 'Muy de acuerdo' en cada ítem",
    x = NULL, y = NULL
  ) +
  tema_base +
  theme(panel.grid = element_blank())

ggsave("grafico_favorable_por_cargo_sin_internos.png", grafico_favorable_cargo, width = 9, height = 7, dpi = 150)

# ---- 2. Índice de satisfacción compuesto vs. experiencia e interacciones ----
# Índice = promedio de los 12 ítems Likert (1-5) por encuestado.

indice_satisfaccion <- data_long %>%
  filter(!is.na(respuesta_num)) %>%
  group_by(id) %>%
  summarise(indice = mean(respuesta_num), .groups = "drop") %>%
  left_join(
    data_wide %>%
      mutate(anios_experiencia_num = anios_a_numero(anios_experiencia)) %>%
      select(id, anios_experiencia_num, interacciones_mes),
    by = "id"
  )

grafico_indice_experiencia <- ggplot(indice_satisfaccion, aes(x = anios_experiencia_num, y = indice)) +
  geom_point(size = 2.5, color = color_azul, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = color_gris, fill = "#c9c8c0") +
  labs(
    title = "Índice de satisfacción vs. años de experiencia",
    subtitle = paste0(
      "Correlación de Spearman: rho = ",
      round(cor(indice_satisfaccion$anios_experiencia_num, indice_satisfaccion$indice,
                method = "spearman", use = "complete.obs"), 2),
      " (n = ", sum(complete.cases(indice_satisfaccion[, c("anios_experiencia_num", "indice")])), ")"
    ),
    x = "Años de experiencia", y = "Índice de satisfacción (1-5)"
  ) +
  tema_base

ggsave("grafico_indice_vs_experiencia_sin_internos.png", grafico_indice_experiencia, width = 8, height = 6, dpi = 150)

grafico_indice_interacciones <- ggplot(indice_satisfaccion, aes(x = interacciones_mes, y = indice)) +
  geom_point(size = 2.5, color = color_azul, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = color_gris, fill = "#c9c8c0") +
  labs(
    title = "Índice de satisfacción vs. interacciones mensuales",
    subtitle = paste0(
      "Correlación de Spearman: rho = ",
      round(cor(indice_satisfaccion$interacciones_mes, indice_satisfaccion$indice,
                method = "spearman", use = "complete.obs"), 2),
      " (n = ", sum(complete.cases(indice_satisfaccion[, c("interacciones_mes", "indice")])), ")"
    ),
    x = "Interacciones con psiquiatría en el último mes", y = "Índice de satisfacción (1-5)"
  ) +
  tema_base

ggsave("grafico_indice_vs_interacciones_sin_internos.png", grafico_indice_interacciones, width = 8, height = 6, dpi = 150)

# ---- 3. Correlación entre los 12 ítems Likert (heatmap) ----

matriz_items <- data_wide %>%
  select(all_of(cols_likert)) %>%
  mutate(across(everything(), as.integer))

cor_items <- cor(matriz_items, method = "spearman", use = "pairwise.complete.obs")

cor_long <- as.data.frame(cor_items) %>%
  mutate(item1 = rownames(cor_items)) %>%
  pivot_longer(-item1, names_to = "item2", values_to = "rho") %>%
  mutate(
    item1_label = factor(etiquetas_preguntas[item1], levels = orden_preguntas),
    item2_label = factor(etiquetas_preguntas[item2], levels = orden_preguntas)
  )

grafico_correlacion <- ggplot(cor_long, aes(x = item1_label, y = item2_label, fill = rho)) +
  geom_tile(color = color_claro) +
  geom_text(aes(label = round(rho, 1)), size = 2.6) +
  scale_fill_gradient2(low = color_rojo, mid = "#f4f3ee", high = color_azul, midpoint = 0,
                        limits = c(-1, 1), name = "rho") +
  labs(title = "Correlación entre ítems (Spearman)", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank(),
        plot.title.position = "plot")

ggsave("grafico_correlacion_items_sin_internos.png", grafico_correlacion, width = 9, height = 8, dpi = 150)

# ---- 4. Consistencia interna de la escala (alpha de Cronbach) ----
# Se usa psych::alpha() en vez de calcular el estadístico a mano: además del
# alpha global entrega la correlación ítem-total y el "alpha si se elimina
# el ítem", que son los valores que normalmente se reportan para justificar
# que los 12 ítems conforman una escala coherente.

alpha_resultado <- psych::alpha(matriz_items)
print(alpha_resultado)

alpha_escala <- alpha_resultado$total$raw_alpha
cat("Alpha de Cronbach (psych::alpha) para los 12 ítems Likert:", round(alpha_escala, 3), "\n")

# ---- 5. Codificación temática de las respuestas abiertas ----
# Con solo 27 respuestas es posible codificarlas a mano en vez de limitarse a
# contar palabras sueltas (una misma respuesta puede tocar más de un tema).
# El detalle de qué id se asignó a qué tema queda documentado en
# codificacion_tematica.R para que la codificación sea auditable/reproducible.

source("codificacion_tematica.R")

# Se restringe la codificación temática (hecha sobre las 27 respuestas) a los
# ids que sobreviven el filtro de esta variante.
temas_positivos <- temas_positivos %>% filter(id %in% data_wide$id)
temas_mejorar <- temas_mejorar %>% filter(id %in% data_wide$id)

n_respondentes_tema <- c(
  "Aspectos positivos" = n_distinct(temas_positivos$id),
  "Aspectos a mejorar" = n_distinct(temas_mejorar$id)
)

resumen_tematico <- bind_rows(
  temas_positivos %>% mutate(tipo = "Aspectos positivos"),
  temas_mejorar %>% mutate(tipo = "Aspectos a mejorar")
) %>%
  count(tipo, tema, name = "n_respuestas") %>%
  mutate(pct = 100 * n_respuestas / n_respondentes_tema[tipo])

grafico_tematico <- ggplot(resumen_tematico, aes(x = reorder(tema, n_respuestas), y = n_respuestas, fill = tipo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~tipo, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = c("Aspectos positivos" = color_azul, "Aspectos a mejorar" = color_rojo)) +
  labs(
    title = "Temas mencionados en las respuestas abiertas",
    subtitle = "Cada respuesta puede tocar más de un tema; n = número de encuestados que lo mencionan",
    x = NULL, y = "N° de encuestados"
  ) +
  tema_base

ggsave("grafico_tematico_sin_internos.png", grafico_tematico, width = 10, height = 6, dpi = 150)

# ---- 6. Palabras más frecuentes en las respuestas abiertas (exploratorio) ----
# Complementa la codificación temática de arriba con un conteo crudo de
# palabras; útil como chequeo rápido, pero menos informativo que los temas.

stopwords_es <- c(
  "a","al","algo","algunas","algunos","ante","antes","como","con","contra",
  "cual","cuando","de","del","desde","donde","durante","el","ella","ellas",
  "ellos","en","entre","era","esa","esas","ese","eso","esos","esta","estaba",
  "estan","estar","este","esto","estos","fue","fueron","ha","han","hasta",
  "hay","la","las","le","les","lo","los","mas","más","mi","mientras","mucho",
  "muchos","muy","nada","ni","no","nos","nosotras","nosotros","nuestra",
  "nuestras","nuestro","nuestros","o","os","otra","otras","otro","otros",
  "para","pero","poco","por","porque","que","quien","quienes","se","si","sí",
  "sin","sobre","sois","somos","son","soy","su","sus","tambien","también",
  "tanto","te","tiene","tienen","todo","todos","tu","tus","un","una","uno",
  "unos","vosotras","vosotros","vuestra","vuestras","vuestro","vuestros",
  "y","ya","yo","es","asi","así","esta","están","está"
)

contar_palabras <- function(vector_texto, top_n = 15) {
  tibble(texto = vector_texto) %>%
    filter(!is.na(texto), texto != "") %>%
    mutate(
      texto = str_to_lower(texto),
      texto = str_remove_all(texto, "[[:punct:]]"),
      palabras = str_split(texto, "\\s+")
    ) %>%
    unnest(palabras) %>%
    mutate(palabras = str_squish(palabras)) %>%
    filter(palabras != "", !palabras %in% stopwords_es, nchar(palabras) > 2) %>%
    count(palabras, sort = TRUE) %>%
    slice_head(n = top_n)
}

palabras_frecuentes <- bind_rows(
  contar_palabras(data_wide$aspectos_positivos) %>% mutate(tipo = "Aspectos positivos"),
  contar_palabras(data_wide$aspectos_mejorar) %>% mutate(tipo = "Aspectos a mejorar")
)

grafico_palabras <- ggplot(palabras_frecuentes, aes(x = reorder(palabras, n), y = n, fill = tipo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~tipo, scales = "free") +
  coord_flip() +
  scale_fill_manual(values = c("Aspectos positivos" = color_azul, "Aspectos a mejorar" = color_rojo)) +
  labs(
    title = "Palabras más frecuentes en las respuestas abiertas",
    x = NULL, y = "Frecuencia"
  ) +
  tema_base

ggsave("grafico_palabras_frecuentes_sin_internos.png", grafico_palabras, width = 10, height = 6, dpi = 150)
