# proactivo

Análisis de la encuesta de percepción del equipo tratante sobre el modelo de
psiquiatría de enlace.

## Reproducir el análisis

```r
renv::restore()        # instala las versiones exactas de los paquetes (renv.lock)
source("proactivo.R")  # genera data_wide.csv, data_long.csv, tablas y gráficos
rmarkdown::render("informe.Rmd")  # informe final en informe.html
```

La codificación temática de las respuestas abiertas está documentada en
`codificacion_tematica.R`.
