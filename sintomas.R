### Paquetes ----
library(pacman)
p_load(tidyverse, readr, readxl, scales, lubridate)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Creo base de inicio de síntomas (Base de Serendipia)----

mexico <- read_excel("1_data/mexico.xlsx", 
                     col_types = c("numeric", "text", "text", 
                                   "numeric", "date", "text", "text", 
                                   "date", "date"))

# Cuento síntomas
sintomas <- mexico %>% 
  group_by(fecha_inicio) %>% 
  count()

# Cuento casos reportados
casos <- mexico %>% 
  group_by(fecha_corte) %>% 
  count() %>% 
  ungroup()

# Uno bases
toda <- full_join(sintomas, casos,
                  by = c("fecha_inicio" = "fecha_corte"))

# Pongo fechas faltantes
fechas <- tibble(dates = c(as.Date("2020-02-20"),
                           as.Date("2020-02-26"),
                           as.Date("2020-02-24")))

# Tranformo a date
toda <- toda %>% 
  mutate(dates = ymd(fecha_inicio)) 

# Uno
toda <- full_join(toda,
                  fechas,
                  by = "dates")

# cambio de nombres y selecciono
toda <- toda %>% 
  mutate(sintomas = n.x,
         casos = n.y) %>% 
  select(dates, sintomas, casos) %>% 
  arrange(dates)

# Reemplazo NA por ceros
toda <- toda %>% 
  mutate_at(vars(sintomas), ~replace(., is.na(.), 0))

toda <- toda %>% 
  mutate_at(vars(casos), ~replace(., is.na(.), 0))

# Hago sumas acumuladas
toda[, 3] <- cumsum(toda[, 3])
toda[, 4] <- cumsum(toda[, 4])

# Fecha máxima de síntomas
max_sintomas <- max(sintomas$fecha_inicio)

# Filtro por fecha máxima
toda2 <- toda %>% 
  filter(dates <= max_sintomas)

# Uno con base filtrada
toda <- full_join(toda, toda2,
                  by = "dates")

# Reemplazo valores por 0
toda <- toda %>% 
  mutate_at(vars(sintomas.y), ~replace(., is.na(.), 0))

#
toda <- toda %>% 
  mutate(casos = casos.x, 
         sintomas = sintomas.y) %>% 
  select(dates, casos, sintomas)

# Para los números
filtrada <- toda %>% 
  filter(dates >= "2020-03-03")

m <- max(sintomas$fecha_inicio)
# 
# filtrada_sint <- toda %>% 
#   filter(dates >= "2020-02-27", 
#          dates <= m)


filtrada_sint <- toda %>% 
  filter(dates >= "2020-02-27")


# Para la diferencia entre casos y síntomas

diferencia <- filtrada_sint %>% 
  mutate(absoluta = sintomas - casos,
         ratio = sintomas/casos,
         promedio = (sintomas + casos) / 2) %>% 
  mutate_if(is.numeric, round, 1) %>% 
  filter(dates >= "2020-03-09")


### Gráfica ----

# Fijo parámetros

# Color síntomas
sint <- "black"
# Color casos
cas <- "white"

maximo <- max(toda$casos)
fecha_limite <- max(toda$dates)


# Gráfica
toda %>% 
  ggplot() + 
  geom_col(aes(x = dates,
               y = sintomas),
           fill = sint,
           data = filtrada_sint) +
  geom_text(aes(x = dates,
                y = sintomas,
                label = sintomas),
            data = filtrada_sint,
            size = 2.5,
            vjust = -0.3,
            color = sint) + 
  geom_text(aes(x = dates,
               y = promedio,
               label = ratio),
           color = "red",
           size = 2.5,
           data = diferencia,
           angle = 0,
           fontface = "bold") +
  geom_col(aes(x = dates,
               y = casos),
           fill = cas,
           alpha = 0.5,
           width = 0.5,
           color = "grey80") +
  geom_text(aes(x = dates,
                y = casos,
                label = casos),
            data = filtrada,
            size = 2.5,
            vjust = -0.3,
            color = cas) + 
  scale_y_continuous(breaks = c(seq(0, maximo, 50)),
                     limits=c(0,maximo)) +
  
  scale_x_date(breaks = c(seq(as.Date("2020-02-28"),
                              as.Date("2020-03-26"),
                              by = "1 days")),
               labels = date_format("%b %d"),
               limits = c(as.Date("2020-02-28"),
                          as.Date(fecha_limite))) + 
  annotate("text",
           x = as.Date("2020-03-05"), 
           y = 355, 
           label = "En blanco: casos reportados por la SSa",
           color = "grey60",
           size = 3.8,
           hjust = .48) +
  annotate("text",
           x = as.Date("2020-03-06"), 
           y = 390, 
           label = "En rojo: Ratio casos sintomáticos/casos reportados",
           color = "red",
           size = 3.8,
           hjust = .485) + 
  annotate("text",
           x = as.Date("2020-03-06"),
           y = 425, 
           label = "En negro: casos sintomáticas (posteriormente reportados por SSa)",
           color = "grey0",
           size = 3.8,
           hjust = .515) +
  
  labs(title = "Acumulado síntomas y reportes de COVID-19 en México",
       subtitle = "Actualizado al 27 de marzo de 2020",
       caption = "@pCobosAlcala\n\nNota: la primera fecha de inicio de síntomas es el 19/feb, pero se omite por no poderse ver\nFuente: base de @guzmart_ con datos de la Secretaría de Salud (bit.ly/2QCgyPN)",
       x = "Fecha",
       y = "Casos acumulados") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5, angle = 90),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave("sint.png",
       width = 8,
       height = 6,
       dpi = 350)
