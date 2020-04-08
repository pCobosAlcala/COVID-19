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
                                   "date", "date", "numeric", "text", 
                                   "numeric", "text"))

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


# Para la diferencia entre casos y síntomas

diferencia <- toda %>% 
  mutate(absoluta = sintomas - casos,
         ratio = sintomas/casos,
         promedio = (sintomas + casos) / 2) %>% 
  mutate_if(is.numeric, round, 1) %>% 
  filter(dates >= "2020-03-08",
         dates <= "2020-03-26")


toda %>% 
  ggplot(aes(dates, sintomas)) +
  geom_line(color = "red", size = 1) +
  geom_line(aes(dates, casos), size = 1) + 
  scale_y_continuous(breaks = c(seq(0,
                                    1200,
                                    200)),
                     limits=c(0,1200),
                     labels = comma) +
  scale_x_date(breaks = c(seq(as.Date("2020-02-27"),
                              as.Date("2020-03-29"),
                              by = "1 days")),
               labels = date_format("%b %d"),
               limits = c(as.Date("2020-01-01"),
                          as.Date("2020-03-29"))) + 
  theme_minimal() +
  labs(x = "Fecha",
       y = "Casos") + 
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  annotate("text",
           x = as.Date("2020-02-27"),
           y = 600, 
           label = "En rojo: fecha de inicio de síntomas",
           color = "red",
           size = 5,
           hjust = 0) + 
  annotate("text",
           x = as.Date("2020-02-27"),
           y = 500, 
           label = "En negro: fecha de reporte de casos ",
           color = "black",
           size = 5,
           hjust = 0)
