### Paquetes ----
library(pacman)
p_load(tidyverse, readr, scales, ggrepel, lubridate, ggimage)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Importo bases ----

## Base de casos de COVID-19
negatives <- read_csv("https://raw.githubusercontent.com/mariorz/covid19-mx-time-series/master/data/covid19_negatives_mx.csv")


confirmed <- read_csv("https://raw.githubusercontent.com/mariorz/covid19-mx-time-series/master/data/covid19_confirmed_mx.csv")

# Nota: 6 cambios
negatives <- negatives %>% 
  select(Estado, `07-04-2020`)

confirmed <- confirmed %>% 
  select(Estado, `07-04-2020`)

base <- left_join(negatives, confirmed, by = "Estado")

base <- base %>% 
  mutate(totales = `07-04-2020.x` + `07-04-2020.y`) %>% 
  rename(negatives = `07-04-2020.x`,
         confirmed = `07-04-2020.y`) %>% 
  mutate(porcentaje = (confirmed/totales)) %>% 
  arrange(-porcentaje)

base <- base %>% 
  mutate(Estado = str_replace_all(Estado, c("Ciudad de México" = "CDMX",
                                         "México" = "Edomex",
                                         "Quintana Roo" = "Q. Roo",
                                         "Baja California Sur" = "BCS",
                                         "San Luis Potosí" = "SLP",
                                         "Baja California" = "BC",
                                         "Queretaro" = "Querétaro")))

base %>% 
  ggplot() +
  geom_col(aes(fct_reorder(Estado, porcentaje), totales,
               fill = porcentaje), color = "grey60") +
  scale_fill_gradient(low = "ivory2",
                      high = "turquoise4") +
  geom_point(aes(Estado, confirmed), color = "red") +
  geom_text(aes(x = Estado,
                y = 0,
                label = percent(round(porcentaje, 3))),
            hjust = 1,
            size = 3) + 
  # Nota: quizá
scale_y_continuous(breaks = c(seq(0, 3200, 200)),
                   labels = comma) + 
 
  coord_flip() + 
  labs(title = "Porcentaje de pruebas de COVID-19 con resultado positivo",
  subtitle = "Entidades de México. Corte: 07-mar-2020",
       caption = "Elaboración: @pCobosAlcala\nDatos: bases de @mariorz con información de la SSa",
       y = "\nPuntos rojos: Casos confirmados\nBarras azules: Pruebas realizadas\nPorcentaje: (Punto/barra)*100",
       fill = "Ratio punto/barra") +
  theme_light() + 
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        axis.title.y = element_blank(),
        axis.title.x = element_text(hjust = .95,
                                    face = "bold"),
        axis.text.y = element_text(vjust = 0.5),
        plot.caption = element_text(hjust = 0,
                                    vjust = 15),
        legend.position = c(.85, .15),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())


ggsave("barras.png",
       width = 7,
       height = 8,
       dpi = 350)


base %>% 
  filter(Estado != "Ciudad de México") %>% 
  ggplot(aes(totales, confirmed)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cor.test(base$totales, base$confirmed)

cdmx <- base %>% 
  filter(Estado != "CDMX")

cor.test(cdmx$totales, cdmx$confirmed)



