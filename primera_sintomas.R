### Paquetes ----
library(pacman)
p_load(tidyverse, readr, scales)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Creo base de inicio de síntomas (Base de Serendipia)----

mexico <- read_csv("1_data/mexico.csv",
                   col_types = cols(`Fecha de Inicio de síntomas` = col_date(format = "%d/%m/%Y")))

mexico <- mexico %>% 
  mutate(date = `Fecha de Inicio de síntomas`) %>% 
  select(date)

mexico <- mexico %>% 
  group_by(date) %>% 
  count()

mexico[is.na(n)] <- 0
mexico[, 3] <- cumsum(mexico[, 2])


### Creo base de casos reportados (Base de la Universidad de Oxford) ----
# Tuve que añadir los datos del último día pues aún no están reportados

base <- read_csv("1_data/total_cases.csv", 
                        col_types = cols(date = col_date(format = "%d/%m/%Y")))
base <- base %>% 
  select(Mexico, date) %>% 
  pivot_longer(Mexico,names_to = "country",
               values_to = "cases")

base$date <- as.Date(base$date)

toda <- full_join(base, mexico, by = "date")

toda <- toda %>% 
  mutate_at(vars(n), ~replace(., is.na(.), 0))

toda[, 6] <- cumsum(toda[, 4])
base[, 4] <- cumsum(base[, 3])


a <- toda %>% 
  filter(date >= as.Date("2020-02-20"),
         date <= as.Date("2020-03-16")) %>% 
  select(-cases)

toda <- full_join(base, a, by = "date")

toda <- toda %>% 
  filter(date >= as.Date("2020-02-20"))

### Gráfica ----
toda %>% 
  ggplot() + 
  geom_col(aes(x = lead(date, n =1),
               y = lead(cases, n = 1)),
           fill = rgb(.663, .353, .631)) +
  geom_col(aes(x = date,
               y = n.2),
           color = rgb(.961,.412,.227),
           alpha = 0.3) +
  scale_y_continuous(breaks = c(seq(0, 140, 20)),
                     limits=c(0,145)) +
  scale_x_date(breaks = c(seq(as.Date("2020-02-01"),
                              as.Date("2020-03-19"),
                              by = "1 days")),
               labels = date_format("%b %d")) + 
  annotate("text",
           x = as.Date("2020-03-06"), 
           y = 80, 
           label = "Inicio de síntomas de\ncasos reportados",
           color = rgb(.961,.412,.227)) +
  
  annotate("text",
           x = as.Date("2020-03-17"),
           y = 138, 
           label = "Casos reportados\npor la SSa",
           color = rgb(.663, .353, .631)) +
  
  labs(title = "Acumulado de COVID-19 en México",
       subtitle = "Actualizado al 18 de marzo de 2020",
       caption = "Fuente: Base de Serendipia con datos de la Secretaría de Salud (https://bit.ly/2wfDYU7).\ny base de la Universidad de Oxford con datos de la OMS (https://bit.ly/2x7wWAS)\n@pCobosAlcala",
       x = "Fecha",
       y = "Casos acumulados") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, angle = 90),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
