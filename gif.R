### Paquetes ----
library(pacman)
p_load(tidyverse, readr, scales, ggrepel, lubridate, ggimage, gganimate)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Importo bases ----

## Base de casos de COVID-19
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

## Base de países y continentes
continents <- read_csv("https://raw.githubusercontent.com/dbouquin/IS_608/master/NanosatDB_munging/Countries-Continents.csv")

## Base para emojis de banderas
flags <- read_csv("https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv")

## Base de crecimiento exponencial (La construí en excel. Nota: hacerla en R)
exp <- read_csv("1_data/exp.csv", col_types = cols(cada_dia = col_double()))

flags <- flags %>% 
  mutate(country = Name,
         code = Code)

flags <- flags %>% select(country, code) %>%
  mutate(code = str_to_lower(code))


### Transformación de datos ----

## Unión de bases, transformación tidy y filtrado para Américas
confirmed <- confirmed %>% 
  mutate(state = `Province/State`,
         country = `Country/Region`)

continents <- continents %>% 
  mutate(country = Country,
         continent = Continent)

confirmed <- confirmed %>% 
  pivot_longer(c(`1/22/20`:`4/2/20`),
               names_to = "date",
               values_to = "cases")

confirmed <- confirmed %>% 
  mutate(date = mdy(date))

confirmed <- confirmed %>% 
  group_by(country, date) %>% 
  summarise(cases = sum(cases))


confirmed <- left_join(x = confirmed,
                       y = continents,
                       by = "country")

confirmed <- left_join(x = confirmed,
                       y = flags,
                       by = "country")

confirmed <- confirmed %>% 
  select(country, continent, date, cases, code)


# Parte para que empiecen a partir de caso 100
confirmed <- confirmed %>% 
  filter(cases >= 100) %>% 
  mutate(days = as.numeric(date - min(date)))

confirmed <- left_join(x = confirmed,
                       y = exp,
                       by = "days")


## Otras transformaciones para generar subbases

### Visualización ----


confirmed <- confirmed %>% 
  filter(days <= 36) %>% 
  filter(country != "China",
         country != "Diamond Princess") 

con <- confirmed %>% 
  filter(continent == "South America") %>% 
  rename(pais = country)

confirmed <- confirmed %>% 
  filter(continent == "South America")

p <- ggplot(confirmed,
       aes(x = days,
           y = cases)) + 
  geom_line(aes(x = days,
                y = cases,
                group = pais),
            data = con,
            size = 1,
            alpha = 0.5,
            inherit.aes = T) + 
  geom_line(aes(colour = country,
                group = country),
            size = 1.5) + 
  geom_line(aes(days,
                y = cada_dia),
            linetype = "dashed",
            color = "darkblue",
            data = exp) + 
  geom_line(aes(days,
                y = cada_dos),
            linetype = "dashed",
            color = "darkblue",
            data = exp) + 
  geom_line(aes(days,
                y = cada_tres),
            linetype = "dashed",
            color = "darkblue",
            data = exp) + 
  geom_line(aes(days,
                y = cada_semana),
            linetype = "dashed",
            color = "darkblue",
            data = exp) + 
  annotate("text",
           x = 7,
           y = 100000, 
           label = "Casos duplicados\ncada día",
           color = "darkblue",
           size = 3) +  
  annotate("text",
           x = 17,
           y = 100000, 
           label = "Casos duplicados\ncada 2 días",
           color = "darkblue",
           size = 3) +  
  annotate("text",
           x = 34,
           y = 80000, 
           label = "Casos duplicados\ncada 3 días",
           color = "darkblue",
           size = 3) +  
  annotate("text",
           x = 34,
           y = 1100, 
           label = "Casos duplicados\ncada semana",
           color = "darkblue",
           size = 3) + 
  
  
  # Otras cositas
  scale_x_continuous(limits = c(0,36),
                     breaks = c(seq(0, 36, 2))) + 
  scale_y_log10(limits = c(100, 220000),
                breaks = c(200, 2000, 20000, 200000),
                labels = comma) + 
  
  labs(title = "Casos reportados acumulados de COVID-19 en el mundo (sin China)",
       subtitle = "País: {closest_state}",
       caption = "@pCobosAlcala\n\nFuente: base de JHE CSSE con datos de la OMS (https://bit.ly/39lu0hq)",
       x = "Días transcurridos desde que un país reportó 100 casos o más",
       y = "Casos reportados (escala logarítmica)") + 
  theme_minimal()  +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        plot.caption=element_text(hjust = 0)) +
  transition_states(country,
                    transition_length = 1,
                    state_length = 1)

animate(plot = p, nframes = 19)
# 
# anim_save("mundo2.gif")
