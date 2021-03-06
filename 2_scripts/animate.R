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
  pivot_longer(c(`1/22/20`:`4/1/20`),
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

# Transformaciones

mexico_flag <- confirmed %>% 
  filter(country == "Brazil")

mexico <- confirmed %>% 
  filter(country == "Brazil")

### Visualización ----

confirmed <- confirmed %>% 
  filter(days <= 36) %>% 
  filter(country != "China",
         country != "Diamond Princess") 


# Líneas negras sobre duplicación de casos  
p <- confirmed %>%
  ggplot(aes(days,
             cases,
             group = country)) + 
  geom_line(aes(days,
                y = cada_dia),
            linetype = "dashed",
            color = "darkblue") + 
  geom_line(aes(days,
                y = cada_dos),
            linetype = "dashed",
            color = "darkblue") + 
  geom_line(aes(days,
                y = cada_tres),
            linetype = "dashed",
            color = "darkblue") + 
  geom_line(aes(days,
                y = cada_semana),
            linetype = "dashed",
            color = "darkblue") + 
  
  geom_line(alpha = 0.1,
            data = confirmed, size = 1) +
  
  # Parte de México
  geom_line(aes(days,
                cases,
                group = country),
            alpha = 1,
            data = mexico,
            size = 1.4,
            color = "green3") +
  geom_flag(aes(x = days,
                y = cases,
                image = code,
                group = country),
            data = mexico_flag,
            size = 0.05) +
  
  # Palabras de países que faltaban
  # Puntos para países con pocos casos que se amontonaban 
  
    # Textos
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
       subtitle = "30 de marzo de 2020",
       caption = "@pCobosAlcala\n\nFuente: base de JHE CSSE con datos de la OMS (https://bit.ly/39lu0hq)",
       x = "Días transcurridos desde que un país reportó 100 casos o más",
       y = "Casos reportados (escala logarítmica)") + 
  theme_minimal()  +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        plot.caption=element_text(hjust = 0)) + transition_reveal(days)

animate(plot = p, nframes = 19)

anim_save("mundo2.gif")
