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
                                   "date", "date", "numeric", "numeric", 
                                   "numeric", "numeric"))

# Cuento casos reportados
casos <- mexico %>% 
  group_by(fecha_corte,ent) %>% 
  count() %>% 
  ungroup()

casos <- casos %>% 
  pivot_wider(names_from = fecha_corte,
              values_from = n)

a <- casos %>% 
  mutate(as.Date("2020-03-02") == )
View(a)
# Pongo fechas faltantes
fechas <- tibble(fecha_inicio = c(as.Date("2020-02-"),
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
  pivot_longer(c(`1/22/20`:`3/26/20`),
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
# 
# today <- tibble(country = c("Argentina",
#                             "Brazil",
#                             "Canada",
#                             "Chile",
#                             "Colombia",
#                             "Dominican Republic",
#                             "Ecuador",
#                             "Mexico",
#                             "Panama",
#                             "Peru",
#                             "Uruguay"),
#                 continent = c((rep("South America", 11))),
#                 date = c((rep(as.Date("2020-03-27"), 11))),
#                 cases = c(rep(1000, 11)),
#                 code = c("ar",
#                          "br",
#                          "ca",
#                          "cl",
#                          "co",
#                          "dr",
#                          "ec",
#                          "mx",
#                          "pa",
#                          "pe",
#                          "ur"))
# 
#   confirmed <- rbind(confirmed, today)


# Parte para que empiecen a partir de caso 100
confirmed <- confirmed %>% 
  filter(cases >= 100) %>% 
  mutate(days = as.numeric(date - min(date)))

confirmed <- left_join(x = confirmed,
                       y = exp,
                       by = "days")

americas <- confirmed %>% 
  filter(continent == "North America" | continent == "South America") %>% 
  filter(country != "US")


## Otras transformaciones para generar subbases

# Primero revisar qué países ya tienen más de 100 (26 marzo hay 13)
americas %>% 
  group_by(country) %>% 
  summarise(days = max(days),
            cases = max(cases)) %>% 
  arrange(-cases) 

# Transformaciones
b <- americas %>% 
  group_by(country) %>% 
  summarise(days = max(days),
            cases = max(cases)) 

b <- left_join(x = b,
               y = flags,
               by = "country")

mexico_flag <- b %>% 
  filter(country == "Mexico")

latam <- americas %>% 
  filter(country != "Colombia",
         country != "Venezuela",
         country != "Costa Rica",
         country != "Uruguay",
         country != "Mexico",
         country != "US")

b <- b %>%
  filter(country != "Colombia",
         country != "Venezuela",
         country != "Costa Rica",
         country != "Uruguay",
         country != "Mexico")

mexico <- americas %>% 
  filter(country == "Mexico")

otros <- americas %>% 
  filter(country == "Colombia" |
           country == "Venezuela" |
           country == "Uruguay" |
           country == "Costa Rica") %>% 
  group_by(country) %>% 
  summarise(days = max(days),
            cases = max(cases))

otros2 <- americas %>% 
  filter(country == "Uruguay" |
           country == "Costa Rica" |
           country == "Venezuela") %>% 
  group_by(country) %>% 
  summarise(days = max(days),
            cases = max(cases))


### Visualización ----

# Líneas negras sobre duplicación de casos  
confirmed %>% 
  
  ggplot() + 
  geom_line(aes(days,
                y = cada_dia),
            linetype = "dashed") + 
  geom_line(aes(days,
                y = cada_dos),
            linetype = "dashed") + 
  geom_line(aes(days,
                y = cada_tres),
            linetype = "dashed") + 
  geom_line(aes(days,
                y = cada_semana),
            linetype = "dashed") + 
  
  geom_line(aes(days,
                cases,
                group = country),
            alpha = 0.2,
            data = confirmed) +
  
  # Líneas de colores, puntos y banderas para países con más casos  
  geom_line(aes(days,
                cases,
                group = country,
                color = country),
            alpha = 0.9,
            data = americas,
            size = 0.7) +
  geom_point(aes(days,
                 cases,
                 group = country,
                 color = country),
             alpha = 0.9,
             data = americas) +
  # Parte de México
  geom_line(aes(days,
                cases,
                group = country),
            alpha = 1,
            data = mexico,
            size = 1.6,
            color = "black") +
  geom_point(aes(days,
                 cases,
                 group = country),
             alpha = 1,
             data = mexico,
             size = 2.1,
             fill = "white",
             color = "black") +
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
           color = "black",
           size = 3) +  
  annotate("text",
           x = 17,
           y = 100000, 
           label = "Casos duplicados\ncada 2 días",
           color = "black",
           size = 3) +  
  annotate("text",
           x = 26,
           y = 80000, 
           label = "Casos duplicados\ncada 3 días",
           color = "black",
           size = 3) +  
  annotate("text",
           x = 28,
           y = 900, 
           label = "Casos duplicados\ncada semana",
           color = "black",
           size = 3) + 
  
  
  # Otras cositas
  scale_x_continuous(limits = c(0,30),
                     breaks = c(seq(0, 30, 2))) + 
  scale_y_log10(limits = c(100, 120000),
                breaks = c(100, 1000, 10000, 100000),
                labels = comma) + 
  
  labs(title = "Casos reportados acumulados de COVID-19 en las Américas - 27/mar/2020",
       subtitle = "Esta información puede estar rezagada con respecto a la reportada por un país",
       caption = "@pCobosAlcala\n\nNota 1: las líneas grises son del resto de países del mundo hasta en sus primeros 30 días con más de 100 casos\ny las de colore son de países de las Américas\nNota 2: no incluyo a Estados Unidos en las líneas con color de las Américas\nFuente: base de JHE CSSE con datos de la OMS (https://bit.ly/39lu0hq)",
       x = "Días transcurridos desde que un país reportó más de 100 casos",
       y = "Casos reportados (escala logarítmica)") + 
  theme_minimal()  +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        plot.caption=element_text(hjust = 0))

## Guardado
ggsave("global.png",
       width = 7,
       height = 5,
       dpi = 350)

