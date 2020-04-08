### Paquetes ----
library(pacman)
p_load(tidyverse, readr, readxl, scales, lubridate, gganimate)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Bases ----

## Base de @guzmart_ 
mexico <- read_excel("1_data/mexico.xlsx", 
                     col_types = c("numeric", "text", "text", 
                                   "numeric", "date", "text", "text", 
                                   "date", "date", "numeric", "text", 
                                   "numeric", "text"))
mexico <- mexico %>% 
  filter(inconsistencia_omision == 0)

mexico <- mexico %>% 
  mutate_if(is.character, str_to_title)

## Base de @mariorz
confirmed <- read_csv("https://raw.githubusercontent.com/mariorz/covid19-mx-time-series/master/data/covid19_confirmed_mx.csv")

## Base de duplicación de días
exp <- read_csv("1_data/exp.csv")



### Transformaciones ----

# Cuento casos reportados
casos <- mexico %>% 
  group_by(fecha_corte, ent) %>% 
  count() %>% 
  ungroup() %>%
  mutate_if(is.character, str_to_title) %>% 
  pivot_wider(names_from = ent,
              values_from = n) %>% 
  replace(is.na(.), 0)  %>% 
  rename(CDMX = `Ciudad De México`,
         Edomex = México,
         Querétaro = Queretaro) %>% 
  mutate(`Baja California Sur` = `Baja California Sur` + `Baja California\r\nSur`) %>% 
  select(-`Baja California\r\nSur`) %>% 
  pivot_longer(c("CDMX":"Tlaxcala"),
               names_to = "ent",
               values_to = "cases") %>%
  pivot_wider(names_from = fecha_corte,
              values_from = cases) %>% 
  mutate(`2020-03-02` = c(rep(0, 32)),
         `2020-03-03`= c(rep(0, 32)),
         `2020-03-04`= c(rep(0, 32)),
         `2020-03-05`= c(rep(0, 32)),
         `2020-03-08`= c(rep(0, 32)),
         `2020-03-09`= c(rep(0, 32)),
         `2020-03-10`= c(rep(0, 32))) %>% 
  pivot_longer(-ent,
               names_to = "date",
               values_to = "cases") %>% 
  arrange(ent, date) %>% 
  group_by(ent) %>% 
  mutate(cumsum = cumsum(cases)) %>% 
  select(-cases) %>% 
  ungroup() %>% 
  mutate(date = ymd(date)) 

casos <- casos %>% 
  mutate( cases = cumsum) %>% 
  select(date, ent, cases)


confirmed <- confirmed %>% 
  pivot_longer(-Estado,
               names_to = "date",
               values_to = "cases") %>% 
  mutate(date = as.Date(date, format = "%d-%m-%Y")) %>% 
  pivot_wider(names_from = Estado,
              values_from = cases) %>% 
  rename(CDMX = `Ciudad de México`,
         Edomex = México, 
         Querétaro = Queretaro) %>% 
  pivot_longer(-date,
               names_to = "ent",
               values_to = "cases")

casos <- rbind(casos, confirmed)

a <- casos %>% 
  group_by(ent) %>% 
  mutate(tasa = (cases - lag(cases)) / lag(cases)) %>% 
  replace(is.na(.), 0) %>% 
  ungroup()

b <- a %>% 
  group_by(ent) %>% 
  mutate(di = ((log(2))/(tasa))) %>% 
  ungroup()

is.na(b)<-sapply(b, is.infinite)
b[is.na(b)]<-0


# Nota: quizá cambiar esto
b <- b %>% 
  group_by(ent) %>% 
  filter(date >= "2020-03-30",
         ent == "CDMX" |
           ent == "Edomex" |
           ent == "Puebla" |
           ent == "Jalisco" |
           ent == "Tabasco" |
           ent == "Sinaloa" |
           ent == "Quintana Roo" |
           ent == "Coahuila" |
           ent == "Baja California" |
           ent == "Yucatán" |
           ent == "Veracruz" |
           ent == "Nuevo León")

b %>% 
  ggplot(aes(x = date,
             y = di,
             group=ent)) + 
    geom_point() +
  geom_smooth() + 
  facet_wrap(~ent, scales = "free_y", ncol = 3) + 
  labs(title = "Tiempos de duplicación de COVID-19 en entidades",
  subtitle = "Corte: 07-mar-2020",
       caption = "Elaboración: @pCobosAlcala, inspirado en el trabajo de El País (bit.ly/39Umvyx)\n\nDatos: bases de @guzmart_ y @mariorz con información de la Secretaría de Salud",
       x = "Fecha",
       y = "Días que tarda una entidad en duplicar casos",
       color = "Entidad") + 
  theme_minimal() + 
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(vjust = .5,
                                   angle = 90))

ggsave("duplicacion.png",
       width = 7,
       height = 6,
       dpi = 350)


