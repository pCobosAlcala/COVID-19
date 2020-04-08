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

casos <- casos %>% 
  group_by(ent) %>% 
  filter(cases >= 1) %>% 
  mutate(days = as.numeric(date - min(date))) %>% 
  ungroup()

casos <- casos %>% 
  arrange(ent, date)

estados <- casos %>% 
  rename(entidad = ent)

cinco <- estados %>% 
  filter(entidad == "CDMX" |
           entidad == "Edomex" |
           entidad == "Nuevo León" |
           entidad == "Jalisco" |
           entidad == "Puebla" | 
           entidad == "Baja California")

c <- estados %>% 
  filter(entidad != "CDMX",
         entidad != "Edomex", 
         entidad != "Nuevo León", 
         entidad != "Jalisco",
         entidad != "Puebla",
         entidad != "Baja California")

casos <- casos %>% 
  filter(ent != "CDMX",
         ent != "Edomex",
         ent != "Nuevo León",
         ent != "Jalisco",
         ent != "Puebla",
         ent != "Baja California")

edos <- n_distinct(casos$ent)


### Visualización ----

p <- ggplot(casos,
       aes(x = date,
           y = cases)) + 
  geom_vline(xintercept = as.Date("2020-03-23"),
             colour="grey40",
             linetype = "longdash") + 
  geom_vline(xintercept = as.Date("2020-03-30"),
             colour="grey40",
             linetype = "longdash") + 
  geom_line(aes(group = entidad),
            data = c,
            size = 1.5,
            alpha = 0.15,
            inherit.aes = T) +
  geom_line(aes(x = date,
                y = cases, 
                colour = entidad,
                group = entidad),
            data = cinco, 
            size = 1.2,
            alpha = 1) + 
  geom_line(aes(group = ent),
            color = "red",
            size = 1.5) + 


  annotate("text",
           x = as.Date("2020-03-23"),
           y = 600,
           label = "1",
           color = "grey0",
           size = 4, 
           hjust = 1) +
  annotate("text",
           x = as.Date("2020-03-30"),
           y = 600,
           label = "2",
           color = "grey0",
           size = 4,
           hjust = 1) +
  
  
  # Nota: límites y breaks

scale_x_date(breaks = c(seq(as.Date("2020-02-27"),
                            as.Date("2020-04-07"),
                            by = "2 days")),
             labels = date_format("%b %d")) +
  scale_y_continuous(limits = c(1, 720), 
                breaks = c(1,
                           seq(10, 90, 10),
                           seq(100, 700, 100)),
                labels = c("0", "10", (rep("", 8)),
                           "100", "200", "300", "400", "500", "600", "700")) +
  scale_color_viridis_d(option = "D") + 
  labs(title = "COVID-19 en entidades de México. Corte: 07-mar-2020",
       subtitle = "En rojo: {closest_state}",
       caption = "Elaboración: @pCobosAlcala, inspirado en @EulerEquation\n\n1 = Inicio de Jornada Nacional de Sana Distancia | 2 = Declaratoria de Emergencia\nDatos: bases de @guzmart_ y @mariorz con información de la Secretaría de Salud",
       x = "Fecha",
       y = "Casos reportados acumulados (escala logarítmica)",
       color = "Entidad") + 
  theme_light() + 
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = 0),
        legend.position = c(0.11, 0.83),
        panel.grid.minor.y = element_blank())  +   
  transition_states(ent,
                    transition_length = edos,
                    state_length = edos)



animate(plot = p, duration = 5)
anim_save("edos_lineal.gif")
