### Paquetes ----
library(pacman)
p_load(tidyverse, readr, readxl, scales, lubridate, gganimate, ggrepel)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


exp <- read_csv("1_data/exp50.csv")


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



### Transformaciones ----

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

casos_50 <- casos %>% 
  group_by(ent) %>% 
  filter(cases >= 50) %>% 
  mutate(days = as.numeric(date - min(date))) %>% 
  ungroup()

casos_0 <- casos %>% 
  group_by(ent) %>% 
  filter(cases < 50 &
           cases >= 1) %>% 
  mutate(days = as.numeric(date - max(date))) %>% 
  ungroup()

casos <- rbind(casos_50, casos_0)

casos <- casos %>% 
  arrange(ent, date) 

# Nota: estados con menos de 50 casos + CDMX
casos %>% 
  group_by(ent) %>% 
  filter(date == "2020-04-06") %>% 
  arrange(cases, ent) %>% 
  print(n = Inf)

c <- casos %>% 
  filter(ent != "CDMX",
         ent != "Colima",
         ent != "Campeche",
         ent != "Nayarit",
         ent != "Zacatecas",
         ent != "Tlaxcala",
         ent != "Durango",
         ent != "Morelos",
         ent != "Chihuahua",
         ent != "Oaxaca",
         ent != "Chiapas",
         ent != "Tamaulipas",
         ent != "Sonora",
         ent != "Michoacán",
         ent != "Guerrero",
         ent != "Hidalgo",
         ent != "San Luis Potosí",
         ent != "Querétaro",
         ent != "Veracruz",
         ent != "Aguascalientes")

estados <- casos %>% 
  rename(entidad = ent)

cinco <- estados %>% 
  filter(entidad == "CDMX")

casos <- estados %>% 
  filter(entidad != "CDMX",
         entidad != "Colima",
         entidad != "Campeche",
         entidad != "Nayarit",
         entidad != "Zacatecas",
         entidad != "Tlaxcala",
         entidad != "Durango",
         entidad != "Morelos",
         entidad != "Chihuahua",
         entidad != "Oaxaca",
         entidad != "Chiapas",
         entidad != "Tamaulipas",
         entidad != "Sonora",
         entidad != "Michoacán",
         entidad != "Guerrero",
         entidad != "Hidalgo",
         entidad != "San Luis Potosí",
         entidad != "Querétaro",
         entidad != "Veracruz",
         entidad != "Aguscalientes")


edos <- n_distinct(casos$entidad)

cinco %>% 
  filter(date == "2020-04-06")

### Visualización ----

p <- ggplot(c,
            aes(x = days,
                y = cases)) + 
  geom_line(aes(group = entidad),
            data = casos,
            size = 1.5,
            alpha = 0.15,
            inherit.aes = T) +
  geom_line(aes(x = days,
                y = cases, 
                group = entidad,
                color = entidad),
            data = cinco, 
            size = 1.2,
            alpha = 1) + 
  geom_line(aes(group = ent),
            color = "green3",
            size = 1.5) + 
  geom_line(aes(x = days,
                y = cada_dos),
            linetype = "dashed",
            color = "grey40",
            data = exp) +
  geom_line(aes(x = days,
                y = cada_tres),
            linetype = "dashed",
            color = "grey40",
            data = exp) +
  geom_line(aes(x = days,
                y = cada_cuatro),
            linetype = "dashed",
            color = "grey40",
            data = exp) +
  geom_line(aes(x = days,
                y = cada_siete),
            linetype = "dashed",
            color = "grey40",
            data = exp) +
  annotate("text",
           x = 5,
           y = 500,
           label = "Casos duplicados\ncada 2 días",
           color = "grey40",
           size = 3) +
  annotate("text",
           x = 10,
           y = 500,
           label = "Casos duplicados\ncada 3 días",
           color = "grey40",
           size = 3) +
  annotate("text",
           x = 13,
           y = 300,
           label = "Casos duplicados\ncada 4 días",
           color = "grey40",
           size = 3) +
  annotate("text",
           x = 13,
           y = 120,
           label = "Casos duplicados\ncada semana",
           color = "grey50",
           size = 3) +
  
  
  # Nota:
  scale_x_continuous(limits = c(-10,14),
  breaks = c(seq(-10, 14, 2))) + 
  
  scale_y_log10(limits = c(10, 620), 
                breaks = c(seq(10, 90, 10),
                           seq(100, 600, 100))) + 
  # labels = c("10", (rep("", 8)), "100", "200", "300")) +
  scale_color_viridis_d(option = "D") +
  labs(title = "COVID-19 en entidades de México. Corte: 07-mar-2020",
       subtitle = "En verde: {closest_state}",
       caption = "Elaboración: @pCobosAlcala, inspirado en @EulerEquation\n\nDatos: bases de @guzmart_ y @mariorz con información de la Secretaría de Salud",
       x = "Días transcurridos desde que una entidad reportó 50 casos o más",
       y = "Casos reportados acumulados (escala logarítmica)",
       color = "Entidad") + 
  theme_light() + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.09, 0.92),
        panel.grid.minor.y = element_blank()) +
  transition_states(ent,
                    transition_length = edos,
                    state_length = edos)

### Guardado ----
animate(plot = p, duration = 4)
anim_save("edos_50.gif")
