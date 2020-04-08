### Paquetes ----
library(pacman)
p_load(tidyverse, readr, scales, ggrepel)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Base importada ----
base <- read_csv("1_data/total_cases.csv")
View(base)
max_cases <- base %>% 
  filter(date == as.Date("2020-03-19"))

max_cases <- max_cases %>% transmute(
    day = 19,
    Argentina = Argentina,
    Bolivia = Bolivia,
    Brasil = Brazil,
    Colombia = Colombia,
    `Costa Rica` = `Costa Rica`,
    Chile = Chile,
    Cuba = Cuba,
    `República Dominicana` = `Dominican Republic`,
    Ecuador = Ecuador,
    Guatemala = Guatemala,
    Honduras = Honduras,
    México = Mexico,
    Panamá = Panama,
    Perú = Peru,
    Uruguay = Uruguay,
    Venezuela = Venezuela)

max_cases <- max_cases %>% 
  pivot_longer(c(Argentina:Venezuela),
               names_to = "country",
               values_to = "cases")


### Preparando visualización ----

## Días sin caso por país
lead_argentina <- sum(is.na(base$Argentina))
lead_bolivia <- sum(is.na(base$Bolivia))
lead_brazil <- sum(is.na(base$Brazil))
lead_colombia <- sum(is.na(base$Colombia))
lead_costarica <- sum(is.na(base$`Costa Rica`))
lead_chile <- sum(is.na(base$Chile))
lead_cuba <- sum(is.na(base$Cuba))
lead_dominicanrepublic <- sum(is.na(base$`Dominican Republic`))
lead_ecuador <- sum(is.na(base$Ecuador))
lead_guatemala <- sum(is.na(base$Guatemala))
lead_honduras <- sum(is.na(base$Honduras))
lead_mexico <- sum(is.na(base$Mexico))
lead_panama <- sum(is.na(base$Panama))
lead_peru <- sum(is.na(base$Peru))
lead_uruguay <- sum(is.na(base$Uruguay))
lead_venezuela <- sum(is.na(base$Venezuela))

days <- nrow(base)

max_cases <- max_cases %>% 
  mutate(day = c((days - lead_argentina),
                 (days - lead_bolivia),
                 (days - lead_brazil),
                 (days - lead_colombia),
                 (days - lead_costarica),
                 (days - lead_chile),
                 (days - lead_cuba),
                 (days - lead_dominicanrepublic),
                 (days - lead_ecuador),
                 (days - lead_guatemala),
                 (days - lead_honduras),
                 (days - lead_mexico),
                 (days - lead_panama),
                 (days - lead_peru),
                 (days - lead_uruguay),
                 (days - lead_venezuela)))


## Preparando base (Falta Guatemala y Uruguay)
base <- base %>% transmute(
  day = 1:days,
  Argentina = lead(Argentina, n = lead_argentina),
  Bolivia = lead(Bolivia, n = lead_bolivia),
  Brasil = lead(Brazil, n = lead_brazil),
  Colombia = lead(Colombia, n = lead_colombia),
  `Costa Rica` = lead(`Costa Rica`, n = lead_costarica),
  Chile = lead(Chile, n = lead_chile),
  Cuba = lead(Cuba, n = lead_cuba),
  `República Dominicana` = lead(`Dominican Republic`, n = lead_dominicanrepublic),
  Ecuador = lead(Ecuador, n = lead_ecuador),
  Guatemala = lead(Guatemala, n = lead_guatemala),
  Honduras = lead(Honduras, n = lead_honduras),
  México = lead(Mexico, n = lead_mexico),
  Panamá = lead(Panama, n = lead_panama),
  Perú = lead(Peru, n = lead_peru),
  Uruguay = lead(Uruguay, n = lead_uruguay),
  Venezuela = lead(Venezuela, n = lead_venezuela))

base <- base %>% 
pivot_longer(c(Argentina:Venezuela),
             names_to = "country",
             values_to = "cases")

View(base)
### Visualización ----

base %>% 
  ggplot(aes(color = country)) + 
  geom_line(aes(day,
                cases,
                group = country)) +
  geom_point(aes(day,
                cases,
                group = country),
            data = max_cases) +
  geom_text_repel(aes(day, cases, label = country),
                   data = max_cases) + 
  scale_x_continuous(limits = c(1,30),
                     breaks = c(1, 5, 10, 15, 20)) + 
  scale_y_log10(limits = c(1, 400),
                breaks = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  
  labs(title = "Evolución de casos diagnosticados de COVID-19",
       subtitle = "Actualizado a 15/mar/2020*",
       caption = "Fuente: Sistematización de la Universidad of Oxford de los datos de la OMS. \n* Esta base puede reflejar al día siguiente el número de casos reportados internamente por un país.\n@pCobosAlcala",
       x = "Días transcurridos desde el primer caso diagnosticado",
       y = "Casos diagnosticados (log)\n") + 
  theme_minimal() 

### 16 marzo ----

base16 <- c("Argentina",
  "Brasil",
  "Chile",
  "Colombia",
  "Costa Rica",
  "Ecuador",
  "México",
  "Panamá",
  "Perú",
  "República Dominicana",
  "Venezuela")

base <- base %>% 
  filter(country %in% base16)

max_cases <- max_cases %>% 
  filter(country %in% base16)


base %>% 
  ggplot(aes(color = country)) + 
  geom_line(aes(day,
                cases,
                group = country), size = 1) +
  geom_point(aes(day,
                cases,
                group = country), size = 1.3) +
  geom_point(aes(day,
                 cases,
                 group = country),
             data = max_cases, size = 2.5, color = "black") +
  geom_point(aes(day,
                 cases,
                 group = country),
             data = max_cases, size = 1.5) +

  geom_label_repel(aes(day, cases, label = country),
                   data = max_cases,
                   alpha = 0.7,
                   size = 3) + 
  scale_x_continuous(limits = c(1,20),
                     breaks = c(1, 5, 10, 15, 20)) + 
  scale_y_log10(limits = c(1, 300),
                breaks = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  labs(title = "Evolución de casos diagnosticados de COVID-19",
       subtitle = "Actualizado a 16/mar/2020 (10am CET)*",
       caption = "Fuente: Sistematización de la Universidad of Oxford de los datos de la OMS. \n* Esta base puede reflejar al día siguiente el número de casos reportados internamente por un país.\n@pCobosAlcala",
       x = "Días transcurridos desde el primer caso diagnosticado",
       y = "Casos diagnosticados (log)\n") + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")



### ----



base %>% 
  ggplot() +
  geom_line(aes(date, lead(Argentina,
                           n = lag_argentina)),
            color = rgb(.46, .67, .86),
            size = 1, alpha = 0.8) + 
  geom_point(aes(date, lead(Argentina,
                           n = lag_argentina)),
            color = rgb(.46, .67, .86),
            size = 2, alpha = 0.8) + 
  
  geom_line(aes(date, lead(Brazil,
                           n = lag_brazil)),
            color = rgb(.0, .61, .23),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Brazil,
                           n = lag_brazil)),
            color = rgb(.0, .61, .23),
            size = 2, alpha = 0.8) +
  
  geom_line(aes(date, lead(Colombia,
                           n = lag_colombia)),
            color = rgb(.99, .82, .09),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Colombia,
                           n = lag_colombia)),
            color = rgb(.99, .82, .09),
            size = 2, alpha = 0.8) +
  
  geom_line(aes(date, lead(`Costa Rica`,
                           n = lag_costarica)),
            color = rgb(.81, .07, .15),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(`Costa Rica`,
                           n = lag_costarica)),
            color = rgb(.81, .07, .15),
            size = 2, alpha = 0.8) +
  
  
  geom_line(aes(date, lead(Chile,
                           n = lag_chile)),
            color = rgb(.84, .17, .12),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Chile,
                           n = lag_chile)),
            color = rgb(.84, .17, .12),
            size = 2, alpha = 0.8) +
  
  geom_line(aes(date, lead(Mexico,
                           n = lag_mexico)),
            color = rgb(.0, .39, .25),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Mexico,
                           n = lag_mexico)),
            color = rgb(.0, .39, .25),
            size = 2, alpha = 0.8) +
  
  geom_line(aes(date, lead(Panama,
                           n = lag_panama)),
            color = rgb(.82, .06, .20),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Panama,
                           n = lag_panama)),
            color = rgb(.82, .06, .20),
            size = 2, alpha = 0.8) +
  
  geom_line(aes(date, lead(Peru,
                           n = lag_peru)),
            color = rgb(.85, .06, .14),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Peru,
                           n = lag_peru)),
            color = rgb(.85, .06, .14),
            size = 2, alpha = 0.8) +
  
              
  geom_line(aes(date, lead(Italy,
                           n = lag_italy)),
            color = rgb (.0, .55, .27),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Italy,
                           n = lag_italy)),
            color = rgb (.0, .55, .27),
            size = 2, alpha = 0.8) +
  
  geom_line(aes(date, lead(Spain,
                           n = lag_spain)),
            color = rgb(.67, .08, .11),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Spain,
                           n = lag_spain)),
            color = rgb(.67, .08, .11),
            size = 2, alpha = 0.8) +
              
  geom_line(aes(date, lead(Iran, n = lag_iran)),
            color = rgb(.14, .62, .25),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Iran, n = lag_iran)),
            color = rgb(.14, .62, .25),
            size = 2, alpha = 0.8) +
  
  scale_x_date(limits = as.Date(c("2020-01-21", "2020-02-07")),
               breaks =
                 c(seq.Date(as.Date("2020-01-21"), as.Date("2020-02-07"), "5 days")),
               labels = c(1, 5, 10, 15)) + 
  
  scale_y_log10(limits = c(1, 200),
                breaks = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  
  labs(title = "Evolución de casos diagnosticados de COVID-19",
       subtitle = "Actualizado a 15/mar/2020*",
       caption = "Fuente: Sistematización de la Universidad of Oxford de los datos de la OMS. \n* Esta base puede reflejar al día siguiente el número de casos reportado internamente por un país.\n@pCobosAlcala",
       x = "Días transcurridos desde primer caso diagnosticado",
       y = "Casos diagnosticados (log)\n") + 
  theme_minimal() 


annotate("text",
         x = 2008, y = 70, 
         label = "Expectativas de vida",
         color = "black")



# Base hoy

color_argentina = "lightblue"

Argentina, Bolivia, Brazil, Colombia, `Costa Rica`, Chile, Cuba, `Dominican Republic`, Ecuador, Honduras, Mexico, Panama, Peru, `Puerto Rico`, Venezuela

base %>% 
  ggplot() +

  # Argentina
  geom_line(aes(date, lead(Argentina,
                           n = lag_argentina)),
            color = color_argentina,
            size = 1, alpha = 0.7) + 
  geom_point(aes(date, lead(Argentina,
                            n = lag_argentina)),
             color = color_argentina,
             size = 2, alpha = 0.7) + 
annotate("text",
         x = as.Date("2020-02-02"), y = 50, 
         label = "Argentina",
         color = color_argentina) +


  # Bolivia
  

  # Brazil
  geom_line(aes(date, lead(Brazil,
                           n = lag_brazil)),
            color = rgb(.0, .61, .23),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Brazil,
                            n = lag_brazil)),
             color = rgb(.0, .61, .23),
             size = 2, alpha = 0.8) +
  annotate("text",
           x = as.Date("2020-02-04"), y = 100, 
           label = "Brazil",
           color = rgb(.0, .61, .23)) + 
           
  geom_line(aes(date, lead(Colombia,
                           n = lag_colombia)),
            color = rgb(.99, .82, .09),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Colombia,
                            n = lag_colombia)),
             color = rgb(.99, .82, .09),
             size = 2, alpha = 0.8) +
    annotate("text",
             x = as.Date("2020-01-26"), y = 3, 
             label = "Colombia",
             color = rgb(.99, .82, .09)) + 
             
  geom_line(aes(date, lead(Chile,
                           n = lag_chile)),
            color = rgb(.84, .17, .12),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Chile,
                            n = lag_chile)),
             color = rgb(.84, .17, .12),
             size = 2, alpha = 0.8) +
  annotate("text",
           x = as.Date("2020-02-01"), y = 75, 
           label = "Chile",
           color = rgb(.84, .17, .12)) + 
  
  geom_line(aes(date, lead(Mexico,
                           n = lag_mexico)),
            color = rgb(.0, .39, .25),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Mexico,
                            n = lag_mexico)),
             color = rgb(.0, .39, .25),
             size = 2, alpha = 0.8) +
  annotate("text",
           x = as.Date("2020-02-06"), y = 40, 
           label = "México",
           color = rgb(.0, .39, .25)) + 
  

  geom_line(aes(date, lead(Italy,
                           n = lag_italy)),
            color = rgb (.0, .55, .27),
            size = 1, alpha = 0.8) +
  geom_point(aes(date, lead(Italy,
                            n = lag_italy)),
             color = rgb (.0, .55, .27),
             size = 2, alpha = 0.8) +
  annotate("text",
           x = as.Date("2020-02-03"), y = 4.5, 
           label = "Italia",
           color = rgb (.0, .55, .27)) + 
  
  scale_x_date(limits = as.Date(c("2020-01-21", "2020-02-07")),
               breaks =
                 c(seq.Date(as.Date("2020-01-21"), as.Date("2020-02-07"), "5 days")),
               labels = c(1, 5, 10, 15)) + 
  
  scale_y_log10(limits = c(1, 200),
                breaks = c(1, 2, 5, 10, 20, 50, 100, 200)) +
  
  labs(title = "Evolución de casos diagnosticados de COVID-19",
       subtitle = "Actualizado a 15/mar/2020*",
       caption = "Fuente: Sistematización de la Universidad of Oxford de los datos de la OMS. \n* Esta base puede reflejar al día siguiente el número de casos reportados internamente por un país.\n@pCobosAlcala",
       x = "Días transcurridos desde el primer caso diagnosticado",
       y = "Casos diagnosticados (log)\n") + 
  theme_minimal() 



