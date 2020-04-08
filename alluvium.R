### Paquetes ----
library(pacman)
p_load(tidyverse, readr, scales, ggalluvial, ggrepel)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())

mexico <- read_excel("1_data/mexico.xlsx", 
                     col_types = c("numeric", "text", "text", 
                                   "numeric", "date", "text", "text", 
                                   "date", "date"))

mexico <- mexico %>% 
  mutate_if(is.character, str_to_title)

mexico <- mexico %>% 
  group_by(procedencia, ent) %>% 
  count() 

a <- mexico %>% 
  group_by(ent) %>% 
  count()

# View(a)

a <- a %>% 
  filter(n < 3)

mexico <- as.data.frame(mexico)

glimpse(a)

mexico %>% 
  ggplot(aes(axis1 = procedencia, 
           axis2 = ent,
           y = n)) +
  geom_alluvium(aes(fill = procedencia),
                alpha = 0.4) +
  geom_stratum(color = "white",
               size = 1, 
               alpha = 0) +
  geom_text(stat = "stratum",
            label.strata = T,
            infer.label = T,
            size = 3,
            check_overlap = T) +
  scale_x_discrete(limits = c("Procedencia", "Estado"),
                   expand = c(.001, .001),
                   position = "top") +
  scale_y_continuous(expand = c(0.05, .005)) +
  labs(title = "Procedencia y entidad de los X casos de COVID-19 en México",
       subtitle = "Actualizado al 22 de marzo de 2020",
       caption = "@pCobosAlcala (CC BY)\nFuente: Datos de la Secretaría de Salud recopilados por @guzmart_ (bit.ly/2QCgyPN)",
       y = "") +
  theme_minimal() + 
theme(axis.text.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none")

