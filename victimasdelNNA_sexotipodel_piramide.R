library(devtools)
library(ggplot2)
library(tidyverse) #requerido para la funcion gather
library(bbplot) #requerido para bbc style
library(plyr) #requerido para hacer gr?ficos de pir?mide
library(dplyr) #requerido para usar la funcion mutate
library(tidyr) #requerido para usar la funcion gather
library(stringr)#requerida para usar str_replace_all
library(apyramid)# paquete dedicado a la creacion de pirámides de edad
library(janitor) # tables and cleaning data
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

#Prepare datos
victimasexodelNNA <- victimasdelito %>%
  select(informacion_sociodem_2, edad__victima_2, prensa, sexo_victima_2, tipo_delito) %>%
  filter(!edad__victima_2 %in% c(18:100))

victimasexodelNNA #requerido para calcular casos perdidos


victimasexodelNNA_sel <- victimasdelito %>%
  select(informacion_sociodem_2, edad__victima_2, prensa, sexo_victima_2, tipo_delito) %>%
  filter(!edad__victima_2 %in% c(18:100)&
           !sexo_victima_2 %in% c("No informa",NA, "NA"))

victimasexodelNNA_sel

prensa_victimasexodelNNA_sel <- length(unique(victimasexodelNNA_sel[["prensa"]]))
prensa_victimasexodelNNA_sel

#Poniendo acentos en la leyenda
victimasexodelNNA_sel$tipo_delito <- str_replace_all(victimasexodelNNA_sel$tipo_delito,
                                                      "[^[:alnum:]]"," ")

victimasexodelNNA_sel$tipo_delito <- str_replace_all(victimasexodelNNA_sel$tipo_delito,
                                                      "Agresi n  incluye lesiones graves o leves  etc ","Agresión lesiones graves o leves")
victimasexodelNNA_sel$tipo_delito <- str_replace_all(victimasexodelNNA_sel$tipo_delito,
                                                      "Violaci n sexual","Violación sexual")

victimasexodelNNA_sel$tipo_delito <- str_replace_all(victimasexodelNNA_sel$tipo_delito,
                                                      "Coacci n  incluye extorsi n","Coacción  incluye extorsión")
victimasexodelNNA_sel$tipo_delito <- str_replace_all(victimasexodelNNA_sel$tipo_delito,
                                                      "Amenaza de agresi n","Amenaza de agresión")
victimasexodelNNA_sel$tipo_delito <- str_replace_all(victimasexodelNNA_sel$tipo_delito,
                                                      "Desaparici n forzada","Desaparición forzada")
# victimasexodelNNA_sel$tipo_delito <- str_replace_all(victimasexodelNNA_sel$tipo_delito,
#                                                       "D?a de la semana en el d a","D?a de la semana en el d?a")

victimasexodelNNA_sel$prensa<- NULL
victimasexodelNNA_sel

victimasexodelNNA_sel$tipo_delito = as.factor(victimasexodelNNA_sel$tipo_delito)
victimasexodelNNA_sel$sexo_victima_2 <- droplevels(victimasexodelNNA_sel$sexo_victima_2)

victimasexodelNNA_graf <- apyramid::age_pyramid(data = victimasexodelNNA_sel,
                                                 age_group = "tipo_delito",
                                                 split_by = "sexo_victima_2", show_midpoint=F)+
  # labels, titles, caption
  labs(#title="Número de niñas, niños y adolescentes víctima de delitos",
       #subtitle = "discriminados por sexo y tipo de delito",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasexodelNNA)} ({sum(is.na(victimasexodelNNA$sexo_victima_2) | is.na(victimasexodelNNA$tipo_delito) |victimasexodelNNA$tipo_delito == 'NA' | victimasexodelNNA$sexo_victima_2 == 'No informa')} casos perdidos por información sobre sexo faltante) en {prensa_victimasexodelNNA_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
       x = "Tipo de delito",
       y = "Número de víctimas",
       fill = "")+

  # display adjustments
  theme_minimal()+
  theme(
    legend.position = "bottom",                             # move legend to bottom
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    #axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0),
    plot.caption = element_text(hjust=0, size=8, face = "italic"))

victimasexodelNNA_graf

ggsave("images/victimasexodelNNA_graf.png",width=8,height=5)
