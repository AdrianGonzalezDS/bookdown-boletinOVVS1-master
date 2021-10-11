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
victimasexotrosdel <- victimasdelito %>%
  select(infodelito2, prensa, sexo_victima_2, tipo_delito) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional") )

victimasexotrosdel #requerido para calcular casos perdidos


victimasexotrosdel_sel <- victimasdelito %>%
  select(infodelito2, prensa, sexo_victima_2, tipo_delito) %>%
  filter(infodelito2 == "Si"&
           !tipo_delito %in% c("Homicidio intencional")&
           !infodelito2 %in% c(NA, "NA")&
           !sexo_victima_2 %in% c("No informa",NA, "NA"))

victimasexotrosdel_sel

prensa_victimasexotrosdel_sel <- length(unique(victimasexotrosdel_sel[["prensa"]]))
prensa_victimasexotrosdel_sel

#Poniendo acentos en la leyenda
victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "[^[:alnum:]]"," ")

victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Agresi n  incluye lesiones graves o leves  etc ","Agresión lesiones graves o leves")
victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Violaci n sexual","Violación sexual")

victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Coacci n  incluye extorsi n","Coacción  incluye extorsión")
victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Amenaza de agresi n","Amenaza de agresión")
victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Desaparici n forzada","Desaparición forzada")
# victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
#                                                       "D?a de la semana en el d a","D?a de la semana en el d?a")

victimasexotrosdel_sel$prensa<- NULL
victimasexotrosdel_sel

victimasexotrosdel_sel$tipo_delito = as.factor(victimasexotrosdel_sel$tipo_delito)
victimasexotrosdel_sel$sexo_victima_2 <- droplevels(victimasexotrosdel_sel$sexo_victima_2)

victimasexotrosdel_graf <- apyramid::age_pyramid(data = victimasexotrosdel_sel,
                      age_group = "tipo_delito",
                      split_by = "sexo_victima_2", show_midpoint=F)+
  # labels, titles, caption
  labs(title="Número de víctimas por delitos distintos a homicidio intencional",
       subtitle = "discriminados por sexo y tipo de delito",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasexotrosdel)} ({sum(is.na(victimasexotrosdel$sexo_victima_2) | is.na(victimasexotrosdel$tipo_delito) |victimasexotrosdel$tipo_delito == 'NA' | victimasexotrosdel$sexo_victima_2 == 'No informa')} casos perdidos por información sobre sexo faltante) \nen {prensa_victimasexotrosdel_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
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
    plot.caption = element_text(hjust=0, size=11, face = "italic"))

victimasexotrosdel_graf
