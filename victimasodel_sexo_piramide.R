library(devtools)
library(ggplot2)
library(tidyverse) #requerido para la funcion gather
library(bbplot) #requerido para bbc style
library(plyr) #requerido para hacer gr?ficos de pir?mide
library(dplyr) #requerido para usar la funcion mutate
library(tidyr) #requerido para usar la funcion gather
library(stringr)#requerida para usar str_replace_all
library(apyramid)# paquete dedicado a la creacion de pir�mides de edad
library(janitor) # tables and cleaning data
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

#Prepare datos
victimasexotrosdel <- victimasdelito %>%
  select(informacion_sociodem_2, prensa, sexo_victima_2, tipo_delito) %>%
  filter(!tipo_delito %in% c("Homicidio intencional") )

victimasexotrosdel #requerido para calcular casos perdidos


victimasexotrosdel_sel <- victimasdelito %>%
  select(informacion_sociodem_2, prensa, sexo_victima_2, tipo_delito) %>%
  filter(!tipo_delito %in% c("Homicidio intencional")&
           !sexo_victima_2 %in% c("No informa",NA, "NA"))

victimasexotrosdel_sel

prensa_victimasexotrosdel_sel <- length(unique(victimasexotrosdel_sel[["prensa"]]))
prensa_victimasexotrosdel_sel

#Poniendo acentos en la leyenda
victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "[^[:alnum:]]"," ")

victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Agresi n  incluye lesiones graves o leves  etc ","Agresi�n lesiones graves o leves")
victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Violaci n sexual","Violaci�n sexual")

victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Coacci n  incluye extorsi n","Coacci�n  incluye extorsi�n")
victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Amenaza de agresi n","Amenaza de agresi�n")
victimasexotrosdel_sel$tipo_delito <- str_replace_all(victimasexotrosdel_sel$tipo_delito,
                                                   "Desaparici n forzada","Desaparici�n forzada")
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
  labs(#title="N�mero de v�ctimas por delitos distintos a homicidio intencional",
       #subtitle = "discriminados por sexo y tipo de delito",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasexotrosdel)} ({sum(is.na(victimasexotrosdel$sexo_victima_2) | is.na(victimasexotrosdel$tipo_delito) |victimasexotrosdel$tipo_delito == 'NA' | victimasexotrosdel$sexo_victima_2 == 'No informa')} casos perdidos por informaci�n sobre sexo faltante) en {prensa_victimasexotrosdel_sel} medios de prensa consultados \nPer�odo de recolecci�n de informaci�n: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
    x = "Tipo de delito",
    y = "N�mero de v�ctimas",
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
    plot.caption = element_text(hjust=0, size=9, face = "italic"))

victimasexotrosdel_graf

ggsave("images/victimasexotrosdel_graf.png",width=8,height=5)

