library(devtools)
library(ggplot2)
library(tidyverse)
library(bbplot)
library(plyr) #requerido para hacer gr?ficos de pir?mide
library(dplyr) #requerido para usar la funcion mutate
library(tidyr) #requerido para usar la funcion gather
library(stringr)#requerida para usar str_replace_all
library(writexl)#requerido para exportar df a excel
library(ggrepel)#requerido para usar geom_text_repel
library(apyramid)# paquete dedicado a la creacion de pirámides de edad
library(janitor) # tables and cleaning data

#library(reshape2)#requerido para el paquete melt
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

#Prepare datos
victimasdelhimotiv <- victimasdelito %>%
  select(sexo_victima_2,prensa, infodelito2, tipo_delito, motivacion) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Homicidio intencional" )

victimasdelhimotiv

victimasdelhimotiv_sel <- victimasdelito %>%
  select(sexo_victima_2, prensa, infodelito2, tipo_delito, motivacion) %>%
  filter(infodelito2 == "Si" &
           tipo_delito == "Homicidio intencional" &
           !motivacion %in% c("No informa", NA, "NA")&
           !tipo_delito %in% c("No informa", NA, "NA")&
           !sexo_victima_2 %in% c("No informa", NA, "NA") )

victimasdelhimotiv_sel


prensa_victimasdelhimotiv_sel <- length(unique(victimasdelhimotiv_sel[["prensa"]]))
prensa_victimasdelhimotiv_sel


#Poniendo acentos en la leyenda
victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                       "[^[:alnum:]]"," ")

victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                       "Provecho Il cito","Provecho Ilícito")
victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                       "Basado en el g nero","Basado en el género")

victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                       "Otra motivaci n","Otra motivación")
victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                       "Ri a","Riña")
victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                       "Conflicto interpersonal distinto de ri a y venganza","Conflicto distinto de riña y venganza")
victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                       "Intereses pol ticos","Intereses políticos")
victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                       "Día de la semana en el d a","Día de la semana en el día")
victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                       "Relacionado con el terrorismo","Terrorismo")
victimasdelhimotiv_sel$motivacion  <- str_replace_all(victimasdelhimotiv_sel$motivacion ,
                                                      "Conflicto interpersonal distinto de riña y venganza","Conflicto distinto de riña y venganza")

victimasdelhimotiv_sel$prensa<- NULL
victimasdelhimotiv_sel$tipo_delito <- NULL
victimasdelhimotiv_sel$infodelito2 <- NULL
victimasdelhimotiv_sel

victimasdelhimotiv_sel$motivacion = as.factor(victimasdelhimotiv_sel$motivacion)
victimasdelhimotiv_sel$motivacion <- droplevels(victimasdelhimotiv_sel$motivacion)
victimasdelhimotiv_sel$sexo_victima_2 <- droplevels(victimasdelhimotiv_sel$sexo_victima_2)

names(victimasdelhimotiv_sel)[names(victimasdelhimotiv_sel) ==
                                "motivacion"] <- "motiv"
names(victimasdelhimotiv_sel)[names(victimasdelhimotiv_sel) ==
                                "sexo_victima_2"] <- "sexo"



victimasdelhimotiv_graf <- apyramid::age_pyramid(data = victimasdelhimotiv_sel,
                                                  age_group = "motiv",
                                                  split_by = "sexo", show_midpoint=F)+# labels, titles, caption
  labs(title="Número de víctimas por homicidio intencional",
       subtitle = "discriminados por sexo y motivación del delito",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasagresiondel)} ({sum(is.na(victimasagresiondel$sexo_victima_2) | is.na(victimasagresiondel$motivacion) |victimasagresiondel$motivacion == 'NA'|victimasagresiondel$motivacion == 'No informa' | victimasagresiondel$sexo_victima_2 == 'No informa')} casos perdidos por información faltante) en {prensa_victimasdelhimotiv_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
       x = "Motivación",
       y = "Número de víctimas",
       fill = "")+
  scale_fill_manual(values = c("Femenino" = "yellow", # assign colors to values in the data
                               "Masculino" = "darkorange"))+

  # display adjustments
  theme_minimal()+
  theme(legend.position = "bottom",                             # move legend to bottom
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        #axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0),
        plot.caption = element_text(hjust=0, size=9, face = "italic"))



victimasdelhimotiv_graf
