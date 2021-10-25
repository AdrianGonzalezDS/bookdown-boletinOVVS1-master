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
victimasagresiondel <- victimasdelito %>%
  select(infodelito2, prensa, sexo_victima_2, tipo_delito,motivacion) %>%
  filter(infodelito2 == "Si"&
           tipo_delito == "Agresión" )

victimasagresiondel #requerido para calcular casos perdidos

sexo_victimasagresiondel_sel <- length(unique(victimasagresiondel[["sexo_victima_2"]]))
sexo_victimasagresiondel_sel

unique(victimasagresiondel[c("sexo_victima_2")])

victimasagresiondel_sel <- victimasdelito %>%
  select(sexo_victima_2, prensa, tipo_delito,motivacion) %>%
  filter(tipo_delito == "Agresión"&
           !sexo_victima_2 %in% c("No informa",NA, "NA")&
           !motivacion %in% c(NA, "NA"))

victimasagresiondel_sel

victimasagresiondel_sel$sexo_victima_2 <- droplevels(victimasagresiondel_sel$sexo_victima_2)

prensa_victimasagresiondel_sel <- length(unique(victimasagresiondel_sel[["prensa"]]))
prensa_victimasagresiondel_sel



#Poniendo acentos en la leyenda
victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "[^[:alnum:]]"," ")

victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "Provecho Il cito","Provecho Ilícito")
victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "Basado en el g nero","Basado en el género")

victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "Otra motivaci n","Otra motivación")
victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "Ri a","Riña")
victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "Conflicto interpersonal distinto de ri a y venganza","Conflicto distinto de riña y venganza")
victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "Intereses pol ticos","Intereses políticos")
victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "Día de la semana en el d a","Día de la semana en el día")
victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "Relacionado con el terrorismo","Terrorismo")
victimasagresiondel_sel$motivacion  <- str_replace_all(victimasagresiondel_sel$motivacion ,
                                                       "Conflicto interpersonal distinto de riña y venganza","Conflicto distinto de riña y venganza")

victimasagresiondel_sel$motivacion = as.factor(victimasagresiondel_sel$motivacion)


victimasagresiondelmotiv_pir <- apyramid::age_pyramid(data = victimasagresiondel_sel,
                                 age_group = "motivacion",
                                 split_by = "sexo_victima_2",
                                 na.rm = T, show_midpoint = F,
                                 pyramid = T)+
  # labels, titles, caption
    labs(#title="Número de víctimas por agresión",
         #subtitle = "discriminados por sexo y motivación del delito",
         caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasagresiondel)} ({sum(is.na(victimasagresiondel$sexo_victima_2) | is.na(victimasagresiondel$motivacion) |victimasagresiondel$motivacion == 'NA'|victimasagresiondel$motivacion == 'No informa' | victimasagresiondel$sexo_victima_2 == 'No informa')} casos perdidos por información faltante) en {prensa_victimasagresiondel_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
         x = "Motivación",
         y = "Número de víctimas",
         fill = "")+
    scale_fill_manual(values = c("Femenino" = "yellow", # assign colors to values in the data
                 "Masculino" = "darkblue"))+
  # display adjustments
  theme_minimal()+
  theme(legend.position = "bottom",                             # move legend to bottom
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    #axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0),
    plot.caption = element_text(hjust=0, size=8, face = "italic"))

victimasagresiondelmotiv_pir

ggsave("images/victimasagresiondelmotiv_pir.png",width=8,height=5)


# unique(victimasagresiondel_sel[c("sexo_victima_2")])

# sexo_victimasagresiondel_sel <- length(unique(victimasagresiondel_sel[["sexo_victima_2"]]))
# sexo_victimasagresiondel_sel
# str(sexo_victimasagresiondel_sel)
#
# unique(victimasagresiondel_sel[c("sexo_victima_2")])
# names(victimasagresiondel_sel)[names(victimasagresiondel_sel) ==
#                                  "sexo_victima_2"] <- "sexo"





# victimasagresiondel_sel$prensa<- NULL
# victimasagresiondel_sel$tipo_delito <- NULL
# victimasagresiondel_sel$infodelito2 <- NULL
# victimasagresiondel_sel
#
#

# victimasagresiondel_sel$motivacion = as.factor(victimasagresiondel_sel$motivacion)
# victimasagresiondel_sel$sexo <- droplevels(victimasagresiondel_sel$motivacion)
# victimasagresiondel_sel$sexo <- droplevels(victimasagresiondel_sel$sexo)




# victimasagresiondel_graf <- apyramid::age_pyramid(data = victimasagresiondel_sel,
#                                                  age_group = "motivacion",
#                                                  split_by = "sexo_victima_2",
#                                                  na.rm = T, show_midpoint=F,
#                                                  pyramid = T)+
#   # labels, titles, caption
#   labs(title="Número de víctimas por agresión",
#        subtitle = "discriminados por sexo y motivación del delito",
#        caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasagresiondel)} ({sum(is.na(victimasagresiondel$sexo_victima_2) | is.na(victimasagresiondel$motivacion) |victimasagresiondel$motivacion == 'NA'|victimasagresiondel$motivacion == 'No informa' | victimasagresiondel$sexo_victima_2 == 'No informa')} casos perdidos por información faltante) \nen {prensa_victimasagresiondel_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
#        x = "Motivación",
#        y = "Número de víctimas",
#        fill = "")+
#   scale_fill_manual(values = c("Femenino" = "yellow", # assign colors to values in the data
#                "Masculino" = "darkblue"))+
#
#   # display adjustments
#   theme_minimal()+
#   theme(legend.position = "bottom",                             # move legend to bottom
#     #panel.grid.major = element_blank(),
#     #panel.grid.minor = element_blank(),
#     #panel.background = element_blank(),
#     #axis.line = element_line(colour = "black"),
#     plot.title = element_text(hjust = 0),
#     plot.caption = element_text(hjust=0, size=11, face = "italic"))
#
# victimasagresiondel_graf
