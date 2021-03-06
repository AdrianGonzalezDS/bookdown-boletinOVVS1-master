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
victimasagresiondonde <- victimasdelito %>%
  select(prensa, sexo_victima_2, tipo_delito,donde_delito) %>%
  filter(tipo_delito == "Agresi�n" )

victimasagresiondonde #requerido para calcular casos perdidos

sexo_victimasagresiondonde_sel <- length(unique(victimasagresiondonde[["sexo_victima_2"]]))
sexo_victimasagresiondonde_sel

unique(victimasagresiondonde[c("sexo_victima_2")])

victimasagresiondonde_sel <- victimasdelito %>%
  select(sexo_victima_2, prensa, tipo_delito,donde_delito) %>%
  filter(tipo_delito == "Agresi�n"&
           !sexo_victima_2 %in% c("No informa",NA, "NA")&
           !donde_delito %in% c(NA, "NA"))

victimasagresiondonde_sel

victimasagresiondonde_sel$sexo_victima_2 <- droplevels(victimasagresiondonde_sel$sexo_victima_2)

prensa_victimasagresiondonde_sel <- length(unique(victimasagresiondonde_sel[["prensa"]]))
prensa_victimasagresiondonde_sel



#Poniendo acentos en la leyenda
victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "[^[:alnum:]]"," ")

victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "Provecho Il cito","Provecho Il�cito")
victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "Basado en el g nero","Basado en el g�nero")

victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "Otra motivaci n","Otra motivaci�n")
victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "Ri a","Ri�a")
victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "Conflicto interpersonal distinto de ri a y venganza","Conflicto distinto de ri�a y venganza")
victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "Intereses pol ticos","Intereses pol�ticos")
victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "D�a de la semana en el d a","D�a de la semana en el d�a")
victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "Relacionado con el terrorismo","Terrorismo")
victimasagresiondonde_sel$donde_delito  <- str_replace_all(victimasagresiondonde_sel$donde_delito ,
                                                       "Conflicto interpersonal distinto de ri�a y venganza","Conflicto distinto de ri�a y venganza")

victimasagresiondonde_sel$donde_delito = as.factor(victimasagresiondonde_sel$donde_delito)


victimasagresiondonde_grafico <- apyramid::age_pyramid(data = victimasagresiondonde_sel,
                                 age_group = "donde_delito",
                                 split_by = "sexo_victima_2",
                                 na.rm = T, show_midpoint = F,
                                 pyramid = T)+
  # labels, titles, caption
  labs(#title="N�mero de v�ctimas por agresi�n",
       #subtitle = "discriminados por sexo y lugar de ocurrencia",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasagresiondonde)} ({sum(is.na(victimasagresiondonde$sexo_victima_2) | is.na(victimasagresiondonde$donde_delito) |victimasagresiondonde$donde_delito == 'NA'|victimasagresiondonde$donde_delito == 'No informa' | victimasagresiondonde$sexo_victima_2 == 'No informa')} casos perdidos por informaci�n faltante) en {prensa_victimasagresiondonde_sel} medios de prensa consultados \nPer�odo de recolecci�n de informaci�n: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
       x = "Lugar de ocurrencia",
       y = "N�mero de v�ctimas",
       fill = "")+
  scale_fill_manual(values = c("Femenino" = "darkred", # assign colors to values in the data
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

victimasagresiondonde_grafico

ggsave("images/victimasagresiondonde_grafico.png",width=8,height=5)


# unique(victimasagresiondonde_sel[c("sexo_victima_2")])

# sexo_victimasagresiondonde_sel <- length(unique(victimasagresiondonde_sel[["sexo_victima_2"]]))
# sexo_victimasagresiondonde_sel
# str(sexo_victimasagresiondonde_sel)
#
# unique(victimasagresiondonde_sel[c("sexo_victima_2")])
# names(victimasagresiondonde_sel)[names(victimasagresiondonde_sel) ==
#                                  "sexo_victima_2"] <- "sexo"





# victimasagresiondonde_sel$prensa<- NULL
# victimasagresiondonde_sel$tipo_delito <- NULL
# victimasagresiondonde_sel$infodelito2 <- NULL
# victimasagresiondonde_sel
#
#

# victimasagresiondonde_sel$donde_delito = as.factor(victimasagresiondonde_sel$donde_delito)
# victimasagresiondonde_sel$sexo <- droplevels(victimasagresiondonde_sel$donde_delito)
# victimasagresiondonde_sel$sexo <- droplevels(victimasagresiondonde_sel$sexo)




# victimasagresiondonde_graf <- apyramid::age_pyramid(data = victimasagresiondonde_sel,
#                                                  age_group = "donde_delito",
#                                                  split_by = "sexo_victima_2",
#                                                  na.rm = T, show_midpoint=F,
#                                                  pyramid = T)+
#   # labels, titles, caption
#   labs(title="N�mero de v�ctimas por agresi�n",
#        subtitle = "discriminados por sexo y motivaci�n del delito",
#        caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasagresiondonde)} ({sum(is.na(victimasagresiondonde$sexo_victima_2) | is.na(victimasagresiondonde$donde_delito) |victimasagresiondonde$donde_delito == 'NA'|victimasagresiondonde$donde_delito == 'No informa' | victimasagresiondonde$sexo_victima_2 == 'No informa')} casos perdidos por informaci�n faltante) \nen {prensa_victimasagresiondonde_sel} medios de prensa consultados \nPer�odo de recolecci�n de informaci�n: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
#        x = "Motivaci�n",
#        y = "N�mero de v�ctimas",
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
# victimasagresiondonde_graf
