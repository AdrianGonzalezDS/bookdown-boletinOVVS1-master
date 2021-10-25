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
victimasexodelactiv <- victimasdelito %>%
  select(informacion_sociodem_2, prensa, tipo_delito,victima_era_2,sexo_victima_2) %>%
  filter(!tipo_delito %in% c("Homicidio intencional"))

victimasexodelactiv


victimasexodelactiv_sel <- victimasdelito %>%
  select(informacion_sociodem_2, prensa, tipo_delito,victima_era_2, sexo_victima_2) %>%
  filter(!tipo_delito %in% c("Homicidio intencional")&
           !victima_era_2 %in% c(NA, "NA","No informa")&
           !sexo_victima_2 %in% c("No informa",NA, "NA"))

victimasexodelactiv_sel

prensa_victimasexodelactiv_sel <- length(unique(victimasexodelactiv_sel[["prensa"]]))
prensa_victimasexodelactiv_sel


#Poniendo acentos en la leyenda
victimasexodelactiv_sel$victima_era_2 <- str_replace_all(victimasexodelactiv_sel$victima_era_2,
                                                         "[^[:alnum:]]"," ")

victimasexodelactiv_sel$victima_era_2 <- str_replace_all(victimasexodelactiv_sel$victima_era_2,
                                                         "Hogar de la victima","Hogar de la víctima")
victimasexodelactiv_sel$victima_era_2 <- str_replace_all(victimasexodelactiv_sel$victima_era_2,
                                                         "Entornos de atenci n institucional","Entornos de atención institucional")

victimasexodelactiv_sel$victima_era_2 <- str_replace_all(victimasexodelactiv_sel$victima_era_2,
                                                         "D a de la semana en la noche","Día de la semana en la noche")
victimasexodelactiv_sel$victima_era_2 <- str_replace_all(victimasexodelactiv_sel$victima_era_2,
                                                         "Fin de semana en el d a","Fin de semana en el día")
victimasexodelactiv_sel$victima_era_2 <- str_replace_all(victimasexodelactiv_sel$victima_era_2,
                                                         "Desaparici n forzada","Desaparición forzada")
victimasexodelactiv_sel$victima_era_2 <- str_replace_all(victimasexodelactiv_sel$victima_era_2,
                                                         "D a de la semana en la madrugada","Día de la semana en la madrugada")
victimasexodelactiv_sel$victima_era_2 <- str_replace_all(victimasexodelactiv_sel$victima_era_2,
                                                         "Día de la semana en el d a","Día de la semana en el día")
victimasexodelactiv_sel$victima_era_2 <- str_replace_all(victimasexodelactiv_sel$victima_era_2,
                                                         "Fin de semaan en la noche","Fin de semana en la noche")

victimasexodelactiv_sel$prensa<- NULL
victimasexodelactiv_sel$tipo_delito <- NULL
victimasexodelactiv_sel



victimasexodelactiv_sel$victima_era_2 = as.factor(victimasexodelactiv_sel$victima_era_2)
victimasexodelactiv_sel$sexo_victima_2 <- droplevels(victimasexodelactiv_sel$sexo_victima_2)


#graficando
victimasexodelactiv_graf <- apyramid::age_pyramid(data = victimasexodelactiv_sel,
                                                 age_group = "victima_era_2",
                                                 split_by = "sexo_victima_2", show_midpoint=F)+
  # labels, titles, caption
  labs(#title="Número de víctimas por delitos distintos a homicidio intencional",
       #subtitle = "discriminados por sexo y actividad",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasexodelactiv)} ({sum(is.na(victimasexodelactiv$sexo_victima_2) | is.na(victimasexodelactiv$victima_era_2) |victimasexodelactiv$victima_era_2 == 'NA'|victimasexodelactiv$victima_era_2 == 'No informa' | victimasexodelactiv$sexo_victima_2 == 'No informa')} casos perdidos por información faltante) en {prensa_victimasexodelactiv_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
       x = "Actividad",
       y = "Número de víctimas",
       fill = "")+
  scale_fill_manual(
         values = c("Femenino" = "orange",         # assign colors to values in the data
                    "Masculino" = "darkgreen"))+

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

victimasexodelactiv_graf

ggsave("images/victimasexodelactiv_graf.png",width=8,height=5)



