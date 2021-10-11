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
victimasexodelvictimariorelNNA <- victimasdelito %>%
  select(infodelito2, edad__victima_2, prensa, sexo_victima_2, tipo_delito, relacion_victimario) %>%
  filter(infodelito2 == "Si"&
           !edad__victima_2 %in% c(18:100))

victimasexodelvictimariorelNNA #requerido para calcular casos perdidos


victimasexodelvictimariorelNNA_sel <- victimasdelito %>%
  select(infodelito2, edad__victima_2, prensa, sexo_victima_2, tipo_delito, relacion_victimario) %>%
  filter(infodelito2 == "Si"&
           !edad__victima_2 %in% c(18:100)&
           !infodelito2 %in% c(NA, "NA")&
           !sexo_victima_2 %in% c("No informa",NA, "NA")&
           !relacion_victimario %in% c("No informa",NA, "NA"))

victimasexodelvictimariorelNNA_sel

prensa_victimasexodelvictimariorelNNA_sel <- length(unique(victimasexodelvictimariorelNNA_sel[["prensa"]]))
prensa_victimasexodelvictimariorelNNA_sel

#Poniendo acentos en la leyenda
victimasexodelvictimariorelNNA_sel$relacion_victimario <- str_replace_all(victimasexodelvictimariorelNNA_sel$relacion_victimario,
                                                                          "[^[:alnum:]]"," ")

victimasexodelvictimariorelNNA_sel$relacion_victimario <- str_replace_all(victimasexodelvictimariorelNNA_sel$relacion_victimario,
                                                                          "Funcionario de organismos de seguridad","Funcionario de seguridad")

victimasexodelvictimariorelNNA_sel$relacion_victimario <- str_replace_all(victimasexodelvictimariorelNNA_sel$relacion_victimario,
                                                                          "Otro transgresor conocido por la victima","Otro transgresor conocido")

victimasexodelvictimariorelNNA_sel$relacion_victimario <- str_replace_all(victimasexodelvictimariorelNNA_sel$relacion_victimario,
                                                                          "Relaci n de autoridad o cuidado  doctor enfermero policia  etc","Relación de autoridad o cuidado")

victimasexodelvictimariorelNNA_sel$relacion_victimario <- str_replace_all(victimasexodelvictimariorelNNA_sel$relacion_victimario,
                                                                          "Relaci n laboral colegas","Relación laboral colegas")
victimasexodelvictimariorelNNA_sel$relacion_victimario <- str_replace_all(victimasexodelvictimariorelNNA_sel$relacion_victimario,
                                                                          "Amenaza de agresi n","Amenaza de agresión")
victimasexodelvictimariorelNNA_sel$relacion_victimario <- str_replace_all(victimasexodelvictimariorelNNA_sel$relacion_victimario,
                                                                          "Desaparici n forzada","Desaparición forzada")
# victimasexodelvictimariorelNNA_sel$relacion_victimario <- str_replace_all(victimasexodelvictimariorelNNA_sel$relacion_victimario,
#                                                       "D?a de la semana en el d a","D?a de la semana en el d?a")

victimasexodelvictimariorelNNA_sel$prensa<- NULL
victimasexodelvictimariorelNNA_sel

victimasexodelvictimariorelNNA_sel$relacion_victimario = as.factor(victimasexodelvictimariorelNNA_sel$relacion_victimario)
victimasexodelvictimariorelNNA_sel$sexo_victima_2 <- droplevels(victimasexodelvictimariorelNNA_sel$sexo_victima_2)

victimasexodelvictimariorelNNA_graf <- apyramid::age_pyramid(data = victimasexodelvictimariorelNNA_sel,
                                                             age_group = "relacion_victimario",
                                                             split_by = "sexo_victima_2", show_midpoint=F)+
  # labels, titles, caption
  labs(title="Número de niñas, niños y adolescentes víctima de delitos",
       subtitle = "discriminados por sexo y tipo de victimario",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasexodelvictimariorelNNA)} ({sum(is.na(victimasexodelvictimariorelNNA$sexo_victima_2) | is.na(victimasexodelvictimariorelNNA$relacion_victimario) |victimasexodelvictimariorelNNA$relacion_victimario == 'NA' | victimasexodelvictimariorelNNA$sexo_victima_2 == 'No informa')} casos perdidos por información sobre sexo faltante) en {prensa_victimasexodelvictimariorelNNA_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
       x = "Tipo de victimario",
       y = "Número de víctimas",
       fill = "")+
  scale_fill_manual(values = c("Femenino" = "darkgreen", # assign colors to values in the data
                               "Masculino" = "yellow"))+


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

victimasexodelvictimariorelNNA_graf

