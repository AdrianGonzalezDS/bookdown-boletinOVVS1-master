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
victimasexodelvictimarioconNNA <- victimasdelito %>%
  select(informacion_sociodem_2, edad__victima_2, prensa, sexo_victima_2, tipo_delito, victimario_conocido) %>%
  filter(!edad__victima_2 %in% c(18:100))

victimasexodelvictimarioconNNA #requerido para calcular casos perdidos


victimasexodelvictimarioconNNA_sel <- victimasdelito %>%
  select(informacion_sociodem_2, edad__victima_2, prensa, sexo_victima_2, tipo_delito, victimario_conocido) %>%
  filter(!edad__victima_2 %in% c(18:100)&
           !sexo_victima_2 %in% c("No informa",NA, "NA")&
           !victimario_conocido %in% c("No informa",NA, "NA"))

victimasexodelvictimarioconNNA_sel

prensa_victimasexodelvictimarioconNNA_sel <- length(unique(victimasexodelvictimarioconNNA_sel[["prensa"]]))
prensa_victimasexodelvictimarioconNNA_sel

#Poniendo acentos en la leyenda
victimasexodelvictimarioconNNA_sel$victimario_conocido <- str_replace_all(victimasexodelvictimarioconNNA_sel$victimario_conocido,
                                                     "[^[:alnum:]]"," ")

victimasexodelvictimarioconNNA_sel$victimario_conocido <- str_replace_all(victimasexodelvictimarioconNNA_sel$victimario_conocido,
                                                     "Otro transgresor conocido por la victima","Otro transgresor conocido")

victimasexodelvictimarioconNNA_sel$victimario_conocido <- str_replace_all(victimasexodelvictimarioconNNA_sel$victimario_conocido,
                                                      "Otro transgresor conocido por la victima","Otro transgresor conocido")

victimasexodelvictimarioconNNA_sel$victimario_conocido <- str_replace_all(victimasexodelvictimarioconNNA_sel$victimario_conocido,
                                                     "Relaci n de autoridad o cuidado  doctor enfermero policia  etc","Relación de autoridad")

victimasexodelvictimarioconNNA_sel$victimario_conocido <- str_replace_all(victimasexodelvictimarioconNNA_sel$victimario_conocido,
                                                     "Relaci n laboral colegas","Relación laboral colegas")
victimasexodelvictimarioconNNA_sel$victimario_conocido <- str_replace_all(victimasexodelvictimarioconNNA_sel$victimario_conocido,
                                                     "Amenaza de agresi n","Amenaza de agresión")
victimasexodelvictimarioconNNA_sel$victimario_conocido <- str_replace_all(victimasexodelvictimarioconNNA_sel$victimario_conocido,
                                                     "Desaparici n forzada","Desaparición forzada")
# victimasexodelvictimarioconNNA_sel$victimario_conocido <- str_replace_all(victimasexodelvictimarioconNNA_sel$victimario_conocido,
#                                                       "D?a de la semana en el d a","D?a de la semana en el d?a")

victimasexodelvictimarioconNNA_sel$prensa<- NULL
victimasexodelvictimarioconNNA_sel

victimasexodelvictimarioconNNA_sel$victimario_conocido = as.factor(victimasexodelvictimarioconNNA_sel$victimario_conocido)
victimasexodelvictimarioconNNA_sel$sexo_victima_2 <- droplevels(victimasexodelvictimarioconNNA_sel$sexo_victima_2)

victimasexodelvictimarioconNNA_graf <- apyramid::age_pyramid(data = victimasexodelvictimarioconNNA_sel,
                                                age_group = "victimario_conocido",
                                                split_by = "sexo_victima_2", show_midpoint=F)+
  # labels, titles, caption
  labs(title="Número de niñas, niños y adolescentes víctimas de delitos",
       subtitle = "discriminados por sexo y relación con el victimario",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasexodelvictimarioconNNA)} ({sum(is.na(victimasexodelvictimarioconNNA$sexo_victima_2) | is.na(victimasexodelvictimarioconNNA$victimario_conocido) |victimasexodelvictimarioconNNA$victimario_conocido == 'NA' | victimasexodelvictimarioconNNA$sexo_victima_2 == 'No informa')} casos perdidos por información sobre sexo faltante) en {prensa_victimasexodelvictimarioconNNA_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"),
       x = "Relación con el victimario",
       y = "Número de víctimas",
       fill = "")+
  scale_fill_manual(values = c("Femenino" = "darkred", # assign colors to values in the data
                               "Masculino" = "darkorange"))+


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

victimasexodelvictimarioconNNA_graf

