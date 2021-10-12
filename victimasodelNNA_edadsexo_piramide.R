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
#library(reshape2)#requerido para el paquete melt
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

#Prepare datos
victimasodelNNA <- victimasdelito %>%
  select(edad__victima_2, sexo_victima_2, prensa, informacion_sociodem_2, tipo_delito) %>%
  filter(!edad__victima_2 %in% c(18:100))

victimasodelNNA

victimasodelNNA_sel <- victimasdelito %>%
  select(edad__victima_2, sexo_victima_2, prensa, informacion_sociodem_2, tipo_delito) %>%
  filter(!edad__victima_2 %in% c(99, "No informa", NA, "NA",18:100) &
           !sexo_victima_2 %in% c("No informa", NA, "NA"))

victimasodelNNA_sel
# victimasodelNNA_sel$Edad = as.numeric(victimasodelNNA_sel$Edad)
# victimasodelNNA_sel

prensa_victimasodelNNA_sel <- length(unique(victimasodelNNA_sel[["prensa"]]))
prensa_victimasodelNNA_sel


#graficando
victimasodelNNA_piramide <- ggplot(data=victimasodelNNA_sel,aes(x=cut(edad__victima_2,
                                                                breaks=c(-1,seq(0,100,5))),
                                                          fill=sexo_victima_2)) +
  geom_bar(data=subset(victimasodelNNA_sel,sexo_victima_2=="Femenino")) +
  geom_bar(data=subset(victimasodelNNA_sel,sexo_victima_2=="Masculino"),aes(y=..count..*(-1))) +
  scale_x_discrete(labels=c("< 1",paste0(seq(1,91,5),"-",seq(5,100,5))), drop=T) +
  scale_y_continuous(breaks=seq(-70,70,10),labels=abs(seq(-70,70,10)))+
  xlab("Edad (años)") + ylab("Número de víctimas") +
  coord_flip()+
  #bbc_style()+
  theme_classic( )+
  #labs(x = "Edad (a?os)", y = "N?mero de v?ctimas", size = 2)+
  scale_fill_manual(values = c("red3","dodgerblue4"))+
  theme(strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 9 ,color='black',
                                   angle=0,vjust = 0.5),
        #axis.title.y = element_text(color='black', angle=90, vjust = 0.5),
        #axis.title.x = element_text(size= 7,color='black', angle=0, vjust = 0.5),
        axis.text = element_text(size= 11,color='black', angle=0, vjust = 0.5),
        strip.text = element_blank())+
  labs(title="Número niñas, niños y adolescentes víctima de delitos",
       subtitle = "discriminados por edad (0-17) y sexo",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasodelNNA)} ({sum(is.na(victimasodelNNA$sexo_victima_2) | is.na(victimasodelNNA$edad__victima_2) |victimasodelNNA$edad__victima_2 == 99 | victimasodelNNA$sexo_victima_2 == 'No informa')} casos perdidos por edad y sexo faltante) en {prensa_victimasodelNNA_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))

victimasodelNNA_piramide


