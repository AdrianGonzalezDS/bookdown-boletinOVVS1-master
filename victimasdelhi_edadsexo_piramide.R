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
victimasdelhi <- victimasdelito %>%
  select(edad__victima_2, sexo_victima_2,prensa, informacion_sociodem_2, tipo_delito) %>%
  filter(tipo_delito == "Homicidio intencional" & informacion_sociodem_2 == "Si" |
           tipo_delito == "Homicidio intencional" & informacion_sociodem_2 == "No")

victimasdelhi

victimasdelhi_sel <- victimasdelito %>%
  select(edad__victima_2, sexo_victima_2, prensa, informacion_sociodem_2, tipo_delito) %>%
  filter(!edad__victima_2 %in% c(99, "No informa", NA, "NA") &
           !sexo_victima_2 %in% c("No informa", NA, "NA") & tipo_delito == "Homicidio intencional" & informacion_sociodem_2 == "Si" |
           tipo_delito == "Homicidio intencional" & informacion_sociodem_2 == "No")

victimasdelhi_sel
# victimasdelhi_sel$Edad = as.numeric(victimasdelhi_sel$Edad)
# victimasdelhi_sel

prensa_victimasdelhi_sel <- length(unique(victimasdelhi_sel[["prensa"]]))
prensa_victimasdelhi_sel

v98<-print(length(which(victimasdelhi_sel$edad__victima_2==98)))
v98

#graficando
victimasdelhi_piramide <- ggplot(data=victimasdelhi_sel,aes(x=cut(edad__victima_2,
                                                  breaks=c(-1,seq(0,100,5))),
                                            fill=sexo_victima_2)) +
  geom_bar(data=subset(victimasdelhi_sel,sexo_victima_2=="Femenino")) +
  geom_bar(data=subset(victimasdelhi_sel,sexo_victima_2=="Masculino"),aes(y=..count..*(-1))) +
  scale_x_discrete(labels=c(paste0(seq(0,91,5),"-",seq(4,100,5))), drop=T) +
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
  labs(#title="Muertes por homicidio intencional",
       #subtitle = "discriminados por edad y sexo de la víctima",
       caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasdelhi)} ({sum(is.na(victimasdelhi$sexo_victima_2) | is.na(victimasdelhi$edad__victima_2) |victimasdelhi$edad__victima_2 == 99 | victimasdelhi$sexo_victima_2 == 'No informa')} casos perdidos por edad y sexo faltante) en {prensa_victimasdelhi_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))

victimasdelhi_piramide

ggsave("images/victimasdelhi_piramide.png",width=8,height=5)
