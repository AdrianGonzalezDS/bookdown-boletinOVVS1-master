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
library(janitor)#requerida para usar adorn total
library(flextable)
library(gt)
#library(reshape2)#requerido para el paquete melt
Sys.setlocale("LC_TIME","Spanish_Spain.1252")
startdate <- as.Date(c("2021-01-01"))
enddate <- as.Date(c("2021-06-30"))

#Prepare datos
victimasdelhiestados <- victimasdelito %>%
  select(estado, prensa, informacion_sociodem_2, tipo_delito) %>%
  filter(tipo_delito == "Homicidio intencional" & informacion_sociodem_2 == "Si" |
           tipo_delito == "Homicidio intencional" & informacion_sociodem_2 == "No")

victimasdelhiestados

victimasdelhiestados_sel <- victimasdelito %>%
  select(estado, prensa, informacion_sociodem_2, tipo_delito) %>%
  filter(tipo_delito == "Homicidio intencional" & informacion_sociodem_2 == "Si" |
           tipo_delito == "Homicidio intencional" & informacion_sociodem_2 == "No")


victimasdelhiestados_sel

prensa_victimasdelhiestados_sel <- length(unique(victimasdelhiestados_sel[["prensa"]]))
prensa_victimasdelhiestados_sel

victimasdelhiestados_sel$prensa<- NULL
victimasdelhiestados_sel$informacion_sociodem_2<-NULL
victimasdelhiestados_sel

#corrigiendo nombres de estados
victimasdelhiestados_sel$estado <- str_replace_all(victimasdelhiestados_sel$estado,
                                      "[^[:alnum:]]", " ")
victimasdelhiestados_sel$estado <- str_replace_all(victimasdelhiestados_sel$estado,
                                      "T chira",
                                      "Táchira")
victimasdelhiestados_sel$estado <- str_replace_all(victimasdelhiestados_sel$estado,
                                      "Bol var",
                                      "Bolívar")
victimasdelhiestados_sel$estado <- str_replace_all(victimasdelhiestados_sel$estado,
                                      "Gu rico",
                                      "Guárico")
victimasdelhiestados_sel$estado <- str_replace_all(victimasdelhiestados_sel$estado,
                                      "Falc n",
                                      "Falcón")
victimasdelhiestados_sel$estado <- str_replace_all(victimasdelhiestados_sel$estado,
                                      "M rida",
                                      "Mérida")


# Data transformation
porcent_grup_victimasdelhiestados <- victimasdelhiestados_sel %>%
  group_by(estado) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))%>%
   adorn_totals(where = "row", fill = "100%")

porcent_grup_victimasdelhiestados

#flextable(porcent_grup_victimasdelhiestados)

# Tabulando
porcent_grup_victimasdelhiestados$tipo_delito <- NULL
porcent_grup_victimasdelhiestados$perc<- NULL
porcent_grup_victimasdelhiestados$label_pos<-NULL
porcent_grup_victimasdelhiestados

tabla_victimasdelhiestados <- porcent_grup_victimasdelhiestados
tabla_victimasdelhiestados

names(tabla_victimasdelhiestados)[names(tabla_victimasdelhiestados) ==
                         "estado"] <- "Estado"
names(tabla_victimasdelhiestados)[names(tabla_victimasdelhiestados) ==
                         "freq"] <- "Víctimas"
names(tabla_victimasdelhiestados)[names(tabla_victimasdelhiestados) ==
                                    "porcentaje"] <- "Porcentaje"
tabla_victimasdelhiestados


#graficando

victimasdelhiestados_barras <- ggplot(porcent_grup_victimasdelhiestados,
                                 aes(x = reorder(estado, freq), y = freq))+
  geom_col(aes(fill = estado), position = 'identity') +
  #geom_bar(aes(fill = organanismo_seguridad_1), position = "identity")+
  #geom_text(aes(label = porcentaje), position = position_fill(vjust = 1))+
  theme_classic()+ #scale_fill_brewer(palette = "GnBu")+coord_flip()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.justification = "left",
        axis.text = element_text(size= 10,color='black',
                                 angle=0, vjust = 0.5),
        legend.text = element_text(size = 7 ,
                                   color='black',
                                   angle=0,
                                   vjust = 0.5))+
  coord_flip()+
  scale_y_continuous( limits=c(0, 120),
                      breaks=seq(0,120,10))+
  geom_label(aes(x = estado, y = freq, label = porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(#title="Proporción y número de sucesos por delitos distintos a HI",
    #subtitle = "discriminados según el tipo de delito",
    caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasdelhiestados)} ({sum(is.na(victimasdelhiestados$estado)| victimasdelhiestados$estado == 'NA'|is.na(victimasdelhiestados$infodelito2)|victimasdelhiestados$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_victimasdelhiestados_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de víctimas")

victimasdelhiestados_barras

ggsave("images/victimasdelhiestados_barras.png",width=8,height=5)
