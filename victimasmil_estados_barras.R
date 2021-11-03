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
victimasmilestados <- victimasmil %>%
  select(estado, prensa, informacion_sociodem_1) %>%
  filter(informacion_sociodem_1 == "Si" | informacion_sociodem_1 == "No") %>%
  filter(!estado %in% c("Barinas"))

victimasmilestados

victimasmilestados_sel <- victimasmil %>%
  select(estado, prensa, informacion_sociodem_1) %>%
  filter(informacion_sociodem_1 == "Si" | informacion_sociodem_1 == "No") %>%
  filter(!estado %in% c("Barinas"))

victimasmilestados_sel

prensa_victimasmilestados_sel <- length(unique(victimasmilestados_sel[["prensa"]]))
prensa_victimasmilestados_sel

victimasmilestados_sel$prensa<- NULL
victimasmilestados_sel$informacion_sociodem_1<-NULL
victimasmilestados_sel

#corrigiendo nombres de estados
victimasmilestados_sel$estado <- str_replace_all(victimasmilestados_sel$estado,
                                                   "[^[:alnum:]]", " ")
victimasmilestados_sel$estado <- str_replace_all(victimasmilestados_sel$estado,
                                                   "T chira",
                                                   "Táchira")
victimasmilestados_sel$estado <- str_replace_all(victimasmilestados_sel$estado,
                                                   "Bol var",
                                                   "Bolívar")
victimasmilestados_sel$estado <- str_replace_all(victimasmilestados_sel$estado,
                                                   "Gu rico",
                                                   "Guárico")
victimasmilestados_sel$estado <- str_replace_all(victimasmilestados_sel$estado,
                                                   "Falc n",
                                                   "Falcón")
victimasmilestados_sel$estado <- str_replace_all(victimasmilestados_sel$estado,
                                                   "M rida",
                                                   "Mérida")


# Data transformation
porcent_grup_victimasmilestados <- victimasmilestados_sel %>%
  group_by(estado) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_victimasmilestados

#flextable(porcent_grup_victimasmilestados)

# Tabulando
tabla_grup_victimasmilestados <- victimasmilestados_sel %>%
  group_by(estado) %>% # Variable a ser transformada
  count() %>%
  ungroup() %>%
  mutate(perc = `freq` / sum(`freq`)) %>%
  arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         porcentaje = paste0(round(perc * 100,1),"%"))%>%
  adorn_totals(where = "row", fill = "100%")

tabla_grup_victimasmilestados

tabla_grup_victimasmilestados$tipo_delito <- NULL
tabla_grup_victimasmilestados$perc<- NULL
tabla_grup_victimasmilestados$label_pos<-NULL
tabla_grup_victimasmilestados

tabla_victimasmilestados <- tabla_grup_victimasmilestados
tabla_victimasmilestados

names(tabla_victimasmilestados)[names(tabla_victimasmilestados) ==
                                    "estado"] <- "Estado"
names(tabla_victimasmilestados)[names(tabla_victimasmilestados) ==
                                    "freq"] <- "Víctimas"
names(tabla_victimasmilestados)[names(tabla_victimasmilestados) ==
                                    "porcentaje"] <- "Porcentaje"
tabla_victimasmilestados


#graficando

victimasmilestados_barras <- ggplot(porcent_grup_victimasmilestados,
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
  scale_y_continuous( limits=c(0, 160),
                      breaks=seq(0,160,20))+
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
    caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nrow(victimasmilestados)} ({sum(is.na(victimasmilestados$estado)| victimasmilestados$estado == 'NA'|is.na(victimasmilestados$infodelito2)|victimasmilestados$infodelito2 == 'NA')} casos perdidos por información faltante) en {prensa_victimasmilestados_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de víctimas")

victimasmilestados_barras

ggsave("images/victimasmilestados_barras.png",width=8,height=5)
