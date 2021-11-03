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
sucesosvicestados <- sucesos %>%
  select(estado, prensa, numero_victimas_1, numero_victimas_2) %>%
  filter(!estado %in% c("Barinas", "Nueva Esparta"))

sucesosvicestados

v1_99 <- length(which(sucesosvicestados$numero_victimas_1 == 99))
v1_99

v2_99 <- length(which(sucesosvicestados$numero_victimas_2 == 99))
v2_99

sucesosvicestados_sel <- sucesos %>%
  select(estado, prensa, numero_victimas_1, numero_victimas_2) %>%
  filter(!estado %in% c("Barinas", "Nueva Esparta"))

sucesosvicestados_sel


prensa_sucesosvicestados_sel <- length(unique(sucesosvicestados_sel[["prensa"]]))
prensa_sucesosvicestados_sel

sucesosvicestados_sel$prensa<- NULL
#sucesosvicestados_sel$informacion_sociodem_1<-NULL
sucesosvicestados_sel

sucesosvicestados_sel$numero_victimas_1[sucesosvicestados_sel$numero_victimas_1 == 98] <- 10
sucesosvicestados_sel$numero_victimas_2[sucesosvicestados_sel$numero_victimas_2 == 98] <- 10


#corrigiendo nombres de estados
sucesosvicestados_sel$estado <- str_replace_all(sucesosvicestados_sel$estado,
                                                 "[^[:alnum:]]", " ")
sucesosvicestados_sel$estado <- str_replace_all(sucesosvicestados_sel$estado,
                                                 "T chira",
                                                 "Táchira")
sucesosvicestados_sel$estado <- str_replace_all(sucesosvicestados_sel$estado,
                                                 "Bol var",
                                                 "Bolívar")
sucesosvicestados_sel$estado <- str_replace_all(sucesosvicestados_sel$estado,
                                                 "Gu rico",
                                                 "Guárico")
sucesosvicestados_sel$estado <- str_replace_all(sucesosvicestados_sel$estado,
                                                 "Falc n",
                                                 "Falcón")
sucesosvicestados_sel$estado <- str_replace_all(sucesosvicestados_sel$estado,
                                                 "M rida",
                                                 "Mérida")
#elongando la data frame
sucesosvicestados_sel_long <- sucesosvicestados_sel %>%
  gather(numero_victimas_1,numero_victimas_2, key = "Perpetrador", value = "nroVictimas")

sucesosvicestados_sel_long$Perpetrador <- NULL

sucesosvicestados_sel_long_no_NA <- sucesosvicestados_sel_long %>%
  #select(estado, nroVictimas) %>%
  filter(!nroVictimas %in% c(NA, 'NA',99))
sucesosvicestados_sel_long_no_NA


# Data transformation
grup_sucesosvicestados <- sucesosvicestados_sel_long_no_NA %>%
  group_by(estado) %>% # Variable a ser transformada
   count() %>%
   ungroup() %>%
   mutate(Víctimas = `freq` * `nroVictimas`) #%>%
  # arrange(desc(`freq`)) %>% #solo organiza en orden alfabetico
  # mutate(label_pos = cumsum(perc) - perc / 2,
  #        porcentaje = paste0(round(perc * 100,1),"%"))

grup_sucesosvicestados$nroVictimas <- NULL
grup_sucesosvicestados$freq <- NULL
grup_sucesosvicestados

aggregate_sucesosvicestados <-aggregate(grup_sucesosvicestados$Víctimas,
                                        by=list(grup_sucesosvicestados$estado), FUN=sum)
aggregate_sucesosvicestados
names(aggregate_sucesosvicestados)[names(aggregate_sucesosvicestados) == "Group.1"] <- "Estado"
names(aggregate_sucesosvicestados)[names(aggregate_sucesosvicestados) == "x"] <- "Víctimas"
aggregate_sucesosvicestados

porcent_grup_sucesosvicestados <- aggregate_sucesosvicestados %>%
  mutate(perc = `Víctimas` / sum(`Víctimas`)) %>%
  arrange(desc(`Víctimas`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         Porcentaje = paste0(round(perc * 100,1),"%"))

porcent_grup_sucesosvicestados

# Tabulando

tabla_grup_sucesosvicestados <- aggregate_sucesosvicestados %>%
  mutate(perc = `Víctimas` / sum(`Víctimas`)) %>%
  arrange(desc(`Víctimas`)) %>% #solo organiza en orden alfabetico
  mutate(label_pos = cumsum(perc) - perc / 2,
         Porcentaje = paste0(round(perc * 100,1),"%"))%>%
  adorn_totals(where = "row", fill = "100%")

tabla_grup_sucesosvicestados






tabla_grup_sucesosvicestados$tipo_delito <- NULL
tabla_grup_sucesosvicestados$perc<- NULL
tabla_grup_sucesosvicestados$label_pos<-NULL
tabla_grup_sucesosvicestados

tabla_sucesosvicestados <- tabla_grup_sucesosvicestados
tabla_sucesosvicestados

names(tabla_sucesosvicestados)[names(tabla_sucesosvicestados) ==
                                  "estado"] <- "Estado"
names(tabla_sucesosvicestados)[names(tabla_sucesosvicestados) ==
                                  "freq"] <- "Víctimas"
names(tabla_sucesosvicestados)[names(tabla_sucesosvicestados) ==
                                  "porcentaje"] <- "Porcentaje"
tabla_sucesosvicestados

nvictimas <- tabla_sucesosvicestados[16, 2]
nvictimas

#graficando

sucesosvicestados_barras <- ggplot(porcent_grup_sucesosvicestados,
                                    aes(x = reorder(Estado, Víctimas), y = Víctimas))+
  geom_col(aes(fill = Estado), position = 'identity') +
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
  scale_y_continuous( limits=c(0, 680),
                      breaks=seq(0,680,40))+
  geom_label(aes(x = Estado, y = Víctimas, label = Porcentaje),
             hjust = -0.01,
             vjust = 0.25,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 3)+
  labs(#title="Proporción y número de sucesos por delitos distintos a HI",
    #subtitle = "discriminados según el tipo de delito",
    caption = stringr::str_glue("Fuente: Observatorio de prensa OVV  \nn = {nvictimas} ({sum(v1_99,v2_99)} casos perdidos por información faltante) en {prensa_sucesosvicestados_sel} medios de prensa consultados \nPeríodo de recolección de información: {format(startdate, '%d %b')}-{format(enddate, '%d %b %Y')}"))+
  xlab("") +
  ylab("Número de víctimas")

sucesosvicestados_barras

ggsave("images/sucesosvicestados_barras.png",width=8,height=5)
