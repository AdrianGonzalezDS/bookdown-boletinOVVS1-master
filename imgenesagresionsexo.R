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
library(cowplot)
library(magick)

p1 <- ggdraw() + draw_image('images/agresionsexo.jpg', scale = 0.9)
p2 <- ggdraw() + draw_image('images/agresionsexo.jpg', scale = 0.9)

#plot_grid(p1, p2)
