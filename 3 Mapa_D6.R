################################################################################
##                     Proyecto MRP - Left No One Behind                      ##
## Title:        Indicador de planificación familiar D6 - Uso de algún método ##
##               de planificación                                             ##
## Returns:      Maps for proportion of women using planning methods by       ##
##               plugin method                                                ##
## Autor: Sebastian Oviedo &Juliana Guerrero                                  ##
## Revisor: Felipe Molina Jaque                                               ##
## Revisor: Andrés Gutiérrez                                                  ##
## Institution:  CEPAL UNFPA                                                  ##
## Date:         2021                                                         ##
################################################################################

###--- Cleaning R environment ---###

rm(list = ls())
gc()

#################
### Libraries ###
#################

library(dplyr)
library(forcats)
library(tidyr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(scales)
library(ggalt)
library(gridExtra)
library(scales)
library(patchwork)
library(sp)
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(rgdal)

#------------------- Loading MrP estimates for post-stratum -------------------#

D6_plugin_Mun <- readRDS("E:/Juliana/SAE_repo/Colombia-UNFPA/3. Modelos Plugin/Output/D6/D6_plugin_Mun.rds")

d6 <- D6_plugin_Mun[,c(1,3)]
head(d6)
saveRDS(d6,"E:/Juliana/SAE_repo/salidas_mapas/d6_municipios.rds")

  write.csv(d6,"E:/Juliana/SAE_repo/salidas_mapas/d6_municipios.csv",row.names = F)



#------------------ Loading shape: Municipalities of Colombia -----------------#

col <- readOGR("3. Modelos Plugin/Input/mpio", "MGN_MPIO_POLITICO")

#----------------- Creando el Divipola para el Shape Municipal ----------------#

col@data$Divipola <- as.numeric(col@data$MPIO_CDPMP)

#---------- Anexando la información de la estimación plugin al shape ----------#

Colombia.I = col %>% merge(D6_plugin_Mun, by = "Divipola") 

#----------------------------------- Paleta -----------------------------------#

brks <- c(0, .15, .3, .4, .5, .6, .7, .8, 1)
paleta <- "RdYlGn"

#------------------------------- Mapa municipal -------------------------------#

windows()
indicador.D6B <- tm_shape(Colombia.I) + tm_polygons("D6B_2020", breaks = brks, 
                                        title = "Indicador D6B", palette = paleta)

#------------------------------ Guardando el mapa -----------------------------#

tmap_save(indicador.D6B, "3. Modelos Plugin/Maps/D6_Plugin_Mun.pdf", 
          width = 2920, asp = 0, height = 3080)

