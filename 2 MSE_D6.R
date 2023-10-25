################################################################################
##                     Proyecto MRP - Left No One Behind                      ##
## Title:        Indicador de planificación familiar D6 - Uso de algún método ##
##               de planificación                                             ##
## Returns:      Estimación del error cuadrático medio del estimador Plugin   ##
##               por dominios de interés                                      ##
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

library(sae)
library(dplyr)
library(nlme)
library(TeachingSampling)
library(dtplyr)
library(data.table)
library(tidyverse)
select <- dplyr::select

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(18000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

###---------------------------------- ENDS ----------------------------------###

encuesta <- readRDS("1. ConciliarBases/Output/XencuestaD.rds")
encuesta$Divipola
###------------ Anexando los Senate Weights a la base de la ENDS ------------###

encuesta$Sweights <- nrow(encuesta) * encuesta$fexp/sum(encuesta$fexp)

###--------------------------------- CENSO ----------------------------------###

Censo <- readRDS("3. Modelos PlugIn/Output/D6/XcensoD6Divipola.rds")

###--------------------- Listado de los 1122 municipios ---------------------###

Divipola <- Censo %>% distinct(Divipola, .keep_all = F)

###--- Total de Municipios ---###

Div = unique(Censo$Divipola)

###------------ Volviendo el departamento un factor ------------###

encuesta$departamento = as.factor(as.vector(encuesta$departamento))

###--- Municipios en la encuesta ---###

Div2 = unique(encuesta$Divipola)

###------------------- Tamaño de muestra ENDS por municipio -----------------### 

n_d = encuesta %>% group_by(Divipola) %>% summarise(nd = n()) %>%
                   right_join(data.frame(Divipola = Div), by = "Divipola") %>%
                   mutate_all(~replace(.,is.na(.), 0)) %>% as.data.frame()

###--------------------- Total poblacional por municipio --------------------### 

N_d = Censo %>% lazy_dt() %>% group_by(Divipola) %>% summarise(Nd = n()) %>% 
      as.data.frame()

#------------------------------------------------------------------------------#
#---------------------- Cargue del modelo Plugin Saturado ---------------------#
#------------------------------------------------------------------------------#

load("3. Modelos PlugIn/Output/D6/PluginD6SW_Sat_Mun.RData")

###----------- Exportando los efectos fijos comunes a los dominios ----------###

betas = as.matrix(fixef(pluginregSW_sat))

###--- MSE ---###
#sd.u = 0.1298 
sd.u = pluginregSW_sat@theta

###------------ Construcción de la matriz censal sintética XBeta ------------###

matriz <- cbind.data.frame(Censo$Divipola, cbind(1, as.matrix(Censo %>%
                           select(rownames(betas)[-1]))) %*% betas)
colnames(matriz) <- c("Divipola","XB")

###----------- Lista donde se guardarán las iteraciones bootstrap -----------###

plugin.estrella = list()

###--- Número de iteraciones Bootstrap ---###

B = 4

###--- Ciclo de estimación Bootstrap ---##

for(b in 1:B){
  print(b)
  
  #----------------------------------------------------------------------------#
  #--- Paso 1: generar los efectos aleatorios para los municipios en la ENDS --#
  #----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Div2, ud = rnorm(length(Div2), 0, sd.u))
  
  #-- Creando vector de efectos aleatorios para todos los municipios del país -#
  
  ud = data.frame(Divipola = Div) %>% left_join(ud, by = "Divipola")
  
  #----- Si el municipio no fue encuestado tendrá un efecto aleatorio de 0 ----#
  
  ud$ud[is.na(ud$ud)] <- 0
  
  #----------------------------------------------------------------------------#
  #-- Paso 2: Generar pseudocenso y estimador del parámetro a partir de este --#
  #----------------------------------------------------------------------------#

  #---     Ubicación en la lista para almacenar el theta estimado en el     ---#
  #---     pseudocenso y el obtenido en la muestra seleccionada de este     ---#

  plugin.estrella[[b]] = cbind(theta.censo = numeric(nrow(ud)), 
                               theta.muestra = numeric(nrow(ud))) %>% 
                         data.frame() %>% mutate(Codmun = ud$Divipola)
  
  #- Censo con variable para generar la estimación de la probabilidad -#
  #-                de uso de algún método anticonceptivo             -#
  
  censoydi = cbind.data.frame(Censo$Divipola, NA, Censo[-c(111)])
  colnames(censoydi) <- c("Divipola", "Ydi", colnames(Censo[-c(111)]))
  
  #--- Para cada Divipola y cada individuo se generan probabilidades ---# 
  #---              de uso de algún método anticonceptivo            ---#
  
  for(i in 1:length(Div)){

    #--- Código del municipio ---#

    index = Div[i]
    
    #--- Total poblacional del municipio ---#
    
    N.d = N_d$Nd[i]
    
    #- Posición de matriz censal sintética XBeta que corresponde al municipio -#
    
    pos = which(matriz$Divipola == index)
    
    #-   Probabilidad de uso de algún método anticonceptivo para cada mujer   -# 
    #-                              del municipio                             -#
    
    theta.di = (exp(matriz[pos,2] + rep(ud[i,2], N.d))/
                  (1 + exp(matriz[pos,2] + rep(ud[i,2], N.d))))
    
    #- Posición en la base censal que corresponde al municipio -#
    
    pos2 = which(censoydi$Divipola == index)
    
    #- Generando la variable respuesta simulada a partir de la probabilidad -#
    #-   de uso de algún método anticonceptivo de cada mujer del municipio  -#
    
    censoydi[pos2, 2] = rbinom(N.d, 1, theta.di)
    
    #- Estimación de la probabilidad de uso de algún método anticonceptivo  -# 
    #-                global del municipio en el pseudocenso                -#
    
    plugin.estrella[[b]][i,1] = (1/N.d) * sum(censoydi[pos2,2])
  }
  
  #----------------------------------------------------------------------------#
  #-- Paso 3: Seleccionar muestra del pseudocenso y estimador del parámetro  --#
  #--         a partir de la pseudomuestra                                   --#
  #----------------------------------------------------------------------------#

  #- Muestra del pseudocenso con tamaño que coincide con el número de mujeres -#
  #-              seleccionada en la muestra municipios de la ENDS            -#
  
  muestra <- censoydi %>% left_join(n_d, by = "Divipola") %>%
             mutate(id_orig = 1:dim(censoydi)[1], Aleatorio = runif(dim(censoydi)[1])) %>% 
             group_by(Divipola) %>% arrange(Divipola, Aleatorio) %>% 
             mutate(id_efecto = row_number()) %>% 
             mutate(Seleccionado = ifelse(id_efecto <= nd, "Seleccionado", "")) %>% 
             filter(Seleccionado == "Seleccionado") %>% ungroup() %>% 
             arrange(id_orig) %>% select(names(censoydi)) %>% as.data.frame()
  
  #--------- Ajuste del modelo para la pseudo-muestra ---------#
  
  pluginbootstrap <- glmer(Ydi ~ ind_hacinamiento + hijos + unida + ocupada + 
                           inasistencia + alfabeta + departamento_5 + 
                           departamento_8 + departamento_11 + departamento_13 +
                           departamento_15 + departamento_17 + departamento_18 +
                           departamento_19 + departamento_20 + departamento_23 +
                           departamento_23 + departamento_25 + departamento_27 + 
                           departamento_41 + departamento_44 + departamento_47 +
                           departamento_50 + departamento_52 + departamento_54 + 
                           departamento_63 + departamento_66 + departamento_68 + 
                           departamento_70 + departamento_73 + departamento_76 + 
                           departamento_81 + departamento_85 + departamento_86 + 
                           departamento_88 + departamento_91 + departamento_94 + 
                           departamento_95 + departamento_97 + etnia_indigena +
                           etnia_gitano_raizal_palanquero + etnia_mulato_afrodescendiente + 
                           edad_13_14 + edad_15_19 + edad_20_24 + edad_25_29 + 
                           edad_30_34 + edad_34_39 + edad_40_44 + 
                           aguacocina_Aguatero + aguacocina_acueducto_veredal +
                           aguacocina_pozo_bomba + aguacocina_agua_lluvia +
                           aguacocina_acueducto_publico + aguacocina_carrotanque + 
                           aguacocina_agua_embotellada + aguacocina_pila_publica +
                           aguacocina_rio_quebrada + aguacocina_pozo_sin_bomba_aljibe +
                           area_rural + servsan_alcantarilla + servsan_letrina +
                           servsan_descarga_directa + servsan_pozo_septico + 
                           servsan_sin_conexion + matpiso_maderaburda_tabla + 
                           matpiso_alfombra + matpiso_marmol_pulida_parque +  
                           matpared_maderaburda_esterilla + matpared_guadua + 
                           matpared_prefabricado + matpared_ladrillo_concreto +
                           matpared_tapia_pisada + matpared_zinc_plastico +
                           matpiso_baldosa_vinilo + matpiso_cemento_gravilla +
                           electricidad_si + alcantarillado_si + internet_Si +
                           basura_si + gas_si + tipo_viv_casa + tasa_rural +
                           tipo_viv_departamento + MDM + Densidad.poblacional +
                           (1|Divipola), family = "binomial", data = muestra,
                           weights = Sweights)
  
  ###---    Exportando los efectos fijos comunes en el modelo bootstrap   ---###
  
  betasB = as.matrix(fixef(pluginbootstrap))
  
  ###---- Exportando los efectos aleatorios para cada uno de los dominios ---###
  
  udB =  data.frame(Divipola = rownames(ranef(pluginbootstrap)$Divipola), 
                    ud = ranef(pluginbootstrap)$Divipola[[1]])
  rownames(udB) <- NULL
  udB$Divipola <- as.numeric(udB$Divipola)
  
  #-- Creando vector de efectos aleatorios para todos los municipios del país -#
  
  udB = data.frame(Divipola = Div) %>% left_join(udB, by = "Divipola")
  
  #----- Si el municipio no fue encuestado tendrá un efecto aleatorio de 0 ----#
  
  udB$ud[is.na(udB$ud)] <- 0
  
  ##------------ Construcción de la matriz censal sintética XBeta ------------##
  
  matrizCenso <- cbind.data.frame(censoydi$Divipola, 
                                  cbind(1, as.matrix(censoydi %>% 
                                  select(rownames(betasB)[-1]))) %*% betasB)
  
  colnames(matrizCenso) <- c("Divipola","XB")
  
  censoydi$pluginB <- NA
  
  #--- Estimaciones boostrap para cada municipio ---#
  
  for(i in 1:length(Div)){
    
    index = which(censoydi$Divipola == Div[i])
    
    #-   Probabilidad de uso de algún método anticonceptivo para cada mujer   -# 
    #-                              del municipio                             -#
    
    censoydi$pluginB[index] = exp(matrizCenso$XB[index] + udB[i,2])/
                              (1 + exp(matrizCenso$XB[index] + udB[i,2]))
    
    #- Estimación de la probabilidad de uso de algún método anticonceptivo  -# 
    #-      global del municipio a partir de la muestra del pseudocenso     -#
    
    plugin.estrella[[b]][i,2] = mean(censoydi$pluginB[index])
  }
}

#------------------------------------------------------------------------------#
#--------------- Exportando salidas: Censo con estimación Plugin --------------#
#------------------------------------------------------------------------------#

saveRDS(plugin.estrella, file = "3. Modelos PlugIn/Output/D6/plugin_estrella_D6.rds")

#------------------------------------------------------------------------------#
#------------ Cargue de base de datos: Censo con estimación Plugin ------------#
#------------------------------------------------------------------------------#

plugin.estrella <- readRDS("3. Modelos PlugIn/Output/D6/plugin_estrella_D6.rds")

#------------------------------------------------------------------------------#
#------------------------------ Estimación del ECM ----------------------------#
#------------------------------------------------------------------------------#

#-- Promedio de las diferencias plugin pseudocenso - pseudomuestra bootstrap --# 

D6_mse <- plugin.estrella %>% map_df(~.x %>% data.frame() %>% 
                              mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
                              group_by(Codmun) %>% summarise(ecm = mean(dif2))

#------------------------------------------------------------------------------#
#--------------------- Exportando salidas: Estimación ECM ---------------------#
#------------------------------------------------------------------------------#

saveRDS(D6_mse,file = "3. Modelos PlugIn/Output/D6/EstimacionMSE_D6.rds")
