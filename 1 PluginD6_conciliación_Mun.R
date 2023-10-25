################################################################################
##                     Proyecto MRP - Left No One Behind                      ##
## Title:        Indicador de planificación familiar D6 - Uso de algún método ##
##               de planificación                                             ##
## Returns:      Estimación Plugin por dominios de interés                    ##
## Author:       Juliana Guerrero & Andres Gutierrez			      ##
## Institution:  CEPAL - UNFPA                                                       ##
## Date:         2021                                                         ##
################################################################################

###--- Cleaning R environment ---###

rm(list = ls())
gc()

#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)
library(sae)
library(lme4)
library(data.table)
library(haven)
library(formula.tools)
library(remotes)
library(StatisticalModels)
select <- dplyr::select

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(180000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

options(encoding = "UTF-8", scipen = 999)

###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS("1. ConciliarBases/Output/XencuestaD.rds")

###------------ Anexando los Senate Weights a la base de la ENDS ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)

###--------------------------------- CENSO ----------------------------------###

Xcenso <- readRDS("1. ConciliarBases/Output/XcensoD.rds")

###-------------------- Número de municipios en la ENDS ---------------------###

sum(unique(Xencuesta$Divipola) %in% unique(Xcenso$Divipola))

###--------------------- Listado de los 1122 municipios ---------------------###

Divipola <- Xcenso %>% distinct(Divipola, .keep_all = F)

################################################################################
                       ### Ajuste del modelo Plugin ###
################################################################################

###----------------------------- Modelo saturado ----------------------------###

pluginregSW_sat <- glmer(usametodo ~ ind_hacinamiento + hijos + unida +  
                         ocupada + inasistencia + alfabeta + departamento_5 +  
                         departamento_8 + departamento_11 + departamento_13 +
                         departamento_15 + departamento_17 + departamento_18 +
                         departamento_19 + departamento_20 + departamento_23 + 
                         departamento_25 + departamento_27 + departamento_41 +
                         departamento_44 + departamento_47 + departamento_50 +
                         departamento_52 + departamento_54 + departamento_63 +
                         departamento_66 + departamento_68 + departamento_70 +
                         departamento_73 + departamento_76 + departamento_81 +
                         departamento_85 + departamento_86 + departamento_88 + 
                         departamento_91 + departamento_94 + departamento_95 +
                         departamento_97 + etnia_gitano_raizal_palanquero + 
                         etnia_indigena + etnia_mulato_afrodescendiente +
                         edad_13_14 + edad_15_19 + edad_20_24 + edad_25_29 + 
                         edad_30_34 + edad_34_39 + edad_40_44 + area_rural +
                         aguacocina_acueducto_veredal + aguacocina_agua_lluvia +
                         aguacocina_acueducto_publico + aguacocina_Aguatero + 
                         aguacocina_agua_embotellada + aguacocina_pila_publica +  
                         aguacocina_carrotanque + aguacocina_rio_quebrada +
                         aguacocina_pozo_bomba + aguacocina_pozo_sin_bomba_aljibe +
                         servsan_alcantarilla + servsan_descarga_directa + 
                         servsan_letrina  + servsan_pozo_septico + 
                         servsan_sin_conexion + electricidad_si + 
                         matpiso_alfombra + matpiso_baldosa_vinilo + 
                         matpared_guadua + matpiso_cemento_gravilla + 
                         matpiso_maderaburda_tabla + matpared_prefabricado +
                         matpared_ladrillo_concreto + matpared_tapia_pisada +
                         matpiso_marmol_pulida_parque + matpared_zinc_plastico +
                         matpared_maderaburda_esterilla + alcantarillado_si +  
                         internet_Si + basura_si + gas_si + tipo_viv_casa +
                         tipo_viv_departamento + Densidad.poblacional + 
                         tasa_rural + MDM + (1|Divipola), data = Xencuesta,
                         family = "binomial", weights = Sweights)

###------------------------------ Modelo simple -----------------------------###

pluginregSW_simple <- glmer(usametodo ~ hijos + unida + ocupada + edad_13_14 +
                            edad_15_19 + edad_20_24 + edad_25_29 + edad_30_34 +                   
                            edad_34_39 + edad_40_44 + Densidad.poblacional + 
                            tasa_rural + MDM + (1|Divipola), weights = Sweights,  
                            family = "binomial", data = Xencuesta)

#------------------------------------------------------------------------------#
#----------------- Exportando salidas: Modelo Plugin ajustado -----------------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

save(pluginregSW_sat, file = "3. Modelos PlugIn/Output/D6/PluginD6SW_Sat_Mun.RData")

###--- Modelo simple ---###

save(pluginregSW_simple, file = "3. Modelos PlugIn/Output/D6/PluginD6SW_Simple_Mun.RData")

################################################################################
#----------------------- Estimación Plugin: indicador D6 ----------------------#
################################################################################

#------------------------------------------------------------------------------#
#--------------------------- Cargue del modelo Plugin -------------------------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

load("3. Modelos PlugIn/Output/D6/PluginD6SW_Sat_Mun.RData")

###--- Modelo simple ---###

load("3. Modelos PlugIn/Output/D6/PluginD6SW_Simple_Mun.RData")

#------------------------------------------------------------------------------#
#------------- Exportando los efectos fijos comunes a los dominios ------------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

betasSW_sat = as.matrix(fixef(pluginregSW_sat))
betasSW_sat

###--- Modelo simple ---###

betasSW_simple = as.matrix(fixef(pluginregSW_simple))
betasSW_simple

#------------------------------------------------------------------------------#
#------- Exportando los efectos aleatorios para cada uno de los dominios ------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

udSW_sat = cbind.data.frame(indice = rownames(ranef(pluginregSW_sat)$Divipola),
                            ud = ranef(pluginregSW_sat)$Divipola[[1]])

###--- Modelo Simple ---###

udSW_simple = cbind.data.frame(indice = rownames(ranef(pluginregSW_simple)$Divipola),
                               ud = ranef(pluginregSW_simple)$Divipola[[1]])

#------------------------------------------------------------------------------#
#-------------- En esta sección se emplea solo el modelo saturado -------------#
#------------------------------------------------------------------------------#

###------------ Construcción de la matriz censal sintética XBeta ------------###

mat_CensoSW_sat <- cbind.data.frame(Xcenso$Divipola,cbind(1,as.matrix(Xcenso %>%
                                    select(rownames(betasSW_sat)[-1]))) %*% 
                                    betasSW_sat)
colnames(mat_CensoSW_sat) <- c("Divipola","XB")
head(mat_CensoSW_sat)

#- Creando el vector nulo para ser reemplazado en el ciclo de estimación Plugin -#

Xcenso$pluginD6 = numeric(13607238)

#-- Códigos de los municipios en el censo para el ciclo de estimación Plugin --#

Div = unique(Xcenso$Divipola)

###---------- Ciclo de estimación plugin por individuo en el censo ----------###
###----------   En esta sección se emplea solo el modelo saturado  ----------###

for(i in 1:length(Div)){
  
  print(i)

  ### Identificación del dominio ###
  
  index = which(Xcenso$Divipola == Div[i])
  
  ###--- Estimación en las áreas muestreadas por la ENDS ---###
  
  if(Div[i] %in% udSW_sat$indice){
   
    ###--- Seleccionando el efecto aleatorio del dominio ---###
    
    u = udSW_sat$ud[which(as.character(udSW_sat$indice) == Div[i])]
    
    ###---Probabilidad de uso de algún método anticonceptivo ---###
    
    Xcenso$pluginD6[index] = exp(mat_CensoSW_sat$XB[index] + u)/
                                 (1 + exp(mat_CensoSW_sat$XB[index] + u))
    
   print(paste(Div[i], "muestreada"))
    
  ###--- Estimación en áreas no muestreadas por la ENDS ---###
    
  }else{
    
    ###---Probabilidad de uso de algún método anticonceptivo ---###
    
    Xcenso$pluginD6[index] = exp(mat_CensoSW_sat$XB[index])/
                                 (1 + exp(mat_CensoSW_sat$XB[index]))
    
    print(paste(Div[i],"No muestreada"))
  }
}

#------------------------------------------------------------------------------#
#--------------- Exportando salidas: Censo con estimación Plugin --------------#
#------------------------------------------------------------------------------#

saveRDS(Xcenso, file = "3. Modelos PlugIn/Output/D6/XcensoD6Divipola.rds")

#------------------------------------------------------------------------------#
#------------ Cargue de base de datos: Censo con estimación Plugin ------------#
#------------------------------------------------------------------------------#

Xcenso <- readRDS("3. Modelos PlugIn/Output/D6/XcensoD6Divipola.rds")

###------------ Proporción de uso de algún método anticonceptivo ------------###
###--------------------- Estimación plugin departamental --------------------### 

deptoD6 <- Xcenso %>% group_by(departamento) %>% summarise(D6 = mean(pluginD6))
deptoD6

###----------------------- Estimación plugin municipal ----------------------###

municipioD6 <- Xcenso %>% group_by(Divipola) %>% summarise(D6 = mean(pluginD6))
municipioD6

###----------------------- Estimación plugin por etnia ----------------------###

###-------- Recodificando la variable etnia según los niveles de MrP ---------##

Xcenso$Etnia <- ifelse(Xcenso$etnia %in% c("indigena"), "Indigena",
                 ifelse(Xcenso$etnia %in% c("gitano_raizal_palanquero",
                        "mulato_afrodescendiente"), "NAM", "NoConsidera"))

etniaD6 <- Xcenso %>% group_by(Etnia) %>% summarise(D6 = mean(pluginD6))
etniaD6

###----------------- Estimación plugin: Departamento X etnia ----------------###

depto_etniaD6 <- Xcenso %>% group_by(departamento, Etnia) %>% 
                            summarise(D6 = mean(pluginD6))
depto_etniaD6

###------------------- Estimación plugin: Municipio X etnia -----------------###

municipio_etniaD6 <- Xcenso %>% group_by(Divipola, Etnia) %>% 
                                summarise(D6 = mean(pluginD6))

municipio_etniaD6

################################################################################
###                                  Benchmark                                 #
################################################################################

###--------------------------------------------------------------------------###
###    Proporción de uso de métodos anticonceptivos reportado por la ENDS    ###
###--------------------------------------------------------------------------###

medias <- numeric(3)

###--- Nacional ---###

medias[1] <- Xencuesta %>% summarise(p = weighted.mean(usametodo, fexp)) %>% 
             as.numeric()

###--- Urbana ---###

medias[2] <- Xencuesta %>% filter(area_urbano == 1) %>% 
             summarise(p = weighted.mean(usametodo, fexp)) %>% as.numeric()

###--- Rural ---###

medias[3] <- Xencuesta %>% filter(area_rural == 1) %>% 
             summarise(p = weighted.mean(usametodo,fexp)) %>% as.numeric()

###--- Departamento ---###

# medias[4:36] <- encuesta %>% group_by(Departamento) %>% 
#   summarise(p = weighted.mean(usametodo,FEXM)) %>% select(p) %>% unlist(use.names = FALSE)
# unique(encuesta$departamento)

###-------------------------- Matriz de calibración -------------------------###

MatrizCalibrada <- cbind(Xcenso %>% transmute(unos = 1, 
                                    area.urbana = ifelse(area_urbano == "1", 1, 0), 
                                    area.rural = 1 - area.urbana)) 

###------------------------ Función de pesos Benchmark ----------------------###

Benchmark <- function(CensoY){
  library(sampling)
  MatrizCalibrada2 = MatrizCalibrada
  for(i in 1:3){
    MatrizCalibrada2[,i] <- MatrizCalibrada[,i] * (CensoY - medias[i])
  }
  a = calib(Xs = MatrizCalibrada2, d = rep(1, dim(MatrizCalibrada)[1]), 
            total = rep(0, 3), method = c("linear")) 
  return(a)
}

###--- Anexando pesos Benchmark al censo ---###

Xcenso$pesos_D6 <- Benchmark(Xcenso$pluginD6)
summary(Xcenso$pesos_D6)

###---------------------- Estimación benchmark Nacional ---------------------###

D6_Nal <- Xcenso %>% summarise(D6_2020 = mean(pluginD6),
                              D6B_2020 = weighted.mean(pluginD6, pesos_D6))
D6_Nal

###-------------------- Estimación benchmark Departamental ------------------###

D6_depto <- Xcenso %>% group_by(departamento) %>%
                       summarise(D6_2020 = mean(pluginD6), 
                                 D6B_2020 = weighted.mean(pluginD6, pesos_D6))

###-------------------- Estimación benchmark Municipal ------------------###

D6_Mun <- Xcenso %>% group_by(Divipola) %>%
                       summarise(D6_2020 = mean(pluginD6), 
                       D6B_2020 = weighted.mean(pluginD6, pesos_D6))

###-------------------- Estimación benchmark por etnia ------------------###

D6_dominio <- Xcenso %>% group_by(Etnia) %>% 
                         summarise(D6_2020 = mean(pluginD6),
                                   D6B_2020 = weighted.mean(pluginD6, pesos_D6))

#----------------- Exporting Plugin estimates for post-stratum ----------------#

###--- Etnia ---###

saveRDS(D6_dominio, file = "3. Modelos PlugIn/Output/D6/D6_plugin_dominio.rds")

###--- Municipio ---###

saveRDS(D6_Mun, file = "3. Modelos PlugIn/Output/D6/D6_plugin_Mun.rds")

###--- Departamento ---###

saveRDS(D6_depto, file = "3. Modelos PlugIn/Output/D6/D6_plugin_Departamento.rds")
