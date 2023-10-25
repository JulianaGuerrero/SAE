#################################################################
## Homologar datos de las 2 fuentes                            ##
##  CNPV2018 Y ENDS 2015                                       ##
## Autor: Sebastian Oviedo &Juliana Guerrero                   ##
## Revisor: Felipe Molina Jaque                                ##
## Revisor: Andrés Gutiérrez                                   ##
## Creacion: 02/02/2021                                        ##
#################################################################

#################################################################
## Borrado de datos y encoding                                 ##
#################################################################

rm(list=ls())
options(encoding = "UTF-8", scipen=999)
memory.limit(180000000)

#################################################################
## Paqºuetes                                                    ##
#################################################################
library(tidyverse)
library(haven)
library(data.table)

#################################################################
## Funciones                                                   ##
#################################################################

#################################################################
## Paths                                                       ##
#################################################################

# !!!!!!!!!!!
# Ejecutar una sola vez y luego comentar
# data_COIR71DT = read_dta(file = "1. ConciliarBases/Input/COIR71FL.DTA")
# saveRDS(data_COIR71DT, "1. ConciliarBases/Output/COIR71FL.rds")
# 
# data_COPR71DT = read_dta(file = "1. ConciliarBases/Input/COPR71FL.DTA")
# saveRDS(data_COPR71DT, "1. ConciliarBases/Output/COPR71FL.rds")

data_COIR71DT <- readRDS("1. ConciliarBases/Output/COIR71FL.rds")
data_COPR71DT <- readRDS("1. ConciliarBases/output/COPR71FL.rds")



#table(data_COIR71DT$s109)
##Variables identificadas en la encuesta
names_var = c("hhid","hvidx","caseid","V012", "V102", "HV106",	"V107", "V113", "V116", "V119", "V127", "V128",
              "HV005", # Household sample weigth
              "V005",# Women individal sample weight
              "v131", "S107", "SH79L", "HV216", "HV242", "v201", "V202", "V204", "V203","sh79l","SH85",
              "V205", "V207", "V206",	"V501", "SH31",	"s109",	"SH53A", "SH53B",	"SH53C","HV121",
              "SH53D",	"SH53E", "SH53F", "SH53G", "SH53H",	"SH53I",	"SH54","HV101","HV202",
              "SH55",	"SH56A", "SH56B", "SH56C", "SH56B", "SH28",	"V150", "SH68E", "SH68B", "SH68AB",	"SH68D",
              "V531","V528", "V536","V624","V625","V632","V626","V761","V767A","V820","V301","V302","V312","V313","s607y_1",
              "V375A","V767A", "V625A", "V626A")
##Nombres a buscar
names_var = tolower(names_var)

## Variables para usar en las base de Individual Record (IR) y Person Record (PR)
names.use.IR = names(data_COIR71DT)[(names(data_COIR71DT) %in% names_var)]
names.use.PR = names(data_COPR71DT)[(names(data_COPR71DT) %in% names_var)]

#######################################################
## Arreglo base Individual Record (IR)              ###
## Buscar las categorias en el diccionario          ###  
#######################################################
data_in = data_COIR71DT %>% as.data.table() %>% .[,..names.use.IR]

## Age at first union
data_in = data_in %>% .[,age_first_union := ifelse(s607y_1 == 9998 | is.na(s607y_1) == T, NA, v012 - 2015 + s607y_1),] %>% 
  .[,age_first_union := cut(age_first_union, breaks = c(-Inf,seq(4,94,5),Inf) , labels = F),]

## Age at first sex - imputed
data_in = data_in %>% .[,age_first_sex := v531,] %>% 
  .[,age_first_sex := cut(age_first_sex, breaks = c(-Inf,seq(4,94,5),Inf) , labels = F),]

## ATENCIÓN: [,age_first_union := v012 - (2015 - S607Y),] 
## La variable S607Y no existe, se reemplaza por la s607y_1
## También existe la S607Y_2, la S607Y_3, la S607Y_4, etc.
## Age at first union
data_in = data_in %>% .[,age_first_union := v012 - (2015 - s607y_1),] %>% 
  .[,age_first_union := cut(age_first_union, breaks = c(-Inf,seq(4,94,5),Inf) , labels = F),]

## Time since last sex - days
data_in = data_in %>% .[,time_last_sex := v528,]

##  Recent sexual activity
data_in = data_in %>% .[,recent_sexual_activity := v536,]

## unmet need for contraception (definition 2)
data_in = data_in %>% .[,unmet_need_2 := v626,]

## unmet need for contraception (definition 3)
data_in = data_in %>% .[,unmet_need_3 := v626a,]

## Exposure to need for contraception (definition 3)
data_in = data_in %>% .[,exposure_3 := v625a,]

## Exposure (definition 2)
data_in = data_in %>% .[,exposure_2 := v625,]

## Decision maker for using contraception
data_in = data_in %>% .[,decision_using_contra := v632,]

## Condom used during last sex with most recent partner
data_in = data_in %>% .[,condom_used_last := v761,]

## Relationship with most recent sex partner
data_in = data_in %>% .[,relations_most_recent := v767a,]

##Condom used at first sex
data_in = data_in %>% .[,condom_usedat_first := v820,]

##Knowledge of any method
data_in = data_in %>% .[,knows_methods := v301,]

## Ever use of any method
data_in = data_in %>% .[,ever_used_any := v302,]

## Current contraceptive method
data_in = data_in %>% .[,current_method := v312,]

## Current contraceptive method
data_in = data_in %>% .[,current_method_grou := v313,]

## Main reason not using a method
data_in = data_in %>% .[,reason_not_use := v375a,]




## Pesos - Women individal sample weight
## Variable de pesos
data_in = data_in %>% .[,PESOS_WISW := v005,]

##CLASE
data_in = data_in %>% .[,UA_CLASE_C := v102 ,]



##Agua alimentos
data_in = data_in %>% .[,H_AGUA_COCIN_C := ifelse(v113 %in% c(11), 1,
                                                  ifelse(v113 %in% c(12), 22,
                                                         ifelse(v113 %in% c(13), 8,
                                                                ifelse(v113 %in% c(21), 4,
                                                                       ifelse(v113 %in% c(22), 5,
                                                                              ifelse(v113 %in% c(42), 7,
                                                                                     ifelse(v113 %in% c(51), 6,
                                                                                            ifelse(v113 %in% c(61), 9,
                                                                                                   ifelse(v113 %in% c(62), 10,
                                                                                                          ifelse(v113 %in% c(71), 11, v113)))))))))),]

##Servicio Sanitario
data_in = data_in %>% .[,V_TIPO_SERSA_C := ifelse(v116 %in% c(11), 1,
                                                  ifelse(v116 %in% c(12), 2,
                                                         ifelse(v116 %in% c(13), 3,
                                                                ifelse(v116 %in% c(21), 4,
                                                                       ifelse(v116 %in% c(22), 5,
                                                                              ifelse(v116 %in% c(31), 6, v116)))))),]

##Servicio de electricidad?

data_in = data_in %>% .[,VA_EE_C := ifelse(v119 %in% c(0), 2, 1),]


##Material de los pisos

data_in = data_in %>% .[,V_MAT_PISO_C := ifelse(v127 %in% c(11), 6,
                                                ifelse(v127 %in% c(22), 4,
                                                       ifelse(v127 %in% c(21), 5,
                                                              ifelse(v127 %in% c(30, 34), 2,
                                                                     ifelse(v127 %in% c(31), 3,
                                                                            ifelse(v127 %in% c(32, 33), 1, v127)))))),]


##Material de las paredes 
# data_in = data_in %>% .[,V_MAT_PARED_C := ifelse(v128 %in% c(11, 12, 22), 4,
#                                           ifelse(v128 %in% c(21), 5,
#                                           ifelse(v128 %in% c(23), 10,
#                                           ifelse(v128 %in% c(24), 8,
#                                           ifelse(v128 %in% c(31), 1,
#                                           ifelse(v128 %in% c(32), 3,
#                                           ifelse(v128 %in% c(95), 9, v128))))))),]


data_in = data_in %>% .[,V_MAT_PARED_C := ifelse(v128 %in% c(11, 12, 23), 1,
                                                 ifelse(v128 %in% c(21,22), 2,
                                                        ifelse(v128 %in% c(31), 3,
                                                               ifelse(v128 %in% c(32), 4,
                                                                      ifelse(v128 %in% c(24), 5,
                                                                             ifelse(v128 %in% c(95), 6,
                                                                                    ifelse(v128 %in% c(95), 7, v128))))))),]

##Pertenencia étnica
data_in = data_in %>% .[,PA1_GRP_ETNIC_C := v131,]

##Sabe leer y escribir?

data_in = data_in %>% .[,P_ALFABETA_C := ifelse(s107 %in% c(0), 2, 1),]

##Asistencia escolar

data_in = data_in %>% .[,PA_ASISTENCIA_C := ifelse(s109 %in% c(0, NA), 2, ifelse(s109 %in% c(1),1,s109)),] 

##Estado civil

data_in = data_in %>% .[,P_EST_CIVIL_C := ifelse(v501 %in% c(0), 7,
                                                 ifelse(v501 %in% c(1), 2,
                                                        ifelse(v501 %in% c(2), 1,
                                                               ifelse(v501 %in% c(3), 6,
                                                                      ifelse(v501 %in% c(4), 3, 8))))),]

##Total de hijos nacidos vivios
data_in = data_in %>% .[,PA1_THNV_C := ifelse(v201 >= 6, 6, v201),]

##Hombres nacidos vivos
data_in = data_in %>% .[,PA2_HNVH_C := ifelse(v202 + v204 + v206 >4, 5,v202 + v204 + v206),]

##Hombres vivos actualmente
data_in = data_in %>% .[,PA2_HSVH_C := ifelse(v202 + v204 >3, 4,v202 + v204),]

### Mujeres nacidas vivas
data_in = data_in %>% .[,PA3_HNVM_C :=ifelse(v203 + v205 + v207 >3, 4,v203 + v205 + v207),]

### Mujeres vivas actualmente
data_in = data_in %>% .[,PA3_HSVM_C := ifelse(v203 + v205 >3, 4,v203 + v205 ),]

## Hijos sobrevivientes
data_in = data_in %>% .[,PA2_HS_C := ifelse(v203 + v205 + v202 + v204 > 3, 4,v202 + v204 + v203 + v205 ),]


## Edad
data_in = data_in %>% .[,P_EDAD_C := v012 ,] %>% 
  .[,P_EDAD_C := cut(P_EDAD_C, breaks = c(-Inf,seq(4,94,5),Inf) , labels = F),]

## id para el merge
data_in = data_in %>% .[,id := gsub(" ", "", caseid, fixed = TRUE),]


# Base final de IR con las variables homologadas con el censo

names(data_in)

var_2select = c("id","caseid","H_AGUA_COCIN_C",  "V_TIPO_SERSA_C",  "VA_EE_C", "V_MAT_PISO_C",   
                "V_MAT_PARED_C", "PA1_GRP_ETNIC_C", "PA_ASISTENCIA_C", "P_ALFABETA_C", 
                "P_EST_CIVIL_C", "PA1_THNV_C", "PA2_HNVH_C", "PA3_HNVM_C","PA2_HS_C",
                "PA3_HSVM_C","PA2_HSVH_C", "P_EDAD_C", "UA_CLASE_C","PESOS_WISW",
                "age_first_sex","age_first_union","time_last_sex", "recent_sexual_activity", "unmet_need_2","unmet_need_3",
                "exposure_2","exposure_3", "decision_using_contra", "condom_used_last", "relations_most_recent", 
                "condom_usedat_first", "knows_methods","ever_used_any","current_method","reason_not_use",
                "current_method_grou")


COIR71DT = data_in %>% .[,..var_2select,] 


rm(data_in, data_COIR71DT)

#######################################################
## Arreglo base Person Record (PR)                  ###
#######################################################

data_in = data_COPR71DT %>% as.data.table() %>% .[,..names.use.PR]

## Variable de pesos -  Household sample weigth
data_in = data_in %>% .[,PESOS_HSW := hv005,]

## ¿En donde preparan los alimentos las personas de este hogar?
data_in = data_in %>%.[,H_DONDE_PREPALIM_C := ifelse(hv242 %in% c(0), 1,0),]

##Relación con el jefe de hogar

# data_in = data_in %>% .[,P_PARENTESCO_C := ifelse(hv101 %in% c(4), 5,
#                                            ifelse(hv101 %in% c(5), 12,
#                                            ifelse(hv101 %in% c(7), 8,
#                                            ifelse(hv101 %in% c(8), 9,
#                                            ifelse(hv101 %in% c(10), 10,
#                                            ifelse(hv101 %in% c(11, 3), 3,
#                                            ifelse(hv101 %in% c(12), 14,
#                                            ifelse(hv101 %in% c(15),11,
#                                            ifelse(hv101 %in% c(16), 15,
#                                            ifelse(hv101 %in% c(17, 18),16, hv101 )))))))))),]

data_in = data_in %>% .[,P_PARENTESCO_C := ifelse(hv101 %in% c(4:10), 4,
                                                  ifelse(hv101 %in% c(12,15,16,17,18), 5,
                                                         ifelse(hv101 %in% c(3,11), 3, hv101 ))),]
##Nivel educativo 
data_in = data_in %>%.[,P_NIVEL_ANOS_C := ifelse(hv106 %in% c(8), NA,hv106),]

## Personas por hogar
data_in = data_in %>%.[,PERSO_HOGAR := max(hvidx) ,.(hhid)]

##Cuartos usados para dormir
data_in = data_in %>% .[,H_NRO_DORMIT_C := hv216,]

##Hacinamiento
data_in = data_in %>%.[,HACINAMIENTO := ifelse(PERSO_HOGAR/H_NRO_DORMIT_C > 3, 1,0),]

##Estrato socioeconómico
data_in = data_in %>% .[,VA1_ESTRATO_C := ifelse(sh68ab == 8, 9,sh68ab),]

##Acceso a gas natural
data_in = data_in %>% .[,VD_GAS_C := ifelse(sh68b %in% c(0), 2, 1),]

##Acceso a Alcantarillado
data_in = data_in %>% .[,VC_ALC_C := ifelse(sh68d %in% c(0), 2, 1),]

##Acceso a Recolección de basuras
data_in = data_in %>% .[,VE_RECBAS_C := ifelse(sh68e %in% c(0), 2, 1),]

##Acceso a Internet
data_in = data_in %>% .[,VF_INTERNET_C := ifelse(sh79l %in% c(0), 2, 1),]

##Acceso a Acueducto
data_in = data_in %>% .[,VB_ACU_C := ifelse(hv202 %in% c(11,12), 1, 2),]


##Residencia hace 5 años
data_in = data_in %>% .[,PA_VIVIA_5ANOS_C := ifelse(sh28 %in% c(1, 2), 3,
                                                    ifelse(sh28 %in% c(3),4,
                                                           ifelse(sh28 %in% c(4),2,sh28))),]

##Ocupación semana pasada
# data_in = data_in %>%.[,P_TRABAJO_C := ifelse(sh31 %in% c(1), 11,
#                                        ifelse(sh31 %in% c(2), 3,
#                                        ifelse(sh31 %in% c(3), 4,
#                                        ifelse(sh31 %in% c(4), 6,
#                                        ifelse(sh31 %in% c(5), 7,
#                                        ifelse(sh31 %in% c(6, 7), 5,
#                                        ifelse(sh31 %in% c(8), 8,
#                                        ifelse(sh31 %in% c(96),9, sh31 )))))))),]


## V_Tipo de vivienda
data_in = data_in %>% .[,V_TIPO_VIV_C := ifelse(sh85 %in% c(1:4), 1,
                                                ifelse(sh85 %in% c(5),3,sh85)),]

##Dificultad: voces y sonidos
data_in = data_in %>% .[,PA_OIR_C := sh53a,]

##Dificultad: hablar y comunicar
data_in = data_in %>% .[,PB_HABLAR_C := sh53b,]

##Dificultad: Mirar de cerca
data_in = data_in %>% .[,PC_VER_C :=sh53c,]

##Dificultad: Mover el cuerpo
data_in = data_in %>% .[,PD_CAMINAR_C := sh53d,]

##Dificultad:Agarrar o mover objetos
data_in = data_in %>% .[,PE_COGER_C := sh53e,]

##Dificultad: Entender o tomar decisiones
data_in = data_in %>% .[,PF_DECIDIR_C := sh53f,]

##Dificultad: COMER Y vestirse
data_in = data_in %>% .[,PG_COMER_C := sh53g,]

##Dificultad: Relacionarse o interactuar
data_in = data_in %>% .[,PH_RELACION_C := sh53h,]

##Dificultad: Actividades diarias
data_in = data_in %>% .[,PI_TAREAS_C := sh53i,]

##Dificultad que más lo afecta
data_in = data_in %>% .[,P_LIM_PPAL_C := sh54,]

##Origen de la dificultad
data_in = data_in %>%.[,P_CAUSA_LIM_C := ifelse(sh55 %in% c(3), 11,
                                                ifelse(sh55 %in% c(5), 12, sh55 )),]

## utiliza de manera permanente: Gafas, lentes,lupas
data_in = data_in %>% .[,PA_AYUDA_TEC_C := ifelse(sh56a %in% c(0), 2, 1),]

##utiliza de manera permanente: Medicamentos o terapias?
data_in = data_in %>% .[,PC_AYUDA_MED_C := ifelse(sh56b %in% c(0), 2, 1),]


##For difficulty permanently use: help from others
data_in = data_in %>% .[,PB_AYUDA_PERS_C := ifelse(sh56c %in% c(0), 2, 1),]

## id para el merge
data_in = data_in %>% .[,a := paste0(hhid,hvidx),] %>% 
  .[,id := gsub(" ", "", a, fixed = TRUE),]


##Base final Person Record (PR)
names(data_in)



var_2select = c("id", "hhid", "hvidx", "P_NIVEL_ANOS_C","H_NRO_DORMIT_C",   "VA1_ESTRATO_C", "VD_GAS_C",        
                "VC_ALC_C","VE_RECBAS_C","VF_INTERNET_C", "PA_VIVIA_5ANOS_C", "P_PARENTESCO_C",    
                "PA_OIR_C","PB_HABLAR_C","PC_VER_C", "PD_CAMINAR_C", "PE_COGER_C", "VB_ACU_C", "V_TIPO_VIV_C",    
                "PF_DECIDIR_C","PG_COMER_C","PH_RELACION_C", "PI_TAREAS_C", "P_LIM_PPAL_C", "H_DONDE_PREPALIM_C",   
                "P_CAUSA_LIM_C","PA_AYUDA_TEC_C","PC_AYUDA_MED_C", "PB_AYUDA_PERS_C","HACINAMIENTO","PESOS_HSW")



COPR71DT = data_in %>% .[,..var_2select,] 

rm(data_in, data_COPR71DT)

#######################################################
## Unir las 2 bases de datos                        ###
#######################################################

str(COIR71DT)
str(COPR71DT)
ENDS = left_join(COIR71DT, COPR71DT
                 , by = "id") %>% as.data.table() 

#Quitar etiquetas
ENDS[, names(ENDS) := lapply(.SD, setattr, "label", NULL)]

saveRDS(ENDS, "1. ConciliarBases/Output/ENDS2015_in.rds")

rm(ENDS, COIR71DT,COPR71DT)

