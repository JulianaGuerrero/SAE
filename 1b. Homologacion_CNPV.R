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
## Paquetes                                                    ##
#################################################################
library(tidyverse)
library(haven)
library(data.table)

#################################################################
## Procesamiento base del censo CNPV 2018                      ##
#################################################################
censo = readRDS("1. ConciliarBases/Input/Censo_Completo.rds") 
censo = censo %>% as.data.table()

censo_hombres = censo %>% .[P_SEXO == 2 & P_EDADR %in% 3:10 & UVA_USO_UNIDAD != 4,,]

#Cargar variables del módulo de hogares imputado
censo_hogaresI =  readRDS("1. ConciliarBases/Input/Censo_hogarNB.rds") %>% as.data.table()
censo_hogaresI$P_PARENTESCOR = NULL
censo_hogaresI$P_EDADR = NULL
censo_hombres = censo %>% .[P_SEXO == 2 & P_EDADR %in% 3:10 & UVA_USO_UNIDAD != 4,,]
censo_hogaresI$P_PARENTESCO = NULL
censo_hogaresI$P_EDAD = NULL
censo_hogaresI$HA_TOT_PER = NULL
censo_hogaresI$PA_ASISTENCIA = NULL

## Variables de hogares en la base del censo
names_var = names(censo)
names.in = names(censo_hogaresI)[(names(censo_hogaresI) %in% names_var)]
names.in = names.in[!names.in %in% c("idhogar","HA_TOT_PER","PA_ASISTENCIA")]
names.in

#Variables que no estan 
names.not.in = names(censo)[(!names(censo) %in% names.in)]
names.not.in

#Conservar la variables que no están e el módulo de hogares
censo_1 = censo_hombres %>% .[,..names.not.in]

#Pegar las variables de Hogares
censo_new = left_join(censo_1, censo_hogaresI,
                      by = "idhogar") 

censo_new <- censo_new %>% as.data.table()

rm(censo_1)
rm(censo)
rm(censo_hogaresI)
rm(censo_hombres)
##Aplicar las codificaciones necesarias a algunas variables para quedar homologadas


## Asistencia 
censo_new = censo_new %>% .[,PA_ASISTENCIA := ifelse(PA_ASISTENCIA %in% c(0), 2, PA_ASISTENCIA),]
censo_new = censo_new %>% .[,PA_ASISTENCIA := ifelse(PA_ASISTENCIA %in% c(1) & P_EDADR < 25, 1, 
                                              ifelse(PA_ASISTENCIA  %in% c(1) & P_EDADR > 24, 2,PA_ASISTENCIA)),]
## Clase 
censo_new = censo_new %>% .[,UA_CLASE := ifelse(UA_CLASE %in% c(4), 2, 1),]

## Nivel educativo años
 censo_new = censo_new %>%.[,P_NIVEL_ANOSR := ifelse(P_NIVEL_ANOSR %in% c(1), 0,
                                              ifelse(P_NIVEL_ANOSR %in% c(2), 1,
                                              ifelse(P_NIVEL_ANOSR %in% c(3, 4, 5, 6), 2, 
                                              ifelse(P_NIVEL_ANOSR %in% c(7:9), 3, 9)))),]


##Agua alimentos
censo_new = censo_new %>% .[,H_AGUA_COCIN := ifelse( H_AGUA_COCIN %in% c(2, 3), 22, H_AGUA_COCIN),]


##Material de las paredes 
#censo_new = censo_new %>% .[,V_MAT_PARED := ifelse(V_MAT_PARED %in% c(1, 2), 1,
#                                      ifelse(V_MAT_PARED %in% c(6, 7), 10, V_MAT_PARED)),]

censo_new = censo_new %>% .[,V_MAT_PARED := ifelse(V_MAT_PARED %in% c(1, 2), 3,
                                            ifelse(V_MAT_PARED %in% c(3), 4, 
                                            ifelse( V_MAT_PARED %in% c(4, 6, 7), 1,
                                            ifelse(V_MAT_PARED %in% c(5), 2,
                                            ifelse(V_MAT_PARED %in% c(8), 5,
                                            ifelse(V_MAT_PARED %in% c(9), 6, V_MAT_PARED)))))),]

##¿En donde preparan los alimentos las personas de este hogar?
censo_new = censo_new %>% .[,H_DONDE_PREPALIM := ifelse(H_DONDE_PREPALIM %in% c(1), 1, 0),]

##¿El estado civil de ... es?
censo_new = censo_new %>% .[,P_EST_CIVIL := ifelse(P_EST_CIVIL %in% c(4, 5), 8, P_EST_CIVIL),]

##¿Durante la semana pasada . . .?
censo_new = censo_new %>% .[,P_TRABAJO := ifelse(P_TRABAJO %in% c(1, 2), 11, P_TRABAJO),]


##¿Cual es la relación de parentesco de ... con el jefe(a) del hoga?
censo_new = censo_new %>% .[,P_PARENTESCOR := ifelse(P_PARENTESCOR %in% c(7, 10, 13), 7, P_PARENTESCOR),]

## Hijos sobrevivientes (Hombres + Mujeres)
censo_new = censo_new %>% .[,PA2_HS :=  ifelse(PA2_HSVH +PA3_HSVM <= 3 & PA_HNV ==1, PA2_HSVH +PA3_HSVM,
                                        ifelse(PA2_HSVH +PA3_HSVM %in% c(4:18) & PA_HNV == 1, 4,
                                        ifelse(PA2_HSVH +PA3_HSVM %in% c(100:120) & PA_HNV == 1, PA2_HSVH +PA3_HSVM - 99,99))),]                                             
##Total de hijos nacidos vivios
censo_new = censo_new %>% .[,PA1_THNV := ifelse( PA_HNV ==2,0,  
                                         ifelse(PA1_THNV %in% 1:6 & PA_HNV ==1 , PA1_THNV, 
                                         ifelse(PA1_THNV %in% 7:22 & PA_HNV ==1, 6,
                                         ifelse(PA1_THNV == 99, 99, PA1_THNV)))),]
##Hombres nacidos vivos
censo_new = censo_new %>% .[,PA2_HNVH := ifelse(PA2_HNVH %in% 0:4 & PA_HNV ==1, PA2_HNVH,
                                         ifelse(PA2_HNVH %in% 5:22 & PA_HNV ==1, 5,
                                         ifelse(PA2_HNVH == 99, 99, PA2_HNVH))),]
##Hombres vivos actualmente

censo_new = censo_new %>% .[,PA2_HSVH := ifelse(PA2_HSVH %in% 0:3 & PA_HNV ==1, PA2_HNVH,
                                         ifelse(PA2_HSVH %in% 4:22 & PA_HNV ==1, 4,
                                         ifelse(PA2_HSVH == 99, 99, PA2_HSVH))),]

### Mujeres nacidas vivas
censo_new = censo_new %>% .[,PA3_HNVM := ifelse(PA3_HNVM %in% 0:3 & PA_HNV ==1, PA3_HNVM,
                                         ifelse(PA3_HNVM %in% 4:22 & PA_HNV ==1, 4,
                                         ifelse(PA3_HNVM == 99, 99, PA3_HNVM))),]

### Mujeres vivas actualmente
censo_new = censo_new %>% .[,PA3_HSVM := ifelse(PA3_HSVM %in% 0:3 & PA_HNV ==1, PA3_HSVM,
                                         ifelse(PA3_HSVM %in% 4:22 & PA_HNV ==1, 4,
                                         ifelse(PA3_HSVM == 99, 99, PA3_HSVM))),]

## La variable de pertenencia étnica no esta en la base del censo "Censo_Completo.rds"
## Pertenencia étnica
censo_new = censo_new %>% .[,PA1_GRP_ETNIC := PA1_GRP_ETNIC,]


## Tipo de vivienda 
censo_new = censo_new %>% .[,V_TIPO_VIV := ifelse(V_TIPO_VIV %in% c(1,2), 1,
                                           ifelse(V_TIPO_VIV %in% c(4,5), 7, V_TIPO_VIV)),]

names(censo_new)
## Variables a seleccionar
names = c("U_DPTO"                ,"U_MPIO"               ,"UA_CLASE"             
          ,"COD_ENCUESTAS"         ,"U_VIVIENDA"            ,"PA1_GRP_ETNIC"            
          ,"TIPO_REG.x"            ,"U_EDIFICA"             ,"UVA_USO_UNIDAD"       
          ,"V_TIPO_VIV"            ,"V_CON_OCUP"            ,"V_TOT_HOG"            
          ,"V_MAT_PARED"           ,"V_MAT_PISO"            ,"VA_EE"                
          ,"VB_ACU"                 ,"VC_ALC"               
          ,"VD_GAS"                ,"VE_RECBAS"             ,"VE1_QSEM"             
          ,"VF_INTERNET"           ,"V_TIPO_SERSA"          ,"PA_VIVIA_5ANOS"           
          ,"L_EXISTEHOG"           ,"L_TOT_PERL"            ,"TIPO_REG.y"           
          ,"H_NRO_CUARTOS"         ,"H_NRO_DORMIT"          ,"H_DONDE_PREPALIM"     
          ,"H_AGUA_COCIN"          ,"HA_NRO_FALL"           ,"HA_TOT_PER"           
          ,"TIPO_REG"              ,"P_NRO_PER"             ,"P_SEXO"               
          ,"P_EDADR"                ,"P_PARENTESCOR"          ,"PA_LUG_NAC"           
          ,"CONDICION_FISICA"      ,"P_ALFABETA"            ,"PA_ASISTENCIA"        
          ,"P_NIVEL_ANOSR"          ,"P_TRABAJO"             ,"P_EST_CIVIL"          
          ,"PA_HNV"                ,"PA1_THNV"              ,"PA2_HNVH"             
          ,"PA3_HNVM"              ,"PA_HNVS"               ,"PA1_THSV"             
          ,"PA2_HSVH"              ,"PA3_HSVM", "PA2_HS", "IDvivienda", "idhogar"
          
          # VARIABLES DEL IPM
          ,"analfabetismo"
          ,"inasistencia"
          ,"barrerassalud"
          ,"privhacinamientomit"
          ,"privparedes"
          ,"privpisos"
          ,"privalcantarillado"
          ,"privacueducto")       

#Exportar

saveRDS(censo_new[,..names], "1. ConciliarBases/Output/censo_new_muje.rds")
