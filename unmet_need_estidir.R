## Estimacion directa indicadores ENDS
## May 5 2021
## Autora: Juliana Guerrero Velasquez
## Consultoria UNFPA
## Calculos directos de indicador de necesidad insatisfecha de planeación familiar

rm(list=ls())
options(encoding = "UTF-8", scipen=999)

## Paquetes
library(tidyverse)

## Carga individual records
data_COIR71DT <- readRDS("1. ConciliarBases/Output/COIR71FL.rds")

## pesos para volumen
data_COIR71DT$pesos <- 13126296*data_COIR71DT$v005/sum(data_COIR71DT$v005)


## unmet need  - 

# Unmet need for contraception - Volumen total
# for spacing
sum(data_COIR71DT[data_COIR71DT$v626a %in% c(1),'pesos'])
# for limiting
sum(data_COIR71DT[data_COIR71DT$v626a %in% c(2),'pesos'])
# total unmet need 
sum(data_COIR71DT[data_COIR71DT$v626a %in% c(1,2),'pesos'])

## calculo de estimación directa indicador necesidad insatisfecha

# % unmet need for currently married women 
married_women <- data_COIR71DT %>% filter(v502==1) %>% 
  mutate(unmet=ifelse(v626a %in% c(1,2),v005,0)) %>% 
  summarise(per_unmet=100*sum(unmet)/sum(v005))

# volumen unmet need for currently married women 
data_COIR71DT %>% filter(v502==1) %>% 
  mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  summarise(n_unmet=sum(unmet))


# % unmet need for sexually active women
sexact_women <- data_COIR71DT %>% filter(v502!=1 & v528 %in% c(0:30)) %>% 
  mutate(unmet=ifelse(v626a %in% c(1,2),v005,0)) %>% 
  summarise(ind_unmet=100*sum(unmet)/sum(v005))

# volumen
data_COIR71DT %>% filter(v502!=1 & v528 %in% c(0:30)) %>% 
  mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  summarise(n_unmet=sum(unmet))


# edad v012
data_COIR71DT <- data_COIR71DT %>% mutate(edad_c = case_when(v012<=14~'13-14',
                                                           v012<=19~'15-19',
                                                           v012<=24~'20-24',
                                                           v012<=29~'25-29',
                                                           v012<=34~'30-34',
                                                           v012<=39~'35-39',
                                                           v012<=44~'40-44',
                                                           v012<=49~'45-49'))
#  % unmet need for currently married women 
data_COIR71DT %>% filter(v502==1) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(edad_c) %>%
  summarise(per_unmet_edad=100*sum(unmet)/sum(pesos),n_unmet_edad=sum(unmet))
# % unmet need for sexually active women
data_COIR71DT %>% filter(v502!=1 & v528 %in% c(0:30)) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(edad_c) %>%
  summarise(per_unmet_edad=100*sum(unmet)/sum(pesos),n_unmet_edad=sum(unmet))
  

# zona V025
#  % unmet need for currently married women 
data_COIR71DT %>% filter(v502==1) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(v025) %>%
  summarise(per_unmet_zona=100*sum(unmet)/sum(pesos),n_unmet_zona=sum(unmet))
# % unmet need for sexually active women
data_COIR71DT %>% filter(v502!=1 & v528 %in% c(0:30)) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(v025) %>%
  summarise(per_unmet_zona=100*sum(unmet)/sum(pesos),n_unmet_zona=sum(unmet))

# region V024
#  % unmet need for currently married women 
data_COIR71DT %>% filter(v502==1) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(v024) %>%
  summarise(per_unmet_region=100*sum(unmet)/sum(pesos),n_unmet_region=sum(unmet))
# % unmet need for sexually active women
data_COIR71DT %>% filter(v502!=1 & v528 %in% c(0:30)) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(v024) %>%
  summarise(per_unmet_region=100*sum(unmet)/sum(pesos),n_unmet_region=sum(unmet))


# dpto sdepto
#  % unmet need for currently married women 
data_COIR71DT %>% filter(v502==1) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(sdepto) %>%
  summarise(per_unmet_dpto=100*sum(unmet)/sum(pesos),n_unmet_dpto=sum(unmet))
# % unmet need for sexually active women
data_COIR71DT %>% filter(v502!=1 & v528 %in% c(0:30)) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(sdepto) %>%
  summarise(per_unmet_dpto=100*sum(unmet)/sum(pesos),n_unmet_dpto=sum(unmet))

# educacion v106
#  % unmet need for currently married women 
data_COIR71DT %>% filter(v502==1) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(v106) %>%
  summarise(per_unmet_educ=100*sum(unmet)/sum(pesos),n_unmet_educ=sum(unmet))
# % unmet need for sexually active women
data_COIR71DT %>% filter(v502!=1 & v528 %in% c(0:30)) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(v106) %>%
  summarise(per_unmet_educ=100*sum(unmet)/sum(pesos),n_unmet_educ=sum(unmet))

# quintil de pobreza v190
#  % unmet need for currently married women 
data_COIR71DT %>% filter(v502==1) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(v190) %>%
  summarise(per_unmet_q=100*sum(unmet)/sum(pesos),n_unmet_q=sum(unmet))
# % unmet need for sexually active women
data_COIR71DT %>% filter(v502!=1 & v528 %in% c(0:30)) %>% mutate(unmet=ifelse(v626a %in% c(1,2),pesos,0)) %>% 
  group_by(v190) %>%
  summarise(per_unmet_q=100*sum(unmet)/sum(pesos),n_unmet_q=sum(unmet))


