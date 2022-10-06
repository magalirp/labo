<<<<<<< HEAD

###### FEATURE ENGINEERING ------------------------------------------------------------------------------------------------------

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


#cargo las librerias que necesito
require("data.table")
#require("rpart")
#require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/magal/OneDrive/Escritorio/EYF_22")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

denero  <- dataset[ foto_mes==202101 ]
dmarzo  <- dataset[ foto_mes==202103 ]  

----------------------------------------------------------------------------------------------------------------------------------
#### CREACION DE NUEVAS VARIABLES

library(tidyverse)
library(dplyr)
library(plyr)

# ANALIZO NA DE LAS VARIABLES CON LAS QUE VOY A TRABAJAR

sum(is.na(denero$Master_mlimitecompra)) #[1] 17014
sum(is.na(denero$Visa_mlimitecompra)) #[1] 7824
sum(is.na(denero$mcuentas_saldo)) #[1] 0
sum(is.na(denero$mprestamos_personaleS)) #[1] 0
sum(is.na(denero$cliente_edad)) #[1] 0
sum(is.na(denero$mcaja_ahorro)) #[1] 0
sum(is.na(denero$mcuenta_corriente)) #[1] 0

sum(is.na(dmarzo$Master_mlimitecompra)) #[1] 16847
sum(is.na(dmarzo$Visa_mlimitecompra)) #[1] 7894
sum(is.na(dmarzo$mcuentas_saldo)) #[1] 0
sum(is.na(dmarzo$mprestamos_personaleS)) #[1] 0
sum(is.na(dmarzo$cliente_edad)) #[1] 0
sum(is.na(dmarzo$mcaja_ahorro)) #[1] 0
sum(is.na(dmarzo$mcuenta_corriente)) #[1] 0


# SUMO [121] "Master_mlimitecompra" Y  [143] "Visa_mlimitecompra"

denero$mlimitecompra_total <- rowSums (denero [, c (121,143)], na.rm = TRUE )
sum(is.na(denero$mlimitecompra_total)) #[1] 0

dmarzo$mlimitecompra_total <- rowSums (dmarzo [, c (121,143)], na.rm = TRUE )
sum(is.na(dmarzo$mlimitecompra_total)) #[1] 0


# DIVIDO [121] "[23] mcuentas_saldo" Y  [34] mcuentas_saldo POR cliente_edad"

denero <- denero %>% mutate(mcuentas_edad = mcuentas_saldo / cliente_edad)
denero <- denero %>% mutate(mprestamos_personales_edad = mprestamos_personales / cliente_edad)

dmarzo <- dmarzo %>% mutate(mcuentas_edad = mcuentas_saldo / cliente_edad)
dmarzo <- dmarzo %>% mutate(mprestamos_personales_edad = mprestamos_personales / cliente_edad)


# DICOTOMIZO mcuentas_saldo en 0 y 1 (saldo positivo / saldo negativo)

denero <- denero %>% 
  mutate(mcuentas_saldo_dic = case_when(mcuentas_saldo > 0 ~ 1,
                                        TRUE ~ 0))

dmarzo <- dmarzo %>% 
  mutate(mcuentas_saldo_dic = case_when(mcuentas_saldo > 0 ~ 1,
                                        TRUE ~ 0))

----------------------------------------------------------------------------------------------------------------------------------
####  TRANSFORMACIONES
  
denero$mcuentas_saldo_bin <- ntile(denero$mcuentas_saldo, 100)  
denero$mcuenta_corriente_bin <- ntile(denero$mcuenta_corriente, 100)  
denero$mcaja_ahorro_bin <- ntile(denero$mcaja_ahorro, 100)  
denero$mprestamos_personales_bin <- ntile(denero$mprestamos_personales, 100)  

denero$mcuentas_edad_bin <- ntile(denero$mcuentas_edad, 100)  
denero$mprestamos_personales_edad_bin <- ntile(denero$mprestamos_personales_edad, 100)  

denero$mlimitecompra_total_bin <- ntile(denero$mlimitecompra_total, 100)  

sum(is.na(denero$mlimitecompra_total)) #[1] 0



dmarzo$mcuentas_saldo_bin <- ntile(dmarzo$mcuentas_saldo, 100)  
dmarzo$mcuenta_corriente_bin <- ntile(dmarzo$mcuenta_corriente, 100)  
dmarzo$mcaja_ahorro_bin <- ntile(dmarzo$mcaja_ahorro, 100)  
dmarzo$mprestamos_personales_bin <- ntile(dmarzo$mprestamos_personales, 100)  

dmarzo$mcuentas_edad_bin <- ntile(dmarzo$mcuentas_edad, 100)  
dmarzo$mprestamos_personales_edad_bin <- ntile(dmarzo$mprestamos_personales_edad, 100)  

dmarzo$mlimitecompra_total_bin <- ntile(dmarzo$mlimitecompra_total, 100)  

sum(is.na(dmarzo$mlimitecompra_total)) #[1] 0

----------------------------------------------------------------------------------------------------------------------------------
####  VUELVO A UNIRLO
  
dataset1 = rbind.fill(denero, dmarzo) 
----------------------------------------------------------------------------------------------------------------------------------
#### EXPORTO EL NUEVO CSV

fwrite( dataset1, 
        file= "./datasets/competencia1_2022_fe.csv",
        sep=  "," )
=======

###### FEATURE ENGINEERING ------------------------------------------------------------------------------------------------------

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


#cargo las librerias que necesito
require("data.table")
#require("rpart")
#require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/magal/OneDrive/Escritorio/EYF_22")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

denero  <- dataset[ foto_mes==202101 ]
dmarzo  <- dataset[ foto_mes==202103 ]  

----------------------------------------------------------------------------------------------------------------------------------
#### CREACION DE NUEVAS VARIABLES

library(tidyverse)
library(dplyr)
library(plyr)

# ANALIZO NA DE LAS VARIABLES CON LAS QUE VOY A TRABAJAR

sum(is.na(denero$Master_mlimitecompra)) #[1] 17014
sum(is.na(denero$Visa_mlimitecompra)) #[1] 7824
sum(is.na(denero$mcuentas_saldo)) #[1] 0
sum(is.na(denero$mprestamos_personaleS)) #[1] 0
sum(is.na(denero$cliente_edad)) #[1] 0
sum(is.na(denero$mcaja_ahorro)) #[1] 0
sum(is.na(denero$mcuenta_corriente)) #[1] 0

sum(is.na(dmarzo$Master_mlimitecompra)) #[1] 16847
sum(is.na(dmarzo$Visa_mlimitecompra)) #[1] 7894
sum(is.na(dmarzo$mcuentas_saldo)) #[1] 0
sum(is.na(dmarzo$mprestamos_personaleS)) #[1] 0
sum(is.na(dmarzo$cliente_edad)) #[1] 0
sum(is.na(dmarzo$mcaja_ahorro)) #[1] 0
sum(is.na(dmarzo$mcuenta_corriente)) #[1] 0


# SUMO [121] "Master_mlimitecompra" Y  [143] "Visa_mlimitecompra"

denero$mlimitecompra_total <- rowSums (denero [, c (121,143)], na.rm = TRUE )
sum(is.na(denero$mlimitecompra_total)) #[1] 0

dmarzo$mlimitecompra_total <- rowSums (dmarzo [, c (121,143)], na.rm = TRUE )
sum(is.na(dmarzo$mlimitecompra_total)) #[1] 0


# DIVIDO [121] "[23] mcuentas_saldo" Y  [34] mcuentas_saldo POR cliente_edad"

denero <- denero %>% mutate(mcuentas_edad = mcuentas_saldo / cliente_edad)
denero <- denero %>% mutate(mprestamos_personales_edad = mprestamos_personales / cliente_edad)

dmarzo <- dmarzo %>% mutate(mcuentas_edad = mcuentas_saldo / cliente_edad)
dmarzo <- dmarzo %>% mutate(mprestamos_personales_edad = mprestamos_personales / cliente_edad)


# DICOTOMIZO mcuentas_saldo en 0 y 1 (saldo positivo / saldo negativo)

denero <- denero %>% 
  mutate(mcuentas_saldo_dic = case_when(mcuentas_saldo > 0 ~ 1,
                                        TRUE ~ 0))

dmarzo <- dmarzo %>% 
  mutate(mcuentas_saldo_dic = case_when(mcuentas_saldo > 0 ~ 1,
                                        TRUE ~ 0))

----------------------------------------------------------------------------------------------------------------------------------
####  TRANSFORMACIONES
  
denero$mcuentas_saldo_bin <- ntile(denero$mcuentas_saldo, 100)  
denero$mcuenta_corriente_bin <- ntile(denero$mcuenta_corriente, 100)  
denero$mcaja_ahorro_bin <- ntile(denero$mcaja_ahorro, 100)  
denero$mprestamos_personales_bin <- ntile(denero$mprestamos_personales, 100)  

denero$mcuentas_edad_bin <- ntile(denero$mcuentas_edad, 100)  
denero$mprestamos_personales_edad_bin <- ntile(denero$mprestamos_personales_edad, 100)  

denero$mlimitecompra_total_bin <- ntile(denero$mlimitecompra_total, 100)  

sum(is.na(denero$mlimitecompra_total)) #[1] 0



dmarzo$mcuentas_saldo_bin <- ntile(dmarzo$mcuentas_saldo, 100)  
dmarzo$mcuenta_corriente_bin <- ntile(dmarzo$mcuenta_corriente, 100)  
dmarzo$mcaja_ahorro_bin <- ntile(dmarzo$mcaja_ahorro, 100)  
dmarzo$mprestamos_personales_bin <- ntile(dmarzo$mprestamos_personales, 100)  

dmarzo$mcuentas_edad_bin <- ntile(dmarzo$mcuentas_edad, 100)  
dmarzo$mprestamos_personales_edad_bin <- ntile(dmarzo$mprestamos_personales_edad, 100)  

dmarzo$mlimitecompra_total_bin <- ntile(dmarzo$mlimitecompra_total, 100)  

sum(is.na(dmarzo$mlimitecompra_total)) #[1] 0

----------------------------------------------------------------------------------------------------------------------------------
####  VUELVO A UNIRLO
  
dataset1 = rbind.fill(denero, dmarzo) 
----------------------------------------------------------------------------------------------------------------------------------
#### EXPORTO EL NUEVO CSV

fwrite( dataset1, 
        file= "./datasets/competencia1_2022_fe.csv",
        sep=  "," )
>>>>>>> 5344b07db831d135339e7c4e0ccb7b0233ed5f15
