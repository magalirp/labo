

###### FEATURE ENGINEERING ------------------------------------------------------------------------------------------------------

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


#cargo las librerias que necesito
require("data.table")
require("Hmisc")
require("tidyverse")
require("dplyr")
require("plyr")

#require("rpart")
#require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/magal/OneDrive/Escritorio/EYF_22")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia2_2022.csv")


denero  <- dataset[ foto_mes==202101 ]
dfebrero  <- dataset[ foto_mes==202102 ]
dmarzo  <- dataset[ foto_mes==202103 ]
dabril  <- dataset[ foto_mes==202104 ]
dmayo  <- dataset[ foto_mes==202105 ]

----------------------------------------------------------------------------------------------------------------------------------
#### CREACION DE NUEVAS VARIABLES

# SUMO [121] "Master_mlimitecompra" Y  [143] "Visa_mlimitecompra"
  
denero$mlimitecompra_total <- rowSums (denero [, c (121,143)], na.rm = TRUE )
sum(is.na(denero$mlimitecompra_total)) #[1] 0

dfebrero$mlimitecompra_total <- rowSums (dfebrero [, c (121,143)], na.rm = TRUE )
sum(is.na(dfebrero$mlimitecompra_total)) #[1] 0

dmarzo$mlimitecompra_total <- rowSums (dmarzo [, c (121,143)], na.rm = TRUE )
sum(is.na(dmarzo$mlimitecompra_total)) #[1] 0

dabril$mlimitecompra_total <- rowSums (dabril [, c (121,143)], na.rm = TRUE )
sum(is.na(dabril$mlimitecompra_total)) #[1] 0

dmayo$mlimitecompra_total <- rowSums (dmayo [, c (121,143)], na.rm = TRUE )
sum(is.na(dmayo$mlimitecompra_total)) #[1] 0


  
# DIVIDO [121] "[23] mcuentas_saldo" Y  [34] mcuentas_saldo POR cliente_edad"

denero <- denero %>% mutate(mcuentas_edad = mcuentas_saldo / cliente_edad)
denero <- denero %>% mutate(mprestamos_personales_edad = mprestamos_personales / cliente_edad)

dfebrero <- dfebrero %>% mutate(mcuentas_edad = mcuentas_saldo / cliente_edad)
dfebrero <- dfebrero %>% mutate(mprestamos_personales_edad = mprestamos_personales / cliente_edad)

dmarzo <- dmarzo %>% mutate(mcuentas_edad = mcuentas_saldo / cliente_edad)
dmarzo <- dmarzo %>% mutate(mprestamos_personales_edad = mprestamos_personales / cliente_edad)

dabril <- dabril %>% mutate(mcuentas_edad = mcuentas_saldo / cliente_edad)
dabril <- dabril %>% mutate(mprestamos_personales_edad = mprestamos_personales / cliente_edad)

dmayo <- dmayo %>% mutate(mcuentas_edad = mcuentas_saldo / cliente_edad)
dmayo <- dmayo %>% mutate(mprestamos_personales_edad = mprestamos_personales / cliente_edad)

----------------------------------------------------------------------------------------------------------------------------------

dataset = rbind.fill(denero, dfebrero, dmarzo, dabril, dmayo)  

#### EXPORTO EL EL DATASET CON LAS NUEVAS VARIABLES PERO SIN RANKEAR

fwrite( dataset, 
        file= "./datasets/competencia2_2022_fe1.csv",
        sep=  "," )



----------------------------------------------------------------------------------------------------------------------------------
#### RANKEO LAS NUEVAS VARIABLES + LAS RELACIONADAS A PROBLEMAS DE DATA DRIFFTING Y CONCEPT DRIFFTING (A PARTIR DE LAS DISTRIBUCIONES QUE VIMOS ENTRE ENERO Y MARZO)
#### LO HAGO SOBRE EL DATASET COMPLETO (EN VEZ DE POR MES) SIGUIENDO LA RECOMENDACIÓN DE GUSTAVO EN ZULIP
 

dataset[, 9] <- ntile(dataset[, 9], 100)  
dataset[, 10] <- ntile(dataset[, 10], 100)  
dataset[, 13] <- ntile(dataset[, 13], 100)  
dataset[, 14] <- ntile(dataset[, 14], 100)  
dataset[, 17] <- ntile(dataset[, 17], 100)  
dataset[, 19] <- ntile(dataset[, 19], 100)  
dataset[, 23] <- ntile(dataset[, 23], 100)
dataset[, 28] <- ntile(dataset[, 28], 100)
dataset[, 33] <- ntile(dataset[, 33], 100)  
dataset[, 36] <- ntile(dataset[, 36], 100)  
dataset[, 52] <- ntile(dataset[, 52], 100)  
dataset[, 53] <- ntile(dataset[, 53], 100)  
dataset[, 54] <- ntile(dataset[, 54], 100)  
dataset[, 55] <- ntile(dataset[, 55], 100)  
dataset[, 57] <- ntile(dataset[, 57], 100)  
dataset[, 59] <- ntile(dataset[, 59], 100)  
dataset[, 61] <- ntile(dataset[, 61], 100)  
dataset[, 63] <- ntile(dataset[, 63], 100)  
dataset[, 64] <- ntile(dataset[, 64], 100)  
dataset[, 70] <- ntile(dataset[, 70], 100)  
dataset[, 71] <- ntile(dataset[, 71], 100)  
dataset[, 72] <- ntile(dataset[, 72], 100)
dataset[, 73] <- ntile(dataset[, 73], 100)  
dataset[, 74] <- ntile(dataset[, 74], 100)  
dataset[, 77] <- ntile(dataset[, 77], 100)  
dataset[, 78] <- ntile(dataset[, 78], 100)  
dataset[, 80] <- ntile(dataset[, 80], 100)  
dataset[, 81] <- ntile(dataset[, 81], 100)  
dataset[, 82] <- ntile(dataset[, 82], 100)  
dataset[, 84] <- ntile(dataset[, 84], 100)  
dataset[, 88] <- ntile(dataset[, 88], 100)  
dataset[, 89] <- ntile(dataset[, 89], 100)  
dataset[, 92] <- ntile(dataset[, 92], 100)  
dataset[, 93] <- ntile(dataset[, 93], 100)  
dataset[, 96] <- ntile(dataset[, 96], 100)  
dataset[, 102] <- ntile(dataset[, 102], 100)  
dataset[, 103] <- ntile(dataset[, 103], 100)  
dataset[, 115] <- ntile(dataset[, 115], 100)  
dataset[, 117] <- ntile(dataset[, 117], 100)  
dataset[, 118] <- ntile(dataset[, 118], 100)  
dataset[, 120] <- ntile(dataset[, 120], 100)  
dataset[, 124] <- ntile(dataset[, 124], 100)  
dataset[, 125] <- ntile(dataset[, 125], 100)  
dataset[, 127] <- ntile(dataset[, 127], 100)
dataset[, 135] <- ntile(dataset[, 135], 100)
dataset[, 136] <- ntile(dataset[, 136], 100)
dataset[, 138] <- ntile(dataset[, 138], 100)
dataset[, 140] <- ntile(dataset[, 140], 100)
dataset[, 147] <- ntile(dataset[, 147], 100)
dataset[, 156] <- ntile(dataset[, 156], 100)
dataset[, 157] <- ntile(dataset[, 157], 100)
dataset[, 158] <- ntile(dataset[, 158], 100)


----------------------------------------------------------------------------------------------------------------------------------
#### EXPORTO EL NUEVO CSV
  
  fwrite( dataset, 
          file= "./datasets/competencia2_2022_fe2.csv",
          sep=  "," )




----------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------


