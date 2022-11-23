

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("D:\\gdrive\\UBA2022\\")  #Aqui se debe poner la ruta de la PC local


dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset

dataset[  , .N, list( foto_mes, clase_ternaria) ]

dfuturo <-  dataset[ foto_mes==202103 ]

nrow( dfuturo )


vector_ids  <-   dfuturo[ , numero_de_cliente]

vector_enviar <-  rep( 1,  nrow( dfuturo))

length( vector_enviar)

tabla_final  <-   as.data.table(  list(  "numero_de_cliente"= vector_ids,
                                         "Predicted"=         vector_enviar))

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/ZH2016/", showWarnings = FALSE )

#genero el archivo para Kaggle
fwrite(tabla_final, 
       file= "./exp/ZH2016/todos_unos.csv",
       sep=  "," )