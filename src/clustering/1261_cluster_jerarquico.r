#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

# REUTILIZO ESTE SCRIPT PARA CREAR UNA NUEVA VARIABLE PARA LA COMPETENCIA 4

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")

#Parametros del script
PARAM <- list()

PARAM$exp_input  <- "FE4003" # LEVANTO EL DATASET CON FE modelo_3 (SIN RF)

PARAM$experimento  <- "CLU4003"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd( "~/buckets/b1/" ) 

#cargo el dataset
dataset_input  <- paste0( "./exp/", PARAM$exp_input, "/dataset3.csv.gz" )
dataset  <- fread( dataset_input )


#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


#armo dos dataset: uno con los meses donde voy a entrenar y otro con los meses de validacion/ testeo / futuro

dataset1  <- dataset[foto_mes %in% c(201907,201908,201909,201910,201911,201912,202101,202102,202103,202104), ]

dataset2  <- dataset[foto_mes %in% c(202105,202106,202107,202108,202109), ]


dataset1_clase_id <- data.frame(dataset1[,c("numero_de_cliente", "clase_ternaria", "foto_mes")]) 

dataset2_clase_id <- data.frame(dataset2[,c("numero_de_cliente", "clase_ternaria", "foto_mes")])


dataset1_modelo <- data.frame(dataset1[,c("ctrx_quarter", "cpayroll_trx", "mcaja_ahorro_rank", "mtarjeta_visa_consumo_rank", "ctarjeta_visa_transacciones",
                                          "mcuentas_saldo_rank", "mrentabilidad_annual_rank", "mprestamos_personales_rank", "mactivos_margen_rank", "mpayroll_rank", "mpayroll2_rank",
                                          "Visa_mpagominimo_rank", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos_rank",
                                          "Visa_Fvencimiento", "mrentabilidad_rank", "Visa_msaldototal_rank", "mcuenta_corriente_rank",
                                          "Visa_fechaalta", "mcomisiones_mantenimiento_rank", "Visa_mfinanciacion_limite_rank",
                                          "mtransferencias_recibidas_rank", "cliente_antiguedad",
                                          "mcaja_ahorro_dolares_rank", "cproductos", "mcomisiones_otras_rank", "thomebanking", "mcuenta_debitos_automaticos_rank",
                                          "mcomisiones_rank", "ccomisiones_otras", "mtransferencias_emitidas_rank",
                                          "mpagomiscuentas_rank", "vm_mfinanciacion_limite_rank", "vm_msaldototal_rank","vm_mconsumototal_rank","numero_de_cliente")])

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset1_modelo  <- na.roughfix( dataset1_modelo )
#dataset2  <- na.roughfix( dataset2 )


#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro_rank", "mtarjeta_visa_consumo_rank", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo_rank", "mrentabilidad_annual_rank", "mprestamos_personales_rank", "mactivos_margen_rank", "mpayroll_rank", "mpayroll2_rank",
                     "Visa_mpagominimo_rank", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos_rank",
                     "Visa_Fvencimiento", "mrentabilidad_rank", "Visa_msaldototal_rank", "mcuenta_corriente_rank",
                     "Visa_fechaalta", "mcomisiones_mantenimiento_rank", "Visa_mfinanciacion_limite_rank",
                     "mtransferencias_recibidas_rank", "cliente_antiguedad",
                     "mcaja_ahorro_dolares_rank", "cproductos", "mcomisiones_otras_rank", "thomebanking", "mcuenta_debitos_automaticos_rank",
                     "mcomisiones_rank", "ccomisiones_otras", "mtransferencias_emitidas_rank",
                     "mpagomiscuentas_rank", "vm_mfinanciacion_limite_rank", "vm_msaldototal_rank","vm_mconsumototal_rank","numero_de_cliente" )


# DATASET 1

#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset1_modelo, 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )


#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()



#genero 7 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)
  
  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]
  
  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset,
        file= "cluster_de_bajas.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset[  , mean(mcuentas_saldo),  cluster2 ]
dataset[  , mean(chomebanking_transacciones),  cluster2 ]
