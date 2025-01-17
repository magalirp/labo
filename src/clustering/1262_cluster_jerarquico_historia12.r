#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")

#Parametros del script
PARAM <- list()
PARAM$experimento  <- "CLU1262"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd( "~/buckets/b1/" )  #cambiar por la carpeta local

#leo el dataset original
# pero podria leer cualquiera que tenga Feature Engineering

dataset  <- fread( "./datasets/competencia3_2022.csv.gz", stringsAsFactors= TRUE)

# me quedo SOLO con los BAJA+2 (DEL DATASET ORIGINAL)
dataset_original  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202006  & foto_mes<=202105, ] 

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


#------------------------------------------------------------------------------
# APLICO RANK CERO FIJO ANTES DE CLUSTERIZAR

drift_rank_cero_fijo  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ get(campo) ==0, paste0(campo,"_rank") := 0 ]
    dataset[ get(campo) > 0, paste0(campo,"_rank") :=   frank(  get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ get(campo) < 0, paste0(campo,"_rank") :=  -frank( -get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ , (campo) := NULL ]
  }
}

#ordeno de esta forma por el ranking
setorder( dataset, foto_mes, numero_de_cliente )

#por como armé los nombres de campos, estos son los campos que expresan variables monetarias
campos_monetarios  <- colnames(dataset)
campos_monetarios  <- campos_monetarios[campos_monetarios %like% "^(m|Visa_m|Master_m|vm_m)"]

PARAM$metodo  <- "rank_cero_fijo"

switch( 
  PARAM$metodo,
  "ninguno"        = cat( "No hay correccion del data drifting" ),
  "rank_simple"    = drift_rank_simple( campos_monetarios ),
  "rank_cero_fijo" = drift_rank_cero_fijo( campos_monetarios ),
  "deflacion"      = drift_deflacion( campos_monetarios ) 
)

names(dataset)

#------------------------------------------------------------------------------

# me quedo SOLO con los BAJA+2 
dataset  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202006  & foto_mes<=202105, ] 

#------------------------------------------------------------------------------

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
dataset12  <- copy( dataset_original[  numero_de_cliente %in%  dataset_original[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
setorderv( dataset12, c("numero_de_cliente", "foto_mes"), c(1,-1) )
dataset12[  , pos := seq(.N) , numero_de_cliente ]

#me quedo solo con los 12 meses antes de morir
dataset12  <- dataset12[  pos <= 12 , ]
gc()


#------------------------------------------------------------------------------
#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )

#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro_rank", "mtarjeta_visa_consumo_rank", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo_rank", "mrentabilidad_annual_rank", "mprestamos_personales_rank", "mactivos_margen_rank", "mpayroll_rank",
                     "Visa_mpagominimo_rank", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos_rank",
                     "Visa_Fvencimiento", "mrentabilidad_rank", "Visa_msaldototal_rank", "Master_Fvencimiento", "mcuenta_corriente_rank",
                     "Visa_mpagospesos_rank", "Visa_fechaalta", "mcomisiones_mantenimiento_rank", "Visa_mfinanciacion_limite_rank",
                     "mtransferencias_recibidas_rank", "cliente_antiguedad", "Visa_mconsumospesos_rank", "Master_mfinanciacion_limite_rank",
                     "mcaja_ahorro_dolares_rank", "cproductos", "mcomisiones_otras_rank", "thomebanking", "mcuenta_debitos_automaticos_rank",
                     "mcomisiones_rank", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas_rank",
                     "mpagomiscuentas_rank")



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[  , campos_buenos, with=FALSE ], 
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


#genero 4 clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=3 & distintos <=4 ) )
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


dataset_original[ dataset,
                  on= "numero_de_cliente",
                  cluster2 := i.cluster2 ]

fwrite( dataset_original, 
        file= "cluster_de_bajas_original.txt",
        sep= "\t" )



dataset12[ dataset,
           on= "numero_de_cliente",
           cluster2 := i.cluster2 ]

fwrite( dataset12, 
        file= "cluster_de_bajas_12meses.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset_original[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset_original[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset_original[  , mean(mcuentas_saldo),  cluster2 ]
dataset_original[  , mean(chomebanking_transacciones),  cluster2 ]





