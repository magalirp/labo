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

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#------------------------------------------------------------------------------
# CREACION DE VARIABLES 

#combino MasterCard y Visa
dataset[ , vm_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
dataset[ , vm_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , vm_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]



#valvula de seguridad para evitar valores infinitos
#paso los infinitos a NULOS
infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
infinitos_qty  <- sum( unlist( infinitos) )
if( infinitos_qty > 0 )
{
  cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
  dataset[mapply(is.infinite, dataset)] <<- NA
}


#valvula de seguridad para evitar valores NaN  que es 0/0
#paso los NaN a 0 , decision polemica si las hay
#se invita a asignar un valor razonable segun la semantica del campo creado
nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
nans_qty  <- sum( unlist( nans) )
if( nans_qty > 0 )
{
  cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
  cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
  dataset[mapply(is.nan, dataset)] <<- 0
}

#------------------------------------------------------------------------------
# GUARDO LOS BAJA+2 DEL DATASET ORIGINAL
dataset_original  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202006  & foto_mes<=202105, ] 

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

#------------------------------------------------------------------------------

#me quedo SOLO con los BAJA+2
dataset  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202006  & foto_mes<=202105, ] 

#------------------------------------------------------------------------------

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
#dataset12  <- copy( dataset[  numero_de_cliente %in%  dataset[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
#setorderv( dataset12, c("numero_de_cliente", "foto_mes"), c(1,-1) )
#dataset12[  , pos := seq(.N) , numero_de_cliente ]

#me quedo solo con los 12 meses antes de morir
#dataset12  <- dataset12[  pos <= 12 , ]
#gc()

#------------------------------------------------------------------------------
# CLUSTERING

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )


#los campos que arbitrariamente decido considerar para el clustering

# Reemplazo por varibales rankedas. 

# Del listado original saco las variables con +de 30% de NA en los BAJA+2: "Master_fechaalta" / "Master_Fvencimiento" / "Visa_mpagospesos" / 
# "Visa_mconsumospesos" / "Master_mfinanciacion_limite" / "Master_status" / "Visa_cconsumos" 

# Sumo las variables que arme mas arriba

campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro_rank", "mtarjeta_visa_consumo_rank", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo_rank", "mrentabilidad_annual_rank", "mprestamos_personales_rank", "mactivos_margen_rank", "mpayroll_rank", "mpayroll2_rank",
                     "Visa_mpagominimo_rank", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos_rank",
                     "Visa_Fvencimiento", "mrentabilidad_rank", "Visa_msaldototal_rank", "mcuenta_corriente_rank",
                     "Visa_fechaalta", "mcomisiones_mantenimiento_rank", "Visa_mfinanciacion_limite_rank",
                     "mtransferencias_recibidas_rank", "cliente_antiguedad",
                     "mcaja_ahorro_dolares_rank", "cproductos", "mcomisiones_otras_rank", "thomebanking", "mcuenta_debitos_automaticos_rank",
                     "mcomisiones_rank", "ccomisiones_otras", "mtransferencias_emitidas_rank",
                     "mpagomiscuentas_rank", "vm_mfinanciacion_limite_rank", "vm_msaldototal_rank","vm_mconsumototal_rank" )



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


# agrego los clusters al dataset original (sin rankeo)

dataset_original[ dataset,
                 on= "numero_de_cliente",
                 cluster2 := i.cluster2 ]

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset_original,
        file="dataset_original_clusters.csv",
        sep= "," )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset_original[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset_original[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset_original[  , mean(mcuentas_saldo),  cluster2 ]
dataset_original[  , mean(chomebanking_transacciones),  cluster2 ]


#Finalmente grabo el archivo para  Juan Pablo Cadaveira
#agrego a dataset12 el cluster2  y lo grabo

#ataset12[ dataset,
           #on= "numero_de_cliente",
           #cluster2 := i.cluster2 ]

#fwrite( dataset12, 
        #file= "cluster_de_bajas_12meses.txt",
        #sep= "\t" )
