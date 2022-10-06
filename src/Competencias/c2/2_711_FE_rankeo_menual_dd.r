#Necesita para correr en Google Cloud
# 32 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


require("data.table")
require("dplyr")
require("plyr")
require("glue")
require("readxl")
require("stringr")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})



setwd( "/Users/magal/OneDrive/Escritorio/EYF_22" )

#cargo el dataset
dataset  <- fread( "./datasets/dataset_7111.csv")


-----------------------------------------------------------------------------------------------------------------------------------------
#### SEPARO LOS MESES
  
denero  <- dataset[ foto_mes==202101 ]
dfebrero  <- dataset[ foto_mes==202102 ]
dmarzo  <- dataset[ foto_mes==202103 ] 
dabril  <- dataset[ foto_mes==202104 ]
dmayo   <- dataset[ foto_mes==202105 ]

-----------------------------------------------------------------------------------------------------------------------------------------
### FUNCION RANKEO

rankear <- function(df){
    
    data <- select(df, numero_de_cliente, foto_mes)
    
    #browser()
    for (i in colnames(df)[-1:-2]) {
      int <- df %>%
        select(numero_de_cliente, foto_mes, i)
      neg  <- filter(int, int[,3] < 0)
      pos  <- filter(int, int[,3] > 0)
      cero <- filter(int, int[,3] == 0)
      neg <- neg %>%
        mutate(
          across(
            .cols = c(where(~is.numeric(.)), -c(numero_de_cliente, foto_mes)),
            .fns  = ~frank(desc(.), ties.method = 'dense')
          )
        )
      neg[,3] <- neg[,3]*-1
      pos <- pos %>%
        mutate(
          across(
            .cols = c(where(~is.numeric(.)), -c(numero_de_cliente, foto_mes)),
            .fns  = ~frank(., ties.method = 'dense')
          )
        )
      
      rankeada <- rbind(neg,cero,pos)
      
      int <- select(int, -i) %>%
        left_join(rankeada)
      
      colnames(int)[3] <- glue::glue(i, "_rank")
      
      data <- left_join(data, int)
    }
    
    return(data)
  }


-----------------------------------------------------------------------------------------------------------------------------------------
### RANKEO: ENERO

seleccionadas_enero <- denero %>% 
  select(-c(active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
            ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
            cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
            cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
            ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
            cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
            ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
            ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
            Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


rankeadas_enero <- rankear(seleccionadas_enero) 


no_seleccionadas_enero <- denero %>% 
  select(c(numero_de_cliente, active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
            ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
            cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
            cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
            ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
            cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
            ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
            ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
            Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


no_seleccionadas_enero$numero_de_cliente <- as.numeric(no_seleccionadas_enero$numero_de_cliente)
rankeadas_enero$numero_de_cliente <- as.numeric(rankeadas_enero$numero_de_cliente)

denero_rank <- no_seleccionadas_enero %>% 
  left_join(., rankeadas_enero, by = "numero_de_cliente")  


-----------------------------------------------------------------------------------------------------------------------------------------
### RANKEO: FEBRERO
  
seleccionadas_feb <- dfebrero %>% 
  select(-c(active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
            ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
            cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
            cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
            ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
            cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
            ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
            ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
            Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


rankeadas_feb <- rankear(seleccionadas_feb) 


no_seleccionadas_feb <- dfebrero %>% 
  select(c(numero_de_cliente, active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
           ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
           cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
           cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
           ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
           cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
           ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
           ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
           Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


no_seleccionadas_feb$numero_de_cliente <- as.numeric(no_seleccionadas_feb$numero_de_cliente)
rankeadas_feb$numero_de_cliente <- as.numeric(rankeadas_feb$numero_de_cliente)

dfebrero_rank <- no_seleccionadas_feb %>% 
  left_join(., rankeadas_feb, by = "numero_de_cliente")  

-----------------------------------------------------------------------------------------------------------------------------------------
### RANKEO: MARZO
  
seleccionadas_mar <- dmarzo %>% 
  select(-c(active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
            ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
            cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
            cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
            ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
            cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
            ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
            ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
            Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


rankeadas_mar <- rankear(seleccionadas_mar) 


no_seleccionadas_mar <- dmarzo %>% 
  select(c(numero_de_cliente, active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
           ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
           cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
           cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
           ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
           cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
           ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
           ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
           Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


no_seleccionadas_mar$numero_de_cliente <- as.numeric(no_seleccionadas_mar$numero_de_cliente)
rankeadas_mar$numero_de_cliente <- as.numeric(rankeadas_mar$numero_de_cliente)

dmarzo_rank <- no_seleccionadas_mar %>% 
  left_join(., rankeadas_mar, by = "numero_de_cliente")  


-----------------------------------------------------------------------------------------------------------------------------------------
### RANKEO: ABRIL
  
seleccionadas_abril <- dabril %>% 
  select(-c(active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
            ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
            cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
            cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
            ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
            cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
            ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
            ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
            Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


rankeadas_abril <- rankear(seleccionadas_abril) 


no_seleccionadas_abril <- dabril %>% 
  select(c(numero_de_cliente, active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
           ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
           cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
           cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
           ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
           cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
           ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
           ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
           Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


no_seleccionadas_abril$numero_de_cliente <- as.numeric(no_seleccionadas_abril$numero_de_cliente)
rankeadas_abril$numero_de_cliente <- as.numeric(rankeadas_abril$numero_de_cliente)

dabril_rank <- no_seleccionadas_abril %>% 
  left_join(., rankeadas_abril, by = "numero_de_cliente")  

-----------------------------------------------------------------------------------------------------------------------------------------
### RANKEO: MAYO
  
seleccionadas_mayo <- dmayo %>% 
  select(-c(active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
            ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
            cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
            cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
            ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
            cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
            ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
            ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
            Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


rankeadas_mayo <- rankear(seleccionadas_mayo) 


no_seleccionadas_mayo <- dmayo %>% 
  select(c(numero_de_cliente, active_quarter, cliente_vip, internet, cproductos, tcuentas, ccuenta_corriente, ccaja_ahorro, cdescubierto_preacordado,
           ctarjeta_debito, ctarjeta_debito_transacciones, ctarjeta_visa, ctarjeta_visa_transacciones, cprestamos_personales, cprestamos_prendarios,
           cprestamos_hipotecarios, cplazo_fijo, cinversion1, cinversion2, cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales, ccaja_seguridad, 
           cpayroll_trx, cpayroll2_trx, ccuenta_debitos_automaticos, ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos, cpagodeservicios, cpagomiscuentas,
           ccajeros_propios_descuentos, ctarjeta_visa_descuentos, ctarjeta_master_descuentos, ccomisiones_mantenimiento, ccomisiones_otras, cforex, cforex_buy,
           cextraccion_autoservicio, ccheques_depositados, ccheques_emitidos, ccheques_depositados_rechazados, 
           ccheques_emitidos_rechazados, tcallcenter, ccallcenter_transacciones, thomebanking, chomebanking_transacciones, ccajas_transacciones, ccajas_consultas,
           ccajas_depositos, ccajas_extracciones, ccajas_otras, catm_trx, catm_trx_other, ctrx_quarter, tmobile_app, cmobile_app_trx, Master_delinquency, Master_status, 
           Master_cconsumos, Master_cadelantosefectivo, Visa_delinquency, Visa_status, Visa_cconsumos, Visa_cadelantosefectivo, clase_ternaria))


no_seleccionadas_mayo$numero_de_cliente <- as.numeric(no_seleccionadas_mayo$numero_de_cliente)
rankeadas_mayo$numero_de_cliente <- as.numeric(rankeadas_mayo$numero_de_cliente)

dmayo_rank <- no_seleccionadas_mayo %>% 
  left_join(., rankeadas_mayo, by = "numero_de_cliente")  


----------------------------------------------------------------------------------------------------------------------------------
####  VUELVO A UNIRLO
  
dataset_rank = rbind.fill(denero_rank, dfebrero_rank, dmarzo_rank, dabril_rank, dmayo_rank) 
----------------------------------------------------------------------------------------------------------------------------------
#### EXPORTO EL NUEVO CSV
  
setwd("./datasets/")   #Establezco el Working Directory DEL EXPERIMENTO
     
#grabo el dataset
fwrite( dataset_rank,
        "dataset_7111_rank_m.csv",
        logical01= TRUE,
        sep= "," )



----------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------

####  GRAFICO DENSIDADES (SCRIPT 613) PARA ANALIZAR DATA DRIFTING Y CONCEPT DISFTING
  
  
----------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------  
  
# ELIMINO VARIABLES PROBLEMATICAS
  
dataset  <- fread( "./datasets/dataset_7111_rank_m.csv")
  
dataset$mcomisiones_mantenimiento_rank <- NULL
dataset$mrentabilidad_rank<- NULL
dataset$mcomisiones_rank<- NULL
dataset$mv_status04_rank<- NULL
dataset$mv_status05_rank<- NULL
dataset$cinversion2<- NULL
dataset$minversion1_dolares_rank<- NULL
dataset$cproductos<- NULL
dataset$ccuenta_corriente<- NULL
dataset$cplazo_fijo<- NULL
dataset$cpagomiscuentas<- NULL
dataset$ccheques_depositados_rechazados<- NULL
dataset$ccaja_ahorro<- NULL
dataset$Master_cadelantosefectivo<- NULL
dataset$Visa_cadelantosefectivo<- NULL
dataset$mplazo_fijo_pesos_rank<- NULL
dataset$mpayroll2_rank<- NULL
dataset$mtarjeta_master_descuentos_rank<- NULL
dataset$mforex_buy_rank<- NULL
dataset$ctransferencias_recibidas_rank<- NULL
dataset$mcheques_depositados_rechazados_rank<- NULL
dataset$mcheques_emitidos_rechazados_rank<- NULL
dataset$Visa_Finiciomora_rank<- NULL
dataset$mv_status03_rank<- NULL
dataset$mv_Finiciomora_rank<- NULL
dataset$mv_cadelantosefectivo_rank<- NULL
dataset$mvr_madelantopesos_rank<- NULL



----------------------------------------------------------------------------------------------------------------------------------
#### EXPORTO EL NUEVO CSV
  
setwd("./datasets/")   #Establezco el Working Directory DEL EXPERIMENTO

#grabo el dataset
fwrite( dataset,
        "dataset_7111_rank_m_176.csv",
        logical01= TRUE,
        sep= "," )

  