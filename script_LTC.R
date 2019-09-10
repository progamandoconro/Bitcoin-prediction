
############################################################################################################
################################################## Litecoin ##################################################
############################################################################################################

library(dplyr) # Para facilitar la mineria de datos
library(zoo) # Tratamiento para los datos faltantes
library(lubridate) # Tratamiento para las fechas en la data

##################### Seleccionar la variable target ########################################################.

output <- read.csv('ltc.csv')


######################## Resultados para mañana ###############################################################           
                # Debido a que los archivos csv descargados tienen diferentes numeros de filas o casos, 

n_to_eval = NROW(output)

n_i <- 1 ; n_f <- NROW(output)  ; n <- n_f-n_i +1

output <- read.csv('ltc.csv')$PriceUSD

########### Seleccionar las variables explicativas ######################################################

input <- list.files(pattern = ".csv") %>%
  lapply (read.csv) 

crip <- sapply (input, function(x) nrow(x) < (n_f - n_i) )

input <- input [!crip]%>%
  lapply (function(x) x[nrow(x)-(nrow(x)-n):nrow(x),  ] )

# Unir las columnas de la lista en una sola data.frame y cambiar los nombres de las columnas repetidas

input <- do.call(cbind, input)

colnames(input)<- make.unique( colnames(input))

############ Transformar fecha. Crear datos cruzados. Tratar los NA #######################################

input$Dia <- day(input$date) ; input$Mes <- month(input$date) ; input$Anio <- year(input$date)
input_fut <- input
input <- input [-1,]

input$OUTPUT <- output [-NROW(output)]

input <- select (input,-starts_with("date"))%>%
  select(-starts_with("date."))%>%
  na.aggregate(by='Anio')%>%
  na.aggregate(by='Mes')%>%
  na.aggregate(by='Dia')%>%
  na.aggregate()

######################### Eliminar la data redundante ####################################################

foo <- function(input) {
  out <- lapply(input, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
}

input <- input[,-foo(input)]

input_fut$OUTPUT <- output

input_fut <- select (input_fut,-starts_with("date"))%>%
  select(-starts_with("date."))%>%
  na.aggregate(by='Anio')%>%
  na.aggregate(by='Mes')%>%
  na.aggregate(by='Dia')%>%
  na.aggregate()

input_fut <- input_fut[,-foo(input_fut)]
              
######################## Normalizar la data entre valores desde 0 hasta 1 ###########################################
 

                normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
input<- lapply(input, normalize)%>%
  as.data.frame()
input_fut<- lapply(input_fut, normalize)%>%
  as.data.frame()

input_60 <- input[(nrow(input)-60):nrow(input),]  
input <- input[1:(nrow(input)-61),] 
###################### Data aleatoria y cortar seccion de validacion ######################################
                

d_train <- input


               
###############################################################################################################              
###############################################################################################################
######################## Selección de variables de entrada #################################################

#library(MASS)  
#m = stepAIC(glm(d_train$OUTPUT~.,data = d_train[,-d_train$OUTPUT]))

#detach("package:MASS", unload = TRUE)

#################### Model result from stepAIC ##############################################################

formula <-    d_train$OUTPUT ~ BlkCnt + CapMVRVCur + CapMrktCurUSD + CapRealUSD + 
  DiffMean + FeeMeanNtv + FeeMedNtv + IssContNtv + NVTAdj90 + 
  PriceUSD + ROI30d + SplyCur + TxTfrValAdjUSD + TxTfrValMeanNtv + 
  TxTfrValMeanUSD + TxTfrValUSD + VtyDayRet180d + VtyDayRet30d + 
  VtyDayRet60d + BlkCnt.1 + BlkSizeByte.1 + BlkSizeMeanByte.1 + 
  CapMVRVCur.1 + CapMrktCurUSD.1 + CapRealUSD.1 + DiffMean.1 + 
  FeeMeanNtv.1 + FeeMeanUSD.1 + FeeMedNtv.1 + FeeMedUSD.1 + 
  FeeTotNtv.1 + FeeTotUSD.1 + IssContNtv.1 + NVTAdj90.1 + PriceBTC.1 + 
  ROI1yr.1 + ROI30d.1 + TxCnt.1 + TxTfr.1 + TxTfrValMeanNtv.1 + 
  TxTfrValNtv.1 + TxTfrValUSD.1 + VtyDayRet180d.1 + VtyDayRet30d.1 + 
  VtyDayRet60d.1 + Dia + Mes + Anio

############ Ejecutar el algoritmo Random Forest #############################################################

library(randomForest)

set.seed(7)   
rf <-  randomForest(formula,data = d_train)
              
p_60 <- predict(rf, input_60 )
##################### Graficar los resultados #################################################################
                
jpeg('plot_ltc.jpg')
                
plot(input_60$OUTPUT,xlab = 'Días desde creación', ylab = 'Precio de Litecoin escalado ($)',ylim=c(0.20,0.80))
points(p_60,col=2)  
lines(p_60,col=2)  
lines(input_60$OUTPUT)
                
dev.off()                 
 
