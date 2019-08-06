############################################################################################################
################################################## Monero ##################################################
############################################################################################################

library(dplyr) # Para facilitar la mineria de datos
library(zoo) # Tratamiento para los datos faltantes
library(caret) # Selección de variables
library(lubridate) # Tratamiento para las fechas en la data

# Utilizamos el precio en $ del Ethereum  como variable respuesta, la que nos interesa predecir.

output <- read.csv('xmr.csv')

# Debido a que los archivos csv descargados tienen diferentes numeros de filas o casos, 

n_to_eval = NROW(output)

n_i <- 1 ; n_f <- NROW(output)  ; n <- n_f-n_i +1

output <- read.csv('xmr.csv')$PriceUSD[n_i:n_f]

# Utilizamos los archivos descargados para seleccionar las variables explicatorias o de entrada para el algoritmo.
# Para eso, listamos todos archivos csv y leemos cada uno con 'read.csv' gracias a 'lapply'

input <- list.files(pattern = ".csv") %>%
  lapply (read.csv) 

# Seleccionamos las variables explicativas que tengan el mismo número de casos que la variable respuesta, las cortamos en funcion de tales casos y  finalmente unimos todas la variables en una sola data.frame

crip <- sapply (input, function(x) nrow(x) < (n_f - n_i) )

input <- input [!crip]%>%
  lapply (function(x) x[nrow(x)-(nrow(x)-n):nrow(x),  ] )

# Unir las columnas de la lista en una sola data.frame y cambiar los nombres de las columnas repetidas

input <- do.call(cbind, input)

colnames(input)<- make.unique( colnames(input))

# Agreguemos data de la fecha a nuestras variables. Creamos los datos cruzados para tener las predicciones de mañana con los datos de hoy. liminemos las variables repetidas con las fechas. Además, reemplazamos los datos faltantes por la media de año, si continuan habiendo NA, por la media mensual y diaria. De esta forma aseguramos que no haya datos faltantes en nuestro set.

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

# Vamos a eliminar la data redundante 

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

# normalizamos la data entre valores 0 a 1

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
input<- lapply(input, normalize)%>%
  as.data.frame()
input_fut<- lapply(input_fut, normalize)%>%
  as.data.frame()


# Data aleatoria y cortar seccion de validacion
set.seed(7)
alea <- sample(1:nrow(input),nrow(input))
ix <- alea[1: floor(NROW(alea)*0.7)]

d_train <- input[ix,]
d_cv <- input[-ix,]


######################## Selección de variables de entrada #####################

library(MASS)  

m = stepAIC(glm(d_train$OUTPUT~.,data = d_train[,-d_train$OUTPUT]))

detach("package:MASS", unload = TRUE)

#################### Model result from stepAIC ##################################

formula <- d_train$OUTPUT ~ BlkCnt + BlkSizeByte + BlkSizeMeanByte + CapMVRVCur + 
  CapMrktCurUSD + DiffMean + NVTAdj90 + PriceUSD + SplyCur + 
  TxCnt + TxTfr + TxTfrValAdjNtv + TxTfrValMeanNtv + TxTfrValUSD + 
  VtyDayRet180d + VtyDayRet30d + VtyDayRet60d + BlkSizeByte.1 + 
  BlkSizeMeanByte.1 + CapMVRVCur.1 + CapMrktCurUSD.1 + CapRealUSD.1 + 
  FeeMeanNtv.1 + FeeMeanUSD.1 + FeeMedNtv.1 + FeeMedUSD.1 + 
  FeeTotNtv.1 + IssContNtv.1 + IssContPctAnn.1 + NVTAdj.1 + 
  PriceUSD.1 + ROI30d.1 + SplyCur.1 + TxTfrValAdjNtv.1 + TxTfrValAdjUSD.1 + 
  TxTfrValMedNtv.1 + VtyDayRet180d.1 + VtyDayRet30d.1 + AdrActCnt.2 + 
  BlkSizeMeanByte.2 + CapMrktCurUSD.2 + DiffMean.2 + FeeMeanUSD.2 + 
  FeeMedNtv.2 + FeeTotNtv.2 + IssContNtv.2 + IssContPctAnn.2 + 
  NVTAdj.2 + NVTAdj90.2 + PriceBTC.2 + PriceUSD.2 + ROI30d.2 + 
  TxCnt.2 + TxTfr.2 + TxTfrValMedNtv.2 + VtyDayRet180d.2 + 
  VtyDayRet30d.2 + BlkSizeMeanByte.3 + CapMrktCurUSD.3 + FeeMeanNtv.3 + 
  FeeMedNtv.3 + IssContNtv.3 + IssContPctAnn.3 + IssContUSD.3 + 
  NVTAdj90.3 + PriceBTC.3 + PriceUSD.3 + ROI1yr.3 + ROI30d.3 + 
  SplyCur.3 + TxTfr.3 + TxTfrValMeanNtv.3 + TxTfrValMeanUSD.3 + 
  VtyDayRet180d.3 + VtyDayRet30d.3 + VtyDayRet60d.3 + BlkSizeByte.4 + 
  CapMVRVCur.3 + CapMrktCurUSD.4 + CapRealUSD.3 + FeeMeanNtv.4 + 
  FeeMeanUSD.4 + FeeMedNtv.4 + FeeMedUSD.4 + FeeTotNtv.4 + 
  FeeTotUSD.4 + IssContPctAnn.4 + NVTAdj90.4 + PriceBTC.4 + 
  ROI1yr.4 + ROI30d.4 + SplyCur.4 + TxCnt.4 + TxTfr.4 + TxTfrValAdjNtv.4 + 
  TxTfrValMeanNtv.4 + TxTfrValMeanUSD.4 + TxTfrValMedUSD.4 + 
  TxTfrValUSD.4 + VtyDayRet30d.4 + VtyDayRet60d.4 + CapMrktCurUSD.5 + 
  FeeMedNtv.5 + FeeMedUSD.5 + IssContNtv.5 + IssContPctAnn.5 + 
  NVTAdj90.5 + PriceBTC.5 + PriceUSD.5 + ROI30d.5 + SplyCur.5 + 
  TxTfrValAdjNtv.5 + TxTfrValNtv.5 + VtyDayRet180d.5 + VtyDayRet30d.5 + 
  BlkCnt.6 + BlkSizeByte.6 + BlkSizeMeanByte.6 + CapMrktCurUSD.6 + 
  FeeMeanNtv.6 + FeeTotNtv.6 + FeeTotUSD.6 + IssContNtv.6 + 
  IssContPctAnn.6 + PriceBTC.6 + PriceUSD.6 + ROI1yr.6 + ROI30d.6 + 
  SplyCur.6 + TxCnt.6 + VtyDayRet180d.6 + VtyDayRet60d.6 + 
  AdrActCnt.6 + CapMrktCurUSD.7 + FeeMedNtv.7 + NVTAdj90.6 + 
  PriceUSD.7 + ROI1yr.7 + TxTfrValAdjNtv.6 + TxTfrValMedUSD.6 + 
  VtyDayRet180d.7 + VtyDayRet30d.7 + VtyDayRet60d.7 + Dia + 
  Mes + Anio

############ Ejecutar el algoritmo Random Forest #####################################

library(randomForest)

set.seed(7)   
rf <-  randomForest(m$formula,data = d_train[,-d_train$OUTPUT])

p_rf <- predict(rf, d_cv[,-d_cv$OUTPUT])

cor(p_rf,d_cv$OUTPUT)
RMSE(p_rf,d_cv$OUTPUT)

set.seed(2)
p_rf <- predict(rf, input[,-input$OUTPUT])

cor(p_rf,input$OUTPUT)
RMSE(p_rf,input$OUTPUT)

output_f <- read.csv('eth.csv')
output_f$date[nrow(output_f)] 
p_fut <- predict(rf, input_fut[NROW(input_fut),-input_fut$OUTPUT])

##################### Graficar los resultados #################################################################
                
jpeg('plot_xmr.jpg')
                
plot(input$OUTPUT, xlab = 'Días desde creación', ylab = 'Precio de Monero escalado ($)')
points(p_rf[1:nrow(input)],col=2)  
lines(p_rf[1:nrow(input)],col=2)  
lines(input$OUTPUT[1:nrow(input)])
                
dev.off()                 
 
######################## Resultados para mañana ###############################################################           
                
tomorrow_xmr <- p_fut - input$OUTPUT[nrow(input)]
write.csv(tomorrow_xmr,'tomorrow_xmr.csv')
                
###############################################################################################################              
###############################################################################################################
