############################################################################################################
################################################## Ethereum ##################################################
############################################################################################################

library(dplyr) # Para facilitar la mineria de datos
library(reticulate) # Conectamos R con Python ('install.packages ("reticulate")' en R)
library(zoo) # Tratamiento para los datos faltantes
library(caret) # Selección de variables
library(lubridate) # Tratamiento para las fechas en la data

# Utilizamos el precio en $ del Ethereum  como variable respuesta, la que nos interesa predecir.

output <- read.csv('eth.csv')

# Debido a que los archivos csv descargados tienen diferentes numeros de filas o casos, 

n_to_eval = NROW(output)

n_i <- 1 ; n_f <- NROW(output)  ; n <- n_f-n_i +1

output <- read.csv('eth.csv')$PriceUSD[n_i:n_f]

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
  CapMrktCurUSD + CapRealUSD + FeeTotNtv + FeeTotUSD + IssContNtv + 
  NVTAdj + PriceUSD + ROI1yr + ROI30d + SplyCur + TxTfr + VtyDayRet180d + 
  VtyDayRet30d + VtyDayRet60d + BlkCnt.1 + BlkSizeByte.1 + 
  BlkSizeMeanByte.1 + CapRealUSD.1 + FeeMeanNtv.1 + FeeMedNtv.1 + 
  FeeMedUSD.1 + FeeTotNtv.1 + FeeTotUSD.1 + IssTotUSD.1 + PriceBTC.1 + 
  PriceUSD.1 + ROI1yr.1 + ROI30d.1 + SplyCur.1 + TxCnt.1 + 
  TxTfrValAdjNtv.1 + TxTfrValAdjUSD.1 + VtyDayRet180d.1 + VtyDayRet30d.1 + 
  VtyDayRet60d.1 + BlkCnt.2 + FeeMeanNtv.2 + FeeMeanUSD.2 + 
  FeeMedNtv.2 + FeeTotNtv.2 + FeeTotUSD.2 + IssContPctAnn.2 + 
  NVTAdj90.2 + PriceBTC.2 + ROI1yr.2 + SplyCur.2 + TxTfrValMeanNtv.2 + 
  TxTfrValNtv.2 + VtyDayRet180d.2 + VtyDayRet30d.2 + BlkSizeByte.3 + 
  BlkSizeMeanByte.3 + CapMVRVCur.2 + CapRealUSD.2 + IssContPctAnn.3 + 
  IssContUSD.3 + PriceUSD.3 + ROI1yr.3 + SplyCur.3 + TxCnt.3 + 
  TxTfr.3 + TxTfrValNtv.3 + TxTfrValUSD.3 + VtyDayRet180d.3 + 
  VtyDayRet30d.3 + VtyDayRet60d.3 + BlkCnt.4 + FeeMeanNtv.4 + 
  FeeMeanUSD.4 + FeeMedUSD.4 + FeeTotUSD.4 + IssContNtv.4 + 
  PriceBTC.4 + SplyCur.4 + TxTfrValAdjNtv.4 + TxTfrValMeanNtv.4 + 
  TxTfrValMeanUSD.4 + TxTfrValMedUSD.4 + TxTfrValUSD.4 + VtyDayRet180d.4 + 
  VtyDayRet30d.4 + CapMVRVCur.3 + CapMrktCurUSD.5 + CapRealUSD.3 + 
  DiffMean.5 + FeeMeanUSD.5 + FeeTotNtv.5 + FeeTotUSD.5 + NVTAdj90.5 + 
  PriceUSD.5 + SplyCur.5 + TxCnt.5 + TxTfrValAdjNtv.5 + TxTfrValMeanNtv.5 + 
  TxTfrValMeanUSD.5 + TxTfrValMedNtv.5 + TxTfrValUSD.5 + VtyDayRet30d.5 + 
  VtyDayRet60d.5 + AdrActCnt.6 + IssTotNtv.6 + NVTAdj90.6 + 
  ROI1yr.6 + ROI30d.6 + TxTfrValAdjNtv.6 + TxTfrValAdjUSD.6 + 
  TxTfrValMeanNtv.6 + TxTfrValMedNtv.6 + TxTfrValMedUSD.6 + 
  TxTfrValNtv.6 + TxTfrValUSD.6 + VtyDayRet30d.6 + VtyDayRet60d.6 + 
  IssTotNtv.7 + IssTotUSD.7 + NVTAdj.7 + PriceBTC.7 + SplyCur.7 + 
  TxTfrValMedNtv.7 + TxTfrValMedUSD.7 + VtyDayRet180d.7 + VtyDayRet30d.7 + 
  FeeMeanNtv.6 + FeeMedNtv.6 + FeeMedUSD.6 + FeeTotNtv.6 + 
  IssContPctAnn.6 + NVTAdj.8 + NVTAdj90.8 + ROI30d.8 + SplyCur.8 + 
  TxTfrValAdjNtv.8 + TxTfrValMedNtv.8 + TxTfrValNtv.8 + VtyDayRet60d.8 + 
  BlkCnt.7 + CapMrktCurUSD.9 + NVTAdj90.9 + PriceBTC.9 + ROI30d.9 + 
  TxTfrValAdjNtv.9 + TxTfrValAdjUSD.9 + TxTfrValMedNtv.9 + 
  VtyDayRet180d.9 + VtyDayRet60d.9 + DiffMean.7 + FeeMeanNtv.8 + 
  ROI1yr.10 + SplyCur.10 + TxCnt.10 + TxTfr.10 + VtyDayRet180d.10 + 
  VtyDayRet60d.10 + AdrActCnt.10 + BlkCnt.9 + CapMrktCurUSD.11 + 
  FeeMeanNtv.9 + FeeMeanUSD.9 + FeeMedNtv.9 + FeeMedUSD.9 + 
  FeeTotNtv.9 + NVTAdj.10 + NVTAdj90.10 + PriceBTC.11 + PriceUSD.11 + 
  ROI30d.11 + SplyCur.11 + TxTfr.11 + TxTfrValMeanUSD.10 + 
  VtyDayRet180d.11 + VtyDayRet30d.11 + VtyDayRet60d.11 + BlkCnt.10 + 
  FeeMeanUSD.10 + FeeTotNtv.10 + IssContNtv.9 + IssContPctAnn.9 + 
  NVTAdj.11 + PriceBTC.12 + ROI1yr.12 + ROI30d.12 + SplyCur.12 + 
  TxTfr.12 + TxTfrValAdjNtv.11 + Dia + Mes + Anio



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

### Graficar los resultados 

plot(input$OUTPUT, xlab = 'Días desde creación', ylab = 'Precio de Ethereum escalado ($)')
points(p_rf[1:nrow(input)],col=2)  
lines(p_rf[1:nrow(input)],col=2)  
lines(input$OUTPUT[1:nrow(input)])
