
############################################################################################################
################################################## Litecoin ##################################################
############################################################################################################

library(dplyr) # Para facilitar la mineria de datos
library(reticulate) # Conectamos R con Python ('install.packages ("reticulate")' en R)
library(zoo) # Tratamiento para los datos faltantes
library(caret) # Selección de variables
library(lubridate) # Tratamiento para las fechas en la data

setwd('~/Dropbox/DataScience/coinmetrics/') # Asignamos la carpeta de trabajado donde tenemos el archivo con las urls

os <- import("os") # Utilizamos python para importar el modulo con comandos de nuestro sistema operativo, en mi caso, Linux
os$system ("find *.csv -delete") # Borrar los archivos csv no actualizados
os$system ("wget -N urls.txt -i  ") # Utilizamos el programa wget para descargar la data actualizada (necesitas instalar wget en tu sistema, por ejemplo, con 'sudo apt-get install wget' o 'sudo yum install wget' )

# Utilizamos el precio en $ del Ethereum  como variable respuesta, la que nos interesa predecir.

output <- read.csv('ltc.csv')

# Debido a que los archivos csv descargados tienen diferentes numeros de filas o casos, 

n_to_eval = NROW(output)

n_i <- 1 ; n_f <- NROW(output)  ; n <- n_f-n_i +1

output <- read.csv('ltc.csv')$PriceUSD[n_i:n_f]

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

plot(input$OUTPUT, xlab = 'Días desde creación', ylab = 'Precio de Litecoin escalado ($)')
points(p_rf[1:nrow(input)],col=2)  
lines(p_rf[1:nrow(input)],col=2)  
lines(input$OUTPUT[1:nrow(input)])
