   # Para obtener los urls de los documentos csv disponibles en coinmetrics utilizamos el promagrama lynx (sudo apt-get install lynx o sudo yum install lynx) y awk para seleccionar las lineas con .csv en el código html5 de la web
    # sudo lynx -dumb https://coinmetrics.io/data-downloads/ | awk '/csv/{print $2}' > urls.txt
    # colocamos el archivo "urls.txt" en la carpeta donde queramos almacenar la data a descargar 
    
    
    library(dplyr) # Para facilitar la mineria de datos
    library(reticulate) # Conectamos R con Python ('install.packages ("reticulate")' en R)
    library(zoo) # Tratamiento para los datos faltantes
    library(caret) # Selección de variables
    library(lubridate) # Tratamiento para las fechas en la data
   
    
      setwd('~/Dropbox/DataScience/coinmetrics/') # Asignamos la carpeta de trabajado donde tenemos el archivo con las urls
    
     os <- import("os") # Utilizamos python para importar el modulo con comandos de nuestro sistema operativo, en mi caso, Linux
         os$system ("find *.csv -delete") # Borrar los archivos csv no actualizados
     os$system ("wget -N urls.txt -i  ") # Utilizamos el programa wget para descargar la data actualizada (necesitas instalar wget en tu sistema, por ejemplo, con 'sudo apt-get install wget' o 'sudo yum install wget' )
    
    # Utilizamos el precio en $ del Bitcoin como variable respuesta, la que nos interesa predecir.
    
    output <- read.csv('btc.csv')
    
    # Debido a que los archivos csv descargados tienen diferentes numeros de filas o casos, 
    
    n_to_eval = 2000
    
    n_i <- 1 ; n_f <- NROW(output) - n_to_eval ; n <- n_f-n_i +1
    
    output <- read.csv('btc.csv')$PriceUSD[n_i:n_f]
    
    # Utilizamos los archivos descargados para seleccionar las variables explicatorias o de entrada para el algoritmo.
    # Para eso, listamos todos archivos csv y leemos cada uno con 'read.csv' gracias a 'lapply'
    
    input <- list.files(pattern = ".csv") %>%
      lapply (read.csv) 
    
    # Seleccionamos las variables explicativas que tengan el mismo número de casos que la variable respuesta, las cortamos en funcion de tales casos y  finalmente unimos todas la variables en una sola data.frame
    
    crip <- sapply (input, function(x) nrow(x) < (n_f - n_i) + 1)
    
    input <- input [!crip]%>%
      lapply (function(x) x[nrow(x)-(nrow(x)-n):nrow(x),  ] )
    
    # Unir las columnas de la lista en una sola data.frame y cambiar los nombres de las columnas repetidas
    
    input <- do.call(cbind, input)
    
    colnames(input)<- make.unique( colnames(input))
    
    # Agreguemos data de la fecha a nuestras variables. Creamos los datos cruzados para tener las predicciones de mañana con los datos de hoy. liminemos las variables repetidas con las fechas. Además, reemplazamos los datos faltantes por la media de año, si continuan habiendo NA, por la media mensual y diaria. De esta forma aseguramos que no haya datos faltantes en nuestro set.
    
    input$Dia <- day(input$date) ; input$Mes <- month(input$date) ; input$Anio <- year(input$date)
    input <- input [-1,]
    input$OUTPUT <- output [-NROW(output)]
    
    input <- select (input,-starts_with("date"))%>%
      select(-starts_with("date."))%>%
      na.aggregate(by='Anio')%>%
      na.aggregate(by='Mes')%>%
      na.aggregate(by='Dia')
    
    # Vamos a eliminar la data redundante 
    
    foo <- function(input) {
      out <- lapply(input, function(x) length(unique(x)))
      want <- which(!out > 1)
      unlist(want)
    }
    
    input <- input[,-foo(input)]
    
    # normalizamos la data entre valores 0 a 1
    
    normalize <- function(x) { 
      return((x - min(x)) / (max(x) - min(x)))
    }
    input<- lapply(input, normalize)%>%
      as.data.frame()
    
  
    
    # Data aleatoria y cortar seccion de validacion
    set.seed(7)
    alea <- sample(1:nrow(input),nrow(input))
    ix <- alea[1: floor(NROW(alea)*0.7)]
    
    d_train <- input[ix,]
    d_cv <- input[-ix,]
    
    ####################
    
    
    library(MASS)  
    
    m = stepAIC(glm(d_train$OUTPUT~.,data = d_train[,-d_train$OUTPUT]))
    
    detach("package:MASS")
     
  model <-lm(m$formula,data = d_train[,-d_train$OUTPUT])
    
 p <- predict(model,d_cv[,-d_cv$OUTPUT])
  
cor(p,d_cv$OUTPUT)
RMSE(p,d_cv$OUTPUT)  
  
  library(neuralnet)

set.seed(7)   
 net <-  neuralnet(m$formula,data = d_train[,-d_train$OUTPUT], hidden = 1)
  
 p_net <- compute(net, d_cv)

 cor(p_net$net.result,d_cv$OUTPUT)
 RMSE(p_net$net.result,d_cv$OUTPUT)
 
 
 library(e1071)
 
 set.seed(7)   
 svm <-  svm(formula,data = d_train[,-d_train$OUTPUT])
 
 p_svm <- predict(svm, d_cv[,-d_cv$OUTPUT])
 
 cor(p_svm,d_cv$OUTPUT)
 RMSE(p_svm,d_cv$OUTPUT)
 
 
 library(randomForest)
 
 set.seed(7)   
 rf <-  randomForest(m$formula,data = d_train[,-d_train$OUTPUT])
 
 p_rf <- predict(rf, d_cv[,-d_cv$OUTPUT])
 
 cor(p_rf,d_cv$OUTPUT)
 RMSE(p_rf,d_cv$OUTPUT)
 
 set.seed(7)
 p_rf <- predict(rf, input[,-input$OUTPUT])
 
 cor(p_rf,input$OUTPUT)
 RMSE(p_rf,input$OUTPUT)

 plot(input$OUTPUT[1800:nrow(input)])
points(p_rf[1800:nrow(input)],col=2)  
lines(p_rf[1800:nrow(input)],col=2)  
lines(input$OUTPUT[1800:nrow(input)])
 
 
 m$formula
 
formula <-  d_train$OUTPUT ~ DiffMean + IssContPctAnn + IssContUSD + NVTAdj90 + 
   ROI1yr + ROI30d + VtyDayRet180d + VtyDayRet30d + CapMVRVCur.1 + 
   CapRealUSD.1 + FeeMeanNtv.1 + FeeTotNtv.1 + IssContNtv.1 + 
   IssContPctAnn.1 + SplyCur.1 + TxTfrValMeanUSD.1 + TxTfrValMedNtv.1 + 
   VtyDayRet180d.1 + VtyDayRet30d.1 + VtyDayRet60d.1 + DiffMean.2 + 
   FeeMedUSD.2 + FeeTotNtv.2 + IssContPctAnn.2 + NVTAdj90.2 + 
   PriceUSD.2 + TxTfr.2 + TxTfrValAdjNtv.2 + TxTfrValAdjUSD.2 + 
   TxTfrValMedNtv.2 + VtyDayRet180d.2 + VtyDayRet30d.2 + VtyDayRet60d.2 + 
   BlkSizeByte.3 + BlkSizeMeanByte.3 + CapMrktCurUSD.3 + FeeMeanNtv.3 + 
   FeeMedNtv.3 + IssContNtv.3 + IssContPctAnn.3 + IssContUSD.3 + 
   NVTAdj90.3 + PriceBTC.3 + PriceUSD.3 + ROI30d.3 + TxCnt.3 + 
   TxTfr.3 + TxTfrValMeanNtv.3 + TxTfrValMeanUSD.3 + TxTfrValMedNtv.3 + 
   TxTfrValNtv.3 + TxTfrValUSD.3 + VtyDayRet180d.3 + VtyDayRet30d.3 + 
   VtyDayRet60d.3 + BlkCnt.4 + CapRealUSD.3 + DiffMean.4 + FeeMeanNtv.4 + 
   FeeMedNtv.4 + FeeMedUSD.4 + IssContPctAnn.4 + NVTAdj.4 + 
   NVTAdj90.4 + PriceUSD.4 + ROI1yr.4 + SplyCur.4 + TxCnt.4 + 
   TxTfrValMeanNtv.4 + TxTfrValMeanUSD.4 + TxTfrValMedNtv.4 + 
   TxTfrValNtv.4 + VtyDayRet60d.4 + IssTotNtv.5 + PriceBTC.5 + 
   ROI30d.5 + TxTfrValMedNtv.5 + TxTfrValMedUSD.5 + TxTfrValUSD.5 + 
   VtyDayRet30d.5 + CapMrktCurUSD.6 + FeeMeanNtv.5 + FeeMedUSD.5 + 
   FeeTotNtv.5 + IssContPctAnn.5 + NVTAdj90.6 + PriceBTC.6 + 
   PriceUSD.6 + ROI30d.6 + SplyCur.6 + TxCnt.6 + TxTfr.6 + TxTfrValMedUSD.6 + 
   VtyDayRet180d.6 + VtyDayRet30d.6 + BlkCnt.6 + CapMrktCurUSD.7 + 
   DiffMean.6 + IssContNtv.6 + IssContPctAnn.6 + IssContUSD.6 + 
   PriceBTC.7 + PriceUSD.7 + ROI1yr.7 + ROI30d.7 + TxCnt.7 + 
   TxTfr.7 + VtyDayRet180d.7 + VtyDayRet30d.7 + BlkCnt.7 + FeeMedNtv.7 + 
   NVTAdj.7 + NVTAdj90.7 + PriceUSD.8 + ROI1yr.8 + SplyCur.8 + 
   TxTfrValMeanNtv.7 + VtyDayRet30d.8 + VtyDayRet60d.8 + Dia + 
   Mes + Anio
 
 
 
 
 
  




