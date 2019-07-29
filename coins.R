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
              input_fut <- input
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
              
              input_fut$OUTPUT <- output
              
              input_fut <- select (input_fut,-starts_with("date"))%>%
                select(-starts_with("date."))%>%
                na.aggregate(by='Anio')%>%
                na.aggregate(by='Mes')%>%
                na.aggregate(by='Dia')
              
              
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
              
              # library(MASS)  
              
              # m = stepAIC(glm(d_train$OUTPUT~.,data = d_train[,-d_train$OUTPUT]))
              
              # detach("package:MASS", unload = TRUE)
              
             #################### Model result from stepAIC ##################################
             
               formula <-  d_train$OUTPUT ~ BlkCnt + BlkSizeByte + BlkSizeMeanByte + CapMVRVCur + 
                DiffMean + FeeMeanNtv + FeeMedNtv + IssContNtv + IssContPctAnn + 
                NVTAdj90 + ROI1yr + ROI30d + TxTfrValMedNtv + VtyDayRet180d + 
                VtyDayRet60d + BlkCnt.1 + BlkSizeByte.1 + BlkSizeMeanByte.1 + 
                CapRealUSD.1 + FeeMeanNtv.1 + IssContNtv.1 + IssContPctAnn.1 + 
                NVTAdj.1 + ROI1yr.1 + ROI30d.1 + SplyCur.1 + TxTfrValAdjNtv.1 + 
                TxTfrValMedNtv.1 + VtyDayRet180d.1 + VtyDayRet30d.1 + VtyDayRet60d.1 + 
                AdrActCnt.2 + BlkCnt.2 + IssContNtv.2 + IssContPctAnn.2 + 
                IssContUSD.2 + NVTAdj90.2 + PriceUSD.2 + SplyCur.2 + TxCnt.2 + 
                TxTfrValAdjNtv.2 + TxTfrValAdjUSD.2 + TxTfrValMedNtv.2 + 
                CapMVRVCur.2 + CapMrktCurUSD.3 + CapRealUSD.2 + DiffMean.3 + 
                FeeMeanNtv.3 + FeeMeanUSD.3 + FeeMedNtv.3 + FeeMedUSD.3 + 
                FeeTotNtv.3 + IssContNtv.3 + IssContPctAnn.3 + IssContUSD.3 + 
                NVTAdj.3 + NVTAdj90.3 + PriceBTC.3 + PriceUSD.3 + ROI1yr.3 + 
                TxTfrValMeanNtv.3 + TxTfrValMeanUSD.3 + TxTfrValMedNtv.3 + 
                TxTfrValNtv.3 + TxTfrValUSD.3 + VtyDayRet30d.3 + VtyDayRet60d.3 + 
                BlkCnt.4 + BlkSizeByte.4 + BlkSizeMeanByte.4 + CapMrktCurUSD.4 + 
                CapRealUSD.3 + DiffMean.4 + FeeMeanUSD.4 + IssContNtv.4 + 
                IssContUSD.4 + NVTAdj.4 + PriceBTC.4 + PriceUSD.4 + ROI1yr.4 + 
                ROI30d.4 + SplyCur.4 + TxCnt.4 + TxTfr.4 + TxTfrValMedUSD.4 + 
                TxTfrValNtv.4 + VtyDayRet30d.4 + IssTotNtv.5 + PriceBTC.5 + 
                TxTfrValMeanNtv.5 + TxTfrValNtv.5 + VtyDayRet30d.5 + BlkSizeByte.5 + 
                BlkSizeMeanByte.5 + CapMrktCurUSD.6 + FeeMedNtv.5 + IssContPctAnn.5 + 
                IssContUSD.5 + NVTAdj90.6 + PriceBTC.6 + PriceUSD.6 + ROI30d.6 + 
                SplyCur.6 + TxTfr.6 + TxTfrValMeanNtv.6 + TxTfrValMedNtv.6 + 
                TxTfrValMedUSD.6 + VtyDayRet180d.6 + VtyDayRet30d.6 + VtyDayRet60d.6 + 
                BlkSizeByte.6 + BlkSizeMeanByte.6 + CapMrktCurUSD.7 + FeeMedUSD.6 + 
                IssContNtv.6 + IssContPctAnn.6 + IssContUSD.6 + PriceUSD.7 + 
                SplyCur.7 + TxCnt.7 + TxTfr.7 + VtyDayRet180d.7 + VtyDayRet30d.7 + 
                VtyDayRet60d.7 + CapMrktCurUSD.8 + NVTAdj.7 + ROI1yr.8 + 
                TxTfrValMeanNtv.7 + VtyDayRet180d.8 + VtyDayRet60d.8 + Dia + 
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
         
         output_f <- read.csv('btc.csv')
         output_f$date[nrow(output_f)] 
         p_fut <- predict(rf, input_fut[NROW(input_fut),-input_fut$OUTPUT])
         
        ### Graficar los resultados 
         
        plot(input$OUTPUT[1800:nrow(input)],xlim = c(0,80), xlab = 'Últimos dos meses', ylab = 'Precio de Bitcoin escalado ($)')
        points(p_rf[1800:nrow(input)],col=2)  
        lines(p_rf[1800:nrow(input)],col=2)  
        lines(input$OUTPUT[1800:nrow(input)])
        text(x=nrow(input)-1799,
             y=input$OUTPUT[nrow(input)]-0.01,
             output_f$date[nrow(output_f)])
        text(x=50,y=0.51, paste( 'Precio del día',output_f$date[nrow(output_f)],'menos la predicción del día siguiente' ))
        text(x=50,y=0.5, p_fut - input$OUTPUT[nrow(input)])
         
 
 
 
  




