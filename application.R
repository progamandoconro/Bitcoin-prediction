library(shiny)
library(shinydashboard)
library(randomForest)
library(ggplot2)
library(lubridate)
library(dplyr)
library(zoo)

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

####################
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

set.seed(7)   
rf <-  randomForest(formula,data = d_train[,-d_train$OUTPUT])

p_rf <- predict(rf, input[,-input$OUTPUT])
p_fut <- predict(rf, input_fut[nrow(input_fut),-input_fut$OUTPUT])

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(checkboxInput("calibration","Calibración de modelo" ),
                   selectInput("coin","Criptomoneda a seleccionar:",multiple = T, c('Bitcoin','Monero','Etherium')),
                   selectInput("algoritmos","Algoritmos a seleccionar:",multiple = T, c('Neural Network','Random Forest','SVM','Deep Learning','RLM')),
                   checkboxInput("bagging","Bagging" ),
                   checkboxInput("ensamble","Ensamble" )),
  dashboardBody(plotOutput('plot'),
                h5(p_fut))
)

server <- function(inputs, outputs) { 
  
  outputs$plot <- renderPlot({
    
    g <- ggplot(data = input[1800:nrow(input),],aes(x=1800:nrow(input),y=input$OUTPUT[1800:nrow(input)]))
    g+ geom_point()+geom_line()+ 
      geom_point(aes(y=p_rf[1800:nrow(input)]),col="red")+
      geom_line(aes(y=p_rf[1800:nrow(input)]),col="red")+
      xlab('Tiempo(días)')+ylab("Valor escalado")

  })
  
 
}

shinyApp(ui, server)
