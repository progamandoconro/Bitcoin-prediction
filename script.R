
library(dplyr) # Para facilitar la mineria de datos
library(zoo) # Tratamiento para los datos faltantes
library(caret) # Selección de variables
library(lubridate) # Tratamiento para las fechas en la data

# Utilizamos el precio en $ del Bitcoin como variable respuesta, la que nos interesa predecir.

output <- read.csv('btc.csv')

# Debido a que los archivos csv descargados tienen diferentes numeros de filas o casos, 

n_i <- 2000 ; n_f <- NROW(output); n <- n_f-n_i +1

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
input <- input [-1:-2,]
input$OUTPUT <- output [-(NROW(output)-1):-NROW(output)]

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

plot(input$OUTPUT[1800:nrow(input)], xlab = 'Últimos dos meses', ylab = 'Precio de Bitcoin escalado ($)',cex=0.05)
points(p_rf[1800:nrow(input)],col=2,cex=0.05)  
lines(p_rf[1800:nrow(input)],col=2)  
lines(input$OUTPUT[1800:nrow(input)])
text(x=30,y=0.42, paste( 'Predicción para el día siguiente a',output_f$date[nrow(output_f)],'menos el valor real de ese día ' ))
text(x=30,y=0.4, p_fut - input$OUTPUT[nrow(input)])


tomorrow <- p_fut - input$OUTPUT[nrow(input)]
write.csv(tomorrow,'tomorrow.csv')