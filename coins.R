  # Para obtener los urls de los documentos csv disponibles en coinmetrics utilizamos el promagrama lynx (sudo apt-get install lynx o sudo yum install lynx) y awk para seleccionar las lineas con .csv en el código html5 de la web
  # sudo lynx -dumb https://coinmetrics.io/data-downloads/ | awk '/csv/{print $2}' > urls.txt
  # colocamos el archivo "urls.txt" en la carpeta donde queramos almacenar la data a descargar 
    
    
    library(dplyr) # Para facilitar la mineria de datos
    library(reticulate) # Conectamos R con Python ('install.packages ("reticulate")' en R)
    library(zoo) # Tratamiento para los datos faltantes
    library(caret) # Selección de variables
    library(lubridate) # Tratamiento para las fechas en la data
    library(SparkR) # Paralelización de los procesos
   
    
    setwd('~/Dropbox/DataScience/coinmetrics/') # Asignamos la carpeta de trabajado donde tenemos el archivo con las urls
    
    os <- import("os") # Utilizamos python para importar el modulo con comandos de nuestro sistema operativo, en mi caso, Linux
    os$system ("wget -N urls.txt -i  ") # Utilizamos el programa wget para descargar la data (necesitas instalar wget en tu sistema, por ejemplo, con 'sudo apt-get install wget' o 'sudo yum install wget' )
    
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
   
  # Agreguemos data de la fecha a nuestras variables y eliminemos las variables repetidas con las fechas. Además, reemplazamos los datos faltantes por la media de año, si continuan habiendo NA, por la media mensual y diaria. De esta forma aseguramos que no haya datos faltantes en nuestro set.
    
    input$Dia <- day(input$date) ; input$Mes <- month(input$date) ; input$Anio <- year(input$date)
   input$OUTPUT <- output
    
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
    
#   Seleccionamos el modelo con más información, evitando las variables redundantes.  

    library(MASS) # Selección de modelo
    
   feat_impor <-  stepAIC(lm(input$OUTPUT~.,data = input[,-input$OUTPUT]))
  
   summary( lm( feat_impor$formula, data = input) )
