library(shiny) # App
library(shinydashboard) # Dashboard
library(dplyr) # Para facilitar la mineria de datos
library(reticulate) # Conectamos R con Python ('install.packages ("reticulate")' en R)
library(zoo) # Tratamiento para los datos faltantes
library(caret) # Selección de variables
library(lubridate) # Tratamiento para las fechas en la data

os <- import("os") # Utilizamos python para importar el modulo con comandos de nuestro sistema operativo, en mi caso, Linux
#os$system ("find *.csv -delete") # Borrar los archivos csv no actualizados
os$system ("lynx -dump  https://coinmetrics.io/data-downloads/ | \
awk '/csv/{print $2}' > urls") 
os$system("wget -N urls -i") # Utilizamos el programa wget para descargar la data actualizada (necesitas instalar wget en tu sistema, por ejemplo, con 'sudo apt-get install wget' o 'sudo yum install wget' )

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(selectInput("coin","Coin to predict:",
                              multiple = F, list.files(pattern = '.csv')),
                   h5("Tuning parameters"),
                   numericInput("p1","ntree",534),
                   numericInput("p2","mtry",sqrt(200)),
                   h5("*sqrt (mtry) "),
                   numericInput("p3","seed",777),
                   h5("Prediction"),
                   sliderInput("p4","Class probability",
                               min=0.1,max=0.9,value=0.499),
                   sliderInput("p5","Number of days in future to predict",min=1,max=12,value=1)
  ),
  dashboardBody(plotOutput('plot') )
)

server <- function(inputs, outputs) { 
  
  outputs$plot <- renderPlot({
    
   
  })
  
}

shinyApp(ui, server)

