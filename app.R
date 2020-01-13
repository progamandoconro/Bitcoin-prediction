library(shiny)
library(shinydashboard) 
library(dplyr) 
library(reticulate) 
library(zoo) 
library(caret) 
library(lubridate) 

normalize <- function(x) { 
      return((x - min(x)) / (max(x) - min(x)))
    }

############################# UPDATED DATA DOWNLOAD ##########################################
lynx and wget are dependencies 
os <- import("os")
os$system ("find *.csv -delete") # Delete outdated data (.csv files)
os$system ("lynx -dump  https://coinmetrics.io/data-downloads/ | \
awk '/csv/{print $2}' > urls") 
os$system("wget -N urls -i") 

####################################### UI ####################################################
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(selectInput("coin","Coin to predict:",
                              multiple = F, list.files(pattern = '.csv')),
                   h5("Tuning parameters"),
                   numericInput("p1","ntree",534),
                   numericInput("p2","mtry",sqrt(200)),
                   h5("*sqrt (mtry) "),
                   numericInput("seed","seed",777),
                   h5("Prediction"),
                   sliderInput("p4","Class probability",
                               min=0.1,max=0.9,value=0.499),
                   sliderInput("fut_days","Number of days in future to predict",min=1,max=12,value=1)
  ),
  dashboardBody(plotOutput('plot') )
)

############################################ SERVER #################################################
server <- function(inputs, outputs) { 
  
  outputs$plot <- renderPlot({
    
    output <- read.csv(inputs$coin)
    n_to_eval = NROW(output)
    
    n_i <- 1 
    n_f <- NROW(output)   
    n <- n_f-n_i +1
    
    output <- read.csv(inputs$coin)$PriceUSD[n_i:n_f]
    input <- list.files(pattern = ".csv") %>%
      lapply (read.csv) 
    
    crip <- sapply (input, function(x) nrow(x) < (n_f - n_i) )
    input <- input [!crip]%>%
      lapply (function(x) x[nrow(x)-(nrow(x)-n):nrow(x),  ] )
    
    input <- do.call(cbind, input)
    colnames(input)<- make.unique( colnames(input))
    
    input$Dia <- day(input$date) ; input$Mes <- month(input$date) ; input$Anio <- year(input$date)
    input_fut <- input
    input <- input [-inputs$fut_days,]
    
    input$OUTPUT <- output [-NROW(output)]
    
    input <- select (input,-starts_with("date"))%>%
      select(-starts_with("date."))%>%
      na.aggregate(by='Anio')%>%
      na.aggregate(by='Mes')%>%
      na.aggregate(by='Dia')%>%
      na.aggregate()
              
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
    input<- lapply(input, normalize)%>%
      as.data.frame()
    input_fut<- lapply(input_fut, normalize)%>%
      as.data.frame()
    
    set.seed(inputs$seed)
    alea <- sample(1:nrow(input),nrow(input))
    ix <- alea[1: floor(NROW(alea)*0.7)]
    d_train <- input[ix,]
    d_cv <- input[-ix,]
    
    library(randomForest)
    set.seed(inputs$seed)   
    rf <-  randomForest(d_train$OUTPUT~.,data = d_train[,-d_train$OUTPUT])
    p_fut <- predict(rf, input_fut[,-input_fut$OUTPUT])
    
    plot(input_fut$OUTPUT, xlab = 'Días desde creación', ylab = paste('Precio de',inputs$coins, ' escalado ($)'))
    points(p_fut,col=2)  
    lines(p_fut,col=2)  
    lines(input_fut$OUTPUT)
    
  })
  
}
################################## APP #################################################
shinyApp(ui, server)

