############### My docker container to predict prices of crypto-currencies ##########################

FROM debian
RUN apt-get update && apt-get install r-base \
wget lynx git python-pip nano elinks -y 

RUN echo "install.packages(c('zoo','lubridate','dplyr','stringr'))" > libs_mineria.R && Rscript libs_mineria.R
RUN echo "install.packages(c('RandomForest','e1071','neuralnet','caret','reticulate','keras'))" > libs_ML.R && Rscript libs_ML.R
RUN echo "install.packages (c('shiny','shinydashboard','ROSE''ggplot2'))" > lib_grafs.R && Rscript lib_graf.R 

RUN  git clone https://github.com/progamandoconro/Bitcoin-prediction.git 
WORKDIR Bitcoin-prediction
RUN bash update_data.sh
