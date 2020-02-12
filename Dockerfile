############### My docker container to predict prices of crypto-currencies ##########################

FROM debian
RUN apt-get update && apt-get install r-base \
wget lynx git python-pip nano elinks -y 

RUN lynx -dump  https://coinmetrics.io/data-downloads/ | \
awk '/csv/{print $2}' > urls  \
&& wget -N urls -i 

RUN echo "install.packages(c('zoo','lubridate','dplyr','stringr'), repos='http://cran.us.r-project.org')" > libs_mineria.R && Rscript libs_mineria.R
RUN echo "install.packages(c('randomForest','e1071','neuralnet','caret','reticulate','keras'), repos='http://cran.us.r-project.org')" > libs_ML.R && Rscript libs_ML.R
RUN echo "install.packages (c('shiny','shinydashboard','ROSE','ggplot2'), repos='http://cran.us.r-project.org')" > lib_graf.R && Rscript lib_graf.R 






