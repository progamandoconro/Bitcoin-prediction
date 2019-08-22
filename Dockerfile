############### My docker container to predict prices of crypto-currencies ##########################

FROM debian

RUN apt-get update && apt-get install r-base \
wget lynx git -y 

RUN echo "install.packages(c('MASS','dplyr', 'zoo','lubridate','randomForest'))" > packages_R.R && Rscript packages_R.R

RUN  git clone https://github.com/progamandoconro/Bitcoin-prediction.git 

WORKDIR Bitcoin-prediction

RUN bash update_data.sh
