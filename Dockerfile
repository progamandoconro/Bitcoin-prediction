############### My docker container with an App to predict tomorrow's Litecoin price ##########################

FROM debian

RUN apt-get update && apt-get install r-base \
wget lynx git imagemagick -y 

RUN echo "install.packages(c('MASS','dplyr', 'zoo','lubridate','randomForest'))" > packages_R.R && Rscript packages_R.R

RUN  git clone https://github.com/progamandoconro/Bitcoin-prediction.git 

WORKDIR Bitcoin-prediction

RUN lynx -dump  https://coinmetrics.io/data-downloads/ | \
awk '/csv/{print $2}' > urls.txt  \
&& wget -N urls.txt -i 

RUN Rscript script_LTC.R

