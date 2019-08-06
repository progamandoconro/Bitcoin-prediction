############### My docker container with an App to predict tomorrow's Litecoin price ##########################
# sudo docker build --build-arg .
FROM debian
RUN apt-get update && apt-get install r-base \
wget lynx git -y
ARG CACHEBUST=1
RUN  git clone https://github.com/progamandoconro/Bitcoin-prediction.git 
WORKDIR Bitcoin-prediction
RUN lynx -dump  https://coinmetrics.io/data-downloads/ | \
awk '/csv/{print $2}' > urls.txt && \
wget -N urls.txt -i && \
Rscript packages.R && \
Rscript script_ltc.R 
#sudo docker cp $container_id:/Bitcoin-prediction/tomorrow_ltc.csv .
#sudo docker cp $container_id:/Bitcoin-prediction/plot_ltc.jpg .
