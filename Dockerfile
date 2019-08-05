FROM debian
RUN apt-get update && apt-get install r-base wget lynx git -y
RUN  git clone https://github.com/progamandoconro/Bitcoin-prediction.git 
WORKDIR Bitcoin-prediction
RUN lynx -dump  https://coinmetrics.io/data-downloads/ | awk '/csv/{print $2}' > urls.txt && wget -N urls.txt -i && \
Rscript packages.R && Rscript script_ltc.R
