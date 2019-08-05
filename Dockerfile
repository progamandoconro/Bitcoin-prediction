FROM debian

COPY script.R /
COPY packages.R /

RUN apt-get update && apt-get install r-base wget lynx -y
RUN lynx -dump  https://coinmetrics.io/data-downloads/ | awk '/csv/{print $2}' > urls.txt
RUN wget -N urls.txt -i                   
RUN chmod 777 packages.R
RUN chmod 777 script.R
RUN packages.R
RUN script.R 
RUN head tomorrow.csv
