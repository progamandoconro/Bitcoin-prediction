git clone https://github.com/progamandoconro/Bitcoin-prediction

lynx -dump  https://coinmetrics.io/data-downloads/ | \
awk '/csv/{print $2}' > urls.txt  \
&& wget -N urls.txt -i 
