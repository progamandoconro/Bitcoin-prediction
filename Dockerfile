############### My docker container to predict prices of crypto-currencies ##########################

FROM debian
RUN apt-get update && apt-get install r-base \
wget lynx git python-pip nano elinks -y 


RUN echo '
https://coinmetrics.io/newdata/zrx.csv
https://coinmetrics.io/newdata/elf.csv
https://coinmetrics.io/newdata/ae.csv
https://coinmetrics.io/newdata/aion.csv
https://coinmetrics.io/newdata/ant.csv
https://coinmetrics.io/newdata/rep.csv
https://coinmetrics.io/newdata/bat.csv
https://coinmetrics.io/newdata/bnb.csv
https://coinmetrics.io/newdata/bnb_mainnet.csv
https://coinmetrics.io/newdata/bch.csv
https://coinmetrics.io/newdata/btg.csv
https://coinmetrics.io/newdata/bsv.csv
https://coinmetrics.io/newdata/btc.csv
https://coinmetrics.io/newdata/btm.csv
https://coinmetrics.io/newdata/ada.csv
https://coinmetrics.io/newdata/cvc.csv
https://coinmetrics.io/newdata/dai.csv
https://coinmetrics.io/newdata/dash.csv
https://coinmetrics.io/newdata/mana.csv
https://coinmetrics.io/newdata/dcr.csv
https://coinmetrics.io/newdata/dgb.csv
https://coinmetrics.io/newdata/doge.csv
https://coinmetrics.io/newdata/drgn.csv
https://coinmetrics.io/newdata/eng.csv
https://coinmetrics.io/newdata/eos.csv
https://coinmetrics.io/newdata/etc.csv
https://coinmetrics.io/newdata/eth.csv
https://coinmetrics.io/newdata/ethos.csv
https://coinmetrics.io/newdata/fun.csv
https://coinmetrics.io/newdata/gas.csv
https://coinmetrics.io/newdata/gusd.csv
https://coinmetrics.io/newdata/gno.csv
https://coinmetrics.io/newdata/gnt.csv
https://coinmetrics.io/newdata/grin.csv
https://coinmetrics.io/newdata/icx.csv
https://coinmetrics.io/newdata/kcs.csv
https://coinmetrics.io/newdata/knc.csv
https://coinmetrics.io/newdata/lsk.csv
https://coinmetrics.io/newdata/ltc.csv
https://coinmetrics.io/newdata/loom.csv
https://coinmetrics.io/newdata/lrc.csv
https://coinmetrics.io/newdata/maid.csv
https://coinmetrics.io/newdata/mkr.csv
https://coinmetrics.io/newdata/xmr.csv
https://coinmetrics.io/newdata/nas.csv
https://coinmetrics.io/newdata/xem.csv
https://coinmetrics.io/newdata/neo.csv
https://coinmetrics.io/newdata/omg.csv
https://coinmetrics.io/newdata/pax.csv
https://coinmetrics.io/newdata/pivx.csv
https://coinmetrics.io/newdata/poly.csv
https://coinmetrics.io/newdata/ppt.csv
https://coinmetrics.io/newdata/powr.csv
https://coinmetrics.io/newdata/qash.csv
https://coinmetrics.io/newdata/qtum.csv
https://coinmetrics.io/newdata/snt.csv
https://coinmetrics.io/newdata/xlm.csv
https://coinmetrics.io/newdata/pay.csv
https://coinmetrics.io/newdata/usdt_eth.csv
https://coinmetrics.io/newdata/usdt.csv
https://coinmetrics.io/newdata/xtz.csv
https://coinmetrics.io/newdata/trx.csv
https://coinmetrics.io/newdata/tusd.csv
https://coinmetrics.io/newdata/usdc.csv
https://coinmetrics.io/newdata/vet.csv
https://coinmetrics.io/newdata/xvg.csv
https://coinmetrics.io/newdata/vtc.csv
https://coinmetrics.io/newdata/wtc.csv
https://coinmetrics.io/newdata/waves.csv
https://coinmetrics.io/newdata/xrp.csv
https://coinmetrics.io/newdata/zec.csv
https://coinmetrics.io/newdata/zil.csv' > urls

RUN lynx -dump  https://coinmetrics.io/data-downloads/ | \
awk '/csv/{print $2}' > urls  \
&& wget -N urls -i 

RUN echo "install.packages(c('zoo','lubridate','dplyr','stringr'))" > libs_mineria.R && Rscript libs_mineria.R
RUN echo "install.packages(c('randomForest','e1071','neuralnet','caret','reticulate','keras'))" > libs_ML.R && Rscript libs_ML.R
RUN echo "install.packages (c('shiny','shinydashboard','ROSE''ggplot2'))" > lib_graf.R && Rscript lib_graf.R 






