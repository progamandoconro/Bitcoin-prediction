## MR. RoBot 
Created by: Rodrigo Diaz-Lupanow
# Bitcoin (and other crypto)-prediction
Data downloaded from coinmetrics.io

This is a multi-plataform software (for Linux, Windows and Mac).

Required software: Docker. 

Docker will create a container running Debian and will install R (r-base), git, wget, lynx. After that it will download script from this github and install required R packages (randomForest, dplyr, MASS, lubridate, zoo). Finally, a script with the algorithm will be executated. Results for tomorrow are obtained using [sudo docker cp $dockerid:/Bitcoin-prediction/tomorrow.csv .]. If value is negative means price will be higher tomorrow, if value is positive means that price will be lower. 


![alt text](https://github.com/progamandoconro/Bitcoin-prediction/blob/master/bitcoin_5_ago_2019?raw=true)
