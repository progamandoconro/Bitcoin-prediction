# MR. RoBot. Bitcoin (and other crypto)-predictor
### Created by: Rodrigo Diaz-Lupanow

Data downloaded from coinmetrics.io

This is a multi-plataform software (for Linux, Windows and Mac).

Required software: Docker. 

Docker will create a container running Debian and will install R (r-base), git, wget, lynx. After that it will download script from this github and install required R packages (randomForest, dplyr, MASS, lubridate, zoo). Finally, a script with the algorithm will be executated. 

![alt text](https://github.com/progamandoconro/Bitcoin-prediction/blob/master/bitcoin_5_ago_2019?raw=true)

Results for tomorrow are obtained using [sudo docker cp $dockerid:/Bitcoin-prediction/tomorrow.csv .]. If value is negative means price will be higher tomorrow, if value is positive means that price will be lower. 

App still in developement. Final appearence will be like this: 

![alt text](https://raw.githubusercontent.com/progamandoconro/Bitcoin-prediction/master/Screenshot_20190727_203738.png)

I am looking for support to finish this project. If you are interested on funding Mr. Robot, please write to programandoconro@gmail.com, or visit programandonconro.wordpress.com/contact. 
