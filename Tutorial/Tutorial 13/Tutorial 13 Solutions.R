options(scipen=999)
library(tidyverse)    #Essential Functions
library(modelr)
DATA=read_csv("AirWaterTemp.csv",col_types=cols()) #River Data

#1.1
NEST.DATA = DATA %>% group_by(L) %>% nest()
head(NEST.DATA)

#1.2
NEST.DATA %>% filter(L==103) %>% unnest() %>% glimpse()
NEST.DATA %>% filter(L!=103) %>% unnest() %>% glimpse()

#1.3
DATA2=DATA
DATA2$linpred=NA

TEST = NEST.DATA %>% filter(L==103) %>% unnest()
TRAIN  = NEST.DATA %>% filter(L!=103) %>% unnest()

linmod=lm(W~dplyr::lag(A,1),data=TRAIN)
linmodpred=predict(linmod,newdata=TEST)

DATA2$linpred[which(DATA2$L==103)]=linmodpred
head(DATA2)

#1.4
DATA2=DATA
DATA2$linpred=NA

for(k in unique(DATA2$L)){
  TEST = NEST.DATA %>% filter(L==k) %>% unnest()
  TRAIN  = NEST.DATA %>% filter(L!=k) %>% unnest()
  
  linmod=lm(W~A,data=TRAIN)
  linmodpred=predict(linmod,newdata=TEST)
  
  DATA2$linpred[which(DATA2$L==k)]=linmodpred
}

#1.5
RMSE.func=function(actual,predict){
  mse=mean((actual-predict)^2,na.rm=T)
  rmse=sqrt(mse)
  return(rmse)
}
RMSE.func(actual=DATA2$W,predict=DATA2$linpred)

