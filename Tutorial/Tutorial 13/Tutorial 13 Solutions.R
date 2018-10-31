options(scipen=999)
library(tidyverse)    #Essential Functions
library(modelr)
DATA=read_csv("AirWaterTemp.csv",col_types=cols()) #River Data

#1.1
NEST.DATA = DATA %>% group_by(L) %>% nest()
head(NEST.DATA)
