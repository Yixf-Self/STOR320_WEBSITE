options(scipen=999)
library(tidyverse)    #Essential Functions
library(rvest)        #Read Tables From Webpages
library(noncensus)    #Contains Zip Codes for US Cities


#1.1
x = 3
if(x > 0){
  print(log(x))
}
x = -3
if(x > 0){
  print(log(x))
}


#1.2
x = 3
if(x > 0){
  print(log(x))
} else{
  message("Unable to Take Logarithm")
}
x = -3
if(x > 0){
  print(log(x))
} else {
  message("Unable to Take Logarithm")
}


#1.3
x="Hallelujah"
if(x > 0){
  print(log(x))
}

if(x > 0){
  print(log(x))
} else{
  message("Unable to Take Logarithm")
}


#1.4
x="Hallelujah"
if(is.numeric(x)){
  if(x > 0){
    print(log(x))
  } else{
    message("Unable to Take Logarithm")
  }
} else{
  message("No Strings Attached")
}


#1.5
x=c(-1,3,200)
print(log(x))

y1 =  if(x > 0){
  log(x)
} else{
  NA
}
print(y1)

y2 = ifelse(x>0,log(x),NA)
print(y2)


#1.6
x=rnorm(1000,mean=0,sd=1)
y=ifelse(abs(x)<1,"Within 1 SD",ifelse(abs(x)>2,"Far Far Away","Between 1 and 2 SD"))
y.fct=factor(y,levels=c("Within 1 SD","Between 1 and 2 SD","Far Far Away"))
ggplot() +
  geom_bar(aes(x=y.fct),fill="lightskyblue1") +
  theme_minimal()
















