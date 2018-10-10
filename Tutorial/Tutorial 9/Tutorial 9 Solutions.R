options(scipen=999)
library(tidyverse)    #Essential Functions
library(Ecdat)    #Contains Economic Datasets


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


#2.1
a=BLANK #Any Number
r=BLANK #Any Number Between -1 and 1: abs(r)<1

theoretical.limit=a/(1-r)

START=a

FINISH.1 = START + a*r^1

FINISH.2 = FINISH.1 + a*r^2

FINISH.3 = FINISH.2 + a*r^3

FINISH.10 = a 
for(k in 1:10){
  FINISH.10=FINISH.10+a*r^k
}

FINISH.100 = a 
for(k in 1:100){
  FINISH.100=FINISH.100+a*r^k
}

DATA = tibble(k=c(1,2,3,10,100,"Infinity"),
              SUMMATION=c(FINISH.1,FINISH.2,FINISH.3,
                          FINISH.10,FINISH.100,
                          theoretical.limit))
print(DATA)

ABSOLUTE.ERROR = abs(FINISH.100-theoretical.limit)
print(ABSOLUTE.ERROR)


#2.2
a=10
r=-2

FINISH=a
k=0
while(abs(FINISH-a/(1-r)) > 1e-10) {
  k=k+1
  FINISH = FINISH + a*r^k
  if(k>100) break
}
print(c(k,FINISH))


#2.3
a=10
r=-0.75
theoretical.limit=a/(1-r)

K=20 #How Many Steps Do You Want to Save?

summation=rep(NA,(K+1))
summation[1]=a
for (k in 1:K) {
  summation[k+1]=summation[k] + a*r^k
}

ggplot() +
  geom_line(aes(x=1:(K+1),y=summation)) +
  geom_hline(yintercept=theoretical.limit,
             linetype="dashed")


#2.4
Cigar=Ecdat::Cigar
head(Cigar)
print(round(cor(Cigar),3))

#2.5
CORR.CIGAR1=matrix(NA,9,9) #Empty Matrix Initialized
rownames(CORR.CIGAR1)=names(Cigar)
colnames(CORR.CIGAR1)=names(Cigar)

seq_along(names(Cigar))

for(j in seq_along(names(Cigar))){
  for(k in seq_along(names(Cigar))){
    CORR.CIGAR1[j,k]=cor(Cigar[,j],Cigar[,k])
  }
}
print(round(CORR.CIGAR1,3))

CORR.CIGAR2=matrix(NA,8,8)
rownames(CORR.CIGAR2)=names(Cigar)[-1]
colnames(CORR.CIGAR2)=names(Cigar)[-1]
for(j in 1:8){
  for(k in 1:8){
    CORR.CIGAR2[j,k]=cor(Cigar[,j+1],Cigar[,k+1])
  }
}
print(round(CORR.CIGAR2,3))


#2.6
HealthInsurance=Ecdat::HI
head(HealthInsurance)
#print(round(cor(HealthInsurance),3)) #Try this Code

#2.7
var.names = names(HealthInsurance)

FiveSum.HI = matrix(NA,length(var.names),6)
colnames(FiveSum.HI) = c("Variable","Min","Q1","Q2","Q3","Max")

for(VAR in seq_along(var.names)){
  if(is.numeric(HealthInsurance[,VAR])){
    MIN=min(HealthInsurance[,VAR])
    Q1=quantile(HealthInsurance[,VAR],0.25)
    Q2=median(HealthInsurance[,VAR],0.5)
    Q3=quantile(HealthInsurance[,VAR],0.75)
    MAX=max(HealthInsurance[,VAR])
    FiveSum.HI[VAR,]=c(names(HealthInsurance)[VAR],MIN,Q1,Q2,Q3,MAX)
  } else {
    cat(str_c("Variable ",var.names[VAR]," is not numeric\n"))
    FiveSum.HI[VAR,]=c(names(HealthInsurance)[VAR],rep(NA,5))
  }
}
print(as.tibble(na.omit(FiveSum.HI)))

FiveSum.HI2 = NULL
Numeric.names = NULL
for(VAR in seq_along(var.names)){
  if(is.numeric(HealthInsurance[,VAR])){
    MIN=min(HealthInsurance[,VAR])
    Q1=quantile(HealthInsurance[,VAR],0.25)
    Q2=median(HealthInsurance[,VAR],0.5)
    Q3=quantile(HealthInsurance[,VAR],0.75)
    MAX=max(HealthInsurance[,VAR])
    FiveSum.HI2=rbind(FiveSum.HI2,c(MIN,Q1,Q2,Q3,MAX))
    Numeric.names=c(Numeric.names,var.names[VAR])
  } 
}
FiveSum.HI3=as.tibble(cbind(Numeric.names,as.tibble(FiveSum.HI2)))
names(FiveSum.HI3) = c("Variable","Min","Q1","Q2","Q3","Max")
print(na.omit(FiveSum.HI3))

#3.1
set.seed(216)
x1=rnorm(BLANK,mean=82,sd=3)
ggplot()+geom_histogram(aes(x1))

set.seed(216)
x2=rbinom(BLANK,size=10,prob=1/6)
ggplot()+geom_bar(aes(x2))

#3.2
prop=rep(NA,1000)
for(k in 1:1000){
  set.seed(k)
  x=sample(c("H","T"),size=k,replace=T,prob=c(0.5,0.5))
  prop[k]=mean(x=="H")
}
ggplot() + 
  geom_line(aes(x=1:1000,y=prop),alpha=0.5) + 
  geom_hline(yintercept=0.5,linetype="dashed",color="red",size=2) +
  theme_minimal()


