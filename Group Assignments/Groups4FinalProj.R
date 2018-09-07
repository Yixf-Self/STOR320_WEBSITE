library(tidyverse)
library(readxl)

#Read Rosters
Section1=read_excel("STOR320_001_FA18_Roster.xlsx")
Section2=read_excel("STOR320_002_FA18_Roster.xlsx")

#Function to Divide Sections into Groups
Group.select.func<-function(data){
  student.names=data$Name
  set.seed(length(student.names))
  random.order=sample(1:length(student.names),replace=F)
  group=tibble(Order=random.order,Name=student.names) %>%
          arrange(Order) %>%
          mutate(Group=c(rep(1:(length(student.names)%/%4),each=4),rep(NA,length(student.names)%%4))) %>%
          mutate(Group=replace_na(Group,max(Group,na.rm=T))) %>%
          mutate(Role="TBD") %>%
          select(-Order)
  return(group)
}

#Application of the Function
Final.Section1=Group.select.func(Section1)
Final.Section2=Group.select.func(Section2)

#Save Datasets
write_csv(Final.Section1,path=str_c(getwd(),"/STOR320.01 Group Assignments.csv"))
write_csv(Final.Section2,path=str_c(getwd(),"/STOR320.02 Group Assignments.csv"))