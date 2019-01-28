library(tidyverse)
library(readxl)

#Read Rosters
Section=read_excel("STOR320_001_SP19_Roster.xlsx")

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
Final.Section=Group.select.func(Section)

#Save Datasets
write_csv(Final.Section,path=str_c(getwd(),"/STOR320.001 Group Assignments.csv"))
