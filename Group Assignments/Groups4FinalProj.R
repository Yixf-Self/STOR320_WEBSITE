setwd("D:/Mario Documents/UNC/STOR 320/STOR320_WEBSITE/Group Assignments")

library(tidyverse)
library(readxl)

#Read Rosters
Section2=read_excel("STOR320_002_ROSTER.xlsx")
data=Section2
#Function to Divide Sections into Groups
Group.select.func<-function(data){
  student.names=data$Name
  set.seed(length(student.names))
  random.order=sample(1:length(student.names),replace=F)
  group=tibble(Order=random.order,Name=student.names) %>%
          arrange(Order) %>%
          mutate(Group=rep(1:(length(student.names)%/%5+(length(student.names)%%5!=0)),
                           each=5,length=length(student.names))) %>%
          mutate(Role="TBD") %>%
          select(-Order) %>%
          arrange(Group)
  return(group)
}

#Application of the Function
Final.Section.2=Group.select.func(Section2)
Final.Section.2[105,"Group"]=22
Final.Section.2[100,"Group"]=22
Final.Section.2[95,"Group"]=22

Final.Section.2=arrange(Final.Section.2,Group)

#Save Datasets
write_csv(Final.Section.2,path=str_c(getwd(),"/STOR_320_002_Group_Assignments.csv"))
