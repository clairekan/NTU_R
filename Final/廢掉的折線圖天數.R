library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)


dsbi <- read.csv('D:/gitHub/NTU_R/Final/Youbike_sbi(1).csv')
dsbi_line <- dsbi%>%gather( Time, value, X2018.8.1.7:X2018.8.4.9, na.rm = TRUE)
dsbi_line<-select(dsbi_line,sarea,Time,value)
dsbi_line$Time <- str_replace(dsbi_line$Time, 'X', '')
dsbi_line$Time <- ymd_h(dsbi_line$Time)
dsbi_line<-separate(dsbi_line,Time,c("day","hour"),sep=" ",remove=FALSE)
dsbi_line<-dsbi_line%>%group_by(sarea,Time,day)%>%summarise(mean=mean(value))
head(dsbi_line)

dsbi_line%>%ggplot(aes(x=Time,y=mean,group =1 ,color=sarea))+geom_line()+ylim(8,25)
dsbi_line%>%ggplot(aes(x=day,y=mean,group = 1))+geom_line()+ylim(8,25)+facet_wrap(~sarea)

