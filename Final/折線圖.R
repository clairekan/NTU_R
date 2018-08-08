library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)

sbi <- read.csv('D:/gitHub/NTU_R/Final/Youbike_sbi(1).csv')
sbi_line <- sbi%>%gather( Time, value, X2018.8.1.7:X2018.8.4.12, na.rm = TRUE)
sbi_line<-select(sbi_line,sarea,Time,value)
sbi_line<-sbi_line%>%group_by(sarea,Time)%>%summarise(mean=mean(value))
sbi_line$Time <- str_replace(sbi_line$Time, 'X', '')
sbi_line$Time <- ymd_h(sbi_line$Time)
sbi_line<-separate(sbi_line,Time,c("day","hour"),sep=" ",remove=FALSE)
head(sbi_line)
min_date <- min(sbi_line$Time)
max_date <- max(sbi_line$Time)
all_regions <- sort(unique(sbi_line$sarea))

shade <- read.csv('D:/gitHub/NTU_R/Final/shade.csv')
shade$x1<-ymd_h(shade$x1)
shade$x2<-ymd_h(shade$x2)




#依照日期的各區空位數變化
sbi_line%>%filter(day=="2018-08-01")%>%ggplot(aes(x=Time,y=mean,color=sarea))+geom_line()+ylim(8,25)
#依照日期、區域的空位數變化
sbi_line%>%filter(day=="2018-08-01",sarea=="大安區")%>%ggplot(aes(x=Time,y=mean,color=sarea))+geom_line()+ylim(8,25)

#目前為止的各區空位數變化

ggplot() + 
  geom_line(aes(x=Time, y=mean, color=sarea),data=sbi_line)+
  geom_rect(data=shade, 
            mapping=aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), color='grey')

#目前為止的依區空位數變化
sbi_line1<-filter(sbi_line,sarea=="大同區",day >= as.Date("2018-08-01") & day <=as.Date("2018-08-03") )
ggplot() + 
  geom_line(aes(x=Time, y=mean), color='red',data=sbi_line1)+
  geom_rect(data=shade, 
            mapping=aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf), color='grey')
