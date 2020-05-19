library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

rm(list = ls());
## the path below should be where you keep your downloaded files
fn<-list.files("/Users/han/data/ICL/butterfly_project/data/info/Nymphalidae", full.names = TRUE, recursive = TRUE, pattern = "occurrence.csv")
info<-data.frame()

for (i in 1:length(fn)) {
  v<-read.csv(fn[i])
  info <- rbind(info, v)
}


# make dataframe to plot
info_1<-rename(count(info,genus,year))
info_1<-na.omit(info_1)
ggplot(data=info_1,mapping = aes(x = year, y = n, colour = genus))+
  geom_line()+
  #geom_point(size = 0.5) +
  xlim(min(info_1$year)*1, max(info_1$year)*1)+
  ylim(min(info_1$n)*1, max(info_1$n)*1)


# filter of latitude 
en<-subset(info,info$stateProvince == 'England') # focus on England specimens

## keep all species got same weight when calculating
n<-rename(count(en,genus))$n %>% min() 
names<-en$genus %>% unique()
en_1<-data.frame()
for (i in names) {
  t<-subset(en,en$genus==i)
  en_1<-rbind(en_1,t[sample(nrow(t),n),])
}

## take x percentile of latitude, here x depends on nrow(en_1). 
## In this case we want to keep at least 1,000 individuals per species, so we take 70 percentile
lat<-quantile(en_1$decimalLatitude,0.70, na.rm = TRUE)
en_2<-subset(en,en$decimalLatitude<lat,na.rm = TRUE) # subset the new data set meet the condition

