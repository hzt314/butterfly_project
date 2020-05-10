library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

rm(list = ls());
# the path below should be where you keep your downloaded info in
fn<-list.files("/Users/han/data/ICL/butterfly_project/data/info", full.names = TRUE, recursive = TRUE, pattern = "occurrence.csv")
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


