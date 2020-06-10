library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(filesstrings)

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


# filter by latitude 
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
# merge file names data to imges info data
fn_2<-list.files("/Users/han/data/ICL/butterfly_project/data/info/Nymphalidae", full.names = TRUE, recursive = TRUE, pattern = "multimedia.csv")
info_2<-data.frame()
rm(v)
for (i in 1:length(fn_2)) {
  v<-read.csv(fn_2[i])
  info_2 <- rbind(info_2, v)
}
info_2<-info_2[!duplicated(info_2[,1]),]
en_3<-inner_join(en_2,info_2,by='X_id', all=FALSE)

# managing the image folders
wd_1<-'/Users/han/data/ICL/butterfly_project/data/images'
setwd(wd_1) ## set work directions 
fdn_1<-list.files(wd_1)
dir.create('butterfly_selected_images_NHM_mastersproject_summer2020')
setwd(paste(wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',sep = '/'))
lapply(1:length(fdn_1),function(i)
  dir.create(paste(fdn_1[i],'selected_NHM_2020',sep = '_')))
fdn_2<-list.files()


# move selected images into separeted subfolders, each subfolder includes 100 specimens
## create a folder named 0 to put all selected images by species temporarily
for (i in 1:length(fdn_2)) {
  setwd(paste(wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',fdn_2[i],sep = '/'))
  dir.create(paste('00',fdn[i],'NHM',sep = '_'))
}

## cut the ids of individuals
for (i in (1:length(en_3$title))) {
  en_3$title[i]<-gsub('BMNHE_','',(gsub('label_','',en_3$title[i])))
}

## split by species
gen<-en_3$genus %>% unique()
sp<-split(en_3,en_3$genus) # split (here we use genus because the scientific names are mixed)
lapply(1:length(sp),function(i)
  sp[[i]] %>% nrow())  #  check how many images remain after filtering in each species

## shuffle the data set
for (i in 1:length(sp)) {
  rows <- sample(nrow(sp[[i]]))
  sp[[i]]<-sp[[i]][rows,]
}
en_3<-unsplit(sp,en_3$genus)

## move images into 00 folders separately
dirs<-list.dirs(wd_1)
for (i in 1:nrow(en_3)) {
  v<-paste(wd_1,list.files(wd_1, full.names = FALSE, recursive = TRUE, pattern = en_3$title[i]), sep = '/')
  t<-dirs[grepl(paste('00',en_3$genus[i],sep = '_'), dirs)]
  file.copy(v,t)
  print(i)
}

## create subfolders for every species
for (i in 1:length(fdn_2)) {
  setwd(paste(wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',fdn_2[i],sep = '/'))
  u<-1:ceiling(0.01*length(list.files(
    paste(wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',fdn_2[i],
          paste('00',fdn[i],'NHM',sep = '_'),sep = '/'))))
  for (t in u) {
    dir.create(paste(sprintf('%02d',t),fdn[i],'NHM',sep = '_'))
  }
}

## move images into separate subfolders
for (i in 1:length(fdn_2)) {
  setwd(paste(
    wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',fdn_2[i],paste('00',fdn[i],'NHM',sep = '_'),sep = '/'))
  fl<-list.files()
  for (t in 1:(length(list.dirs(paste(wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',fdn_2[i],sep = '/')))-2)) {
    if(100*t < length(fl)){
      file.move(fl[((t-1)*100+1):(100*t)],paste(
        wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',fdn_2[i],paste(sprintf('%02d',t),fdn[i],'NHM',sep = '_'),sep = '/'))
      
    } else {
      file.move(fl[((t-1)*100+1):length(fl)],paste(
        wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',fdn_2[i],paste(sprintf('%02d',t),fdn[i],'NHM',sep = '_'),sep = '/'))
      
    }
      }
  
}

## remove '00' folders
lapply(1:length(fdn_2),function(i)
  unlink(paste(wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',fdn_2[i],
             paste('00',fdn[i],'NHM',sep = '_'),sep = '/'),recursive =TRUE))
