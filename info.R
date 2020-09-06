### Scripts for butterfly project
### Coded by Han

library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(filesstrings)
library(Momocs)
library(stringr)
library(zoo)
library(beginr)
library(forecast)
library(lubridate)
library(xts)
library(tiff)
library(rgdal)
library(maps)
library(ggmap)
library(geomorph)
library(lme4)
library(lmerTest)

library(plyr) # Don't active packages below until you make maps since it may cover some of functions of dplyr
library(maptools)
library(sp)
library(mapproj)

### data wrangling before landmarking
rm(list = ls());
## the path below should be where you keep your downloaded files
fn<-list.files("/Users/han/data/ICL/butterfly_project/data/info/Nymphalidae", full.names = TRUE, recursive = TRUE, pattern = "occurrence.csv")
info<-data.frame()

for (i in 1:length(fn)) {
  v<-read.csv(fn[i])
  info <- rbind(info, v)
}


## make dataframe to plot
info_1<-rename(count(info,genus,year))
info_1<-na.omit(info_1)
ggplot(data=info_1,mapping = aes(x = year, y = n, colour = genus))+
  geom_line()+
  #geom_point(size = 0.5) +
  xlim(min(info_1$year)*1, max(info_1$year)*1)+
  ylim(min(info_1$n)*1, max(info_1$n)*1)


## filter by latitude 
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
## merge file names data to imges info data
fn_2<-list.files("/Users/han/data/ICL/butterfly_project/data/info/Nymphalidae", full.names = TRUE, recursive = TRUE, pattern = "multimedia.csv")
info_2<-data.frame()
rm(v)
for (i in 1:length(fn_2)) {
  v<-read.csv(fn_2[i])
  info_2 <- rbind(info_2, v)
}
info_2<-info_2[!duplicated(info_2[,1],fromLast = TRUE),]
en_3<-inner_join(en_2,info_2,by='X_id', all=FALSE)

## managing the image folders
wd_1<-'/Users/han/data/ICL/butterfly_project/data/images'
setwd(wd_1) ## set work directions 
fdn_1<-list.files(wd_1)
dir.create('butterfly_selected_images_NHM_mastersproject_summer2020')
setwd(paste(wd_1,'butterfly_selected_images_NHM_mastersproject_summer2020',sep = '/'))
lapply(1:length(fdn_1),function(i)
  dir.create(paste(fdn_1[i],'selected_NHM_2020',sep = '_')))
fdn_2<-list.files()


## move selected images into separeted subfolders, each subfolder includes 100 specimens
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


### data analysis
## try import .tps file
tn_1_b<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Pararge_aegeria_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "body")
tn_1_l<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Pararge_aegeria_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "LFW")
tn_1_r<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Pararge_aegeria_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "RFW")

# t_1<-data.frame()
body_1 <-list()
lfw_1<-list()
rfw_1<-list()

for (i in 1:length(tn_1_b)) {
  body_1[[i]]<-import_tps(tn_1_b[i])
  lfw_1[[i]]<-import_tps(tn_1_l[i])
  rfw_1[[i]]<-import_tps(tn_1_r[i])
}



# lfw_1 $coo %>% Opn() %>% panel()

## find floder 1 info
# cut the id of individuals
id_1<-list()
for (i in 1:length(body_1)) {
  id_1[[i]] <- body_1[[i]] $coo %>% names() %>% str_sub(7,18) ###others got 19
}

# get the year of individuals by id
year<-vector()
for (i in 1:length(id_1)) {
  for (u in 1:length(id_1[[i]])) {
    if (length(en_3[grep(id_1[[i]][u],en_3[,73]),71])==0) {
      year[(i-1)*100+u]<-NA
      } else {
      year[(i-1)*100+u]<-en_3[grep(id_1[[i]][u],en_3[,73]),71]
    }
  }
}

# calculate wingspan
lws<-matrix(nrow = 2,ncol = 2)
rws<-matrix(nrow = 2,ncol = 2)
lwsp<-vector()
rwsp<-vector()
for (i in 1:length(lfw_1)) {
  for (u in 1:length(lfw_1[[i]]$coo)) {
    if (length(lfw_1[[i]]$coo[[u]])==32){
      if (length(rfw_1[[i]]$coo[[u]])==32) {
        lws[1,]<-lfw_1[[i]]$coo[[u]][1,]
        lws[2,]<-lfw_1[[i]]$coo[[u]][2,]
        rws[1,]<-rfw_1[[i]]$coo[[u]][1,]
        rws[2,]<-rfw_1[[i]]$coo[[u]][2,]
        lwsp[(i-1)*100+u]<-dist(lws)*lfw_1[[i]]$scale[[u]]
        rwsp[(i-1)*100+u]<-dist(rws)*rfw_1[[i]]$scale[[u]]
      }
      else {
        lws[1,]<-lfw_1[[i]]$coo[[u]][1,]
        lws[2,]<-lfw_1[[i]]$coo[[u]][2,]
        lwsp[(i-1)*100+u]<-dist(lws)*lfw_1[[i]]$scale[[u]]
        rwsp[(i-1)*100+u]<-NA
      }
    }
    else {
      lwsp[(i-1)*100+u]<-NA
      if (length(rfw_1[[i]]$coo[[u]])==32) {
        rws[1,]<-rfw_1[[i]]$coo[[u]][1,]
        rws[2,]<-rfw_1[[i]]$coo[[u]][2,]
        rwsp[(i-1)*100+u]<-dist(rws)*lfw_1[[i]]$scale[[u]]
      }
      else {
        rwsp[(i-1)*100+u]<-NA
      }
    }
  }
}

wy<-data.frame(lwsp,rwsp,year)
wy_l<-tapplydf(wy, 'lwsp', 'year', mean) 
wy_r<-tapplydf(wy, 'rwsp', 'year', mean) 
wy_l[,1]<-wy_l[,1] %>% as.numeric()
wy_r[,1]<-wy_r[,1] %>% as.numeric()

ggplot(data=wy_l, mapping = aes(x = year, y = lwsp))+
  geom_line()+
  #geom_point(size = 0.5) +
  xlim(min(wy$year)*1, max(wy$year)*1)+
  ylim(min(wy$lwsp)*1, max(wy$lwsp)*1)

ggplot(data=wy_r, mapping = aes(x = year, y = rwsp))+
  geom_line()+
  #geom_point(size = 0.5) +
  xlim(min(wy$year)*1, max(wy$year)*1)+
  ylim(min(wy$lwsp)*1, max(wy$lwsp)*1)


## try linear regression
attach(wy)
reg_lwy <-lm(lwsp~year,wy)
reg_lwy
plot(lwsp~year)
abline(reg_lwy)
summary(reg_lwy)
res_lwy<-residuals(reg_lwy)
shapiro.test(res_lwy)
plot(res_lwy)
abline(h=0)
plot(reg_lwy)

reg_rwy <-lm(rwsp~year,wy)
reg_rwy
plot(rwsp~year)
abline(reg_rwy)
summary(reg_rwy)
res_rwy<-residuals(reg_rwy)
shapiro.test(res_rwy)
plot(res_rwy)
abline(h=0)
plot(reg_rwy)

## difference between left and right wingspan
difws<-lwsp-rwsp
reg_difwsy<-lm(difws~year)
plot(difws~year)
abline(reg_difwsy)
summary(reg_difwsy)
plot(reg_difwsy)

## try non-linear regression
ggplot(wy, aes(year, wsp) ) +
  geom_point() +
  stat_smooth()
# here ggplot selected 'gam' regression for default


## try time series analysis

plot(wy_1,type="l")
acf(wy_1$wsp,main="自相关图")
#xts(wy_1$wsp,wy_1$year)
#ndiffs(wy_1)
#diff(wy_1$wsp)


# calculate body size
bda<-vector()
for (i in 1:length(body_1)) {
  for (u in 1:length(body_1[[i]]$coo)) {
    body_1[[i]]$coo[[u]]<-body_1[[i]]$coo[[u]][c(1,5,2,4,3),]
    bda[(i-1)*100+u]<-body_1[[i]]$coo[[u]]%>% coo_area() *(body_1[[i]]$scale[[u]])^2
  }
}

bdy<-data.frame(bda,year)
bdy_1<-tapplydf(bdy, 'bda', 'year', mean) 
bdy_1[,1]<-bdy_1[,1] %>% as.numeric()
ggplot(data=bdy_1, mapping = aes(x = year, y = bda))+
  geom_line()+
  #geom_point(size = 0.5) +
  xlim(min(bdy$year)*1, max(bdy$year)*1)+
  ylim(min(bdy$bda)*1, max(bdy$bda)*1)


## try linear regression

attach(bdy)
reg_bdy <-lm(bda~year,bdy)
reg_bdy
plot(bda~year)
abline(reg_bdy)
summary(reg_bdy)
res_bdy<-residuals(reg_bdy)
shapiro.test(res_bdy)
plot(res_bdy)
abline(h=0)
plot(reg_bdy)




# calculate wings symmetry
ef<-vector()
dw<-list()
for (i in 1:length(lfw_1)) {
  for (u in 1:length(lfw_1[[i]]$coo)) {
    if (length(lfw_1[[i]]$coo[[u]])==28){
      if (length(rfw_1[[i]]$coo[[u]])==28) {
        dw[[(i-1)*100+u]]<-rbind(lfw_1[[i]]$coo[[u]],rfw_1[[i]]$coo[[u]])
      }
      else {
        print(i)
        print(u)
      }
    }
    else {
      print(i)
      print(u)
      #dw[[(i-1)*100+u]]<-NA
    }
  }
}

dw<-Filter(Negate(is.null), dw)
dw<-lapply(dw,function(x){as.matrix(x[,c(1,2)])})
dw<-Out(dw)

dw %>% stack()
dw<-dw %>% coo_centre %>% coo_align()

dw.f <- efourier(dw,10)
sym <- symmetry(dw.f)

sy<-data.frame(sym[,4],year[c(-605,-762,-797)])
colnames(sy)<-c('sym','year_s')
sy_1<-tapplydf(sy, 'sym', 'year_s', mean) 
sy_1[,1]<-sy_1[,1] %>% as.numeric()
ggplot(data=sy_1, mapping = aes(x = year_s, y = sym))+
  geom_line()+
  #geom_point(size = 0.5) +
  xlim(min(sy_1$year_s)*1, max(sy_1$year_s)*1)+
  ylim(min(sy_1$sym)*1, max(sy_1$sym)*1)

ggplot(data  = sy,aes(x = year_s,y = sym))+
  geom_point(size = .8,alpha = .4)+# to add some random noise for plotting purposes
  theme_minimal()+
  geom_smooth(method   = loess,
              se       = T, 
              size     = 1.1, 
              linetype = 1, 
              alpha    = .2,
              colour='pink')+
  ylab('Symmetry')+
  xlab('Year')+
  labs(title = "bssym")

#subset(sy, sy$sym <0.6 )
#subset(sy, sy$sym >.75 )
#sy<-sy[c(-194,-613),]
#sy<-sy[-127,]
#sy<-sy[-469,]#mj


plot_PCA(PCA(dw.f),points = TRUE, points_transp = 0.1, morphospace = TRUE, chull = FALSE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)


## try linear regression
attach(sy)
reg_sy <-lm(sym~year_s,sy)
reg_sy
plot(sym~year)
abline(reg_sy)
summary(reg_sy)
res_sy<-residuals(reg_sy)
shapiro.test(res_sy)
plot(res_sy)
abline(h=0)
plot(reg_sy)

# calculate wing size(contour)
lwa<-vector()
rwa<-vector()

for (i in 1:length(lfw_1)) {
  for (u in 1:length(lfw_1[[i]]$coo)) {
    if (length(lfw_1[[i]]$coo[[u]])==28){
      if (length(rfw_1[[i]]$coo[[u]])==28) {
        lwa[[(i-1)*100+u]]<-lfw_1[[i]]$coo[[u]][(1:10),] %>% coo_area() *(lfw_1[[i]]$scale[[u]])^2
        rwa[[(i-1)*100+u]]<-rfw_1[[i]]$coo[[u]][(1:10),] %>% coo_area() *(rfw_1[[i]]$scale[[u]])^2
      }
      else {
        rwa[[(i-1)*100+u]]<-NA
        lwa[[(i-1)*100+u]]<-lfw_1[[i]]$coo[[u]][(1:10),] %>% coo_area() *(lfw_1[[i]]$scale[[u]])^2
      }
    }
    else {
      lwsp[(i-1)*100+u]<-NA
      if (length(rfw_1[[i]]$coo[[u]])==28) {
        rwa[[(i-1)*100+u]]<-rfw_1[[i]]$coo[[u]][(1:10),] %>% coo_area() *(rfw_1[[i]]$scale[[u]])^2
      }
      else {
        rwa[[(i-1)*100+u]]<-NA
      }
    }
  }
}



way<-data.frame(lwa,rwa,year)
lay_1<-tapplydf(way, 'lwa', 'year', mean) 
ray_1<-tapplydf(way, 'rwa', 'year', mean) 
lay_1[,1]<-lay_1[,1] %>% as.numeric()
ray_1[,1]<-ray_1[,1] %>% as.numeric()
ggplot(data=lay_1, mapping = aes(x = year, y = lwa))+
  geom_line()+
  #geom_point(size = 0.5) +
  xlim(min(lay_1$year)*1, max(lay_1$year)*1)+
  ylim(min(lay_1$lwa)*1, max(lay_1$lwa)*1)
ggplot(data=ray_1, mapping = aes(x = year, y = rwa))+
  geom_line()+
  #geom_point(size = 0.5) +
  xlim(min(ray_1$year)*1, max(ray_1$year)*1)+
  ylim(min(ray_1$rwa)*1, max(ray_1$rwa)*1)



# calculate symmetry of wing contours
efc<-vector()
dwc<-list()
for (i in 1:length(lfw_1)) {
  for (u in 1:length(lfw_1[[i]]$coo)) {
    if (length(lfw_1[[i]]$coo[[u]])==28){
      if (length(rfw_1[[i]]$coo[[u]])==28) {
        dwc[[(i-1)*100+u]]<-rbind(rbind(lfw_1[[i]]$coo[[u]][(1:9),],lfw_1[[i]]$coo[[u]][1,]),rbind(rfw_1[[i]]$coo[[u]][(1:9),],rfw_1[[i]]$coo[[u]][1,]))
      }
      else {
        print(i)
        print(u)
      }
    }
    else {
      print(i)
      print(u)
      #dw[[(i-1)*100+u]]<-NA
    }
  }
}

dwc<-Filter(Negate(is.null), dwc)
dwc<-lapply(dwc,function(x){as.matrix(x[,c(1,2)])})
dwc<-Out(dwc)

dwc %>% stack()
dwc<-dwc %>% coo_centre %>% coo_align()

dwc.f <- efourier(dwc,9)
symc <- symmetry(dwc.f)

syc<-data.frame(symc[,4],year[c(-605,-762,-797)])
colnames(syc)<-c('symc','year_s')
syc_1<-tapplydf(syc, 'symc', 'year_s', mean) 
syc_1[,1]<-syc_1[,1] %>% as.numeric()
ggplot(data=syc_1, mapping = aes(x = year_s, y = symc))+
  geom_line()+
  #geom_point(size = 0.5) +
  xlim(min(syc_1$year_s)*1, max(syc_1$year_s)*1)+
  ylim(min(syc_1$symc)*1, max(syc_1$symc)*1)

#syc<-syc[-469,]
#syc<-syc[c(-194,-613),]
ggplot(data  = syc,aes(x = year_s,y = symc))+
  geom_point(size = .8,alpha = .4)+# to add some random noise for plotting purposes
  theme_minimal()+
  geom_smooth(method   = loess,
              se       = T, 
              size     = 1.1, 
              linetype = 1, 
              alpha    = .2,
              colour='pink')+
  ylab('Symmetry')+
  xlab('Year')+
  labs(title = "bssymc")



plot_PCA(PCA(dwc.f),points = TRUE, points_transp = 0.1, morphospace = TRUE, chull = FALSE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)

# error analysis
err<-import_tps('/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Boloria_selene_selected_NHM_2020/01_Boloria_selene_NHM/error.tps')$coo
## get standard error of every landmark
e<-vector()
e_1<-vector()
for (i in 1:70) {
  for (t in 1:18) {
    e[t]<-err[[t]][i]
  }
  e_1[i]<-sd(e)
}

## sd * scale to get the error
s<-vector()
for (i in 1:length(body_1)){
  for (u in 1:length(body_1[[i]]$scale)) {
    s[(i-1)*100+u]<-body_1[[i]]$scale[[u]]
  }
}
max(s)*mean(e_1)
min(s)*mean(e_1)



# making map of individuals
## Load the shapefile
uk_map <- map_data(map = "world", xlim=c(-7,3), ylim=c(49,52.5))
eng <- c(left = -7, bottom = 49, right = 3, top = 52.5)

lat_1<-vector()
lon_1<-vector()
for (i in 1:length(id_1)) {
  for (u in 1:length(id_1[[i]])) {
    if (length(en_3[grep(id_1[[i]][u],en_3[,73]),11])==0) {
      lat_1[(i-1)*100+u]<-NA
      lon_1[(i-1)*100+u]<-NA
  } else {
      if (length(en_3[grep(id_1[[i]][u],en_3[,73]),12])==0) {
        lat_1[(i-1)*100+u]<-NA
        lon_1[(i-1)*100+u]<-NA
    } else {
      lat_1[(i-1)*100+u]<-en_3[grep(id_1[[i]][u],en_3[,73]),11]
      lon_1[(i-1)*100+u]<-en_3[grep(id_1[[i]][u],en_3[,73]),12]
        }
      }
  }
}

lalo<-cbind(lat_1,lon_1)
lalo<-lalo%>% na.omit() %>% as.data.frame()
city<-lalo %>% unique()
count.dups <- function(DF){
  
  DT <- data.table(DF)
  DT[,.N, by = names(DT)]
}
count_lalo<-count.dups(lalo)

ggplot() + 
  geom_polygon(data = uk_map, aes(x = long, y = lat, group = group),fill="grey95",colour="grey50") +
  geom_point(data=count_lalo,aes(x=lon_1,y=lat_1,size=N),shape=21,fill="#8E0F2E",colour="#8E0F2E",alpha=0.6)+
  scale_size(limits = c(1,125),range = c(1,9))+
  coord_map("polyconic",xlim=c(-7,3), ylim=c(49,52.5))+
  guides(size=guide_legend(reverse=TRUE,title=NULL))+
  ggtitle("Distribution map of Pararge aegeria samples")+
  labs(x = "Longitude",y='Latitude')+
  theme(
    panel.grid = element_line(colour  = 'grey80',size = 0.3),
    panel.background = element_rect(fill = 'white'),
    #axis.text = element_blank(),
    axis.ticks = element_blank(),
    #axis.title = element_blank(),
    #legend.position =c(0.15,0.4),
    legend.background=element_rect(colour="white",fill="white"),
    legend.text.align=1
  )


# geomorph on wing symmetry
## put lfw same side with rfw
lfw_2<-list()
rfw_2<-list()
for (i in 1:length(lfw_1)) {
  for (u in 1:length(lfw_1[[i]]$coo)) {
    if (length(lfw_1[[i]]$coo[[u]])==28){
      if (length(rfw_1[[i]]$coo[[u]])==28) {
        lfw_2[[(i-1)*100+u]]<-lfw_1[[i]]$coo[[u]]
        rfw_2[[(i-1)*100+u]]<-rfw_1[[i]]$coo[[u]]
      }
      else {
        rfw_2[[(i-1)*100+u]]<-NA
        lfw_2[[(i-1)*100+u]]<-lfw_1[[i]]$coo[[u]]
        print(i)
        print(u)
      }
    }
    else {
      lfw_2[[(i-1)*100+u]]<-NA
      print(i)
      print(u)
      if (length(rfw_1[[i]]$coo[[u]])==28) {
        rfw_2[[(i-1)*100+u]]<-rfw_1[[i]]$coo[[u]]
      }
      else {
        rfw_2[[(i-1)*100+u]]<-NA
        print(i)
        print(u)
      }
    }
  }
}


lfw_2<-lfw_2[!is.na(lfw_2)]
rfw_2<-rfw_2[!is.na(rfw_2)]

for (i in 1:length(lfw_2)) {
  if ((lfw_2[[i]][1])>(lfw_2[[i]][2])) {
    lfw_2[[i]]<-lfw_2[[i]] %>% coo_flipy()
  }
}



for (i in 1:length(rfw_2)) {
  if ((rfw_2[[i]][1])>(rfw_2[[i]][2])) {
    rfw_2[[i]]<-rfw_2[[i]] %>% coo_flipy()
  }
}

## check if the flip successed
lfw_3<-Out(lfw_2)
rfw_3<-Out(rfw_2)
lfw_3 %>% stack()
rfw_3 %>% stack()

for (i in 1:length(lfw_2)) {
  lfw_2[[i]]<-lfw_2[[i]][!duplicated(lfw_2[[i]]),]
}
for (i in 1:length(rfw_2)) {
  rfw_2[[i]]<-rfw_2[[i]][!duplicated(rfw_2[[i]]),]
}

for (i in 1:length(lfw_2)) {
  if (length(lfw_2[[i]])!=28) {
    print(i)
  }
}

for (i in 1:length(rfw_2)) {
  if (length(rfw_2[[i]])!=28) {
    print(i)
  }
}
## create array of landmarks
lfwa <- array(dim=c(14,2,length(lfw_2)))
rfwa <- array(dim=c(14,2,length(rfw_2)))
dfwa <- array(dim = c(14,2,length(lfw_2)+length(rfw_2)))
for (i in 1:length(dfwa[1,1,])) {
  if (i<(length(lfw_2)+1)) {
    dfwa[,,i]<-lfw_2[[i]]
  }
  else{
    dfwa[,,i]<-rfw_2[[i-length(lfw_2)]]
  }
}

### contour arrey
lfwac<-array(dim=c(9,2,length(lfw_2)))
rfwac<-array(dim=c(9,2,length(rfw_2)))
dfwac<-array(dim=c(9,2,length(lfw_2)+length(rfw_2)))
for (i in 1:length(dfwa[1,1,])) {
  dfwac[,,i]<-dfwa[,,i][1:9,]
}
#dfwa_0<-list()  
#dfwa_0[[1]]<-dfwa
col.lr<-c(rep('left',length(lfw_2)),rep('right',length(rfw_2)))
#col.lr<-c(rep('left',length(lfw_2)),rep('right',length(rfw_2)-1))
## Generalized Procrustes Analysis

#dfwa<-dfwa[,,-1458]
#dfwac<-dfwac[,,-1458]
dfwa_1<-gpagen(dfwa)
dfwac_1<-gpagen(dfwac)
summary(dfwa_1)
summary(dfwac_1)
plot(dfwa_1)
plot(dfwac_1)

msp<-mshape(dfwa_1$coords)
mspc<-mshape(dfwac_1$coords)
plot(msp)
plot(mspc)
### pca

pca<-plotTangentSpace(dfwa_1$coords)
pcac<-plotTangentSpace(dfwac_1$coords)
pca_1<-pca$pc.scores %>% as.data.frame() %>% cbind(col.lr)
pcac_1<-pcac$pc.scores %>% as.data.frame() %>% cbind(col.lr)
percentage<-round(pca$sdev / sum(pca$sdev) *100,2)
percentagec<-round(pcac$sdev / sum(pcac$sdev) *100,2)
percentage<-paste(colnames(pca),"(", paste(as.character(percentage),"%",")", sep=""))
percentagec<-paste(colnames(pcac),"(", paste(as.character(percentagec),"%",")", sep=""))

ggplot(pca_1,aes(x=PC1,y=PC2,color=col.lr))+ 
  geom_point()+ 
  ggtitle('pca plot of pa Procrustes')+
  xlab(paste('pc1',percentage[1])) +ylab(paste('pc2',percentage[2]))+
  theme_bw() +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"))

ggplot(pcac_1,aes(x=PC1,y=PC2,color=col.lr))+ 
  geom_point()+ 
  ggtitle('pca plot of pa Procrustes contour')+
  xlab(paste('pc1',percentagec[1])) +ylab(paste('pc2',percentagec[2]))+
  theme_bw() +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"))


## fourier analysis and pca
dwef<-dfwa %>% Out() %>% efourier() %>% PCA()
dwefc<-dfwac %>% Out() %>% efourier() %>% PCA()
dwef_1<-dwef$x %>% as.data.frame() %>% cbind(col.lr)
dwefc_1<-dwefc$x %>% as.data.frame() %>% cbind(col.lr)

ggplot(dwef_1,aes(x=PC1,y=PC2,color=col.lr))+ 
  geom_point(size=1)+ 
  ggtitle('pca plot of efourier of mj')+
  xlab(paste('pc1(',round(dwef$eig[1]*100,2) %>% as.character(percentage),"%",")"))+
  ylab(paste('pc2(',round(dwef$eig[2]*100,2) %>% as.character(percentage),"%",")"))+
  theme_bw() +
  labs(colour = "")+
  stat_ellipse(level = 0.95, show.legend = F) + 
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"))

ggplot(dwefc_1,aes(x=PC1,y=PC2,color=col.lr))+ 
  geom_point(size=1)+ 
  ggtitle('pca plot of efourier of mj contour')+
  xlab(paste('pc1(',round(dwefc$eig[1]*100,2) %>% as.character(percentage),"%",")"))+
  ylab(paste('pc2(',round(dwefc$eig[2]*100,2) %>% as.character(percentage),"%",")"))+
  theme_bw() +
  labs(colour = "")+
  stat_ellipse(level = 0.95, show.legend = F) + 
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"))

# mixed linear model on year~PC1
## construt a dataframe
year_1<-c(year[c(-605,-762,-797)],year)
pcy<-cbind(pca$pc.scores[,1],year_1) 
pcyc<-cbind(pcac$pc.scores[,1],year_1) 

ggplot(data  = pcy %>% as.data.frame(),aes(x = year_1,y = V1,colour=as.factor(col.lr)))+
  geom_point(size = 1,alpha = .5)+# to add some random noise for plotting purposes
  theme_minimal()+
  geom_smooth(method   = lm,
              se       = T, 
              size     = .5, 
              linetype = 1, 
              alpha    = .2)+
  ylab('PC1')+
  xlab('Year')+
  labs(colour = "")+
  scale_color_manual(labels = c("Left", "Right"),values = c("red", "blue"))+
  labs(title = "pa")



ggplot(data  = pcyc %>% as.data.frame(),aes(x = year_1,y = V1,colour=col.lr))+
  geom_point(size = 1,alpha = .5)+# to add some random noise for plotting purposes
  theme_minimal()+
  geom_smooth(method   = lm,
              se       = T, 
              size     = .5, 
              linetype = 1, 
              alpha    = .2)+
  ylab('PC1')+
  xlab('Year')+
  labs(colour = "")+
  scale_color_manual(labels = c("Left", "Right"),values = c("red", "blue"))+
  labs(title = "pac")



# box plot of symmetry 
sya<-sy
sya<-rbind(sya,sy)
syca<-syc
syca<-rbind(syca,syc)


sya_1<-cbind(sya,rep(c('Boloria selene','Lasiommata megera','Maniola jurtina','Pararge aegeria'),c(996,841,995,997)))
syca_1<-cbind(syca,rep(c('Boloria selene','Lasiommata megera','Maniola jurtina','Pararge aegeria'),c(997,841,995,997)))
colnames(sya_1) <- c("Symmetry", "year",'Species')
colnames(syca_1) <- c("Symmetry", "year",'Species')

ggplot(sya_1, aes(y=Symmetry,x=Species))+
  geom_boxplot(fill='lightblue1',size=.4, outlier.size = .9, outlier.alpha = .7)+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))


ggplot(syca_1, aes(y=Symmetry,x=Species))+
  geom_boxplot(fill='lightblue1',size=.4, outlier.size = .9, outlier.alpha = .7)+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))


ggplot(syaa, aes(y=Symmetry,x=Species, colour = sc))+
  geom_boxplot(fill='lightblue1',size=.4, outlier.size = .9, outlier.alpha = .7, varwidth = F, width = .4)+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))
