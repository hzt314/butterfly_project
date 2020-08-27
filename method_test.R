# Test methods of morphology in both contours and shapes
# coded by Han
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
library(geomorph)

### data wrangling before analysis
## read data
rm(list = ls());
tn_1_b<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Boloria_selene_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "body")
tn_1_l<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Boloria_selene_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "LFW")
tn_1_r<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Boloria_selene_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "RFW")
tn_2_b<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Lasiommata_megera_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "body")
tn_2_l<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Lasiommata_megera_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "LFW")
tn_2_r<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Lasiommata_megera_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "RFW")
tn_3_b<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Maniola_jurtina_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "body")
tn_3_l<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Maniola_jurtina_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "LFW")
tn_3_r<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Maniola_jurtina_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "RFW")
tn_4_b<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Pararge_aegeria_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "body")
tn_4_l<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Pararge_aegeria_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "LFW")
tn_4_r<-list.files("/Users/han/data/ICL/butterfly_project/data/images/butterfly_selected_images_NHM_mastersproject_summer2020/Pararge_aegeria_selected_NHM_2020", full.names = TRUE, recursive = TRUE, pattern = "RFW")

body_1 <-list()
lfw_1<-list()
rfw_1<-list()
body_2 <-list()
lfw_2<-list()
rfw_2<-list()
body_3 <-list()
lfw_3<-list()
rfw_3<-list()
body_4 <-list()
lfw_4<-list()
rfw_4<-list()

for (i in 1:length(tn_1_b)) {
  body_1[[i]]<-import_tps(tn_1_b[i])
  lfw_1[[i]]<-import_tps(tn_1_l[i])
  rfw_1[[i]]<-import_tps(tn_1_r[i])
}
for (i in 1:length(tn_2_b)) {
  body_2[[i]]<-import_tps(tn_2_b[i])
  lfw_2[[i]]<-import_tps(tn_2_l[i])
  rfw_2[[i]]<-import_tps(tn_2_r[i])
}
for (i in 1:length(tn_3_b)) {
  body_3[[i]]<-import_tps(tn_3_b[i])
  lfw_3[[i]]<-import_tps(tn_3_l[i])
  rfw_3[[i]]<-import_tps(tn_3_r[i])
}
for (i in 1:length(tn_4_b)) {
  body_4[[i]]<-import_tps(tn_4_b[i])
  lfw_4[[i]]<-import_tps(tn_4_l[i])
  rfw_4[[i]]<-import_tps(tn_4_r[i])
}

## merge body and wings, also eliminate bad individuals
w_1<-list()
w_2<-list()
w_3<-list()
w_4<-list()
for (i in 1:length(lfw_1)) {
  for (u in 1:length(lfw_1[[i]]$coo)) {
    if (length(lfw_1[[i]]$coo[[u]])==32){
      if (length(rfw_1[[i]]$coo[[u]])==32) {
        if (length(body_1[[i]]$coo[[u]])==10) {
          if (3*dist(rbind(body_1[[i]]$coo[[u]][1,],body_1[[i]]$coo[[u]][2,]))>dist(rbind(body_1[[i]]$coo[[u]][1,],body_1[[i]]$coo[[u]][5,]))) {
            w_1[[(i-1)*100+u]]<-rbind(lfw_1[[i]]$coo[[u]],rfw_1[[i]]$coo[[u]],body_1[[i]]$coo[[u]][c(-1,-2),])
          }
        }
      }
    }  
  }
}
for (i in 1:length(lfw_2)) {
  for (u in 1:length(lfw_2[[i]]$coo)) {
    if (length(lfw_2[[i]]$coo[[u]])==28){
      if (length(rfw_2[[i]]$coo[[u]])==28) {
        if (length(body_2[[i]]$coo[[u]])==10) {
          if (3*dist(rbind(body_2[[i]]$coo[[u]][1,],body_2[[i]]$coo[[u]][2,]))>dist(rbind(body_2[[i]]$coo[[u]][1,],body_2[[i]]$coo[[u]][5,]))) {
            w_2[[(i-1)*100+u]]<-rbind(lfw_2[[i]]$coo[[u]],rfw_2[[i]]$coo[[u]],body_2[[i]]$coo[[u]][c(-1,-2),])
          }
        }
      }
    }  
  }
}
for (i in 1:length(lfw_3)) {
  for (u in 1:length(lfw_3[[i]]$coo)) {
    if (length(lfw_3[[i]]$coo[[u]])==28){
      if (length(rfw_3[[i]]$coo[[u]])==28) {
        if (length(body_3[[i]]$coo[[u]])==10) {
          if (3*dist(rbind(body_3[[i]]$coo[[u]][1,],body_3[[i]]$coo[[u]][2,]))>dist(rbind(body_3[[i]]$coo[[u]][1,],body_3[[i]]$coo[[u]][5,]))) {
            w_3[[(i-1)*100+u]]<-rbind(lfw_3[[i]]$coo[[u]],rfw_3[[i]]$coo[[u]],body_3[[i]]$coo[[u]][c(-1,-2),])
          }
        }
      }
    }  
  }
}
for (i in 1:length(lfw_4)) {
  for (u in 1:length(lfw_4[[i]]$coo)) {
    if (length(lfw_4[[i]]$coo[[u]])==28){
      if (length(rfw_4[[i]]$coo[[u]])==28) {
        if (length(body_4[[i]]$coo[[u]])==10) {
          if (3*dist(rbind(body_4[[i]]$coo[[u]][1,],body_4[[i]]$coo[[u]][2,]))>dist(rbind(body_4[[i]]$coo[[u]][1,],body_4[[i]]$coo[[u]][5,]))) {
            w_4[[(i-1)*100+u]]<-rbind(lfw_4[[i]]$coo[[u]],rfw_4[[i]]$coo[[u]],body_4[[i]]$coo[[u]][c(-1,-2),])
          }
        }
      }
    }  
  }
}

w_1<-Filter(Negate(is.null), w_1)
w_2<-Filter(Negate(is.null), w_2)
w_3<-Filter(Negate(is.null), w_3)
w_4<-Filter(Negate(is.null), w_4)

w_1<-lapply(w_1,function(x){as.matrix(x[,c(1,2)])})
w_2<-lapply(w_2,function(x){as.matrix(x[,c(1,2)])})
w_3<-lapply(w_3,function(x){as.matrix(x[,c(1,2)])})
w_4<-lapply(w_4,function(x){as.matrix(x[,c(1,2)])})
ww<-c(w_1,w_2,w_3,w_4)
lw <-list()
for (i in 1:length(w_1)) {
  lw[[i]]<-rbind(w_1[[i]][1:10,],w_1[[i]][1,])
}
for (i in 1:length(w_2)) {
  lw[[i+991]]<-rbind(w_2[[i]][1:9,],w_2[[i]][1,])
}
for (i in 1:length(w_3)) {
  lw[[i+1830]]<-rbind(w_3[[i]][1:9,],w_3[[i]][1,])
}
for (i in 1:length(w_4)) {
  lw[[i+2824]]<-rbind(w_4[[i]][1:9,],w_4[[i]][1,])
}

for (i in 1:length(ww)) {
  if ((ww[[i]][1])>(ww[[i]][2])) {
    ww[[i]]<-ww[[i]] %>% coo_flipy()
  }
}
for (i in 1:length(lw)) {
  if ((lw[[i]][1])>(lw[[i]][2])) {
    lw[[i]]<-lw[[i]] %>% coo_flipy()
  }
}
fac<-data.frame(species=rep(c('Boloria selene','Lasiommata megera','Maniola jurtina','Pararge aegeria'),c(991,839,994,991)))
ww<-Out(ww,fac)
lw<-Out(lw,fac)

### superinposition

ww<- ww %>% coo_scale() %>% coo_centre()
ww %>% stack()
ww %>% panel()

lw<- lw %>% coo_scale() %>% coo_centre() %>% coo_align() %>% coo_slidedirection("right") %>% coo_untiltx() 
stack(lw)
### try different methods
## sample 400 individuals
ww_1<-ww %>% sample_n(400)
ww_1 %>% stack()

## fourier analysis
w.f<-efourier(ww_1,nb.h = 10,norm = TRUE)
w.fr<-rfourier(ww_1,nb.h = 10,norm = TRUE)
w.ft<-tfourier(ww_1,nb.h = 10, norm = TRUE)
w.fs<-sfourier(ww_1,nb.h = 10)

w.pr<-PCA(w.fr)
w.pt<-PCA(w.ft)
w.ps<-PCA(w.fs)
w.p<-PCA(w.f)

PCcontrib(w.p, nax = 1:3)
PCcontrib(w.pr, nax = 1:3)
PCcontrib(w.pt, nax = 1:3)

MSHAPES(w.f) %>% coo_plot()
MSHAPES(w.fr) %>% coo_plot()
MSHAPES(w.ft) %>% coo_plot()
MSHAPES(w.fs)%>% coo_plot()

plot_PCA(w.p,~species,points = TRUE, points_transp = 0, morphospace = FALSE, chull = T, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)
plot_PCA(w.pr,~species,points = TRUE, points_transp = 0, morphospace = FALSE, chull = TRUE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)
plot_PCA(w.pt,~species,points = TRUE, points_transp = 0, morphospace = FALSE, chull = TRUE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)
plot_PCA(w.ps,~species,points = TRUE, points_transp = 0, morphospace = FALSE, chull = T, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)

KMEANS(w.p,centers=4,nax=1:2, cex = 1.8)
KMEANS(w.pr,centers=4,nax=1:2, cex = 1.8)
KMEANS(w.pt,centers=4,nax=1:2, cex = 1.8)
KMEANS(w.ps,centers=4,nax=1:2, cex = 1.8)

CLUST(w.f,type = "fan", cex=1,hclust_method = "complete")
CLUST(w.fr,type = "fan", cex=0.2,hclust_method = "complete", ~species, palette = pal_manual(c("green","yellow","red","purple","pink","orange","grey","blue")))
CLUST(w.ft,type = "fan", cex=1,hclust_method = "complete")
CLUST(w.fs,type = "fan", cex=1,hclust_method = "complete")



### on contours of lfw
lw_1<-lw %>% sample_n(400)

l.f<-efourier(lw_1,nb.h = 10,norm = TRUE)
l.fr<-rfourier(lw_1,nb.h = 10,norm = TRUE)
l.ft<-tfourier(lw_1,nb.h = 10, norm = TRUE)
l.fs<-sfourier(lw_1,nb.h = 10)

l.pr<-PCA(l.fr)
l.pt<-PCA(l.ft)
l.ps<-PCA(l.fs)
l.p<-PCA(l.f)

PCcontrib(l.p, nax = 1:3)
PCcontrib(l.pr, nax = 1:3)
PCcontrib(l.pt, nax = 1:3)

MSHAPES(l.f) %>% coo_plot()
MSHAPES(l.fr) %>% coo_plot()
MSHAPES(l.ft) %>% coo_plot()
MSHAPES(l.fs)%>% coo_plot()

plot_PCA(l.p,~species,points = TRUE, points_transp = 0, morphospace = FALSE, chull = T, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)
plot_PCA(l.pr,~species,points = TRUE, points_transp = 0, morphospace = FALSE, chull = TRUE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)
plot_PCA(l.pt,~species,points = TRUE, points_transp = 0, morphospace = FALSE, chull = TRUE, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)
plot_PCA(l.ps,~species,points = TRUE, points_transp = 0, morphospace = FALSE, chull = T, legend = FALSE, center_origin = TRUE, eigen = TRUE, box = FALSE, axesnames = TRUE, axesvar = TRUE, labelgroups = TRUE)

KMEANS(l.p,centers=4,nax=1:2, cex = 1.8)
KMEANS(l.pr,centers=4,nax=1:2, cex = 1.8)
KMEANS(l.pt,centers=4,nax=1:2, cex = 1.8)
KMEANS(l.ps,centers=4,nax=1:2, cex = 1.8)

CLUST(l.f,type = "fan", cex=1,hclust_method = "complete")
CLUST(l.fr,type = "fan", cex=0.2,hclust_method = "complete", ~species, palette = pal_manual(c("green","yellow","red","purple","pink","orange","grey","blue")))
CLUST(l.ft,type = "fan", cex=1,hclust_method = "complete")
CLUST(w.fs,type = "fan", cex=1,hclust_method = "complete")
