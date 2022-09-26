library(spdep)
library(rgdal)
library(maptools)
library(raster)
library(plyr)
library(ggplot2)
library(rgdal)
library(dplyr)
#install.packages("tidyverse")
library("readxl")
library(cowplot)
library(apsimx)
setwd("D:/project")

################################GET COUNTRY SHAPEFILE
Zim<-getData("GADM", country="ZM", level=0)
#Kenya1<-getData("GADM", country="KE", level=1)
set.seed(100)
points<-spsample(Zim, n=3,"nonaligned")

plot(Zim)
plot(points,add=T)

df<-as.data.frame(points)
colnames(df) <- c('Longitude','Latitude')
stn<-df

##################Get Met data##################################################
## Not run: 
require(nasapower)
## This will not write a file to disk
my_list_clm <- list()  

for (i in 1:nrow(stn)){
  pwr <- get_power_apsim_met(lonlat = c(stn$Longitude[[i]], stn$Latitude[[i]]), 
                             dates = c("1985-01-01","2022-01-01"))
  my_list_clm[[i]] <- pwr
}
#############################GET SOIL###########################################
my_list_sol <- list()  

for (i in 1:nrow(stn)){
  sp <- get_isric_soil_profile(lonlat = c(stn$Longitude[[i]], stn$Latitude[[i]]))
  my_list_sol[[i]] <- sp
}

my_list_sol[[3]]$soil$SAT

###################################APSIM PART#############################################
tmp.dir <- tempdir()
extd.dir <-"D:/project"

#####################Edit weather############################################################
my_list_sim <- list()  
for(i in 1:length(my_list_clm)){
    write_apsim_met(my_list_clm[[i]], wrt.dir = extd.dir, filename = paste0('wth_loc_',i,'.met'))
  dir.create(paste0(extd.dir, '/', i))
    edit_apsimx(paste0('soybean.apsimx'), 
              src.dir = extd.dir,
              wrt.dir = paste0(extd.dir, '/', i),
              node = "Weather", 
              value = paste0(extd.dir, "/", 'wth_loc_',i,'.met'), overwrite = TRUE)
  setwd(paste0(extd.dir, '/', i))
  # my_list_sol[[i]]$soil$BD <-  my_list_sol[[i]]$soil$BD * 0.86
  # my_list_sol[[i]]$soil$crop.LL <-  my_list_sol[[i]]$soil$LL15 + 0.01
  edit_apsimx_replace_soil_profile("soybean.apsimx", soil.profile = my_list_sol[[i]], overwrite = TRUE)
  edit_apsimx_replace_soil_profile("soybean.apsimx", soil.profile = my_list_sol[[i]])
###################Edit clock###################################################################
   edit_apsimx("soybean.apsimx", 
               node = "Clock",
               parm = c("Start", "End"),
               value = c("1985-01-01T00:00:00", "2020-12-31T00:00:00"),
               overwrite = TRUE)
   ## Change the sowing rule for when rain is available
   edit_apsimx("soybean.apsimx", 
               node = "Manager",
               manager.child = "SowingRule",
               parm = "StartDate", ## This is for start date
               value = "1-nov",
               overwrite = TRUE)
   edit_apsimx("soybean.apsimx", 
               node = "Manager",
               manager.child = "SowingRule",
               parm = "EndDate", ## This is for end date
               value = "30-nov",
               overwrite = TRUE)
   edit_apsimx("soybean.apsimx", 
               node = "Manager",
               manager.child = "SowingRule",
               parm = "CultivarName", ## This is for end date
               value = "Davis",
               overwrite = TRUE)
    sim <- apsimx('soybean.apsimx', value = "HarvestReport")
  my_list_sim[[i]] <- sim
  print(head(sim))
  # print(my_list_sol[[i]]$soil)
}
my_list_sim[[3]]


