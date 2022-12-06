library(raster)
library(ncdf4)
######change this######
setwd("E:/climate_data/Data_Kenya/") 

#Rainfall
b <- brick("ppt/Histo/Ensemble/pr_ensemble_1951-2005_day.nc", varname = "")
print(b)
########station has columns lon, lat and station name
pts <- read.csv("station.csv", stringsAsFactors = FALSE, check.names = F)
#names(pts)<-c('ID', 'STN', 'STATE', 'Longitude', 'Latitude')
idx <- getZ(b)
# Put coordinates and extract values for all time steps
ts <- extract(b, cbind(pts$Longitude, pts$Latitude), df=T)
rain <- data.frame(idx, t(ts)[-1,])
rownames(rain) <- NULL
j<-pts$stations
as.vector(j)
names(rain) <- c('date',j)


#max temp
b <- brick("tasmax/Histo/Ensemble/max_ensemble_1951-2005_day.nc", varname = "")
print(b)
########station has columns lon, lat and station name
pts <- read.csv("station.csv", stringsAsFactors = FALSE, check.names = F)
#names(pts)<-c('ID', 'STN', 'STATE', 'Longitude', 'Latitude')
idx <- getZ(b)
# Put coordinates and extract values for all time steps
ts <- extract(b, cbind(pts$Longitude, pts$Latitude), df=T)
max <- data.frame(idx, t(ts)[-1,])
rownames(max) <- NULL
j<-pts$stations
as.vector(j)
names(max) <- c('date',j)


#min temp
b <- brick("tasmin/Histo/Ensemble/min_ensemble_1951-2005_day.nc", varname = "")
print(b)
########station has columns lon, lat and station name
pts <- read.csv("station.csv", stringsAsFactors = FALSE, check.names = F)
#names(pts)<-c('ID', 'STN', 'STATE', 'Longitude', 'Latitude')
idx <- getZ(b)
# Put coordinates and extract values for all time steps
ts <- extract(b, cbind(pts$Longitude, pts$Latitude), df=T)
min <- data.frame(idx, t(ts)[-1,])
rownames(min) <- NULL
j<-pts$stations
as.vector(j)
names(min) <- c('date',j)

#solar
b <- brick("solar/Histo/Ensemble/solar_ensemble_1951-2005_day.nc", varname = "")
print(b)
########station has columns lon, lat and station name
pts <- read.csv("station.csv", stringsAsFactors = FALSE, check.names = F)
#names(pts)<-c('ID', 'STN', 'STATE', 'Longitude', 'Latitude')
idx <- getZ(b)
# Put coordinates and extract values for all time steps
ts <- extract(b, cbind(pts$Longitude, pts$Latitude), df=T)
solar <- data.frame(idx, t(ts)[-1,])
rownames(solar) <- NULL
j<-pts$stations
as.vector(j)
names(solar) <- c('date',j)




