setwd("D:/j")

pts<- read.csv("Rwanda/station.csv")
names(pts)<- c("Longitude", 'Latitude', "Location")
rain<-read.csv("Rwanda/Rainfall.data.coordinates_Rwanda.csv")
rain$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")

max<-read.csv("Rwanda/Tmax.data.coordinates_Rwanda.csv")
max$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")

min<-read.csv("Rwanda/Tmin.data.coordinates_Rwanda.csv")
min$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")

solar<-read.csv("Rwanda/S.Rad.data.coordinates_Rwanda.csv")
solar$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")

apsimMetFile<-function(filename = NULL){
    
  if(missing(filename)) filename <- "noname.met"
  
  if(!grepl(".met", filename, fixed = TRUE)) stop("filename should end in .met")
       
pwr <- foreach (i = 1:(nrow(pts)-1)) %do% {data.frame(date = rain[1],
                              rain = rain[i+1] ,
                              max = max[i+1],
                              min = min[i+1],
                              solar = solar[i+1])}

foreach (i = 1: length(pwr))%do%{
  names(pwr[[i]])<- c('Date', 'rain', 'maxt', 'mint', 'radn')}
           
pwr2<-foreach(i = 1:length(pwr))%do%{
  pwr[[i]]%>%
  mutate(day = lubridate::yday(Date))%>%
  mutate(year=format(as.Date(pwr[[i]]$Date, format="%Y/%m/%d"),"%Y"))%>%
  select("year", "day", 'rain', 'maxt', 'mint', 'radn')}
          
units <- c("()", "()","(mm)","(oC)", "(oC)", "(MJ/m2/day)")
           
comments <- paste("!data from various areas. retrieved: ", Sys.time())
           
## Calculating annual amplitude in mean monthly temperature
pwr<- foreach(i = 1:length(pwr2))%do%{                   
attr(pwr2[[i]], "filename") <- filename
attr(pwr2[[i]], "site") <- paste("site =", sub(".met", "", filename, fixed = TRUE))
attr(pwr2[[i]], "latitude") <- paste("latitude =",  pts$Latitude[[i]])
attr(pwr2[[i]], "longitude") <- paste("longitude =", pts$Longitude[[i]])
attr(pwr2[[i]], "tav") <- paste("tav =", mean(colMeans(pwr2[[i]][,c("maxt","mint")], na.rm=TRUE), na.rm=TRUE))
attr(pwr2[[i]], "colnames") <- names(pwr2[[i]])
attr(pwr2[[i]], "units") <- units
attr(pwr2[[i]], "comments") <- comments
## No constants
class(pwr2[[i]]) <- c("met", "data.frame")
pwr2[[i]] <- amp_apsim_met(pwr2[[i]])
}

if(filename != "noname.met"){
  #write_apsim_met(pwr2, wrt.dir = wrt.dir, filename = filename)
}
return(invisible(pwr2))
}
 
my_list_clm<-apsimMetFile()

##APSIM PART##
#Write the weather files to a working directory and Edit the weather as per location
foreach (i =1:length(my_list_clm)) %dopar% {
  apsimx::write_apsim_met(my_list_clm[[i]], wrt.dir = "D:/j", filename = paste0('met_loc_',i,'.met'))}

#write_apsim_met(my_list_clm[[1]], wrt.dir = "D:/project", filename = "file.met")
 