#wkdir = Working directory where your files will be saved
#cell = The spatial resolution you want e.g 1 degree
#b = Choose the country shapefile you want e.g "ZM" for Zimbabwe
#date = How may years of weather do you want to download e.g c("1985-01-01","2022-01-01")
#crop = The crop in APSIM you want to simulate e.g. "maize.apsimx"
#clck = How many years do you want the simulation to run e.g. c("1985-01-01T00:00:00", "2020-12-31T00:00:00")
#sd = The start date e.g.  "2-jan"
#ed = The start date e.g.  "10-jan"
#variety = The cultivar you want to simulate e.g "A_103"

apsim.spatial <- function(wkdir, cell, b, date, crop, clck, sd, ed, variety, rep1, rep2) {
my_packages <- c("spdep", "rgdal", "maptools", "raster", "plyr", "ggplot2", "rgdal",
                   "dplyr", "cowplot","readxl", "apsimx", "gtools", "foreach","doParallel",
                   "ranger")
  list.of.packages <- my_packages
  new.packages <- list.of.packages[!(my_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(my_packages, require, character.only = TRUE)
  
  cores<- detectCores()
  myCluster <- makeCluster(cores -2, # number of cores to use
                           type = "PSOCK") # type of cluster
  registerDoParallel(myCluster)
  
#Change depending on where you crop.apsimx file is stored, this script used the examples#
  setwd(wkdir)
  ex.dir <- "D:/Egypt"
  #ex.dir <- system.file("extdata", package = "apsimx")
  extd.dir <-wkdir
  file.copy(paste0(ex.dir, "/", crop),  extd.dir, overwrite = TRUE)
  
  #list.files(ex.dir)
  list.files(ex.dir)
  
#GET COUNTRY SHAPEFILE
  country<-raster::getData("GADM", country= b, level=0)

  country <- st_as_sf(country)
  
# Make a grid of points
  points <- country %>% 
    st_make_grid(cellsize = cell, what = "centers") %>% # grid of points
    st_intersection(country) 

#View the number of points sampled and convert them to a dataframe
  print(ggplot() + 
          geom_sf(data = country) + 
          geom_sf(data = points))
  
  df<-as.data.frame(st_coordinates(st_centroid(points)))
  colnames(df) <- c('Longitude','Latitude')
  stn<-df
  #text(stn$Longitude, y = stn$Latitude, pos = 3)
  write.csv(stn, "station.csv", row.names=FALSE )

#Get Meteorological data from nasapower
  
  require(nasapower)
  ## This will not write a file to disk
  my_list_clm <- foreach (i = 1:nrow(stn)) %dopar% {
    apsimx::get_power_apsim_met(lonlat = c(stn$Longitude[[i]], stn$Latitude[[i]]), 
                                dates = date)
  }

#Get soil data from iscric
  my_list_sol <- foreach (i = 1:nrow(stn)) %dopar% {
    tryCatch(apsimx::get_isric_soil_profile(lonlat = c(stn$Longitude[i], stn$Latitude[[i]]))
             , error=function(err) NA)
  }
  
                                      ##APSIM PART##
#Write the weather files to a working directory and Edit the weather as per location  
  foreach (i =1:length(my_list_clm)) %dopar% {
    apsimx::write_apsim_met(my_list_clm[[i]], wrt.dir = extd.dir, filename = paste0('wth_loc_',i,'.met'))}
  
  foreach (i =1:length(my_list_clm)) %dopar% {
    dir.create(paste0(extd.dir, '/', i))
    apsimx::edit_apsimx(paste0(crop), 
                        src.dir = extd.dir,
                        wrt.dir = paste0(extd.dir, '/', i),
                        node = "Weather", 
                        value = paste0(extd.dir, "/", 'wth_loc_',i,'.met'), overwrite = TRUE)
  }
  
  foreach (i =1:length(my_list_sol)) %do% {  
    setwd(paste0(extd.dir, '/', i))
    my_list_sol[[i]]$soil$BD <-  my_list_sol[[i]]$soil$BD * 0.86
    my_list_sol[[i]]$soil$crop.LL <-  my_list_sol[[i]]$soil$LL15 + 0.01
    my_list_sol[[i]]$soil$SAT <-  c(0.521, 0.521, 0.497, 0.488, 0.478, 0.440)
    #edit_apsimx_replace_soil_profile(crop, soil.profile = my_list_sol[[i]], overwrite = TRUE)
    tryCatch(edit_apsimx_replace_soil_profile(crop, soil.profile = my_list_sol[[i]], overwrite = TRUE), error=function(err) NA)
  }
  
#Edit clock#
  foreach (i =1:length(my_list_sol)) %dopar% {  
    setwd(paste0(extd.dir, '/', i))
    apsimx::edit_apsimx(crop, 
                        node = "Clock",
                        parm = c("Start", "End"),
                        value = clck,
                        overwrite = TRUE)
  }
# Change the sowing rule for when rain is available
  foreach (i =1:length(my_list_sol)) %dopar% {  
    setwd(paste0(extd.dir, '/', i))
    apsimx::edit_apsimx(crop, 
                        node = "Manager",
                        manager.child = "SowingRule",
                        parm = "StartDate", ## This is for start date
                        value = sd,
                        overwrite = TRUE)
    
    apsimx::edit_apsimx(crop, 
                        node = "Manager",
                        manager.child = "SowingRule",
                        parm = "EndDate", ## This is for end date
                        value = ed,
                        overwrite = TRUE)
    apsimx::edit_apsimx(crop, 
                        node = "Manager",
                        manager.child = "SowingRule",
                        parm = "CultivarName", ## This is for end date
                        value = variety,
                        overwrite = TRUE)
    apsimx::edit_apsimx(crop,
                        node = "Report",
                        parm = "VariableNames", 
                        value = rep1, 
                        verbose = TRUE, overwrite = TRUE)
    apsimx::edit_apsimx(crop,
                        node = "Report",
                        parm = "VariableNames", 
                        value = rep2, 
                        verbose = TRUE, overwrite = TRUE)
  }

# Run the simulation for the entire study area    
  my_list_sim<- foreach (i =1:length(my_list_sol)) %dopar% {  
    setwd(paste0(extd.dir, '/', i))  
    tryCatch(apsimx::apsimx(crop, value = "HarvestReport"), error=function(err) NA)
#apsim.spatial("D:/project", 3, "KE", c("2020-01-01","2022-01-01"), "soybean.apsimx", c("2010-11-01T00:00:00", "2020-12-31T00:00:00"),"1-nov", "30-nov", "Davis")
  }
  return(my_list_sim)
}

####################################SECOND FUNCTION###############################################################
##SECOND FUNCTION ON PLOTTING USING THE RESULTS OBTAINED FROM apsim.spatial COMMAND##
#results = the results list obtained from apsim.spatial command
#b = the country shapefile e.g "ZM" for Zimbabwe
#wkdir = the directory where station data is saved

apsim.plots<- function(results, b, wkdir){
  setwd(wkdir)
  stn<- read.csv("station.csv")
  
 foreach (i = 1:length(results))%do%{
  results[[i]]$Longitude<-stn$Longitude[[i]]
  results[[i]]$Latitude<-stn$Latitude[[i]]
 }

foreach (i = 1:length(results))%do%{ 
if(lengths(results[i])< 5){
  results[[i]] <- NULL
  }
 }

foreach (i = 1:length(results))%do%{ 
  if(lengths(results[i])< 5){
    results[[i]] <- NULL
  }
}
##############################Graphs######################################
  foreach (i = 1:length(results))%do%{
    print(results[[i]]  %>%
            ggplot(aes(x=Clock.Today, y=Yield)) +
            geom_point(na.rm=TRUE)+
            ggtitle(paste0("Yield ",i)))
  }
  ###########################################################################
  final<- do.call("smartbind", results)
  glimpse(final)
  
  final<-final%>%
    group_by(Longitude, Latitude)%>%
    slice(which.max(Yield))%>%
    as.data.frame()
  
  country<-getData("GADM", country=b, level=0)
  
  print(ggplot()+geom_polygon(data=country, aes(x=long, y=lat), fill = "white")+
          geom_point(data=final, aes(x=Longitude, y=Latitude, color= Wheat.SowingDate), size = 4))
  print(ggplot()+geom_polygon(data=country, aes(x=long, y=lat), fill = "white")+
          geom_point(data=final, aes(x=Longitude, y=Latitude, color= Yield), size = 4))
  
  print(ggplot() +  geom_point(data=final, aes(x=Longitude, y=Latitude, color= Wheat.SowingDate), size = 4))
  print(ggplot() +  geom_point(data=final, aes(x=Longitude, y=Latitude, color= Yield), size = 4))
}

results<- apsim.spatial(wkdir ="D:/Egypt/project", 
                        cell = 2,
                        b= "EGY", 
                        date = c("2000-01-01","2015-01-01"),
                        crop = "Wheat_Egypta.apsimx", 
                        clck = c("2000-01-01T00:00:00", "2015-12-01T00:00:00"),
                        sd = "15-nov", 
                        ed = "20-nov",
                        variety = "Dekan",
                        rep1 ="[Wheat].Grain.Total.Wt*10 as Yield",
                        rep2 ="[Wheat].SowingDate")

apsim.plots(results,"EGY", "D:/Egypt/project")
