apsim.spatial <- function(wkdir, nop, a, b, crop, clck, sd, ed, variety) {
  ####wkdir-working directory ("D:/project"),NOP-3 (number of points) , a- country ("GADM"), b-country ("ZM"), crop ("maize.apsimx")
  #clck- clock c("1985-01-01T00:00:00", "2020-12-31T00:00:00"), variety - cultivar ("Davis") 
  my_packages <- c("spdep", "rgdal", "maptools", "raster", "plyr", "ggplot2", "rgdal",
                   "dplyr", "cowplot","readxl", "apsimx", "gtools", "foreach","doParallel",
                   "ranger")
list.of.packages <- my_packages
new.packages <- list.of.packages[!(my_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
  lapply(my_packages, require, character.only = TRUE)
  
  ##################################################################################
  setwd(wkdir)
  #ex.dir <- auto_detect_apsimx_examples()
  ex.dir <- system.file("extdata", package = "apsimx")
  extd.dir <-wkdir
  file.copy(paste0(ex.dir, "/", crop),  extd.dir, overwrite = TRUE)
  
  list.files(ex.dir)
  list.files(extd.dir)
  
  ################################GET COUNTRY SHAPEFILE
  country<-getData(a, country= b, level=0)
  #Kenya1<-getData("GADM", country="KE", level=1)
  set.seed(100)
  
  points<-spsample(country, n=nop,"nonaligned")
  
  plot(country)
  plot(points,add=T)
  
  #text(stn$Longitude, y = stn$Latitude, paste0(format(round(stn$Longitude, 2), nsmall = 2), ", ", format(round(stn$Latitude, 2), pos = 3)))
  
  df<-as.data.frame(points)
  colnames(df) <- c('Longitude','Latitude')
  stn<-df
  text(stn$Longitude, y = stn$Latitude, pos = 3)
  write.csv(stn, "station.csv", row.names=FALSE )
  ##################Get Met data##################################################
  ## Not run: 
  require(nasapower)
  ## This will not write a file to disk
  my_list_clm <- list()  
  
  foreach (i = 1:nrow(stn)) %do% {
    pwr <- get_power_apsim_met(lonlat = c(stn$Longitude[[i]], stn$Latitude[[i]]), 
                               dates = c("1985-01-01","2022-01-01"))
    my_list_clm[[i]] <- pwr
  }
  #############################GET SOIL###########################################
  my_list_sol <- list() 
  foreach (i = 1:nrow(stn)) %do% {
    s <- tryCatch(
      expr = {
        sp <-  apsimx::get_isric_soil_profile(lonlat = c(stn$Longitude[i], stn$Latitude[[i]]))
        my_list_sol[[i]] <- sp
      },
      error = function(e){
        #sp <-  apsimx::get_isric_soil_profile(lonlat = c(stn$Longitude[[i]], stn$Latitude[[i]]))
        sp<- NA
        return(sp)
      }) 
  }
  ###################################APSIM PART#############################################
  #####################Edit weather############################################################
  my_list_sim <- list()  
  foreach (i =1:length(my_list_clm)) %do% {
    write_apsim_met(my_list_clm[[i]], wrt.dir = extd.dir, filename = paste0('wth_loc_',i,'.met'))
    dir.create(paste0(extd.dir, '/', i))
    edit_apsimx(paste0(crop), 
                src.dir = extd.dir,
                wrt.dir = paste0(extd.dir, '/', i),
                node = "Weather", 
                value = paste0(extd.dir, "/", 'wth_loc_',i,'.met'), overwrite = TRUE)
    setwd(paste0(extd.dir, '/', i))
    my_list_sol[[i]]$soil$BD <-  my_list_sol[[i]]$soil$BD * 0.86
    my_list_sol[[i]]$soil$crop.LL <-  my_list_sol[[i]]$soil$LL15 + 0.01
    my_list_sol[[i]]$soil$SAT <-  c(0.521, 0.521, 0.497, 0.488, 0.478, 0.440)
    #edit_apsimx_replace_soil_profile(crop, soil.profile = my_list_sol[[i]], overwrite = TRUE)
    tryCatch(
      expr = {
        edit_apsimx_replace_soil_profile(crop, soil.profile = my_list_sol[[i]], overwrite = TRUE)
      },
      error = function(e){
        return(NA)
      }) 
    ###################Edit clock###################################################################
    edit_apsimx(crop, 
                node = "Clock",
                parm = c("Start", "End"),
                value = clck,
                overwrite = TRUE)
    ## Change the sowing rule for when rain is available
    edit_apsimx(crop, 
                node = "Manager",
                manager.child = "SowingRule",
                parm = "StartDate", ## This is for start date
                value = sd,
                overwrite = TRUE)
    edit_apsimx(crop, 
                node = "Manager",
                manager.child = "SowingRule",
                parm = "EndDate", ## This is for end date
                value = ed,
                overwrite = TRUE)
    edit_apsimx(crop, 
                node = "Manager",
                manager.child = "SowingRule",
                parm = "CultivarName", ## This is for end date
                value = as.character(variety),
                overwrite = TRUE)
    tryCatch(
      expr = {
        sim <- apsimx(crop, value = "HarvestReport")
      },
      error = function(e){
        return(NA)
      }) 
    my_list_sim[[i]] <- sim
    #apsim.spatial("D:/project", 3, "GADM", "KE", "soybean.apsimx", c("2010-11-01T00:00:00", "2020-12-31T00:00:00"),"1-nov", "30-nov", "Davis")
  }
  return(my_list_sim)
}


####################################SECOND FUNCTION###############################################################

apsim.plots<- function(results, wkdir){
  setwd(wkdir)
  stn<- read.csv("station.csv")
  
  foreach (i = 1:length(results))%do%{
    if(nrow(results[[i]])==0){
      results[[i]][nrow(results[[i]]) + 1, ] <- NA
    }
    results[[i]]$Longitude<-stn$Longitude[[i]]
    results[[i]]$Latitude<-stn$Latitude[[i]]
  }
  
  ##############################Graphs######################################
  foreach (i = 1:length(results))%do%{
    print(results[[i]]  %>%
            ggplot(aes(x=Clock.Today, y=Maize.Grain.Wt)) +
            geom_point(na.rm=TRUE)+
            ggtitle(paste0("Yield ",i)))
  }
  ###########################################################################
  final<- do.call("smartbind", results)
  glimpse(final)
  
  final<-final%>%
    group_by(Longitude, Latitude)%>%
    slice(which.max(Maize.Grain.Wt))%>%
    as.data.frame()
  
  country<-getData("GADM", country="ZM", level=0)
  
  print(ggplot()+geom_polygon(data=country, aes(x=long, y=lat), fill = "white")+
          geom_point(data=final, aes(x=Longitude, y=Latitude, color= Clock.Today), size = 4))
  
  print(ggplot() +  geom_point(data=final, aes(x=Longitude, y=Latitude, color= Clock.Today), size = 4))
}

results<- apsim.spatial(wkdir ="D:/project", nop = 3, a = "GADM", b= "ZM", crop = "Maize.apsimx", 
                        clck = c("2019-10-01T00:00:00", "2020-12-01T00:00:00"),
                        sd = "1-jan", ed = "30-dec",variety = "A_100")
apsim.plots(results, "D:/project")
