apsim.spatial <- function(wkdir, cell, a, b, crop, clck, sd, ed, variety) {
  ####wkdir-working directory ("D:/project"),cell-0.1 (cellsize) , a- country ("GADM"), b-country ("ZM"), crop ("maize.apsimx")
  #clck- clock c("1985-01-01T00:00:00", "2020-12-31T00:00:00"), variety - cultivar ("Davis") 
  my_packages <- c("spdep", "rgdal", "maptools", "raster", "plyr", "ggplot2", "rgdal",
                   "dplyr", "cowplot","readxl", "apsimx", "gtools", "foreach","doParallel",
                   "ranger")
  lapply(my_packages, require, character.only = TRUE)
  detectCores()
  myCluster <- makeCluster(6, # number of cores to use
                           type = "PSOCK") # type of cluster
  registerDoParallel(myCluster)
  ##################################################################################
  setwd(wkdir)
  ex.dir <- auto_detect_apsimx_examples()
  extd.dir <-wkdir
  file.copy(paste0(ex.dir, "/", crop),  extd.dir, overwrite = TRUE)
  
  list.files(ex.dir)
  list.files(extd.dir)
  
  ################################GET COUNTRY SHAPEFILE
  country<-raster::getData(a, country= b, level=0)
  #Kenya1<-getData("GADM", country="KE", level=1)
  country <- st_as_sf(country)
  
  #set.seed(100)
  points <- country %>% 
    st_make_grid(cellsize = cell, what = "centers") %>% # grid of points
    st_intersection(country) 
  
  #plot(country)
  #plot(points, pch = ".", add = T)
  #text(stn$Longitude, y = stn$Latitude, paste0(format(round(stn$Longitude, 2), nsmall = 2), ", ", format(round(stn$Latitude, 2), pos = 3)))
  
  ggplot() + 
    geom_sf(data = country) + 
    geom_sf(data = points)
  
  df<-as.data.frame(st_coordinates(st_centroid(points)))
  colnames(df) <- c('Longitude','Latitude')
  stn<-df
  #text(stn$Longitude, y = stn$Latitude, pos = 3)
  write.csv(stn, "station.csv", row.names=FALSE )
  ##################Get Met data##################################################
  ## Not run: 
  require(nasapower)
  ## This will not write a file to disk
  my_list_clm <- foreach (i = 1:nrow(stn)) %dopar% 
    apsimx::get_power_apsim_met(lonlat = c(stn$Longitude[[i]], stn$Latitude[[i]]),
                                dates = c("1985-01-01","2022-01-01"))
  
  #############################GET SOIL###########################################
  my_list_sol <- foreach (i = 1:nrow(stn)) %dopar% {
    s <- tryCatch(
      expr = {
        sp <-  apsimx::get_isric_soil_profile(lonlat = c(stn$Longitude[i], stn$Latitude[[i]]))
        #my_list_sol[[i]] <- sp
      },
      error = function(e){
        #sp <-  apsimx::get_isric_soil_profile(lonlat = c(stn$Longitude[[i]], stn$Latitude[[i]]))
        sp<- NA
        return(sp)
      }) 
  }
  ###################################APSIM PART#############################################
  #####################Edit weather############################################################
  foreach (i = 1:length(my_list_clm)) %dopar% {
    apsimx::write_apsim_met(my_list_clm[[i]], wrt.dir = extd.dir, filename = paste0('wth_loc_',i,'.met'))
    dir.create(paste0(extd.dir, '/', i))
    apsimx::edit_apsimx(paste0(crop), 
                        src.dir = extd.dir,
                        wrt.dir = paste0(extd.dir, '/', i),
                        node = "Weather", 
                        value = paste0(extd.dir, "/", 'wth_loc_',i,'.met'), overwrite = TRUE)
  }
  
  my_list_sim<- list() 
  foreach (i = 1:length(my_list_clm)) %do% { 
    setwd(paste0(extd.dir, '/', i))
    tryCatch(
      expr = {
        edit_apsimx_replace_soil_profile(crop, src.dir = paste0(extd.dir, '/', i),
                                         soil.profile = my_list_sol[[i]], overwrite = TRUE)
      },
      error = function(e){
        return(NA)
      }) 
    ###################Edit clock###################################################################
    apsimx::edit_apsimx(crop, 
                        node = "Clock",
                        parm = c("Start", "End"),
                        value = clck,
                        overwrite = TRUE)
    ## Change the sowing rule for when rain is available
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
    #apsim.spatial("D:/project", 0.1, "GADM", "KE", "soybean.apsimx", c("2010-11-01T00:00:00", "2020-12-31T00:00:00"),"1-nov", "30-nov", "Davis")
  }
  return(my_list_sim)
}

####################################SECOND FUNCTION###############################################################

apsim.plots<- function(results, wkdir){
  setwd(wkdir)
  stn<- read.csv("station.csv")
  
  for (i in 1:length(results)){
    if(nrow(results[[i]])==0){
      results[[i]][nrow(results[[i]]) + 1, ] <- NA
    }
    results[[i]]$Longitude<-stn$Longitude[[i]]
    results[[i]]$Latitude<-stn$Latitude[[i]]
  }
  
  ##############################Graphs######################################
  for (i in 1:length(results)){
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
  
  country<-getData("GADM", country="ZM", level=0)
  
  print(ggplot()+geom_polygon(data=country, aes(x=long, y=lat), fill = "white")+
          geom_point(data=final, aes(x=Longitude, y=Latitude, color= Clock.Today), size = 4))
  
  print(ggplot() +  geom_point(data=final, aes(x=Longitude, y=Latitude, color= Clock.Today), size = 4))
  print(ggplot() +  geom_point(data=final, aes(x=Longitude, y=Latitude, color= Yield), size = 4))
  
}

results<- apsim.spatial(wkdir ="D:/project", cell = 2, a = "GADM", b= "ZM", crop = "soybean.apsimx", 
                        clck = c("2019-10-01T00:00:00", "2020-12-01T00:00:00"),
                        sd = "1-nov", ed = "1-feb", variety = "Davis")
apsim.plots(results, "D:/project")

