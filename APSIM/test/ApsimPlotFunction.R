################################################################################################
#results = the results list obtained from apsim.spatial command
#b = the country shapefile e.g "ZM" for Zimbabwe
#wkdir = the directory where station data is saved
#' Title
#'
#' @param results 
#' @param b 
#' @param wkdir 
#'
#' @return
#' @export
#'
#' @examples
apsim.plots<- function(stn, results, b, wkdir){
  setwd(wkdir)
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
            ggplot(aes(x=Maize.SowingDate, y=Yield)) +
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
  
  #country<-raster::getData("GADM", country=b, level=0)
  country<-geodata::gadm(country=b, level=0, path=tempdir())
  
  print(ggplot()+tidyterra::geom_spatvector(data=country, fill = "white")+
          geom_point(data=final, aes(x=Longitude, y=Latitude, color= Maize.SowingDate), size = 2))
  
  print(ggplot() +  geom_point(data=final, aes(x=Longitude, y=Latitude, color= Maize.SowingDate), size = 2))
  
  print(ggplot()+tidyterra::geom_spatvector(data=country, fill = "white")+
          geom_point(data=final, aes(x=Longitude, y=Latitude, color= Yield), size = 2))
  
  print(ggplot() +  geom_point(data=final, aes(x=Longitude, y=Latitude, color= Yield), size = 2))
}




