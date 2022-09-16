library(nasapower)

#Download point data. Example Harare
#working
ag_d <- get_power(
  community = "ag",
  lonlat = c(33.768143, -13.987511),
  pars = c("PRECTOTCORR","T2M_MAX", "T2M_MIN","ALLSKY_SFC_PAR_TOT"),
  dates = c("1985-01-01","1990-01-01"),
  temporal_api = "daily"
)

#Download regional data. Example Harare.
#working
harare_met0 <- get_power(
  community = "ag",
  lonlat = c(30.9, -19.8, 32.9, -17.8),
  dates = c("1985-01-01","1986-01-01"),
  temporal_api = "daily",
  pars = c("PRECTOTCORR","T2M_MAX", "T2M_MIN","ALLSKY_SFC_PAR_TOT")
)

#Download Bako regional data.
#nonworking
Bako_met0 <- get_power(
  community = "ag",
  lonlat = c(30.9, -19.8, 32.9, -17.8),
  dates = c("1985-01-01","1986-12-31"),
  temporal_api = "daily",
  pars = c("PRECTOTCORR","T2M_MAX", "T2M_MIN","ALLSKY_SFC_PAR_TOT")
)


library(nasapower)

ag_d <- get_power(
  community = "ag",
  lonlat = c(30.92, -17.82),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("2013-01-01","2014-12-01"),
  temporal_api = "daily"
)

write.table(ag_d,file="C:/Users/Siyabusa/OneDrive - CGIAR/EiA/Scripts/bkoo.txt",append=FALSE,sep=" ",dec =".", row.names=FALSE,col.names=TRUE)


###########################Get soil data
library(apsimx)

#Source of soil data https://soilgrids.org/
soil.isric <- get_isric_soil_profile(lonlat = c(-93, 42))









