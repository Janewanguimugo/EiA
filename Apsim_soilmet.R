
###########################Get soil data
library(apsimx)

##################Get Met data
## Not run: 
require(nasapower)
## This will not write a file to disk
pwr <- get_power_apsim_met(lonlat = c(33.64, -13.98), 
                           dates = c("1985-01-01","1990-01-01"))
extd.dir <- system.file("extdata", package = "apsimx") #or 
tmp.dir <- tempdir()
write_apsim_met(pwr, wrt.dir = extd.dir, filename = "chitedze.met")
chitedze<-read_apsim_met("chitedze.met", src.dir = extd.dir)

## Let's insert a missing value###########################################
pwr[100, "radn"] <- NA
summary(pwr)
## Check the met file 
check_apsim_met(pwr)
## Impute using linear interpolation
pwr.imptd <- impute_apsim_met(pwr, verbose = TRUE)
summary(pwr.imptd)
check_apsim_met(pwr.imptd)
######################################################################

## Here I write to a temporary directory, but change this to where
## you want to write to

#Source of soil data https://soilgrids.org/
sp <- get_isric_soil_profile(lonlat = c(33.64, -13.98))

sp <- apsimx_soil_profile()
extd.dir <- system.file("extdata", package = "apsimx")

## I write to a temp directory but replace as needed
tmp.dir <- tempdir()

edit_apsimx_replace_soil_profile("Maize.apsimx", soil.profile = sp, 
                                 src.dir = extd.dir, wrt.dir = tmp.dir)
inspect_apsimx("Maize-edited.apsimx", src.dir = tmp.dir,
               node = "Soil")