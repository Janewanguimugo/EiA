# Libraries
library(ggplot2)
library(dplyr)
setwd("~/")
#install.packages("tidyverse")
library("readxl")
library(dplyr)
library("ggplot2")
library(cowplot)
library(apsimx)
setwd("D:/Jane/apsimruns")
tmp.dir <- tempdir()
extd.dir <-"D:/Jane/apsimruns"
sim <- apsimx("soy.apsimx", value = "HarvestReport")
summary(sim)
glimpse(sim)
unique(sim$SimulationID)

##################Get Met data
## Not run: 
require(nasapower)
## This will not write a file to disk
pwr <- get_power_apsim_met(lonlat = c(32.66, -13.67), 
                           dates = c("1985-01-01","2022-01-01"))

write_apsim_met(pwr, wrt.dir = "D:/Jane/apsimruns", filename = "goodnature.met")

chitedze<-read_apsim_met("goodnature.met", src.dir = "D:/Jane/apsimruns")

inspect_apsimx("soy.apsimx", node = "Weather")
inspect_apsimx("soy.apsimx", node = "Clock")
inspect_apsimx("soy.apsimx", node = "Manager",
               parm = list("SowingRule", NA))

#####################Edit weather
edit_apsimx("soy.apsimx", 
            src.dir = "D:/Jane/apsimruns",
            node = "Weather", 
            value = "goodnature.met", overwrite = TRUE)
###################Edit clock
edit_apsimx("soy.apsimx", 
            node = "Clock",
            parm = c("Start", "End"),
            value = c("1985-01-01T00:00:00", "2020-12-31T00:00:00"),
            overwrite = TRUE)
## Change the sowing rule for when rain is available
edit_apsimx("soy.apsimx", 
            node = "Manager",
            manager.child = "SowingRule",
            parm = "StartDate", ## This is for start date
            value = "1-nov",
            overwrite = TRUE)
edit_apsimx("soy.apsimx", 
            node = "Manager",
            manager.child = "SowingRule",
            parm = "EndDate", ## This is for end date
            value = "30-nov",
            overwrite = TRUE)
edit_apsimx("soy.apsimx", 
            node = "Manager",
            manager.child = "SowingRule",
            parm = "CultivarName", ## This is for end date
            value = "Generic_MG0",
            overwrite = TRUE)
############################Soil 
#sp <- get_isric_soil_profile(lonlat = c(32.66, -13.67))

#edit_apsimx_replace_soil_profile("soy.apsimx", soil.profile = sp, overwrite = TRUE)
sim1 <- apsimx("soy.apsimx", value = "HarvestReport")

# Plot
sim1  %>%
  ggplot( aes(x=as.Date(Clock.Today), y=Yield)) +
  geom_line()
