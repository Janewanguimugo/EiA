#remotes::install_github("femiguez/apsimx")
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
library(anytime)
setwd("D:/Jane/apsimruns")
extd.dir <- "D:/Jane/apsimruns"
tmp.dir <- tempdir()

crop<-"soy_fac_jane.apsimx" 
sd<-"1-jan"
ed<- "30-dec" 
variety<-"mridina" 
clck <- c("2019-10-01T00:00:00", "2020-12-01T00:00:00")
edit_apsimx(crop,
            root = c("pd", "Base"), 
            node = "Weather",
            overwrite = TRUE,
            value = "chitedze.met")
edit_apsimx(crop, 
            root = c("pd", "Base"),
            node = "Clock",
            parm = c("Start", "End"),
            value = clck,
            overwrite = TRUE)
edit_apsimx(crop, 
            node = "Manager",
            manager.child = "SowingRule",
            parm = "CultivarName", ## This is for end date
            value = as.character(variety),
            overwrite = TRUE)

sp <-  apsimx::get_isric_soil_profile(lonlat = c(33.64, -13.98))
edit_apsimx_replace_soil_profile(crop, root = c("pd", "Base"), soil.profile = sp, overwrite = TRUE)

inspect_apsimx(crop,
               root = c("pd", "Base"),
               node = "Soil")
sim <- apsimx(crop, value = "HarvestReport")

sim<- sim %>%
  mutate(DayMonth = format(anydate(SowDate), "%d-%m"))

sim  %>%
  ggplot( aes(x=format(anydate(DayMonth), "%d-%m"), y=Yield)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sim  %>%
  ggplot( aes(x= SimulationID , y=Yield)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 365, by = 5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

final<-sim %>%
  slice(which.max(Yield))%>%
  as.data.frame()


