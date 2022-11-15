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
variety<-"safari" 
clck <- c("2019-10-01T00:00:00", "2020-12-01T00:00:00")
ppln<- 30
edit_apsimx(crop,
            root = c("pd", "Base_one"), 
            node = "Weather",
            overwrite = TRUE,
            value = "chitedze.met")
edit_apsimx(crop, 
            root = c("pd", "Base_one"),
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
edit_apsimx(crop, 
            node = "Manager",
            manager.child = "SowingRule",
            parm = "Population", ## This is for end date
            value = ppln,
            overwrite = TRUE)

apsimx::edit_apsimx(crop,
                    root = c("pd", "Base_one"),
                    node = "Report",
                    parm = "VariableNames", 
                    value = "[Soybean].SowingDate", 
                    verbose = TRUE, overwrite = TRUE)

sp <-  apsimx::get_isric_soil_profile(lonlat = c(30.64, -13.98))
sp$soil$BD <-  sp$soil$BD * 0.86
sp$soil$crop.LL <-  sp$soil$LL15 + 0.01
sp$soil$SAT <-  c(0.521, 0.521, 0.497, 0.488, 0.478, 0.440)

edit_apsimx_replace_soil_profile(crop, 
                                 root = c("pd", "Base_one"),
                                 soil.profile = sp, 
                                 overwrite = TRUE)

inspect_apsimx(crop,
               root = c("pd", "Base_one"),
               node = "Crop")

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

p_Win <- sim  %>% 
  arrange(desc(Yield)) %>% 
  slice(1:10)
p_Win
