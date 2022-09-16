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
sim <- apsimx("soybean_Chitedze.apsimx", value = "HarvestReport")
print(sim$SimulationID)
summary(sim)
glimpse(sim)
unique(sim$SimulationID)
## Inspect the Wheat .apsimx file

# Keep only 3 names
graph <- sim %>% 
  mutate(SimulationID = replace (SimulationID, SimulationID==1, "Sc Serenade"))%>%
  mutate(SimulationID = replace (SimulationID, SimulationID==2, "Sc Sentinile"))%>%
  mutate(SimulationID = replace (SimulationID, SimulationID==3, "Sc Safari"))%>%
  mutate(SimulationID = replace (SimulationID, SimulationID==4, "MRI Dina"))
# Plot
graph  %>%
  ggplot(aes(x=SimulationID, y=Soybean.Phenology.FloweringDAS, group=SimulationID, color=SimulationID)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Flowering Chitedze")

graph  %>%
  ggplot(aes(x=SimulationID, y=Soybean.Phenology.MaturityDAS, group=SimulationID, color=SimulationID)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Maturity Chitedze")

graph  %>%
  ggplot(aes(x=SimulationID, y=Yield, group=SimulationID, color=SimulationID)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Yield Chitedze")

graph  %>%
  ggplot( aes(x=as.Date(Clock.Today), y=Yield, group=SimulationID, color=SimulationID)) +
  geom_line()+
  ggtitle("Yield Chitedze")

