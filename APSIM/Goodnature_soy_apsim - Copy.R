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
sim <- apsimx("soybean_Goodnature.apsimx", value = "report")
summary(sim)
glimpse(sim)
unique(sim$SimulationID)
graph <- sim %>% 
  mutate(SimulationID = replace (SimulationID, SimulationID==1, "MRI Dina"))%>%
  mutate(SimulationID = replace (SimulationID, SimulationID==2, "Sc Sentinile"))%>%
  mutate(SimulationID = replace (SimulationID, SimulationID==3, "Sc Safari"))%>%
  mutate(SimulationID = replace (SimulationID, SimulationID==4, "Sc Serenade"))
glimpse(graph)

graph  %>%
  ggplot(aes(x=SimulationID, y=Soybean.Phenology.FloweringDAS, group=SimulationID, color=SimulationID)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Flowering Good Nature")

graph  %>%
  ggplot(aes(x=SimulationID, y=Soybean.Phenology.MaturityDAS, group=SimulationID, color=SimulationID)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Maturity Good Nature")

graph  %>%
  ggplot(aes(x=SimulationID, y=Yield, group=SimulationID, color=SimulationID)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Yield Good Nature")

graph  %>%
  ggplot( aes(x=as.Date(Clock.Today), y=Yield, group=SimulationID, color=SimulationID)) +
  geom_line()+
  ggtitle("Yield Good Nature")
