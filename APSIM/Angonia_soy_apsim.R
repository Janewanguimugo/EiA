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
sim <- apsimx("soybean_Angonia.apsimx", value = "report")
summary(sim)
glimpse(sim)
unique(sim$SimulationID)

# Keep only 3 names
graph <- sim %>% 
  mutate(SimulationID = replace (SimulationID, SimulationID==1, "Sc Sentinile"))%>%
  mutate(SimulationID = replace (SimulationID, SimulationID==2, "MRI Dina"))%>%
  mutate(SimulationID = replace (SimulationID, SimulationID==3, "Sc Serenade"))%>%
  mutate(SimulationID = replace (SimulationID, SimulationID==4, "Sc Safari"))
# Plot
graph  %>%
  ggplot( aes(x=as.Date(Clock.Today), y=Yield, group=SimulationID, color=SimulationID)) +
  geom_line()

graph  %>%
  ggplot(aes(x=as.Date(Clock.Today), y=Yield, group=SimulationID, color=SimulationID)) +
  geom_boxplot(notch=TRUE)+
  geom_jitter()
