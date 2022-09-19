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
sim <- apsimx("Soybean_3sites.apsimx", value = "HarvestReport")
glimpse(sim)
angonia<- sim %>%
  filter(Experiment == "Angonia")

angonia  %>%
  ggplot(aes(x=Cultivar, y=Soybean.Phenology.FloweringDAS, group=Cultivar, color=Cultivar)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Flowering Angonia")

angonia  %>%
  ggplot(aes(x=Cultivar, y=Soybean.Phenology.MaturityDAS, group=Cultivar, color=Cultivar)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Maturity Angonia")

angonia  %>%
  ggplot(aes(x=Cultivar, y=Yield, group=Cultivar, color=Cultivar)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Yield Angonia")

angonia  %>%
  ggplot( aes(x=as.Date(Clock.Today), y=Yield, group=Cultivar, color=Cultivar)) +
  geom_line()+
  ggtitle("Yield Angonia")

#############################################################################
chitedze<- sim %>%
  filter(Experiment == "Chitedze")

chitedze  %>%
  ggplot(aes(x=Cultivar, y=Soybean.Phenology.FloweringDAS, group=Cultivar, color=Cultivar)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Flowering Chitedze")

chitedze  %>%
  ggplot(aes(x=Cultivar, y=Soybean.Phenology.MaturityDAS, group=Cultivar, color=Cultivar)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Maturity Chitedze")

chitedze  %>%
  ggplot(aes(x=Cultivar, y=Yield, group=Cultivar, color=Cultivar)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Yield Chitedze")

chitedze  %>%
  ggplot( aes(x=as.Date(Clock.Today), y=Yield, group=Cultivar, color=Cultivar)) +
  geom_line()+
  ggtitle("Yield Chitedze")
############################################################################################
goodnature<- sim %>%
  filter(Experiment == "Chitedze")

goodnature  %>%
  ggplot(aes(x=Cultivar, y=Soybean.Phenology.FloweringDAS, group=Cultivar, color=Cultivar)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Flowering Good Nature")

goodnature  %>%
  ggplot(aes(x=Cultivar, y=Soybean.Phenology.MaturityDAS, group=Cultivar, color=Cultivar)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Days to Maturity Good Nature")

goodnature  %>%
  ggplot(aes(x=Cultivar, y=Yield, group=Cultivar, color=Cultivar)) +
  geom_boxplot(notch=F)+
  geom_jitter()+
  ggtitle("Yield Good Nature")

goodnature  %>%
  ggplot( aes(x=as.Date(Clock.Today), y=Yield, group=Cultivar, color=Cultivar)) +
  geom_line()+
  ggtitle("Yield Good Nature")
