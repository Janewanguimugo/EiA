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
tmp.dir <- tempdir()
sim <- apsimx("Soybean_3sites_PD.apsimx", value = "HarvestReport")
glimpse(sim)
unique(sim$SowDate)
angonia<- sim %>%
  filter(Experiment == "Angonia", Cultivar == "mridina")%>%
   mutate(SowDate1 = recode(SowDate, "1-nov" = "1-nov-2019", "7-nov" ="7-nov-2019", "14-nov"="14-nov-2019", "21-nov" ="21-nov-2019",
       "28-nov" = "28-nov-2019", "5-dec" = "5-dec-2019", "12-dec"= "12-dec-2019", "19-dec" ="19-dec-2019", "26-dec" ="26-dec-2019",
        "2-jan" = "2-jan-2020", "9-jan" = "9-jan-2020", "16-jan" = "16-jan-2020", "23-jan"= "23-jan-2020",
        "30-jan"= "30-jan-2020", "6-feb"= "6-feb-2020",  "13-feb"= "13-feb-2020", "20-feb"= "20-feb-2020", "27-feb"= "27-feb-2020"))


#angonia<- sim %>%
 # filter(Experiment == "Angonia", Cultivar == "mridina")%>%
  #mutate(SowDate = recode(SowDate, "1-nov" = "01-11-2019", "7-nov" ="07-11-2019",
                         # "14-nov"="14-11-2019", "21-nov" ="21-11-2019","28-nov"="28-11-2019", 
                         # "5-dec" = "05-12-2019", "12-dec"= "12-12-2019", "19-dec" ="19-12-2019", 
                        #  "26-dec" ="26-12-2019","2-jan" = "02-01-2020", "9-jan" = "09-01-2020", 
                         # "16-jan" = "16-01-2020", "23-jan"= "23-01-2020","30-jan"= "30-01-2020", 
                        #  "6-feb"= "06-02-2020",  "13-feb"= "13-02-2020", "20-feb"= "20-02-2020", 
                         # "27-feb"= "27-02-2020"))
 #angonia<- sim %>%
 # filter(Experiment == "Angonia", Cultivar == "mridina")%>%
  #mutate(SowDate= paste0(SowDate,"-2020"))%>%
  #filter(grepl("2020", Clock.Today))

glimpse(angonia)

one.way <- aov(Yield ~ SowDate, data = angonia)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

summary(one.way)

angonia  %>%
  ggplot(aes(x=anydate(SowDate), y=Yield, group=SowDate, color=SowDate)) +
  geom_boxplot(notch=F)+
  ggtitle("Yield Angonia MRI Dina")+
  stat_summary(fun.y="mean", color="black", shape=13)

angonia  %>%
  ggplot(aes(x=as.factor(anydate(SowDate1)), y=Yield, group=SowDate, color=SowDate)) +
  geom_point()+
  ggtitle("Yield Angonia MRI Dina")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#############################################################################
chitedze<- sim %>%
  filter(Experiment == "Angonia", Cultivar == "mridina")


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
