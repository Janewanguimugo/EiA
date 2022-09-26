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
sam <- apsimx("Soybean_3sites_PDall.apsimx", value = "HarvestReport")
glimpse(sim)
unique(sim$SowDate)
angonia<- sim %>%
  filter(Experiment == "Angonia", Cultivar == "scserenade")%>%
  mutate(SowDate1 = recode(SowDate, "1-nov" = "1-nov-2019", "7-nov" ="7-nov-2019",
                           "14-nov"="14-nov-2019", "21-nov" ="21-nov-2019",
                           "28-nov" = "28-nov-2019", "5-dec" = "5-dec-2019",
                           "12-dec"= "12-dec-2019", "19-dec" ="19-dec-2019", 
                           "26-dec" ="26-dec-2019", "2-jan" = "2-jan-2020", 
                           "9-jan" = "9-jan-2020", "16-jan" = "16-jan-2020",
                           "23-jan"= "23-jan-2020","30-jan"= "30-jan-2020",
                           "6-feb"= "6-feb-2020",  "13-feb"= "13-feb-2020", 
                           "20-feb"= "20-feb-2020", "27-feb"= "27-feb-2020"))
angonia  %>%
  ggplot(aes(x=as.factor(anydate(SowDate1)), y=Yield, group=fertiliser, color=fertiliser)) +
  geom_point()+
  ggtitle("Planting Dates Angonia_2020")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Sowing Dates")


angonia1<- sam %>%
  filter(Experiment == "Angonia",  Cultivar == "scserenade")

angonia1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates Angonia")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of scserenade")


angonia1<- sam %>%
  filter(Experiment == "Angonia",  Cultivar == "scsentinile")

angonia1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates Angonia")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of scsentinile")


angonia1<- sam %>%
  filter(Experiment == "Angonia",  Cultivar == "mridina")

angonia1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates Angonia")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of MRI Dina")

angonia1<- sam %>%
  filter(Experiment == "Angonia",  Cultivar == "safari")

angonia1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates Angonia")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of Safari")
#################################################
Chitedze1<- sam %>%
  filter(Experiment == "Chitedze",  Cultivar == "scserenade")

Chitedze1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates Chitedze")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of scserenade")


Chitedze1<- sam %>%
  filter(Experiment == "Chitedze",  Cultivar == "scsentinile")

Chitedze1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates Chitedze")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of scsentinile")


Chitedze1<- sam %>%
  filter(Experiment == "Chitedze",  Cultivar == "mridina")

Chitedze1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates Chitedze")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of MRI Dina")

Chitedze1<- sam %>%
  filter(Experiment == "Chitedze",  Cultivar == "safari")

Chitedze1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates Chitedze")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of Safari")

#######################################################
GoodNature1<- sam %>%
  filter(Experiment == "GoodNature",  Cultivar == "scserenade")

GoodNature1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates GoodNature")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of scserenade")


GoodNature1<- sam %>%
  filter(Experiment == "GoodNature",  Cultivar == "scsentinile")

GoodNature1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates GoodNature")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of scsentinile")


GoodNature1<- sam %>%
  filter(Experiment == "GoodNature",  Cultivar == "mridina")

GoodNature1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates GoodNature")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of MRI Dina")

GoodNature1<- sam %>%
  filter(Experiment == "GoodNature",  Cultivar == "safari")

GoodNature1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= fertiliser)) +
  geom_boxplot()+
  ggtitle("Planting Dates GoodNature")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")+
  ylab("Yield of Safari")











##############################END ########################################################
Chitedze<- sim %>%
  filter(Experiment == "Chitedze")%>%
  mutate(SowDate1 = recode(SowDate, "1-nov" = "1-nov-2019", "7-nov" ="7-nov-2019",
                           "14-nov"="14-nov-2019", "21-nov" ="21-nov-2019",
                           "28-nov" = "28-nov-2019", "5-dec" = "5-dec-2019",
                           "12-dec"= "12-dec-2019", "19-dec" ="19-dec-2019", 
                           "26-dec" ="26-dec-2019", "2-jan" = "2-jan-2020", 
                           "9-jan" = "9-jan-2020", "16-jan" = "16-jan-2020",
                           "23-jan"= "23-jan-2020","30-jan"= "30-jan-2020",
                           "6-feb"= "6-feb-2020",  "13-feb"= "13-feb-2020", 
                           "20-feb"= "20-feb-2020", "27-feb"= "27-feb-2020"))
Chitedze  %>%
  ggplot(aes(x=as.factor(anydate(SowDate1)), y=Yield, group=Cultivar, color=Cultivar)) +
  geom_point()+
  geom_line()+
  ggtitle("Planting Dates Chitedze_2020")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Sowing Dates")

Chitedze1<- sam %>%
  filter(Experiment == "Chitedze")

one.way <- aov(Yield ~ SowDate, data = Chitedze1)
summary(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

Chitedze1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec",  
                                         "12-dec", "19-dec", "26-dec", "2-jan", 
                                         "9-jan",  "16-jan", "23-jan", "30-jan",
                                         "6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= Cultivar)) +
  geom_boxplot(notch=F)+
  ggtitle("Planting Dates Chitedze")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")

###########################################################################################

GoodNature<- sim %>%
  filter(Experiment == "GoodNature")%>%
  mutate(SowDate1 = recode(SowDate, "1-nov" = "1-nov-2019", "7-nov" ="7-nov-2019",
                           "14-nov"="14-nov-2019", "21-nov" ="21-nov-2019",
                           "28-nov" = "28-nov-2019", "5-dec" = "5-dec-2019",
                           "12-dec"= "12-dec-2019", "19-dec" ="19-dec-2019", 
                           "26-dec" ="26-dec-2019", "2-jan" = "2-jan-2020", 
                           "9-jan" = "9-jan-2020", "16-jan" = "16-jan-2020",
                           "23-jan"= "23-jan-2020","30-jan"= "30-jan-2020",
                           "6-feb"= "6-feb-2020",  "13-feb"= "13-feb-2020", 
                           "20-feb"= "20-feb-2020", "27-feb"= "27-feb-2020"))
GoodNature  %>%
  ggplot(aes(x=as.factor(anydate(SowDate1)), y=Yield, group=Cultivar, color=Cultivar)) +
  geom_point()+
  geom_line()+
  ggtitle("Planting Dates GoodNature_2020")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Sowing Dates")

GoodNature1<- sam %>%
  filter(Experiment == "GoodNature")

one.way <- aov(Yield ~ SowDate, data = GoodNature1)
summary(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

GoodNature1  %>%
  ggplot(aes(x= factor(SowDate, level =c("1-nov",  "7-nov",  "14-nov", "21-nov",
                                         "28-nov", "5-dec", "12-dec", "19-dec", 
                                         "26-dec", "2-jan", "9-jan",  "16-jan", "23-jan",
                                         "30-jan","6-feb",  "13-feb", "20-feb", "27-feb")), 
             y=Yield, group= SimulationID, color= Cultivar)) +
  geom_boxplot(notch=F)+
  ggtitle("Planting Dates GoodNature")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(fun.y="mean", color="black", size = 0.02)+
  xlab("Sowing Dates")

write.csv(sam, "3sites_fertiliser_PD_Cultivar.csv")
