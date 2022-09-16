setwd("~/")
#install.packages("tidyverse")
library("readxl")
library(dplyr)
library("ggplot2")
library(cowplot)

ADV001<-read_excel("Solidarid/data/Malawi/2021 ADV 001 COMBINED DATA.xls", sheet = "Observation")

glimpse(ADV001)
unique(ADV001$Location_Name)

ANGONIA <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "ANGONIA")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha), Days_to_maturity=mean(Days_to_maturity))%>%
  filter(yield >= 2500, between(Days_to_maturity,100,120))

names(ANGONIA)[1]<-"Variety"


GURUE <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "GURUE")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha), Days_to_maturity=mean(Days_to_maturity))%>%
  filter(yield >= 2500, between(Days_to_maturity,100,120))
names(GURUE)[1]<-"Variety"

SARAH <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "IITA-SARAH")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha), Days_to_maturity=mean(Days_to_maturity))%>%
  filter(yield >= 2500, between(Days_to_maturity,100,120))
names(SARAH)[1]<-"Variety"

SEEDCO <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "SEEDCO")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha), Days_to_maturity=mean(Days_to_maturity))%>%
  filter(yield >= 2500, between(Days_to_maturity,100,120))  
names(SEEDCO)[1]<-"Variety"

KARI <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "KARI")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha), Days_to_maturity=mean(Days_to_maturity))%>%
  filter(yield >= 2500, between(Days_to_maturity,100,120))   
names(KARI)[1]<-"Variety"

GOOD_NATURE <-ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "GOOD NATURE")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha), Days_to_maturity=mean(Days_to_maturity))%>%
  filter(yield >= 2500, between(Days_to_maturity,100,120))  
names(GOOD_NATURE)[1]<-"Variety"

CHITEDZE <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "CHITEDZE")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha), Days_to_maturity=mean(Days_to_maturity))%>%
  filter(yield >= 2500, between(Days_to_maturity,100,120)) 
names(CHITEDZE)[1]<-"Variety"

CHITALA <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "CHITALA")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha), Days_to_maturity=mean(Days_to_maturity))%>%
  filter(yield >= 2500, between(Days_to_maturity,100,120))  
names(CHITALA)[1]<-"Variety"

BVUMBWE <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "BVUMBWE")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha), Days_to_maturity=mean(Days_to_maturity))%>%
  filter(yield >= 2500, between(Days_to_maturity,100,120))   
names(BVUMBWE)[1]<-"Variety"

new_data_frame(list(Location = unique(ADV001$Location_Name), count= c(nrow(SARAH),
                                                                      nrow(SEEDCO),
                                                                      nrow(KARI),
                                                                      nrow(GOOD_NATURE),
                                                                      nrow(CHITEDZE),
                                                                      nrow(CHITALA),
                                                                      nrow(BVUMBWE),
                                                                      nrow(ANGONIA),
                                                                      nrow(GURUE))))


