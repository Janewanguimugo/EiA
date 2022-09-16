setwd("~/")
#install.packages("tidyverse")
library("readxl")
library(dplyr)
library("ggplot2")
library(cowplot)

ADV001<-read_excel("Solidarid/data/Malawi/2021 ADV 001 COMBINED DATA.xls", sheet = "Observation")

glimpse(ADV001)
unique(ADV001$DESIGNATION)

final <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "ANGONIA")
names(final)[2]<-"Variety"

final1 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "GURUE")  
names(final1)[2]<-"Variety"

final2 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "IITA-SARAH")  
names(final2)[2]<-"Variety"

final3 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "SEEDCO") 
names(final3)[2]<-"Variety"

final4 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "KARI")  
names(final4)[2]<-"Variety"

final5 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "GOOD NATURE")  
names(final5)[2]<-"Variety"

final6 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "CHITEDZE") 
names(final6)[2]<-"Variety"

final7 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "CHITALA")%>%
  group_by(DESIGNATION)
names(final7)[2]<-"Variety"

final8 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha, Days_to_maturity)%>% 
  filter(Location_Name== "BVUMBWE")  
names(final8)[2]<-"Variety"

################################################################################
p1=ggplot(final, aes(x= Variety, y= Grain_Yield_Kg_Ha)) + 
  geom_boxplot()+ coord_flip()

p2=ggplot(final1, aes(x= Variety, y= Grain_Yield_Kg_Ha)) + 
  geom_boxplot()+ coord_flip()

p3=ggplot(final2, aes(x= Variety, y= Grain_Yield_Kg_Ha)) + 
  geom_boxplot()+ coord_flip()

p4=ggplot(final3, aes(x= Variety, y= Grain_Yield_Kg_Ha)) + 
  geom_boxplot()+ coord_flip()

p5=ggplot(final4, aes(x= Variety, y= Grain_Yield_Kg_Ha)) + 
  geom_boxplot()+ coord_flip()

p6=ggplot(final5, aes(x= Variety, y= Grain_Yield_Kg_Ha)) + 
  geom_boxplot()+ coord_flip()

p7=ggplot(final6, aes(x= Variety, y= Grain_Yield_Kg_Ha)) + 
  geom_boxplot()+ coord_flip()

p8=ggplot(final7, aes(x= Variety, y= Grain_Yield_Kg_Ha)) + 
  geom_boxplot()+ coord_flip()

p9=ggplot(final8, aes(x= Variety, y= Grain_Yield_Kg_Ha)) + 
  geom_boxplot()+ coord_flip()

plot_grid(p1, p2, p3, p4, p5,p6,p7, p8, p9, labels = "auto")

################################################################################
p1=ggplot(final, aes(x= Variety, y= Days_to_maturity)) + 
  geom_boxplot()+ coord_flip()

p2=ggplot(final1, aes(x= Variety, y= Days_to_maturity)) + 
  geom_boxplot()+ coord_flip()

p3=ggplot(final2, aes(x= Variety, y= Days_to_maturity)) + 
  geom_boxplot()+ coord_flip()

p4=ggplot(final3, aes(x= Variety, y= Days_to_maturity)) + 
  geom_boxplot()+ coord_flip()

p5=ggplot(final4, aes(x= Variety, y= Days_to_maturity)) + 
  geom_boxplot()+ coord_flip()

p6=ggplot(final5, aes(x= Variety, y= Days_to_maturity)) + 
  geom_boxplot()+ coord_flip()

p7=ggplot(final6, aes(x= Variety, y= Days_to_maturity)) + 
  geom_boxplot()+ coord_flip()

p8=ggplot(final7, aes(x= Variety, y= Days_to_maturity)) + 
  geom_boxplot()+ coord_flip()

p9=ggplot(final8, aes(x= Variety, y= Days_to_maturity)) + 
  geom_boxplot()+ coord_flip()

plot_grid(p1, p2, p3, p4, p5,p6,p7, p8, p9, labels = "auto")



