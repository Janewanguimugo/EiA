setwd("~/")
#install.packages("tidyverse")
library("readxl")
library(dplyr)
library("ggplot2")
library(cowplot)

ADV001<-read_excel("Solidarid/data/Malawi/2021 ADV 001 COMBINED DATA.xls", sheet = "Observation")

glimpse(ADV001)
unique(ADV001$Location_Name)

final <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha)%>% 
  filter(Location_Name== "ANGONIA")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha))

  names(final)[1]<-"Variety"

final1 <- ADV001 %>% 
    select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha)%>% 
    filter(Location_Name== "GURUE")%>%
    group_by(DESIGNATION)%>%
    summarize(yield=mean(Grain_Yield_Kg_Ha))  
names(final1)[1]<-"Variety"


final2 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha)%>% 
  filter(Location_Name== "IITA-SARAH")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha))  
names(final2)[1]<-"Variety"

final3 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha)%>% 
  filter(Location_Name== "SEEDCO")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha))  
names(final3)[1]<-"Variety"

final4 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha)%>% 
  filter(Location_Name== "KARI")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha))  
names(final4)[1]<-"Variety"

final5 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha)%>% 
  filter(Location_Name== "GOOD NATURE")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha))  
names(final5)[1]<-"Variety"

final6 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha)%>% 
  filter(Location_Name== "CHITEDZE")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha))  
names(final6)[1]<-"Variety"

final7 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha)%>% 
  filter(Location_Name== "CHITALA")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha))  
names(final7)[1]<-"Variety"

final8 <- ADV001 %>% 
  select(Location_Name, DESIGNATION, Grain_Yield_Kg_Ha)%>% 
  filter(Location_Name== "BVUMBWE")%>%
  group_by(DESIGNATION)%>%
  summarize(yield=mean(Grain_Yield_Kg_Ha))  
names(final8)[1]<-"Variety"


p1=ggplot(final, aes(x= Variety, y= yield)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

p2=ggplot(final1, aes(x= Variety, y= yield)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

p3=ggplot(final2, aes(x= Variety, y= yield)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

p4=ggplot(final3, aes(x= Variety, y= yield)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

p5=ggplot(final4, aes(x= Variety, y= yield)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

p6=ggplot(final5, aes(x= Variety, y= yield)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

p7=ggplot(final6, aes(x= Variety, y= yield)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

p8=ggplot(final7, aes(x= Variety, y= yield)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

p9=ggplot(final8, aes(x= Variety, y= yield)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()


#############################################################################
data_new <- final %>% 
  select(Variety, yield,)%>% 
  arrange(desc(yield)) %>% 
  slice(1:5)


data_new1 <- final1 %>% 
  select(Variety, yield,)%>% 
  arrange(desc(yield)) %>% 
  slice(1:5)

data_new2 <- final2 %>% 
  select(Variety, yield,)%>% 
  arrange(desc(yield)) %>% 
  slice(1:5)
 
data_new3 <- final3 %>% 
  select(Variety, yield,)%>% 
  arrange(desc(yield)) %>% 
  slice(1:5)

data_new4 <- final4 %>% 
  select(Variety, yield,)%>% 
  arrange(desc(yield)) %>% 
  slice(1:5)

data_new5 <- final5 %>% 
  select(Variety, yield,)%>% 
  arrange(desc(yield)) %>% 
  slice(1:5)

data_new6 <- final6 %>% 
  select(Variety, yield,)%>% 
  arrange(desc(yield)) %>% 
  slice(1:5)

data_new7 <- final7 %>% 
  select(Variety, yield,)%>% 
  arrange(desc(yield)) %>% 
  slice(1:5)

data_new8 <- final8 %>% 
  select(Variety, yield,)%>% 
  arrange(desc(yield)) %>% 
  slice(1:5)

x1=ggplot(data_new, aes(x= Variety, y= yield, fill = Variety)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

x2=ggplot(data_new1, aes(x= Variety, y= yield, fill = Variety)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

x3=ggplot(data_new2, aes(x= Variety, y= yield, fill = Variety)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

x4=ggplot(data_new3, aes(x= Variety, y= yield, fill = Variety)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

x5=ggplot(data_new4, aes(x= Variety, y= yield, fill = Variety)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

x6=ggplot(data_new5, aes(x= Variety, y= yield, fill = Variety)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

x7=ggplot(data_new6, aes(x= Variety, y= yield, fill = Variety)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

x8=ggplot(data_new7, aes(x= Variety, y= yield, fill = Variety)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()

x9=ggplot(data_new8, aes(x= Variety, y= yield, fill = Variety)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()


plot_grid(p1, x1,labels = "auto")
plot_grid(p2, x2,labels = "auto")
plot_grid(p3, x3,labels = "auto")
plot_grid(p4, x4,labels = "auto")
plot_grid(p5, x5,labels = "auto")
plot_grid(p6, x6,labels = "auto")
plot_grid(p7, x7,labels = "auto")
plot_grid(p8, x8,labels = "auto")
plot_grid(p9, x9,labels = "auto")

##############################################################################

