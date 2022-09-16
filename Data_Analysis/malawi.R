setwd("~/")
#install.packages("tidyverse")
library("readxl")
library(dplyr)
library("ggplot2")
library(cowplot)
ADV001<-read_excel("data/Malawi/2021 ADV 001 COMBINED DATA.xls", sheet = "bvumbwe_yield_maturity_flower")

glimpse(ADV001)
unique(ADV001$varieties)
p1<-ggplot(ADV001, aes(x= varieties, y=`yield (kg/ha)`)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

data_new <- ADV001 %>% 
  select(varieties, `yield (kg/ha)`,)%>% 
  arrange(desc(`yield (kg/ha)`)) %>% 
  slice(1:5)
data_new 

p2<-ggplot(data_new, aes(x= varieties, y=`yield (kg/ha)`, fill = varieties)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

plot_grid(p1, p2,labels = "auto")

##########################################################################
ADV002<-read_excel("data/Malawi/2021 ADV002 COMBINED DATA.xls", sheet = "yield_flow_maturity_chitedze")

glimpse(ADV002)
unique(ADV002$variety)
p3<-ggplot(ADV002, aes(x= variety, y=`yield (kg/ha)`)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

data_new <- ADV002 %>% 
  select(variety, `yield (kg/ha)`,)%>% 
  arrange(desc(`yield (kg/ha)`)) %>% 
  slice(1:5)
data_new 

p4<-ggplot(data_new, aes(x= variety, y=`yield (kg/ha)`, fill = variety)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

plot_grid(p3, p4,labels = "auto")

############################################################################
ADV003<-read_excel("data/Malawi/2021 ADV 003 COMBINED DATA.xls", sheet = "flower_maturity_yield_bvumbwe")

glimpse(ADV003)
unique(ADV003$varieties)
p5<-ggplot(ADV003, aes(x= varieties, y=`yield (kg/ha)`)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

data_new <- ADV003 %>% 
  select(varieties, `yield (kg/ha)`,)%>% 
  arrange(desc(`yield (kg/ha)`)) %>% 
  slice(1:5)
data_new 

p6<-ggplot(data_new, aes(x= varieties, y=`yield (kg/ha)`, fill = varieties)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

plot_grid(p5, p6,labels = "auto")

############################################################################
chitedze<-read_excel("data/Malawi/chitedze_2020_2021.xls", sheet = 1)
glimpse(chitedze)
unique(chitedze$varieties)
p7<-ggplot(chitedze, aes(x= varieties, y=`yield (kg/ha)`)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

data_new <- chitedze %>% 
  select(varieties, `yield (kg/ha)`,)%>% 
  arrange(desc(`yield (kg/ha)`)) %>% 
  slice(1:5)
data_new 

p8<-ggplot(data_new, aes(x= varieties, y=`yield (kg/ha)`, fill = varieties)) + 
  geom_bar(position="dodge", stat="identity")+ coord_flip()

plot_grid(p7, p8,labels = "auto")
