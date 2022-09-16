setwd("~/")
#install.packages("tidyverse")
library("readxl")
library(dplyr)
library("ggplot2")
ADV001<-read_excel("data/Zambia/Summary_ 2021 ADV 001_Zambia.xls", sheet = 1)

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
  geom_bar(position="dodge", stat="identity")+coord_flip()

plot_grid(p1, p2,labels = "auto")
