setwd("~/Solidarid/N2Africa Monitoringdata")
#install.packages("tidyverse")
library("readxl")
library(dplyr)
library("ggplot2")
library(cowplot)
library(ggplot2)
library(hrbrthemes)
year1<-read_excel("Mozambique/2011_12/Moz_Consolidated_2011_12.xlsx", sheet = 1)
year2<-read_excel("Mozambique/2011_12_11/Moz_Consolidated_2011_12_11.xlsx", sheet = 1)
year3<-read_excel("Mozambique/2011_12_111/Moz_Consolidated_2011_12_111.xlsx", sheet = 1)
year4<-read_excel("Mozambique/2012_13/Moz_Consolidated_2012_13.xlsx", sheet = 1)

glimpse(year4)
unique(year3$crop_1)
unique(year4$crop)


data1<- year1 %>% 
  mutate(year = "2011_12")%>% 
  select( year, crop_1, mineral_fert_type, mineral_fert_amount, crop_1_area_harvested, crop_1_weight_grain)%>%
  mutate(grain_yield =crop_1_weight_grain/(crop_1_area_harvested/10000), 
         Ferilizer_amount =mineral_fert_amount/(crop_1_area_harvested/10000))%>%
  filter(crop_1== "soybean")

data2<- year2 %>% 
  mutate(year = "2011_12_11")%>% 
  select( year, crop_1, mineral_fert_type, mineral_fert_amount, crop_1_area_harvested, crop_1_weight_grain)%>%
  mutate(grain_yield =crop_1_weight_grain/(crop_1_area_harvested/10000), 
         Ferilizer_amount =mineral_fert_amount/(crop_1_area_harvested/10000))%>%
  filter(!grepl("Groundnut", crop_1))

data3<- year3 %>% 
  mutate(year = "2011_12_111")%>% 
  select( year, crop_1, mineral_fert_type, mineral_fert_amount, crop_1_area_harvested, crop_1_weight_grain)%>%
  mutate(grain_yield =crop_1_weight_grain/(crop_1_area_harvested/10000), 
         Ferilizer_amount =mineral_fert_amount/(crop_1_area_harvested/10000))%>%
  filter(!grepl("Groundnut", crop_1))

data4<- year4 %>% 
 mutate(year = "2012_13")%>% 
  rename(mineral_fert_type = min_fertilizer_type, mineral_fert_amount = min_fertiliser_amount_kg, 
         crop_1_area_harvested = area_harvested_m2, crop_1_weight_grain = weight_grain , crop_1 = crop)%>% 
  select( year,crop_1, mineral_fert_type, mineral_fert_amount, crop_1_area_harvested, crop_1_weight_grain)%>%
  mutate(mineral_fert_amount= as.numeric(gsub(".*?([0-9]+).*", "\\1", mineral_fert_amount)),
         crop_1_weight_grain= as.numeric(gsub(".*?([0-9]+).*", "\\1", crop_1_weight_grain)))%>%
  mutate(grain_yield =crop_1_weight_grain/(crop_1_area_harvested/10000), 
         Ferilizer_amount =mineral_fert_amount/(crop_1_area_harvested/10000))%>%
  filter(!grepl('Groundnuit"|Groudnuit|NA|100', crop_1))

Merged <- do.call("rbind", list(data1, data2, data3))
glimpse(Merged)

Merged<- do.call(data.frame,      # Replace Inf in data by NA
                 lapply(Merged,
                        function(x) replace(x, is.infinite(x), NA)))
Merged <- Merged[complete.cases(Merged), ] 

p1<- ggplot(Merged, aes(x=Ferilizer_amount, y=grain_yield, color=mineral_fert_type)) + 
  geom_point(size=6) +
  ggtitle ("Mozambique")+
  theme(plot.title = element_text(hjust = 0.5))

###########################################################################################################
setwd("~/Solidarid/N2Africa Monitoringdata")
#install.packages("tidyverse")
library("readxl")
library(dplyr)
library("ggplot2")
library(cowplot)
library(ggplot2)
library(hrbrthemes)
data201011<-read_excel("Malawi/2010_11/Mal_Consolidated_2010_11.xlsx", sheet = 1)
data201112<-read_excel("Malawi/2011_12/Mal_Consolidated_2011_12.xlsx", sheet = 1)
data201213<-read_excel("Malawi/2012_13/Mal_Consolidated_2012_13.xlsx", sheet = 1)


glimpse(data201011)
unique(data201213$crop)
glimpse(data201112)
glimpse(data201213)

data1<- data201011 %>% 
  mutate(year = 1011)%>% 
  select( year, crop_1, mineral_fert_type, mineral_fert_amount, crop_1_area_harvested, crop_1_weight_grain)%>%
  mutate(grain_yield =crop_1_weight_grain/(crop_1_area_harvested/10000), 
         Ferilizer_amount =mineral_fert_amount/(crop_1_area_harvested/10000))%>%
  filter(crop_1== "soybean")

data2<- data201112 %>% 
  mutate(year = 1112)%>% 
  select( year, crop_1, mineral_fert_type, mineral_fert_amount, crop_1_area_harvested, crop_1_weight_grain)%>%
  mutate(grain_yield =crop_1_weight_grain/(crop_1_area_harvested/10000), 
         Ferilizer_amount =mineral_fert_amount/(crop_1_area_harvested/10000))%>%
  filter(grepl('soybean|Soybean', crop_1))


data3<- data201213 %>% 
  mutate(year = 1213)%>% 
  rename(mineral_fert_type = min_fertilizer_type, mineral_fert_amount = min_fertiliser_amount_kg, 
         crop_1_area_harvested = area_harvested_m2, crop_1_weight_grain = weight_grain , crop_1 = crop)%>% 
  select( year,crop_1, mineral_fert_type, mineral_fert_amount, crop_1_area_harvested, crop_1_weight_grain)%>%
  mutate(mineral_fert_amount= as.numeric(gsub(".*?([0-9]+).*", "\\1", mineral_fert_amount)),
         crop_1_weight_grain= as.numeric(gsub(".*?([0-9]+).*", "\\1", crop_1_weight_grain)))%>%
  mutate(grain_yield =crop_1_weight_grain/(crop_1_area_harvested/10000), 
         Ferilizer_amount =mineral_fert_amount/(crop_1_area_harvested/10000))%>%
  filter(crop_1== "soybean")

#Merged <- Merged[is.finite(Merged$grain_yield) & is.finite(Merged$Ferilizer_amount), ] remove rows with inf

#k<-as.data.frame (as.numeric(gsub(".*?([0-9]+).*", "\\1", Merged$mineral_fert_amount))) 
#j<-as.data.frame (as.numeric(gsub(".*?([0-9]+).*", "\\1", Merged$crop_1_weight_grain)))   

Merged <- do.call("rbind", list(data1, data2, data3))
glimpse(Merged)

Merged<- do.call(data.frame,      # Replace Inf in data by NA
                 lapply(Merged,
                        function(x) replace(x, is.infinite(x), NA)))
Merged <- Merged[complete.cases(Merged), ] 

p2<-ggplot(Merged, aes(x=Ferilizer_amount, y=grain_yield, color=mineral_fert_type)) + 
  geom_point(size=6) +
  ggtitle ("Malawi")+
  theme(plot.title = element_text(hjust = 0.5))

##############################################################################################################
setwd("~/Solidarid/N2Africa Monitoringdata")
#install.packages("tidyverse")
library("readxl")
library(dplyr)
library("ggplot2")
library(cowplot)
library(ggplot2)
library(hrbrthemes)
library(cowplot)
year1<-read_excel("Zimbabwe/2011_12/Zim_Consolidated_2011_12.xlsx", sheet = 1)
year2<-read_excel("Zimbabwe/2012_13/Zim_Consolidated_2012_13.xlsx", sheet = 1)

glimpse(year2)
unique(year1$crop_1)
glimpse(data201112)
glimpse(data201213)

data1<- year1 %>% 
  mutate(year = "2011_12")%>% 
  select( year, crop_1, mineral_fert_type, mineral_fert_amount, crop_1_area_harvested, crop_1_weight_grain)%>%
  mutate(grain_yield =crop_1_weight_grain/(crop_1_area_harvested/10000), 
         Ferilizer_amount =mineral_fert_amount/(crop_1_area_harvested/10000))%>%
  filter(grepl('soybean|Soybean', crop_1))

data2<- year2 %>% 
  mutate(year = "2012_13")%>% 
  rename(mineral_fert_type = min_fertilizer_type, mineral_fert_amount = min_fertiliser_amount_kg, 
         crop_1_area_harvested = area_harvested_m2, crop_1_weight_grain = weight_grain , crop_1 = crop)%>% 
  select( year,crop_1, mineral_fert_type, mineral_fert_amount, crop_1_area_harvested, crop_1_weight_grain)%>%
  mutate(mineral_fert_amount= as.numeric(gsub(".*?([0-9]+).*", "\\1", mineral_fert_amount)),
         crop_1_weight_grain= as.numeric(gsub(".*?([0-9]+).*", "\\1", crop_1_weight_grain)))%>%
  mutate(grain_yield =crop_1_weight_grain/(crop_1_area_harvested/10000), 
         Ferilizer_amount =mineral_fert_amount/(crop_1_area_harvested/10000))%>%
  filter(grepl('soya bean|soyabean|soya|soya beans|Soya beans', crop_1))


Merged <- do.call("rbind", list(data1, data2))
glimpse(Merged)

Merged<- do.call(data.frame,      # Replace Inf in data by NA
                 lapply(Merged,
                        function(x) replace(x, is.infinite(x), NA)))
Merged <- Merged[complete.cases(Merged), ] 

p3<-ggplot(Merged, aes(x=Ferilizer_amount, y=grain_yield, color=mineral_fert_type)) + 
  geom_point(size=6) +
  ggtitle ("Zimbabwe")+
  theme(plot.title = element_text(hjust = 0.5))
plot_grid(p1, p2, p3, labels = "auto")