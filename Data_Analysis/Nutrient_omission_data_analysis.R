setwd("D:/OneDrive - CGIAR/Documents/Solidarid")
# Libraries
library("readxl")
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(skimr)

data<-read_excel("SMART Farm Omission trial yields & net-margins 2021-12-01.xlsx", sheet = 1)
skimmed <- skim_to_wide(data)
glimpse(data)

# Plot
data %>%
  ggplot(aes(x=`Treatment (Short name)`, y=as.numeric(`Yield (ton/ha)`), fill=`Treatment (Short name)`)) +
  geom_boxplot(notch=TRUE) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Yield vs Treatment") +
  ylab("Yield (ton/ha)")

data %>%
  ggplot(aes(x=`Treatment (Short name)`, y=as.numeric(`Yield (ton/ha)`), fill=`Treatment (Short name)`)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  ggtitle("Yield vs Treatment") +
  ylab("Yield (ton/ha)")+
  xlab("Treatment")

data %>%
  ggplot(aes(x=`Treatment (Short name)`, y=as.numeric(`Yield (ton/ha)`), fill= Country)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  ggtitle("Yield vs Treatment") +
  ylab("Yield (ton/ha)")+
  xlab("Treatment")

data %>%
  ggplot(aes(x=as.numeric(`Rank Yield`), y=as.numeric(`Yield (ton/ha)`), fill= Location)) +
  geom_line() +
  ggtitle("Yield vs Treatment") +
  ylab("Yield (ton/ha)")+
  xlab("Treatment")

