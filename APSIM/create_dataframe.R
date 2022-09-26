library(ggplot2)

dtfA <- data.frame(Cultivar = c("Safari", "MRI Dina", "Sc Serenade", "Sc Sentinile"),
                 OBS =  c(3125, 1790, 3177, 2404),
                PRED = c(3096, 2684, 3451, 3177)
)
print (dtfA)

dtfA  %>%
  ggplot(aes(x=as.factor(OBS), y=as.factor(PRED), group=Cultivar, color=Cultivar)) +
  geom_point()+
  ggtitle("Accuracy Yield")+
  xlab("Observed")+
  ylab("Predicted")
