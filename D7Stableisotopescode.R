
  #Stable isotopes deliverable 7
    #Tray 1 only
library(ggplot2)
library(dplyr)
#onboard clean csv file
sit1<-read.csv(file.choose(), header = TRUE)
str(sit1)
plot(sit1$X13C,sit1$X15N, col=factor(sit1$Site), pch=19) 

pone<-ggplot(data = sit1, aes(X13C,X15N))+geom_point()
pone + facet_wrap(vars(sit1$Site))


#anova between sites for d13C
out1=aov(X13C~Site, sit1)
summary(out1)
model.tables(out1,type="means")
TukeyHSD(out1)
#anova between sites for d15N
out2=aov(X15N~Site, sit1)
summary(out2)
model.tables(out2,type="means")
TukeyHSD(out2)
#2-way Anova
out3=aov(X13C~X15N+Site, sit1)
summary (out3)
