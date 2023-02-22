
  #Stable isotopes deliverable 7
  
library(ggplot2)
library(tidyverse)
library(dplyr)
#onboard clean csv file
#change site to an ordered factor
sit1<-read.csv(file.choose(), header = TRUE)%>%
  mutate(Site=ordered(Site, levels=c("DL","GC", "BG")))
str(sit1)
sit1$Site<-as.factor(sit1$Site)
sit1$Site<- ordered(sit1$Site, levels=c("DL", "GC", "BG"))
#basic plots
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

#brown trout one-way anova
sill<-read.csv(file.choose(), header = TRUE) %>%
  mutate(Site=ordered(sill$Site, levels=c("DL","GC", "BG")))
str(sill)
class(sill$ï..Site)
sill$ï..Site<-as.factor(sill$ï..Site)
llout1<-aov(d13C~sill$ï..Site,data = sill)
summary(llout1)

plot(fitted(llout1),resid(llout1))
#add in length as covariate
llout2<-aov(d13C~sill$ï..Site*sill$Length,data = sill)
summary(llout2)
plot(fitted(llout2),resid(llout2))
#same thing by 15N
llout3<-aov(d15N~sill$ï..Site,data = sill)
summary(llout3)
TukeyHSD(llout3)
plot(fitted(llout3),resid(llout3))
#add length
llout4<-aov(d15N~sill$ï..Site*sill$Length,data = sill)
summary(llout4)
plot(fitted(llout4),resid(llout4))

#boxplot
ggplot(data = sill, aes(Site,d15N))+geom_boxplot()+theme_classic()

#large scale suckers one-way anova
silssu<-read.csv(file.choose(), header = TRUE) %>%
  mutate(Site=ordered(Site, levels=c("DL","GC", "BG")))
str(silssu)
class(silssu$Site)

#anova series begins...
lssuout1<-aov(d13C~silssu$ï..Site,data = silssu)
summary(lssuout1)
plot(fitted(lssuout1),resid(lssuout1))
#add in length as covariate
lssuout2<-aov(d13C~silssu$ï..Site*silssu$Length,data = silssu)
summary(lssuout2)
plot(fitted(lssuout2),resid(lssuout2))
lssuout3<-aov(d15N~silssu$ï..Site,data = silssu)
summary(lssuout3)
TukeyHSD(lssuout3)
plot(fitted(lssuout3),resid(lssuout3))
lssuout4<-aov(d15N~silssu$ï..Site*silssu$Length,data = silssu)
summary(lssuout4)
plot(fitted(lssuout4),resid(lssuout4))
ggplot(data = silssu, aes(Site,d15N))+geom_boxplot()+theme_classic()



#mountain whitefish one-way anova
simwf<-read.csv(file.choose(), header = TRUE) %>%
  mutate(Site=ordered(Site, levels=c("DL","GC", "BG")))
str(simwf)
class(simwf$ï..Site)
simwf$ï..Site<-as.factor(simwf$ï..Site)
#anova series begins...
mwfout1<-aov(d13C~simwf$ï..Site,data = simwf)
summary(mwfout1)
TukeyHSD(mwfout1)
plot(fitted(mwfout1),resid(mwfout1))
#add in length as covariate
mwfout2<-aov(d13C~simwf$ï..Site*simwf$Length,data = simwf)
summary(mwfout2)
plot(fitted(mwfout2),resid(mwfout2))
mwfout3<-aov(d15N~simwf$ï..Site,data = simwf)
summary(mwfout3)
plot(fitted(mwfout3),resid(mwfout3))
mwfout4<-aov(d15N~simwf$ï..Site*simwf$Length,data = simwf)
summary(mwfout4)
TukeyHSD(mwfout4)
plot(fitted(mwfout4),resid(mwfout4))
#not going to include histogram because its only moderate and weak evidence for a difference
ggplot(data = simwf, aes(Site,d13C))+geom_boxplot()+theme_classic()

#compare 15N among sites by length#
plot(sit1$Length, sit1$X15N, pch=as.integer(sit1$Site))
legend("bottomright", c("DL","GC", "BG"), cex = .5, pch = 1:3)  
ggplot(data = sit1, aes(Length, X15N, color=Site))+geom_point()+geom_smooth(method = "lm")+theme_classic()

Site <- as.character(sit1$Site)
out1<-glm(sit1$X15N~Site+sit1$Length)                                            
summary(out1)
###
