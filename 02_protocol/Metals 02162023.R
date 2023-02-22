library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
totmet<-read.csv(file.choose(), header = TRUE)
totmet$Site<-as.factor(totmet$Site)
totmet$Site <- ordered(totmet$Site, levels=c("DL","GC", "BG"))
Length <-as.numeric(totmet$Length)
Weight <- as.numeric(totmet$Weight)
class(totmet$Tissue)
str(totmet)
is.na(totmet$As)
As<-na.exclude(totmet$As)
--
####Summary Stats####
#Deerlodge
dfllg<- filter(totmet, Site == "DL", Tissue == "g", Species == "LL")
summary(dfllg)
stdev<- c(sd(dfllg$As), sd(dfllg$Cd), sd(dfllg$Cu),sd(dfllg$Pb), sd(dfllg$Se), sd(dfllg$Zn))
stdev
dflll<- filter(totmet, Site == "DL", Tissue == "l", Species == "LL")
summary(dflll)
stdevlll<- c(sd(dflll$As), sd(dflll$Cd), sd(dflll$Cu),sd(dflll$Pb), sd(dflll$Se), sd(dflll$Zn))
stdevlll
dfllm<- filter(totmet, Site == "DL", Tissue == "m", Species == "LL")
summary(dfllm)
stdevllm<- c(sd(dfllm$As), sd(dfllm$Cd), sd(dfllm$Cu),sd(dfllm$Pb), sd(dfllm$Se), sd(dfllm$Zn))
stdevllm

dflssug<- filter(totmet, Site == "DL", Tissue == "g", Species == "LSSU")
summary(dflssug)
stdevlssug<- c(sd(dflssug$As), sd(dflssug$Cd), sd(dflssug$Cu),sd(dflssug$Pb), sd(dflssug$Se), sd(dflssug$Zn))
stdevlssug
dflssul<- filter(totmet, Site == "DL", Tissue == "l", Species == "LSSU")
summary(dflssul)
stdevlssul<- c(sd(dflssul$As), sd(dflssul$Cd), sd(dflssul$Cu),sd(dflssul$Pb), sd(dflssul$Se), sd(dflssul$Zn))
stdevlssul
dflssum<- filter(totmet, Site == "DL", Tissue == "m", Species == "LSSU")
summary(dflssum)
stdevlssum<- c(sd(dflssum$As), sd(dflssum$Cd), sd(dflssum$Cu),sd(dflssum$Pb), sd(dflssum$Se), sd(dflssum$Zn))
stdevlssum

dfmwfg<- filter(totmet, Site == "DL", Tissue == "g", Species == "MWF")
summary(dfmwfg)
sd(dfmwfg$Zn)
dfmwfl<- filter(totmet, Site == "DL", Tissue == "l", Species == "MWF")
summary(dfmwfl)
sd(dfmwfl$Zn)
dfmwfm<- filter(totmet, Site == "DL", Tissue == "m", Species == "MWF")
summary(dfmwfm)

dfrssh<- filter(totmet, Site == "DL", Tissue == "wb", Species == "RSSH")
summary(dfrssh)
stdevrssh<- c(sd(dfrssh$As), sd(dfrssh$Cd), sd(dfrssh$Cu),sd(dfrssh$Pb), sd(dfrssh$Se), sd(dfrssh$Zn))
stdevrssh

dflnsum<- filter(totmet, Site == "DL", Tissue == "m", Species == "LNSU")
summary(dflnsum)

#Gold Creek
dfllg<- filter(totmet, Site == "GC", Tissue == "g", Species == "LL")
summary(dfllg)
stdev<- c(sd(dfllg$As), sd(dfllg$Cd), sd(dfllg$Cu),sd(dfllg$Pb), sd(dfllg$Se), sd(dfllg$Zn))
stdev
dflll<- filter(totmet, Site == "GC", Tissue == "l", Species == "LL")
summary(dflll)
stdevlll<- c(sd(dflll$As), sd(dflll$Cd), sd(dflll$Cu),sd(dflll$Pb), sd(dflll$Se), sd(dflll$Zn))
stdevlll
dfllm<- filter(totmet, Site == "GC", Tissue == "m", Species == "LL")
summary(dfllm)
stdevllm<- c(sd(dfllm$As), sd(dfllm$Cd), sd(dfllm$Cu),sd(dfllm$Pb), sd(dfllm$Se), sd(dfllm$Zn))
stdevllm

dflssuwb<- filter(totmet, Site == "GC", Tissue == "WB", Species == "LSSU")
summary(dflssuwb)
stdevlssuwb<- c(sd(dflssuwb$As), sd(dflssuwb$Cd), sd(dflssuwb$Cu),sd(dflssuwb$Pb), sd(dflssuwb$Se), sd(dflssuwb$Zn))
stdevlssuwb


dfmwfg<- filter(totmet, Site == "GC", Tissue == "g", Species == "MWF")
summary(dfmwfg)
stdevmwfg<- c(sd(dfmwfg$As), sd(dfmwfg$Cd), sd(dfmwfg$Cu),sd(dfmwfg$Pb), sd(dfmwfg$Se), sd(dfmwfg$Zn))
stdevmwfg
dfmwfl<- filter(totmet, Site == "GC", Tissue == "l", Species == "MWF")
summary(dfmwfl)
stdevmwfl<- c(sd(dfmwfl$As), sd(dfmwfl$Cd), sd(dfmwfl$Cu),sd(dfmwfl$Pb), sd(dfmwfl$Se), sd(dfmwfl$Zn))
stdevmwfl
dfmwfm<- filter(totmet, Site == "GC", Tissue == "m", Species == "MWF")
summary(dfmwfm)
stdevmwfm<- c(sd(dfmwfm$As), sd(dfmwfm$Cd), sd(dfmwfm$Cu),sd(dfmwfm$Pb), sd(dfmwfm$Se), sd(dfmwfm$Zn))
stdevmwfm

#Bear Gulch
dfllg<- filter(totmet, Site == "BG", Tissue == "g", Species == "LL")
summary(dfllg)
stdev<- c(sd(dfllg$As), sd(dfllg$Cd), sd(dfllg$Cu),sd(dfllg$Pb), sd(dfllg$Se), sd(dfllg$Zn))
stdev
dflll<- filter(totmet, Site == "BG", Tissue == "l", Species == "LL")
summary(dflll)
stdevlll<- c(sd(dflll$As), sd(dflll$Cd), sd(dflll$Cu),sd(dflll$Pb), sd(dflll$Se), sd(dflll$Zn))
stdevlll
dfllm<- filter(totmet, Site == "BG", Tissue == "m", Species == "LL")
summary(dfllm)
stdevllm<- c(sd(dfllm$As), sd(dfllm$Cd), sd(dfllm$Cu),sd(dfllm$Pb), sd(dfllm$Se), sd(dfllm$Zn))
stdevllm

dflssug<- filter(totmet, Site == "BG", Tissue == "g", Species == "LSSU")
summary(dflssug)
stdevlssug<- c(sd(dflssug$As), sd(dflssug$Cd), sd(dflssug$Cu),sd(dflssug$Pb), sd(dflssug$Se), sd(dflssug$Zn))
stdevlssug
dflssum<- filter(totmet, Site == "BG", Tissue == "m", Species == "LSSU")
summary(dflssum)
stdevlssum<- c(sd(dflssum$As), sd(dflssum$Cd), sd(dflssum$Cu),sd(dflssum$Pb), sd(dflssum$Se), sd(dflssum$Zn))
stdevlssum

dflnsug<- filter(totmet, Site == "BG", Tissue == "g", Species == "LNSU")
summary(dflnsug)
stdevlnsug<- c(sd(dflnsug$As), sd(dflnsug$Cd), sd(dflnsug$Cu),sd(dflnsug$Pb), sd(dflnsug$Se), sd(dflnsug$Zn))
stdevlnsug
dflnsul<- filter(totmet, Site == "BG", Tissue == "L", Species == "LNSU")
summary(dflnsul)
stdevlnsul<- c(sd(dflnsul$As), sd(dflnsul$Cd), sd(dflnsul$Cu),sd(dflnsul$Pb), sd(dflnsul$Se), sd(dflnsul$Zn))
stdevlnsul
dflnsum<- filter(totmet, Site == "BG", Tissue == "m", Species == "LNSU")
summary(dflnsum)
stdevlnsum<- c(sd(dflnsum$As), sd(dflnsum$Cd), sd(dflnsum$Cu),sd(dflnsum$Pb), sd(dflnsum$Se), sd(dflnsum$Zn))
stdevlnsum

dfmwfg<- filter(totmet, Site == "BG", Tissue == "g", Species == "MWF")
summary(dfmwfg)
stdevmwfg<- c(sd(dfmwfg$As), sd(dfmwfg$Cd), sd(dfmwfg$Cu),sd(dfmwfg$Pb), sd(dfmwfg$Se), sd(dfmwfg$Zn))
stdevmwfg
dfmwfl<- filter(totmet, Site == "BG", Tissue == "l", Species == "MWF")
summary(dfmwfl)
stdevmwfl<- c(sd(dfmwfl$As), sd(dfmwfl$Cd), sd(dfmwfl$Cu),sd(dfmwfl$Pb), sd(dfmwfl$Se), sd(dfmwfl$Zn))
stdevmwfl
dfmwfm<- filter(totmet, Site == "BG", Tissue == "m", Species == "MWF")
summary(dfmwfm)
stdevmwfm<- c(sd(dfmwfm$As), sd(dfmwfm$Cd), sd(dfmwfm$Cu),sd(dfmwfm$Pb), sd(dfmwfm$Se), sd(dfmwfm$Zn))
stdevmwfm

dfrssh<- filter(totmet, Site == "BG", Tissue == "wb", Species == "RSSH")
summary(dfrssh)
stdevrssh<- c(sd(dfrssh$As), sd(dfrssh$Cd), sd(dfrssh$Cu),sd(dfrssh$Pb), sd(dfrssh$Se), sd(dfrssh$Zn))
stdevrssh
####end####

#####Two- Way ANOVAS####
table(totmet$As, totmet$Site)
ggplot(totmet, mapping=aes(Site, As, fill=Tissue))+geom_boxplot()+theme_classic()
Asaov2<-aov(As~Site * Species, data=totmet)
summary(Asaov2)
ggplot(totmet, mapping=aes(Site, Cd, fill=Tissue))+geom_boxplot()+theme_classic()
Cdaov2<-aov(Cd~Site * Species, data=totmet)
summary(Cdaov2)
Cdaov3<-aov(Cd~Site + Species, data=totmet)
summary(Cdaov3)
ggplot(totmet, mapping=aes(Site, Cu, fill=Tissue))+geom_boxplot()+theme_classic()
Cuaov2<-aov(Cu~Site * Species, data=totmet)
summary(Cuaov2)
Cuaov3<-aov(Cu~Site + Species, data=totmet)
summary(Cuaov3)
ggplot(totmet, mapping=aes(Site, Pb, fill=Tissue))+geom_boxplot()+theme_classic()
Pbaov2<-aov(Pb~Site * Species, data=totmet)
summary(Pbaov2)
ggplot(totmet, mapping=aes(Site, Se, fill=Tissue))+geom_boxplot()+theme_classic()
Seaov2<-aov(Se~Site * Species, data=totmet)
summary(Seaov2)
Seaov3<-aov(Se~Site + Species, data=totmet)
summary(Seaov3)
ggplot(totmet, mapping=aes(Site, Zn, fill=Tissue))+geom_boxplot()+theme_classic()
Znaov2<-aov(Zn~Site * Species, data=totmet)
summary(Znaov2)
Znaov3<-aov(Zn~Site + Species, data=totmet)
summary(Znaov3)
####end####