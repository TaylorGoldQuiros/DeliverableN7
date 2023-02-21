library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(cowplot)
library(scales)
library(stringr)
library(car)
 
totmet<-read.csv(file.choose(), header = TRUE)
totmet$Site<-as.factor(totmet$Site)
totmet$Site <- ordered(totmet$Site, levels=c("DL","GC", "BG"))
Length <-as.numeric(totmet$Length)
totmet$Weight<-na.exclude(totmet$Weight)
Weight <- as.numeric(totmet$Weight)
class(totmet$Tissue)
str(totmet)
is.na(totmet$As)
As<-na.exclude(totmet$As)
Cu<-na.exclude(totmet$Cu)
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

#####Two- Way ANOVAS and figs####
table(totmet$As, totmet$Site)

succomb<-read.csv(file.choose(), header = TRUE)
succomb$Site<-as.factor(succomb$Site)
succomb$Site <- ordered(succomb$Site, levels=c("DL","GC", "BG"))
mwfll<-filter(succomb, Species!="Suckers")
str(mwfll)
ggplot(totmet, mapping=aes(Site, As, fill=Tissue))+geom_boxplot()+theme_classic()
Asaov2<-aov(As~Site * Species, data=totmet)
summary(Asaov2)
asglm<-glm(As~Site*Species,data=succomb)
summary(asglm)
asaovg<-aov(asglm)
summary(asaovg)
asglm2<-glm(As~Site*Species*Tissue, data=succomb)
as2aov<-aov(asglm2)
summary(as2aov)
asbm<-glm(As~Site*Species*Tissue, data = mwfll)
Anova(asbm)

ggplot(filter(totmet, Tissue!="wb"), mapping = aes(Species,As,fill=Tissue))+geom_boxplot()+theme_classic()+facet_wrap(~Site)
As_tissues <- ggplot(filter(succomb, Tissue!="wb", Species!="LNDC"), 
                      mapping = aes(Species,As,fill=Tissue)) + 
    geom_boxplot()+
    theme_classic()+
    scale_y_log10() +
    scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish","Suckers"="Sucker spp."))+
    theme(axis.text.x = element_text(angle = 45,hjust = 1) )+
   labs(y= expression(paste("As ("*mu~"g/g dry weight)")),) +
    facet_wrap(~Site)
ggplot(totmet, mapping=aes(Site, Cd, fill=Tissue))+geom_boxplot()+theme_classic()
Cdaov2<-aov(Cd~Site * Species, data=totmet)
summary(Cdaov2)
Cdaov3<-aov(Cd~Site + Species, data=totmet)
summary(Cdaov3)
Cdglm<-glm(Cd~Site*Species,data=succomb)
Cdaovg<-aov(Cdglm)
summary(Cdaovg)
cdglm2<-glm(Cd~Site*Species*Tissue, data=succomb)
cd2aov<-aov(cdglm2)
summary(cd2aov)
Anova(cdglm2)
Cd_tissues <- ggplot(filter(succomb, Tissue!="wb", Species!="LNDC"), 
                      mapping = aes(Species,Cd,fill=Tissue)) + 
    geom_boxplot()+
    theme_classic()+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish","Suckers"="Sucker spp."))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1) )+
  labs(y= expression(paste("Cd ("*mu~"g/g dry weight)")),) +
    facet_wrap(~Site)

ggplot(totmet, mapping=aes(Site, Cu, fill=Tissue))+geom_boxplot()+theme_classic()
Cuaov2<-aov(Cu~Site * Species, data=totmet)
summary(Cuaov2)
Cuaov3<-aov(Cu~Site + Species, data=totmet)
summary(Cuaov3)
Cuglm<-glm(Cu~Site*Species,data=succomb)
Cuaovg<-aov(Cuglm)
summary(Cuaovg)
cuglm2<-glm(Cu~Site*Species*Tissue, data=succomb)
Anova(cuglm2)
cu2aov<-aov(cuglm2)
summary(cu2aov)
cu_tissues <- ggplot(filter(succomb, Tissue!="wb", Species!="LNDC"), 
                      mapping = aes(Species,Cu,fill=Tissue)) + 
    geom_boxplot()+
    theme_classic()+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish","Suckers"="Sucker spp."))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1) )+
  labs(y= expression(paste("Cu ("*mu~"g/g dry weight)")),) +
    facet_wrap(~Site)
                
ggplot(totmet, mapping=aes(Site, Pb, fill=Tissue))+geom_boxplot()+theme_classic()
Pbaov2<-aov(Pb~Site * Species, data=totmet)
summary(Pbaov2)
Pbglm<-glm(Pb~Site*Species,data=succomb)
Pbaovg<-aov(Pbglm)
summary(Pbaovg)
pbglm2<-glm(Pb~Site*Species*Tissue, data=succomb)
pb2aov<-aov(pbglm2)
summary(pb2aov)
Anova(pbglm2)
Pb_tissues <- ggplot(filter(succomb, Tissue!="wb", Species!="LNDC"), 
                      mapping = aes(Species,Pb,fill=Tissue)) + 
    geom_boxplot()+
    theme_classic()+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish","Suckers"="Sucker spp."))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1) )+
  labs(y= expression(paste("Pb ("*mu~"g/g dry weight)")),) +
    facet_wrap(~Site)

ggplot(totmet, mapping=aes(Site, Se, fill=Tissue))+geom_boxplot()+theme_classic()
Seaov2<-aov(Se~Site * Species, data=totmet)
summary(Seaov2)
Seaov3<-aov(Se~Site + Species, data=totmet)
summary(Seaov3)
Seglm<-glm(Se~Site*Species,data=succomb)
Seaovg<-aov(Seglm)
summary(Seaovg)
seglm2<-glm(Se~Site*Species*Tissue, data=succomb)
Anova(seglm2)
se2aov<-aov(seglm2)
summary(se2aov)
Se_tissues <- ggplot(filter(succomb, Tissue!="wb", Species!="LNDC"), 
                      mapping = aes(Species, Se,fill=Tissue)) + 
    geom_boxplot()+
    theme_classic()+
    scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish","Suckers"="Sucker spp."))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1) )+
  labs(y= expression(paste("Se ("*mu~"g/g dry weight)")),) +
    facet_wrap(~Site)

ggplot(totmet, mapping=aes(Site, Zn, fill=Tissue))+geom_boxplot()+theme_classic()
Znaov2<-aov(Zn~Site * Species, data=totmet)
summary(Znaov2)
Znaov3<-aov(Zn~Site + Species, data=totmet)
summary(Znaov3)
Znglm<-glm(Zn~Site*Species,data=succomb)
Znaovg<-aov(Znglm)
summary(Znaovg)
znglm2<-glm(Zn~Site*Species*Tissue, data=succomb)
Anova(znglm2)
zn2aov<-aov(znglm2)
summary(zn2aov)
Zn_tissues <- ggplot(filter(succomb, Tissue!="wb", Species!="LNDC"), 
                      mapping = aes(Species,Zn,fill=Tissue)) + 
    geom_boxplot()+
    theme_classic()+
  scale_y_log10() +
    scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish","Suckers"="Sucker spp."))+
    theme(axis.text.x = element_text(angle = 45,hjust = 1) )+
    labs(y= expression(paste("Zn ("*mu~"g/g dry weight)")),) +
    facet_wrap(~Site)

#combine plots
plots<-plot_grid(As_tissues+theme(legend.position = "none"),
          Cd_tissues+theme(legend.position = "none"),
          cu_tissues+theme(legend.position = "none"),
          Pb_tissues+theme(legend.position = "none"),
          Se_tissues+theme(legend.position = "none"),
          Zn_tissues+theme(legend.position = "none"),
            labels = c('A','B','C','D','E','F', label_size=12))
legend<-get_legend(As_tissues+theme(legend.box.margin = margin(0,0,0,12)))
plot_grid(plots,legend, rel_widths = c(3,.4))

####end####
#MWF and LL metals plot
succomb<-read.csv(file.choose(), header = TRUE)
succomb$Site<-as.factor(succomb$Site)
succomb$Site <- ordered(succomb$Site, levels=c("DL","GC", "BG"))
mwfll<-filter(succomb, Species!="Suckers")
str(mwfll)
As_mwfll <- ggplot(filter(mwfll, Tissue!="wb", Species!="LNDC"), 
  mapping = aes(Species,As,fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("As ("*mu~"g/g dry weight)")),x ="") +
  facet_wrap(~Site)

Cd_mwfll <- ggplot(filter(mwfll, Tissue!="wb", Species!="LNDC"), 
   mapping = aes(Species,Cd,fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Cd ("*mu~"g/g dry weight)")),x ="") +
  facet_wrap(~Site)
Cu_mwfll <- ggplot(filter(mwfll, Tissue!="wb", Species!="LNDC"), 
       mapping = aes(Species,Cu,fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Cu ("*mu~"g/g dry weight)")),x ="") +
  facet_wrap(~Site)
Pb_mwfll <- ggplot(filter(mwfll, Tissue!="wb", Species!="LNDC"), 
   mapping = aes(Species,Pb,fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Pb ("*mu~"g/g dry weight)")),x ="") +
  facet_wrap(~Site)
Se_mwfll <- ggplot(filter(mwfll, Tissue!="wb", Species!="LNDC"), 
     mapping = aes(Species,Se,fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Se ("*mu~"g/g dry weight)")),) +
  facet_wrap(~Site)
Zn_mwfll <- ggplot(filter(mwfll, Tissue!="wb", Species!="LNDC"), 
  mapping = aes(Species,Zn,fill=Tissue))+ 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Zn ("*mu~"g/g dry weight)")), ) +
  facet_wrap(~Site)
newplot<-plot_grid(As_mwfll+theme(legend.position = "none"),
                 Cd_mwfll+theme(legend.position = "none"),
                 Cu_mwfll+theme(legend.position = "none"),
                 Pb_mwfll+theme(legend.position = "none"),
                 Se_mwfll+theme(legend.position = "none"),
                 Zn_mwfll+theme(legend.position = "none"),
                 nrow = 3,
                 ncol = 2,
                 hjust = -1.8,
                 labels = c('A','B','C','D','E','F', label_size=7))
legend2<-get_legend(As_mwfll+theme(legend.box.margin = margin(0,0,0,12)))
plot_grid(newplot,legend2,rel_widths = c(2,.4))
#Single species tissues graphs
#Brown trout
ll<-read.csv(file.choose(), header = TRUE)
ll$Site<-as.factor(ll$Site)
ll$Site <- ordered(ll$Site, levels=c("DL","GC", "BG"))
str(ll)
dlasll<-ggplot(filter(ll, Site == "DL"), 
          mapping = aes(Length,As, color = Tissue)) + 
            geom_point()+
            geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
            scale_y_continuous(expand = c(0,0))+
            labs(y= expression(paste("As ("*mu~"g/g dry weight)")),x = "Length (mm)" ) +
            theme_classic()
gcasll<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,As, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
bgasll<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,As, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
    geom_point()+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
asllplot<-plot_grid(dlasll+theme(legend.position = "none"),
                   gcasll+theme(legend.position = "none"),
                   bgasll+theme(legend.position = "none"),
                   ncol = 3,
                   align = "h" ,
                   labels = c('A','B','C', label_size=7))
legend3<-get_legend(dlasll+theme(legend.box.margin = margin(0,0,0,12)))
asllend<-plot_grid(asllplot,legend3,rel_widths = c(10,.4) )

dlcdll<-ggplot(filter(ll, Site == "DL"), 
               mapping = aes(Length,Cd, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= expression(paste("Cd ("*mu~"g/g dry weight)")),x = "Length (mm)" ) +
  theme_classic()
gccdll<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Cd, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
bgCdll<-ggplot(filter(ll, Site == "BG"), 
                mapping = aes(Length,Cd, color = Tissue)) + 
    geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
    geom_point()+
    scale_y_continuous(expand = c(0,0))+
    labs(y= "",x = "Length (mm)" ) +
    theme_classic()
cdllplot<-plot_grid(dlcdll+theme(legend.position = "none"),
                    gccdll+theme(legend.position = "none"),
                    bgCdll+theme(legend.position = "none"),
                    ncol = 3,
                    align = "h" )
cdllend<-plot_grid(cdllplot,rel_widths = c(10,.4) )

dlcull<-ggplot(filter(ll, Site == "DL"), 
               mapping = aes(Length,Cu, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= expression(paste("Cu ("*mu~"g/g dry weight)")),x = "Length (mm)" ) +
  theme_classic()
gccull<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Cu, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
bgcull<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,Cu, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
cullplot<-plot_grid(dlcull+theme(legend.position = "none"),
                    gccull+theme(legend.position = "none"),
                    bgcull+theme(legend.position = "none"),
                    ncol = 3,
                    align = "h" )
cullend<-plot_grid(cullplot,rel_widths = c(10,.4) )

dlpbll<-ggplot(filter(ll, Site == "DL"), 
               mapping = aes(Length,Pb, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= expression(paste("Pb ("*mu~"g/g dry weight)")),x = "Length (mm)" ) +
  theme_classic()
gcpbll<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Pb, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y=" ",x = "Length (mm)" ) +
  theme_classic()
bgpbll<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,Pb, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
pbllplot<-plot_grid(dlpbll+theme(legend.position = "none"),
                    gcpbll+theme(legend.position = "none"),
                    bgpbll+theme(legend.position = "none"),
                    ncol = 3,
                    align = "h" )
pbllend<-plot_grid(pbllplot,rel_widths = c(10,.4) )

dlsell<-ggplot(filter(ll, Site == "DL"), 
               mapping = aes(Length,Se, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= expression(paste("Se ("*mu~"g/g dry weight)")),x = "Length (mm)" ) +
  theme_classic()
gcsell<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Se, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
bgsell<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,Se, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
sellplot<-plot_grid(dlsell+theme(legend.position = "none"),
                    gcsell+theme(legend.position = "none"),
                    bgsell+theme(legend.position = "none"),
                    ncol = 3,
                    align = "h" )
sellend<-plot_grid(sellplot,rel_widths = c(10,.4) )

dlznll<-ggplot(filter(ll, Site == "DL"), 
               mapping = aes(Length,Zn, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= expression(paste("Zn ("*mu~"g/g dry weight)")),x = "Length (mm)" ) +
  theme_classic()
gcznll<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Zn, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
bgznll<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,Zn, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  theme_classic()
znllplot<-plot_grid(dlznll+theme(legend.position = "none"),
                    gcznll+theme(legend.position = "none"),
                    bgznll+theme(legend.position = "none"),
                    ncol = 3,
                    align = "h" )
znllend<-plot_grid(cullplot,rel_widths = c(10,.4) )
llfin<-plot_grid(asllend,
                 cdllend+theme(legend.position = "none"),
                 cullend+theme(legend.position = "none"),
                 pbllend+theme(legend.position = "none"),
                 sellend+theme(legend.position = "none"),
                 znllend+theme(legend.position = "none"),
                 ncol = 1,
                 align = "v")
llfin
#
#Kcondition constant comparisons####
kvalues<-read.csv(file.choose(), header = TRUE)
kvalues$Site<-as.factor(kvalues$Site)
kvalues$Site <- ordered(kvalues$Site, levels=c("DL","GC", "BG"))
summary(kvalues)
kaov<-aov(K~Site*Species,kvalues)
summary(kaov)

dlkll<- filter(kvalues, Site == "DL", Species == "MWF")
summary(dlkll)
sd(dlkll$K)
#end####