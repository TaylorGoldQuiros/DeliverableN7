
  #Stable isotopes deliverable 7
  
library(car)
library(multcomp)
library(tidyverse)
library(emmeans)
library(cowplot)

#onboard clean csv file
#change site to an ordered factor
sit1<-read.csv("./01_input/SIallforR.csv", header = TRUE) %>%
  filter(is.na(Year) == FALSE) %>%
  mutate(Site = ordered(Site, levels=c("DL","GC", "BG")), 
         Species = ordered(Species, levels = c("LL", "MWF", "LNSU", "LSSU",
                                               "LNDC", "RSSH"))) %>%
  select(1:9)

colnames(sit1) <- c("Sample", "Site", "Year", "Species", "Num", "Length", "Weight", "d13C", "d15N")
str(sit1)

#basic plots
plot(sit1$d13C,sit1$d15N, col=factor(sit1$Site), pch=19) 

pone<-ggplot(data = sit1, aes(d13C,d15N))+geom_point()
pone + facet_wrap(vars(sit1$Site))

#anova between sites for d13C
out1=aov(d13C~Site, sit1)
summary(out1)
model.tables(out1,type="means")
TukeyHSD(out1)
#anova between sites for d15N
out2=aov(d15N~Site, sit1)
summary(out2)
model.tables(out2,type="means")
TukeyHSD(out2)
#2-way Anova
out3=aov(d13C~d15N+Site, sit1)
summary (out3)

#brown trout one-way anova
sill<-read.csv(file.choose(), header = TRUE) %>%
  mutate(Site=ordered(sill$Site, levels=c("DL","GC", "BG")))
str(sill)
class(sill$?..Site)
sill$?..Site<-as.factor(sill$?..Site)
llout1<-aov(d13C~sill$?..Site,data = sill)
summary(llout1)

plot(fitted(llout1),resid(llout1))
#add in length as covariate
llout2<-aov(d13C~sill$?..Site*sill$Length,data = sill)
summary(llout2)
plot(fitted(llout2),resid(llout2))
#same thing by 15N
llout3<-aov(d15N~sill$?..Site,data = sill)
summary(llout3)
TukeyHSD(llout3)
plot(fitted(llout3),resid(llout3))
#add length
llout4<-aov(d15N~sill$?..Site*sill$Length,data = sill)
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
lssuout1<-aov(d13C~silssu$?..Site,data = silssu)
summary(lssuout1)
plot(fitted(lssuout1),resid(lssuout1))
#add in length as covariate
lssuout2<-aov(d13C~silssu$?..Site*silssu$Length,data = silssu)
summary(lssuout2)
plot(fitted(lssuout2),resid(lssuout2))
lssuout3<-aov(d15N~silssu$?..Site,data = silssu)
summary(lssuout3)
TukeyHSD(lssuout3)
plot(fitted(lssuout3),resid(lssuout3))
lssuout4<-aov(d15N~silssu$?..Site*silssu$Length,data = silssu)
summary(lssuout4)
plot(fitted(lssuout4),resid(lssuout4))
ggplot(data = silssu, aes(Site,d15N))+geom_boxplot()+theme_classic()



#mountain whitefish one-way anova
simwf<-read.csv(file.choose(), header = TRUE) %>%
  mutate(Site=ordered(Site, levels=c("DL","GC", "BG")))
str(simwf)
class(simwf$?..Site)
simwf$?..Site<-as.factor(simwf$?..Site)
#anova series begins...
mwfout1<-aov(d13C~simwf$?..Site,data = simwf)
summary(mwfout1)
TukeyHSD(mwfout1)
plot(fitted(mwfout1),resid(mwfout1))
#add in length as covariate
mwfout2<-aov(d13C~simwf$?..Site*simwf$Length,data = simwf)
summary(mwfout2)
plot(fitted(mwfout2),resid(mwfout2))
mwfout3<-aov(d15N~simwf$?..Site,data = simwf)
summary(mwfout3)
plot(fitted(mwfout3),resid(mwfout3))
mwfout4<-aov(d15N~simwf$?..Site*simwf$Length,data = simwf)
summary(mwfout4)
TukeyHSD(mwfout4)
plot(fitted(mwfout4),resid(mwfout4))
#not going to include histogram because its only moderate and weak evidence for a difference
ggplot(data = simwf, aes(Site,d13C))+geom_boxplot()+theme_classic()

#compare 15N among sites by length#
plot(sit1$Length, sit1$d15N, pch=as.integer(sit1$Site))
legend("bottomright", c("DL","GC", "BG"), cex = .5, pch = 1:3)  
ggplot(data = sit1, aes(Length, d15N, color=Site))+geom_point()+geom_smooth(method = "lm")+theme_classic()

Site <- as.character(sit1$Site)
out1<-glm(sit1$d15N~Site+sit1$Length)                                            
summary(out1)
###
###
###
###
par(mfrow = c(2,2)) # Makes it so diagnostic plots from base R are 2x2 grid
# Model data for 13C and 15N for each taxon between sites
all_d15N.glm <- glm(d15N ~ Site*Species,
               family = gaussian(link = "identity"),
               sit1)
plot(all_d15N.glm)
Anova(all_d15N.glm)
all_d15N.emm <- emmeans(all_d15N.glm, ~Site*Species, type = "response")
(all_d15N_site.cld <- cld(all_d15N.emm, 
                    alpha = 0.05, 
                    by = c("Species"), 
                    Letters = letters))

(all_d15N_species.cld <- cld(all_d15N.emm, 
                          alpha = 0.05, 
                          by = c("Site"), 
                          Letters = letters))

all_d15N_site.cld$.group <- gsub(" ", "", all_d15N_site.cld$.group)
all_d15N_site.cld <- subset(all_d15N_site.cld)


all_d13C.glm <- glm(d13C ~ Site*Species,
                    family = gaussian(link = "identity"),
                    sit1)
plot(all_d13C.glm)
Anova(all_d13C.glm)
all_d13C.glm1 <- glm(d13C ~ Site+Species,
                     family = gaussian(link = "identity"),
                     sit1)
plot(all_d13C.glm1)
Anova(all_d13C.glm1)

all_d13C.emm <- emmeans(all_d13C.glm, ~Site*Species, type = "response")
all_d13C.cld <- cld(all_d13C.emm, 
                    by = "Species", 
                    alpha = 0.05, 
                    Letters = letters)


(d15N_graph <- ggplot()+
    geom_point(data = all_d15N_site.cld, 
               aes(x = Site, y = emmean), 
               size = 4, 
               shape = 1) +
    geom_errorbar(data = all_d15N_site.cld, 
                  aes(x = Site, ymin = lower.CL, ymax = upper.CL), 
                  width = 0) +
    geom_text(data = all_d15N_site.cld, 
              aes(x = Site, y = emmean, 
                  label = .group), vjust = -3, hjust = -0.5) +
    labs(y = expression(delta^15*N~("\u2030"))) +
    facet_grid( ~ all_d15N_site.cld$Species) +
    theme_classic() +
    theme(panel.background = element_rect(fill = NA, color = "black"))
) 

save_plot("./03_incremental/d15N_site.png", d15N_graph,
          base_height = 4, base_width = 8)

(d13C_graph <- ggplot()+
    geom_point(data = all_d13C.cld, 
               aes(x = Site, y = emmean), 
               size = 4, 
               shape = 1) +
    geom_errorbar(data = all_d13C.cld, 
                  aes(x = Site, ymin = lower.CL, ymax = upper.CL), 
                  width = 0) +
    # geom_text(data = all_d15N_site.cld, 
    #           aes(x = Site, y = emmean, 
    #               label = .group), vjust = -3, hjust = -0.5) +
    labs(y = expression(delta^13*C~("\u2030"))) +
    facet_grid( ~ all_d13C.cld$Species) +
    theme_classic() +
    theme(panel.background = element_rect(fill = NA, color = "black"))
) 

save_plot("./03_incremental/d13C_site.png", d13C_graph,
          base_height = 4, base_width = 8)
