#Deliverable#7 Code####
## Preliminaries #####
library(Rmisc) # For summarySE function, load before tidyverse
library(multcomp) # For cld, load before tidyverse
library(tidyverse)
library(ggpubr)
library(cowplot)
library(scales)
library(stringr)
library(car)
library(viridisLite)
library(emmeans)

options(scipen = 8) # Get rid of scientific notation
par(mfrow = c(2,2)) # Makes it so diagnostic plots from base R are 2x2 grid

## To do:
## Remove all of the "theme_classic" calls and add a set_theme call up here

## Loading in data####
### Metals: Data for summary table####
totmet <- read.csv("./01_input/Metals Master.csv", header = TRUE)
totmet$Site <- as.factor(totmet$Site)
totmet$Site <- ordered(totmet$Site, levels = c("DL", "GC", "BG"))
totmet$Weight <- na.exclude(totmet$Weight)
# Length <- as.numeric(totmet$Length) Can likely delete
# Weight <- as.numeric(totmet$Weight) Can likely delete

### Metals: All species with suckers combined into "Suckers"####
succomb <- read.csv("./01_input/Suckerscombined.csv", header = TRUE)
succomb$Site <- as.factor(succomb$Site)
succomb$Site <- ordered(succomb$Site, levels=c("DL","GC", "BG"))

### Metals: Just MWF and LL with the proper vector attributes####
mwfll <- filter(succomb, Species %in% c("LL", "MWF")) %>%
  droplevels() %>%
  mutate(Species = as.factor(Species), 
         Tissue = as.factor(Tissue),
         Length = as.numeric(Length),
         Weight = as.numeric(Weight))

### Isotopes

## Metals: Summary statistics for all taxa####

totmet_summary <-
  summarySE((pivot_longer(totmet, 10:15, names_to = "element",
                          values_to = "concentration")), 
            measurevar = "concentration",
            groupvars = c("Site", "Species", "Tissue", "element"))

## Statistical analyses####


# Stats using glm, Anova, emmeans, and cld
# As
as_mwfll.glm <- glm(As ~ Tissue*Species*Site, 
                    family = Gamma(link = "log"), 
                    data = mwfll)
plot(as_mwfll.glm)
Anova(as_mwfll.glm) # Only main effects. Three emmeans and three clds.

(as_mwfll_species.emm <- emmeans(as_mwfll.glm, ~ Species, 
                         type = "response"))
(as_mwfll_site.emm <- emmeans(as_mwfll.glm, ~ Site, 
                                type = "response"))
(as_mwfll_tissue.emm <- emmeans(as_mwfll.glm, ~ Tissue, 
                                type = "response"))

(as_mwfll_species.cld <- cld(as_mwfll_species.emm, 
                             alpha = 0.05, Letters = letters))
(as_mwfll_tissue.cld <- cld(as_mwfll_tissue.emm, 
                            alpha = 0.05, Letters = letters))
(as_mwfll_site.cld <- cld(as_mwfll_site.emm, 
                          alpha = 0.05, Letters = letters))  


# Site was significant in the model, but not at 0.05 when considering just this
# individual main effect. Same pattern emerges if model is reduced, and the 
# p value on the anova for site also changes to > 0.05

# Cd
cd_mwfll.glm <- glm(Cd ~ Tissue*Species*Site, 
                    family = Gamma(link = "log"), 
                    data = mwfll)
plot(cd_mwfll.glm)
Anova(cd_mwfll.glm)
# Three way interaction
(cd_mwfll_all.emm <- emmeans(cd_mwfll.glm, ~ Tissue | Species | Site, 
                                type = "response"))

(cd_mwfll_tissue.cld <- cld(cd_mwfll_all.emm, by = c("Species", "Site"),
                             alpha = 0.05, Letters = letters))
(cd_mwfll_site.cld <- cld(cd_mwfll_all.emm, by = c("Species", "Tissue"),
                            alpha = 0.05, Letters = letters))
(cd_mwfll_species.cld <- cld(cd_mwfll_all.emm, by = c("Site", "Tissue"),
                            alpha = 0.05, Letters = letters))

# Cu
cu_mwfll.glm <- glm(Cu ~ Tissue*Species*Site, 
                    family = Gamma(link = "log"), 
                    data = mwfll)
plot(cu_mwfll.glm)
Anova(cu_mwfll.glm)

# Three two-way interactions. Lame. Below, we have the emmeans and cld two 
# different wasy. In one, it is three emm and six clds. Yuck. In the other
# we can leave in the non-significant three-way interaction and use the
# emmeans for the three way interaction. This simpler second approach is what I 
# recommend we use.
 
(cu_mwfll_all.emm <- emmeans(cu_mwfll.glm, ~ Site*Tissue*Species, 
                                        type = "response"))

# Three emmeans, one for each two-way interaction
(cu_mwfll_tissue_species.emm <- emmeans(cu_mwfll.glm, ~ Tissue*Species, 
                                type = "response"))
(cu_mwfll_tissue_site.emm <- emmeans(cu_mwfll.glm, ~ Tissue*Site, 
                                        type = "response"))
(cu_mwfll_site_species.emm <- emmeans(cu_mwfll.glm, ~ Species*Site, 
                                        type = "response"))

# Sea of clds for two-way interacitons, order in the cld name indicates the 
# variable being examined (first) and the one being examined within. Yuck:
(cu_mwfll_tissue_site.cld <- cld(cu_mwfll_tissue_site.emm, 
                            by = c("Site"),
                            alpha = 0.05, Letters = letters))
(cu_mwfll_site_tissue.cld <- cld(cu_mwfll_tissue_site.emm, 
                                 by = c("Tissue"),
                                 alpha = 0.05, Letters = letters))


(cu_mwfll_tissue_species.cld <- cld(cu_mwfll_tissue_species.emm, 
                                    by = c("Species"),
                                    alpha = 0.05, Letters = letters))
(cu_mwfll_species_tissue.cld <- cld(cu_mwfll_tissue_species.emm, 
                                    by = c("Tissue"),
                                    alpha = 0.05, Letters = letters))

(cu_mwfll_site_species.cld <- cld(cu_mwfll_site_species.emm, 
                                  by = c("Species"),
                                  alpha = 0.05, Letters = letters))
(cu_mwfll_species_site.cld <- cld(cu_mwfll_site_species.emm, 
                                  by = c("Site"),
                                  alpha = 0.05, Letters = letters))

# Method including three-way interaction and three clds for the one emmeans.
(cu_mwfll_species.cld <- cld(cu_mwfll_all.emm, 
                                  by = c("Tissue", "Site"),
                                  alpha = 0.05, Letters = letters))
(cu_mwfll_tissue.cld <- cld(cu_mwfll_all.emm, 
                                         by = c("Species", "Site"),
                                         alpha = 0.05, Letters = letters))
(cu_mwfll_site.cld <- cld(cu_mwfll_all.emm, 
                                         by = c("Tissue", "Species"),
                                         alpha = 0.05, Letters = letters))


# Pb
pb_mwfll.glm <- glm(Pb ~ Tissue*Species*Site, 
                    family = Gamma(link = "log"), 
                    data = mwfll)
plot(pb_mwfll.glm)
Anova(pb_mwfll.glm)

# Three way interaction

(pb_mwfll_all.emm <- emmeans(pb_mwfll.glm, ~ Tissue | Site | Species, 
                             type = "response"))

(pb_mwfll_tissue.cld <- cld(pb_mwfll_all.emm, by = c("Site", "Species"),
                            alpha = 0.05, Letters = letters))
(pb_mwfll_species.cld <- cld(pb_mwfll_all.emm, 
                             by = c("Site", "Tissue"),
                             alpha = 0.05, Letters = letters))
(pb_mwfll_site.cld <- cld(pb_mwfll_all.emm, 
                             by = c("Species", "Tissue"),
                             alpha = 0.05, Letters = letters))


# Se
se_mwfll.glm <- glm(Se ~ Tissue*Species*Site, 
                    family = Gamma(link = "log"), 
                    data = mwfll)
plot(se_mwfll.glm)
Anova(se_mwfll.glm)

# Two two-way interactions, but simpler if we go ahead and just leave it all in
# there.

(se_mwfll_all.emm <- emmeans(se_mwfll.glm, ~ Tissue | Site | Species,
                         type = "response"))
(se_mwfll_tissue.cld <- cld(se_mwfll_all.emm, by = c("Site", "Species"),
                            alpha = 0.05, Letters = letters))
(se_mwfll_species.cld <- cld(se_mwfll_all.emm, by = c("Site", "Tissue"),
                            alpha = 0.05, Letters = letters))
(se_mwfll_site.cld <- cld(se_mwfll_all.emm, by = c("Tissue", "Species"),
                            alpha = 0.05, Letters = letters))

# Zn

zn_mwfll.glm <- glm(Zn ~ Tissue*Species*Site, 
                    family = Gamma(link = "log"), 
                    data = mwfll)
plot(zn_mwfll.glm)
Anova(zn_mwfll.glm)

# Three way interaction

(zn_mwfll.emm <- emmeans(zn_mwfll.glm, ~ Site | Tissue | Species,
                         type = "response"))
(zn_mwfll_tissue.cld <- cld(zn_mwfll.emm, by = c("Site", "Species"),
                            alpha = 0.05, Letters = letters))
(zn_mwfll_species.cld <- cld(zn_mwfll.emm, by = c("Tissue", "Site"),
                            alpha = 0.05, Letters = letters))
(zn_mwfll_site.cld <- cld(zn_mwfll.emm, by = c("Tissue", "Species"),
                             alpha = 0.05, Letters = letters))
# Write output to a file, comment back in if you want to rewrite:
sink("./03_incremental/Anova_SitexSpeciesxTissue_mwf_ll.txt")
Anova(as_mwfll.glm)
Anova(cd_mwfll.glm)
Anova(cu_mwfll.glm)
Anova(pb_mwfll.glm)
Anova(se_mwfll.glm)
Anova(zn_mwfll.glm)
sink()

# Calling and writing output for pairwise comparison p values:
sink("./03_incremental/p_values_pairwise_mwf_ll.txt")
print("Arsenic")
pairs(as_mwfll_tissue.emm)
pairs(as_mwfll_site.emm)
pairs(as_mwfll_species.emm)
print("----------------------------------------------------------")
print("Cadmium")
print("----------------------------------------------------------")
pairs(cd_mwfll_all.emm, simple = "each")
print("----------------------------------------------------------")
print("Copper")
print("----------------------------------------------------------")
contrast(cu_mwfll_all.emm, simple = "each")
print("----------------------------------------------------------")
print("Lead")
print("----------------------------------------------------------")
contrast(pb_mwfll_all.emm, simple = "each")
print("----------------------------------------------------------")
print("Selenium")
pairs(pb_mwfll_all.emm, simple = "each")
print("----------------------------------------------------------")
print("Zinc")
print("----------------------------------------------------------")
pairs(zn_mwfll.emm, simple = "each")
sink()

# Calling and writing output for cld:
sink("./03_incremental/cld_post_hoc_mwf_ll.txt")
print("Arsenic")
as_mwfll_species.cld
as_mwfll_site.cld
as_mwfll_tissue.cld
print("----------------------------------------------------------")
print("Cadmium")
print("----------------------------------------------------------")
cd_mwfll_species.cld
cd_mwfll_site.cld
cd_mwfll_tissue.cld
print("----------------------------------------------------------")
print("Copper")
print("----------------------------------------------------------")
cu_mwfll_species.cld
cu_mwfll_site.cld
cu_mwfll_tissue.cld
print("----------------------------------------------------------")
print("Lead")
pb_mwfll_species.cld
pb_mwfll_site.cld
pb_mwfll_tissue.cld
print("----------------------------------------------------------")
print("Selenium")
print("----------------------------------------------------------")
se_mwfll_species.cld
se_mwfll_site.cld
se_mwfll_tissue.cld
print("----------------------------------------------------------")
print("Zinc")
print("----------------------------------------------------------")
zn_mwfll_species.cld
zn_mwfll_site.cld
zn_mwfll_tissue.cld
sink()

# Setting up cld objects for use as geom_text labels on the plots
as_mwfll_tissue.cld$.group <- gsub(" ", "", as_mwfll_tissue.cld$.group)
as_mwfll_tissue.cld <- subset(as_mwfll_tissue.cld)
as_mwfll_tissue.cld <- merge(data.frame(Site = c("DL", "DL", "DL",
                                                 "GC", "GC", "GC", 
                                                 "BG", "BG", "BG",
                                                 "DL", "DL", "DL",
                                                 "GC", "GC", "GC", 
                                                 "BG", "BG", "BG"),
                                        Species = c(rep("LL", 9), 
                                                    rep("MWF", 9)),
                                        Tissue = c(rep(c("m", "g", "l"), 6))), 
                             as_mwfll_tissue.cld) %>% 
                        arrange(Site, Species, Tissue)

cd_mwfll_tissue.cld$.group <- gsub(" ", "", cd_mwfll_tissue.cld$.group)
cd_mwfll_tissue.cld <- subset(cd_mwfll_tissue.cld)
cd_mwfll_tissue.cld <- 
  merge(data.frame(Site = c("DL", "DL", "DL",
                            "GC", "GC", "GC", 
                            "BG", "BG", "BG",
                            "DL", "DL", "DL",
                            "GC", "GC", "GC", 
                            "BG", "BG", "BG"),
                   Species = c(rep("LL", 9), 
                               rep("MWF", 9)),
                   Tissue = c(rep(c("m", "g", "l"), 6))),
        cd_mwfll_tissue.cld) %>% 
  arrange(Site, Species, Tissue)

cu_mwfll_tissue.cld
pb_mwfll_tissue.cld
se_mwfll_tissue.cld
zn_mwfll_tissue.cld


(As_mwfll <- ggplot(mwfll, 
                   mapping = aes(Species,As, fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(panel.grid = element_blank())+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression("As (kg/g dry weight)"),x ="") +
  scale_fill_viridis_d( labels=c("Gill","Liver","Muscle"))+
  facet_wrap(~Site))

(Cd_mwfll <- ggplot(mwfll, 
                   mapping = aes(Species,Cd,fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Cd (kg/g dry weight)")),x ="") +
  scale_fill_viridis_d()+
  facet_wrap(~Site))

(Cu_mwfll <- ggplot(mwfll, 
                   mapping = aes(Species,Cu,fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Cu ("*mu~"g/g dry weight)")),x ="") +
  scale_fill_viridis_d()+
  facet_wrap(~Site))

(Pb_mwfll <- ggplot(mwfll, 
                   mapping = aes(Species,Pb,fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Pb ("*mu~"g/g dry weight)")),x ="") +
  scale_fill_viridis_d()+
  facet_wrap(~Site))

(Se_mwfll <- ggplot(mwfll, 
                   mapping = aes(Species,Se,fill=Tissue)) + 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Se ("*mu~"g/g dry weight)")),) +
  scale_fill_viridis_d()+
  facet_wrap(~Site))

(Zn_mwfll <- ggplot(mwfll, 
                   mapping = aes(Species,Zn,fill=Tissue))+ 
  geom_boxplot()+
  theme_classic()+
  theme(text=element_text(size = 8))+
  scale_y_log10() +
  scale_x_discrete(labels=c("LL"="Brown trout", "MWF"="Mountain whitefish"))+
  theme(axis.text.x = element_text(angle = 25,hjust = 1) )+
  labs(y= expression(paste("Zn ("*mu~"g/g dry weight)")), ) +
  scale_fill_viridis_d()+
  facet_wrap(~Site))

newplot<-plot_grid(As_mwfll+theme(legend.position = "none"),
                   Cd_mwfll+theme(legend.position = "none"),
                   Cu_mwfll+theme(legend.position = "none"),
                   Pb_mwfll+theme(legend.position = "none"),
                   Se_mwfll+theme(legend.position = "none"),
                   Zn_mwfll+theme(legend.position = "none"),
                   nrow = 3,
                   ncol = 2,
                   hjust = -1.8,
                   labels = c('A','B','C','D','E','F', label_size=7),
                   align = "hv")
legend2<-get_legend(As_mwfll+theme(legend.box.margin = margin(0,0,0,12)))
mwfllplot<-plot_grid(newplot,legend2,rel_widths = c(2,.4))
mwfllplot
save_plot("./03_incremental/MWFLLplot.jpg", mwfllplot
          , base_height = 8, base_width =12)


###Stats and figs on tissue conc by length for Brown Trout----
# For each element, made a glm, diagnostic plots, and Anova. Copper and Zn 
# showed some evidence of a three way Site*Tissue*Length interaction, however
# plotting them out separately is a lot and plotting just Tissue*Length is a 
# bit more interesting and useful. Cu, Cd, and Se all have interesitng patterns
# There are individual plots, but the money plot is a faceted one at the bottom

#Single species tissues graphs
#Brown trout
ll<-read.csv("./01_input/LLmetals.csv", header = TRUE)
ll$Site<-as.factor(ll$Site)
ll$Site <- ordered(ll$Site, levels=c("DL","GC", "BG"))
str(ll)

# As:
as_ll_tissue.glm <- glm(As ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), ll)
plot(as_ll_tissue.glm)
Anova(as_ll_tissue.glm)

(as_ll_fig <- ggplot(data = ll, aes(Length, As, color = Tissue)) + 
  geom_point()+
  theme_classic()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y = expression(As~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))) 

# Cd:
cd_ll_tissue.glm <- glm(Cd ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), ll)
plot(cd_ll_tissue.glm)
Anova(cd_ll_tissue.glm)

(cd_ll_fig <- ggplot(data = ll, aes(Length, Cd, color = Tissue)) + 
  geom_point()+
  theme_classic()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y = expression(Cd~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle")))


# Cu:
cu_ll_tissue.glm <- glm(Cu ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), ll,
                        maxit = 1000) # Wasn't converging, so added iterations
plot(cu_ll_tissue.glm)
Anova(cu_ll_tissue.glm)

(cu_ll_fig <- ggplot(data = ll, aes(Length, Cu, color = Tissue)) + 
  geom_point()+
  theme_classic()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y = expression(Cu~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle")) )

# Pb:
pb_ll_tissue.glm <- glm(Pb ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), ll)
plot(pb_ll_tissue.glm)
Anova(pb_ll_tissue.glm)

(pb_ll_fig <- ggplot(data = ll, aes(Length, Pb, color = Tissue)) + 
  geom_point()+
  theme_classic()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y = expression(Pb~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))) 

# Se:
# 
se_ll_tissue.glm <- glm(Se ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), ll)
plot(se_ll_tissue.glm)
Anova(se_ll_tissue.glm)

(se_ll_fig <- ggplot(data = ll, aes(Length, Se, color = Tissue)) + 
  geom_point()+
  theme_classic()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y = expression(Se~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))) 

#Zn:

zn_ll_tissue.glm <- glm(Zn ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), ll)
plot(zn_ll_tissue.glm)
Anova(zn_ll_tissue.glm)

(zn_ll_fig <- ggplot(data = ll, aes(Length, Zn, color = Tissue)) + 
  geom_point()+
  theme_classic()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y = expression(Zn~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle")))

# All together
(ll_tissue_length_fig <- 
    ggplot(data = pivot_longer(ll, 
                               10:15, 
                               names_to = "element",
                               values_to = "concentration"), 
                   aes(Length, concentration, color = Tissue)) + 
    geom_point()+
    theme_classic()+
    geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(clip = "off")+
    labs(y = expression(Concentration~(mu*g~g^-1~dry~weight)),
         x = "Length (mm)" )  +
    scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))+
  facet_grid(rows = "element", scales = "free"))

save_plot("./03_incremental/ll_tissue_length.png", ll_tissue_length_fig,
          base_height = 5, base_width = 4)


###Stats on tissue conc by length for Mountain Whitefish----
# For each element, made a glm, diagnostic plots, and Anova. Copper and Zn 
# showed some evidence of a three way Site*Tissue*Length interaction, however
# plotting them out separately is a lot and plotting just Tissue*Length is a 
# bit more interesting and useful. Cu, Cd, and Se all have interesitng patterns
# There are individual plots, but the money plot is a faceted one at the bottom

mwf <- read.csv("./01_input/MWFmetals.csv", header = FALSE)
colnames(mwf) <- colnames(ll)
mwf <- mwf %>% 
  mutate(Site = ordered(Site, levels=c("DL","GC", "BG")))

# As:
as_mwf_tissue.glm <- glm(As ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), mwf)
plot(as_mwf_tissue.glm)
Anova(as_mwf_tissue.glm)

(as_mwf_fig <- ggplot(data = mwf, aes(Length, As, color = Tissue)) + 
    geom_point()+
    theme_classic()+
    geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(clip = "off")+
    labs(y = expression(As~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
    scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))) 

# Cd:
cd_mwf_tissue.glm <- glm(Cd ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), mwf)
plot(cd_mwf_tissue.glm)
Anova(cd_mwf_tissue.glm)

(cd_mwf_fig <- ggplot(data = mwf, aes(Length, Cd, color = Tissue)) + 
    geom_point()+
    theme_classic()+
    geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(clip = "off")+
    labs(y = expression(Cd~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
    scale_color_viridis_d(labels=c("Gill","Liver","Muscle")))


# Cu:
cu_mwf_tissue.glm <- glm(Cu ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), mwf) 
plot(cu_mwf_tissue.glm)
Anova(cu_mwf_tissue.glm)

(cu_mwf_fig <- ggplot(data = mwf, aes(Length, Cu, color = Tissue)) + 
    geom_point()+
    theme_classic()+
    geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(clip = "off")+
    labs(y = expression(Cu~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
    scale_color_viridis_d(labels=c("Gill","Liver","Muscle")) )

# Pb:
pb_mwf_tissue.glm <- glm(Pb ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), mwf,
                        maxit = 1000)
plot(pb_mwf_tissue.glm)
Anova(pb_mwf_tissue.glm)

(pb_mwf_fig <- ggplot(data = mwf, aes(Length, Pb, color = Tissue)) + 
    geom_point()+
    theme_classic()+
    geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(clip = "off")+
    labs(y = expression(Pb~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
    scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))) 

# Se:
# 
se_mwf_tissue.glm <- glm(Se ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), mwf)
plot(se_mwf_tissue.glm)
Anova(se_mwf_tissue.glm)

(se_mwf_fig <- ggplot(data = mwf, aes(Length, Se, color = Tissue)) + 
    geom_point()+
    theme_classic()+
    geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(clip = "off")+
    labs(y = expression(Se~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
    scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))) 

#Zn:

zn_mwf_tissue.glm <- glm(Zn ~ Tissue*Site*Length,
                        family = Gamma(link = "log"), mwf)
plot(zn_mwf_tissue.glm)
Anova(zn_mwf_tissue.glm)

(zn_mwf_fig <- ggplot(data = mwf, aes(Length, Zn, color = Tissue)) + 
    geom_point()+
    theme_classic()+
    geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(clip = "off")+
    labs(y = expression(Zn~(mu*g~g^-1~dry~weight)),x = "Length (mm)" )  +
    scale_color_viridis_d(labels=c("Gill","Liver","Muscle")))

# All together
(mwf_tissue_length_fig <- 
    ggplot(data = pivot_longer(mwf, 
                               10:15, 
                               names_to = "element",
                               values_to = "concentration"), 
           aes(Length, concentration, color = Tissue)) + 
    geom_point()+
    theme_classic()+
    geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
    scale_y_continuous(expand = c(0,0))+
    coord_cartesian(clip = "off")+
    labs(y = expression(Concentration~(mu*g~g^-1~dry~weight)),
         x = "Length (mm)" )  +
    scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))+
    facet_grid(rows = "element", scales = "free"))

save_plot("./03_incremental/mwf_tissue_length.png", mwf_tissue_length_fig,
          base_height = 5, base_width = 4)

# Saving outputs of all of the length by element by tissue plots:

sink("./03_incremental/length_tissue_Anovas.txt")
print("Trout")
Anova(as_ll_tissue.glm)
print("Whitefish")
Anova(as_mwf_tissue.glm)
print("Trout")
Anova(cd_ll_tissue.glm)
print("Whitefish")
Anova(cd_mwf_tissue.glm)
print("Trout")
Anova(cu_ll_tissue.glm)
print("Whitefish")
Anova(cu_mwf_tissue.glm)
print("Trout")
Anova(pb_ll_tissue.glm)
print("Whitefish")
Anova(pb_mwf_tissue.glm)
print("Trout")
Anova(se_ll_tissue.glm)
print("Whitefish")
Anova(se_mwf_tissue.glm)
print("Trout")
Anova(zn_ll_tissue.glm)
print("Whitefish")
Anova(zn_mwf_tissue.glm)
sink()


dlasll<-ggplot(filter(ll, Site == "DL"), 
               mapping = aes(Length,As, color = Tissue)) + 
  geom_point()+
  theme_classic()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE )+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y= "As (kg/g dry weight)",x = "Length (mm)" )  +
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))

gcasll<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,As, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  coord_cartesian(clip = "off")+
  scale_color_viridis_d()+
  theme_classic()
bgasll<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,As, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y= "",x = "Length (mm)" ) +
  scale_color_viridis_d()+
  theme_classic()
asllplot<-plot_grid(dlasll+theme(legend.position = "none"),
                    gcasll+theme(legend.position = "none"),
                    bgasll+theme(legend.position = "none"),
                    ncol = 3,
                    align = "h",
                    labels = c('Deerlodge','Gold Creek','Bear Gulch'),
                    label_size=12,
                    hjust = -1.5)
legend3<-get_legend(dlasll+theme(legend.position = "right"))
asllend<-plot_grid(asllplot,legend3, rel_widths = c(3,.4))
asllend
dlcdll<-ggplot(filter(ll, Site == "DL"), 
               mapping = aes(Length,Cd, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= expression(paste("Cd ("*mu~"g/g dry weight)")),x = "Length (mm)" ) +
  theme_classic()+
  coord_cartesian(clip = "off")+
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))

gccdll<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Cd, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  scale_colour_viridis_d()+
  coord_cartesian(clip = "off")+
  theme_classic()
bgCdll<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,Cd, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  scale_colour_viridis_d()+
  coord_cartesian(clip = "off")+
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
  theme_classic()+
  coord_cartesian(clip = "off")+
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))
gccull<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Cu, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y= "",x = "Length (mm)" ) +
  scale_colour_viridis_d()+
  theme_classic()
bgcull<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,Cu, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  scale_colour_viridis_d()+
  coord_cartesian(clip = "off")+
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
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))+
  coord_cartesian(clip = "off")+
  theme_classic()
gcpbll<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Pb, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y=" ",x = "Length (mm)" ) +
  coord_cartesian(clip = "off")+
  scale_colour_viridis_d()+
  theme_classic()
bgpbll<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,Pb, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  scale_colour_viridis_d()+
  coord_cartesian(clip = "off")+
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
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))+
  coord_cartesian(clip = "off")+
  theme_classic()
gcsell<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Se, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  scale_colour_viridis_d()+
  coord_cartesian(clip = "off")+
  theme_classic()
bgsell<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,Se, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  scale_colour_viridis_d()+
  coord_cartesian(clip = "off")+
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
  labs(y= expression(paste("Zn ("*mu~"kg/g dry weight)")),x = "Length (mm)" ) +
  scale_color_viridis_d(labels=c("Gill","Liver","Muscle"))+
  coord_cartesian(clip = "off")+
  theme_classic()
gcznll<-ggplot(filter(ll, Site == "GC"), 
               mapping = aes(Length,Zn, color = Tissue)) + 
  geom_point()+
  geom_smooth(aes(group=Tissue),method = lm, se = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  scale_colour_viridis_d()+
  coord_cartesian(clip = "off")+
  theme_classic()
bgznll<-ggplot(filter(ll, Site == "BG"), 
               mapping = aes(Length,Zn, color = Tissue)) + 
  geom_smooth(aes(group=Tissue),method = lm, se=FALSE)+
  geom_point()+
  scale_y_continuous(expand = c(0,0))+
  labs(y= "",x = "Length (mm)" ) +
  scale_colour_viridis_d()+
  coord_cartesian(clip = "off")+
  theme_classic()
znllplot<-plot_grid(dlznll+theme(legend.position = "none"),
                    gcznll+theme(legend.position = "none"),
                    bgznll+theme(legend.position = "none"),
                    ncol = 3,
                    align = "h" )
znllend<-plot_grid(znllplot,rel_widths = c(10,.4) )
llfin<-plot_grid(asllend,
                 cdllend+theme(legend.position = "none"),
                 cullend+theme(legend.position = "none"),
                 pbllend+theme(legend.position = "none"),
                 sellend+theme(legend.position = "none"),
                 znllend+theme(legend.position = "none"),
                 ncol = 1,
                 align = "v")
llfin
save_plot("./03_incremental//LLmetalslength.jpg", 
          llfin, base_height = 12, base_width = 8)
#
#Kcondition constant comparisons####
kvalues<-read.csv("./01_input/kcalc.csv", header = TRUE)
colnames(kvalues) <- c("Site", "Species", "K", "Length", "X", "X1", "X2")
kvalues <- kvalues %>%
  mutate(Site = ordered(Site, levels=c("DL","GC", "BG")),
         Species = as.factor(Species)) %>%
  select(1:4)

kvalues_glm <- glm(K ~ Site*Species, 
                   family = Gamma(link = "log"),
                   kvalues)
plot(kvalues_glm)
Anova(kvalues_glm)

# Little evidence of differences

kvalues_fig <- 
  ggplot(kvalues, aes(x = Site, y = K, fill = Species)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_viridis_d()

save_plot("./03_incremental/kvalues.png", 
          kvalues_fig, base_height = 4, base_width = 6)

summary(kvalues)
kaov<-aov(K~Site*Species,kvalues)
summary(kaov)

dlkll<- filter(kvalues, Site == "DL", Species == "MWF")
summary(dlkll)
sd(dlkll$K)
#end####
#where isthis going in git?#

####Isotope data stats and figures:
