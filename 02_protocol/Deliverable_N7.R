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
theme_set(theme_bw())
## Loading in data####
### Metals: Data for summary table####
totmet <- read.csv("./01_input/Metals_Master_03272023.csv", header = TRUE) %>%
  mutate(Site = ordered(Site, levels = c("DL", "GC", "BG")), 
         Species = as.factor(Species),
         Tissue = as.factor(Tissue),
         Length = as.numeric(Length),
         Weight = as.numeric(Weight)) %>%
  droplevels()

# Combine two suckers into one "Suckers" factor level
levels(totmet$Species) <- c("LL", "LNDC", "LNSU", "LSSU", "LSSU", 
                            "MWF", "MWF", "RSSH")

colnames(totmet)<-c("Cap", "Site", "Year", "Species", "Tissue", "Num", 
                    "Sample.weight", "Length", "Weight", "As", "Cd", "Cu", "Pb", 
                    "Se", "Zn")

levels(totmet$Species) <- c("LL", "LNDC", "LNSU", "LSSU", "LSSU", 
                            "MWF", "MWF", "RSSH")

levels(totmet$Tissue) <- c("g", "l", "l", "m", "m", NA, "wb")
totmet <- filter(totmet, !is.na(Tissue))

totmet <- totmet %>%
  mutate(CharNum = ifelse(Num < 10, as.character(paste0(0, Num)),
                        as.character(Num))) %>%
  mutate(Sample = paste0(Site, Year - 2000, Species, CharNum))
# Code for old metals data, for comparison to newly censored data

totmet_old <- read.csv("./01_input/Metals Master_old.csv", header = TRUE)
totmet_old$Site <- as.factor(totmet_old$Site)
totmet_old$Site <- ordered(totmet_old$Site, levels = c("DL", "GC", "BG"))
totmet_old$Weight <- na.exclude(totmet_old$Weight)
colnames(totmet_old)<-c("Cap", "Site", "Year", "Species", "Tissue", "Num", 
                    "Length", "Weight", "Sample.weight", "As", "Cd", "Cu", "Pb",
                    "Se", "Zn")

  
# Length <- as.numeric(totmet$Length) Can likely delete
# Weight <- as.numeric(totmet$Weight) Can likely delete


### Metals: All species with suckers combined into "Suckers"####
succomb <- read.csv("./01_input/Suckerscombined.csv", header = TRUE)
succomb$Site <- as.factor(succomb$Site)
succomb$Site <- ordered(succomb$Site, levels=c("DL","GC", "BG"))

### Metals: Just MWF and LL with the proper vector attributes####
mwfll <- filter(totmet, Species %in% c("LL", "MWF")) %>%
  droplevels()
mwfll <- filter(totmet, Tissue %in% c("g", "m", "l"))

### Isotopes####

#onboard clean csv file
#change site to an ordered factor
sit1<-read.csv("./01_input/SIallforR_03272023.csv", header = TRUE) %>%
  filter(is.na(Year) == FALSE) %>%
  mutate(Site = ordered(Site, levels=c("DL","GC", "BG")), 
         Species = ordered(Species, levels = c("LL", "MWF", "LNSU", "LSSU",
                                               "LNDC", "RSSH"))) %>%
  select(1:9)

colnames(sit1) <- c("Sample", "Site", "Year", "Species", "Num", "Length", "Weight", "d13C", "d15N")

# levels(sit1$Species) <- c("LL", "MWF", "Suckers", "Suckers", "LNDC", "RSSH")
# 
# levels(totmet$Species) <- c("LL", "LNDC", "LNSU", "LSSU", "LSSU", 
#                             "MWF", "MWF", "RSSH")
# levels(sit1$Species) <- c("LL", "MWF", "Suckers", "Suckers", "LNDC", "RSSH")

str(sit1)

## Metals: Summary statistics for all taxa####

totmet_summary <-
  summarySE((pivot_longer(totmet, 10:15, names_to = "element",
                          values_to = "concentration")), 
            measurevar = "concentration",
            groupvars = c("Site", "Species", "Tissue", "element"), 
            .drop = FALSE)

succomb_summary <-
  summarySE((pivot_longer(succomb, 10:15, names_to = "element",
                          values_to = "concentration")), 
            measurevar = "concentration",
            groupvars = c("Site", "Species", "Tissue", "element"))

totmet_old_summary <-
  summarySE((pivot_longer(totmet_old, 10:15, names_to = "element",
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
Anova(as_mwfll.glm) 
# Only main effects for tissue and species. Could do two emmeans and two 
# clds (commented out) but better to stick with the full model

# (as_mwfll_species.emm <- emmeans(as_mwfll.glm, ~ Species, 
#                          type = "response"))
# 
# (as_mwfll_tissue.emm <- emmeans(as_mwfll.glm, ~ Tissue, 
#                                 type = "response"))
# 
# (as_mwfll_species.cld <- cld(as_mwfll_species.emm, 
#                              alpha = 0.05, Letters = letters))
# (as_mwfll_tissue.cld <- cld(as_mwfll_tissue.emm, 
#                             alpha = 0.05, Letters = letters))

(as_mwfll_all.emm <- emmeans(as_mwfll.glm, ~ Species | Site | Tissue,
                            type = "response"))

(as_mwfll_species.cld <- cld(as_mwfll_all.emm, by = c("Tissue", "Site"),
                             alpha = 0.05, Letters = letters))
(as_mwfll_tissue.cld <- cld(as_mwfll_all.emm, by = c("Site", "Species"),
                            alpha = 0.05, Letters = letters))
(as_mwfll_site.cld <- cld(as_mwfll_all.emm, by = c("Species", "Tissue"),
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
# Three way interaction with p < 0.05.

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
                    data = filter(mwfll, Cu < 8000),
                    maxit = 1000)
plot(cu_mwfll.glm)
Anova(cu_mwfll.glm)

# Three two-way interactions. If we don't compare the three-way interaction, 
# then we need to calculate three different  emmeans and clds. 
# I've done this (commented out) and it yields three emm and six clds. 
# 
# A better approach is to leave the non-significant three-way interactions and
# use the emmeans for the three way interaction. 
 
(cu_mwfll_all.emm <- emmeans(cu_mwfll.glm, ~ Site*Tissue*Species, 
                             type = "response"))

# # Three emmeans, one for each two-way interaction. Pain in the butt, and 
# (cu_mwfll_tissue_species.emm <- emmeans(cu_mwfll.glm, ~ Tissue*Species, 
#                                 type = "response"))
# (cu_mwfll_tissue_site.emm <- emmeans(cu_mwfll.glm, ~ Tissue*Site, 
#                                         type = "response"))
# (cu_mwfll_site_species.emm <- emmeans(cu_mwfll.glm, ~ Species*Site, 
#                                         type = "response"))
# 
# # Sea of clds for two-way interacitons, order in the cld name indicates the 
# # variable being examined (first) and the one being examined within. Yuck:
# (cu_mwfll_tissue_site.cld <- cld(cu_mwfll_tissue_site.emm, 
#                             by = c("Site"),
#                             alpha = 0.05, Letters = letters))
# (cu_mwfll_site_tissue.cld <- cld(cu_mwfll_tissue_site.emm, 
#                                  by = c("Tissue"),
#                                  alpha = 0.05, Letters = letters))
# 
# 
# (cu_mwfll_tissue_species.cld <- cld(cu_mwfll_tissue_species.emm, 
#                                     by = c("Species"),
#                                     alpha = 0.05, Letters = letters))
# (cu_mwfll_species_tissue.cld <- cld(cu_mwfll_tissue_species.emm, 
#                                     by = c("Tissue"),
#                                     alpha = 0.05, Letters = letters))
# 
# (cu_mwfll_site_species.cld <- cld(cu_mwfll_site_species.emm, 
#                                   by = c("Species"),
#                                   alpha = 0.05, Letters = letters))
# (cu_mwfll_species_site.cld <- cld(cu_mwfll_site_species.emm, 
#                                   by = c("Site"),
#                                   alpha = 0.05, Letters = letters))

# Method including three-way interaction and three clds for the one emmeans.
(cu_mwfll_species.cld <- cld(cu_mwfll_all.emm, 
                             by = c("Tissue", "Site"),
                             alpha = 0.05, 
                             Letters = letters))
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

# Weak evidence of a two-way interaction, strong evidence of two ways for 
# tisue:species and tissue:site

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

# Three-way interaction

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
print("==========================================================")
print("Summary of ANOVAs for Figure 2")
print("==========================================================")
print("----------------------------------------------------------")
print("Arsenic")
print("----------------------------------------------------------")
Anova(as_mwfll.glm)
print("----------------------------------------------------------")
print("Cadmium")
print("----------------------------------------------------------")
Anova(cd_mwfll.glm)
print("----------------------------------------------------------")
print("Copper")
print("----------------------------------------------------------")
Anova(cu_mwfll.glm)
print("----------------------------------------------------------")
print("Lead")
print("----------------------------------------------------------")
Anova(pb_mwfll.glm)
print("----------------------------------------------------------")
print("Selenium")
print("----------------------------------------------------------")
Anova(se_mwfll.glm)
print("----------------------------------------------------------")
print("Zinc")
print("----------------------------------------------------------")
Anova(zn_mwfll.glm)
sink()

# Calling and writing output for pairwise comparison p values:
sink("./03_incremental/p_values_pairwise_mwf_ll.txt")
print("==========================================================")
print("Summary of p-values for pairwise comparisons for Figure 2")
print("==========================================================")
print("----------------------------------------------------------")
print("Arsenic")
print("----------------------------------------------------------")
pairs(as_mwfll_all.emm, simple = "each")
print("----------------------------------------------------------")
print("Cadmium")
print("----------------------------------------------------------")
pairs(cd_mwfll_all.emm, simple = "each")
print("----------------------------------------------------------")
print("Copper")
print("----------------------------------------------------------")
pairs(cu_mwfll_all.emm, simple = "each")
print("----------------------------------------------------------")
print("Lead")
print("----------------------------------------------------------")
pairs(pb_mwfll_all.emm, simple = "each")
print("----------------------------------------------------------")
print("Selenium")
print("----------------------------------------------------------")
pairs(pb_mwfll_all.emm, simple = "each")
print("----------------------------------------------------------")
print("Zinc")
print("----------------------------------------------------------")
pairs(zn_mwfll.emm, simple = "each")
sink()

# Calling and writing output for cld:
sink("./03_incremental/cld_post_hoc_mwf_ll.txt")
print("==========================================================")
print("Compact Letter Display for pairwise comparisons for Figure 2")
print("==========================================================")
print("----------------------------------------------------------")
print("Arsenic")
print("----------------------------------------------------------")
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

# # Setting up cld objects for use as geom_text labels on the plots [on pause]
# as_mwfll_tissue.cld$.group <- gsub(" ", "", as_mwfll_tissue.cld$.group)
# as_mwfll_tissue.cld <- subset(as_mwfll_tissue.cld)
# as_mwfll_tissue.cld <- merge(data.frame(Site = c("DL", "DL", "DL",
#                                                  "GC", "GC", "GC", 
#                                                  "BG", "BG", "BG",
#                                                  "DL", "DL", "DL",
#                                                  "GC", "GC", "GC", 
#                                                  "BG", "BG", "BG"),
#                                         Species = c(rep("LL", 9), 
#                                                     rep("MWF", 9)),
#                                         Tissue = c(rep(c("m", "g", "l"), 6))), 
#                              as_mwfll_tissue.cld) %>% 
#                         arrange(Site, Species, Tissue)
# 
# cd_mwfll_tissue.cld$.group <- gsub(" ", "", cd_mwfll_tissue.cld$.group)
# cd_mwfll_tissue.cld <- subset(cd_mwfll_tissue.cld)
# cd_mwfll_tissue.cld <- 
#   merge(data.frame(Site = c("DL", "DL", "DL",
#                             "GC", "GC", "GC", 
#                             "BG", "BG", "BG",
#                             "DL", "DL", "DL",
#                             "GC", "GC", "GC", 
#                             "BG", "BG", "BG"),
#                    Species = c(rep("LL", 9), 
#                                rep("MWF", 9)),
#                    Tissue = c(rep(c("m", "g", "l"), 6))),
#         cd_mwfll_tissue.cld) %>% 
#   arrange(Site, Species, Tissue)
# 
# cu_mwfll_tissue.cld
# pb_mwfll_tissue.cld
# se_mwfll_tissue.cld
# zn_mwfll_tissue.cld


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
save_plot("./03_incremental/MWFLLplot.jpg", mwfllplot, 
          base_height = 8, base_width =12)


###Stats and figs on tissue conc by length for Brown Trout----
# For each element, made a glm, diagnostic plots, and Anova. Copper and Zn 
# showed some evidence of a three way Site*Tissue*Length interaction, however
# plotting them out separately is a lot and plotting just Tissue*Length is a 
# bit more interesting and useful. Cu, Cd, and Se all have interesitng patterns
# There are individual plots, but the money plot is a faceted one at the bottom

#Single species tissues graphs
#Brown trout

# ll<-read.csv("./01_input/LLmetals.csv", header = TRUE)

ll <- filter(mwfll, Species == "LL") %>% droplevels()
ll$Site<-as.factor(ll$Site)
ll$Site <- ordered(ll$Site, levels=c("DL","GC", "BG"))
ll <- mutate(ll, kvalue = 10^5*(Weight/(Length^3)))
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
                        family = Gamma(link = "log"), ll,
                        maxit = 1000)
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
cu_ll_tissue.glm <- glm(log10(Cu) ~ Tissue*Site*Length,
                        family = gaussian(link = "identity"), ll,
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

save_plot("./03_incremental/ll_tissue_length_new.png", ll_tissue_length_fig,
          base_height = 5, base_width = 4)


###Stats on tissue conc by length for Mountain Whitefish----
# For each element, made a glm, diagnostic plots, and Anova. Copper and Zn 
# showed some evidence of a three way Site*Tissue*Length interaction, however
# plotting them out separately is a lot and plotting just Tissue*Length is a 
# bit more interesting and useful. Cu, Cd, and Se all have interesitng patterns
# There are individual plots, but the money plot is a faceted one at the bottom

mwf <- filter(mwfll, Species == "MWF")
colnames(mwf) <- colnames(ll[ ,1:15])
mwf <- mwf %>% 
  mutate(Site = ordered(Site, levels=c("DL","GC", "BG")))
mwf <- mutate(mwf, kvalue = 10^5*(Weight/(Length^3)))
str(mwf)



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
print("==========================================================")
print("ANOVAS for Figures 3 and 4")
print("==========================================================")

print("----------------------------------------------------------")
print("Arsenic")
print("----------------------------------------------------------")
print("Trout")
print("----------------------------------------------------------")
Anova(as_ll_tissue.glm)
print("----------------------------------------------------------")
print("Whitefish")
print("----------------------------------------------------------")
Anova(as_mwf_tissue.glm)
print("")
print("----------------------------------------------------------")
print("Cadmium")
print("----------------------------------------------------------")
print("Trout")
print("----------------------------------------------------------")
Anova(cd_ll_tissue.glm)
print("----------------------------------------------------------")
print("Whitefish")
print("----------------------------------------------------------")
Anova(cd_mwf_tissue.glm)
print("")
print("----------------------------------------------------------")
print("Copper")
print("----------------------------------------------------------")
print("Trout")
print("----------------------------------------------------------")
Anova(cu_ll_tissue.glm)
print("----------------------------------------------------------")
print("Whitefish")
print("----------------------------------------------------------")
Anova(cu_mwf_tissue.glm)
print("")
print("----------------------------------------------------------")
print("Lead")
print("----------------------------------------------------------")
print("Trout")
print("----------------------------------------------------------")
Anova(pb_ll_tissue.glm)
print("----------------------------------------------------------")
print("Whitefish")
print("----------------------------------------------------------")
Anova(pb_mwf_tissue.glm)
print("")
print("----------------------------------------------------------")
print("Selenium")
print("----------------------------------------------------------")
print("Trout")
Anova(se_ll_tissue.glm)
print("----------------------------------------------------------")
print("Whitefish")
print("----------------------------------------------------------")
Anova(se_mwf_tissue.glm)
print("")
print("----------------------------------------------------------")
print("Zinc")
print("----------------------------------------------------------")
print("Trout")
print("----------------------------------------------------------")
Anova(zn_ll_tissue.glm)
print("----------------------------------------------------------")
print("Whitefish")
print("----------------------------------------------------------")
Anova(zn_mwf_tissue.glm)
sink()



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

(kvalues_fig <- 
  ggplot(kvalues, aes(x = Site, y = K, fill = Species)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_viridis_d())

save_plot("./03_incremental/kvalues.png", 
          kvalues_fig, base_height = 4, base_width = 6)
### Comparison of kvalues and metal concentrations ####
### Comparison in brown trout tissues

tissue_types <- c("Gill", "Liver", "Muscle")
names(tissue_types) <- c("g", "l", "m")

(ll_length_condition <- ggplot(pivot_longer(ll, 10:15, 
                                            names_to = "element", 
                                            values_to = "concentration")) +
    geom_point(aes(x = kvalue, y = concentration)) +
    facet_grid(Tissue~element, 
               labeller = labeller(Tissue = tissue_types)) + 
    theme_bw() +
    labs(y = expression(Concentreation~(mu*g~g^-1))) +
    scale_y_log10())

### Comparison in MWF tissues

(mwf_length_condition <- 
    ggplot(pivot_longer(mwf, 10:15, 
                        names_to = "element", 
                        values_to = "concentration")) +
    geom_point(aes(x = kvalue, y = concentration)) +
    facet_grid(Tissue~element, labeller = labeller(Tissue = tissue_types)) + 
    theme_bw() +
    labs(y = expression(Concentreation~(mu*g~g^-1))) +
    scale_y_log10())

fish_length_condition_fig <-
  plot_grid(ll_length_condition, mwf_length_condition, 
            labels = c("Trout", "MWF"), nrow = 2)

save_plot("./03_incremental/fish_length_condition_.png", 
          fish_length_condition_fig, base_width = 10, base_height = 8)
# Isotope data stats and figures####
par(mfrow = c(2,2)) # Makes it so diagnostic plots from base R are 2x2 grid
# Model data for 13C and 15N for each taxon between sites

fish_d13C_sum <- summarySE(sit1, groupvars = c("Site", "Species"),
                          measurevar = "d13C")

colnames(fish_d13C_sum) <- c("Site", "Species", "N", "d13C", "sd_13C", "se_13C", "ci_13C")

fish_d15N_sum <- summarySE(sit1, groupvars = c("Site", "Species"),
                           measurevar = "d15N") 

colnames(fish_d15N_sum) <- c("Site", "Species", "N", "d15N", "sd_15N", "se_15N", "ci_15N")

fish_summary <- merge(fish_d13C_sum, fish_d15N_sum)

fish_label <- c("Brown\n Trout", 
                "Mountain\n Whitefish",
                "Suckers",
                "Long Nose\n Dace", 
                "Redside\n Shiner")
names(fish_label) <- c("LL", "MWF", "Suckers", "LNDC", "RSSH")

site_label <- c("Deer Lodge", "Gold Creek", "Bear Gulch")
names(site_label) <- c("DL", "GC", "BG")

(fish_biplot <- 
  ggplot(fish_summary, 
         aes(x = d13C, 
             y = d15N, 
             color = Species,
             label = N)) +
    geom_point(aes(), size = 3) +
    # geom_text(hjust = 1.5, vjust = 1.5) + #Labels with reps
    geom_errorbarh(aes(xmin = d13C - se_13C,
                       xmax = d13C + se_13C,
                       color = Species)) +
    geom_errorbar(aes(ymin = d15N - se_15N,
                      ymax = d15N + se_15N,
                      color = Species)) +
    labs(x = expression(delta^13*C~("\u2030")),
         y = expression(delta^15*N~("\u2030"))) +
    scale_color_viridis_d(labels = gsub("\n", "", fish_label)) +
    facet_wrap(~Site, labeller = labeller(Site = site_label)) +
    scale_x_continuous(limits = c(-32, -26)) +
    scale_y_continuous(limits = c(8, 16)))

save_plot("./03_incremental/fish_biplot.png", fish_biplot,
          base_width = 7, base_height = 4)



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


(d15N_graph <- ggplot(all_d15N_site.cld)+
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
    facet_grid( ~ Species, labeller = labeller(Species = fish_label)) +
    theme_classic() +
    theme(panel.background = element_rect(fill = NA, color = "black"))
) 

save_plot("./03_incremental/d15N_site.png", d15N_graph,
          base_height = 4, base_width = 8)


(d13C_graph <- ggplot(all_d13C.cld)+
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
    facet_grid( ~ Species, 
                labeller = 
                  labeller(Species = fish_label)) +
    theme_classic() +
    theme(panel.background = element_rect(fill = NA, color = "black"))
) 

save_plot("./03_incremental/d13C_site.png", d13C_graph,
          base_height = 4, base_width = 8)

sink("./03_incremental/isotope_anovas_cld")
print("==========================================================")
print("ANOVAS and CLD for Figures 6 and 7")
print("==========================================================")
print("----------------------------------------------------------")
print("ANOVA d15N")
print("----------------------------------------------------------")
Anova(all_d15N.glm)
print("----------------------------------------------------------")
print("ANOVA d13C")
print("----------------------------------------------------------")
Anova(all_d13C.glm)
print("----------------------------------------------------------")
print("d15N Compact Letter Display")
print("----------------------------------------------------------")
all_d15N_species.cld
print("----------------------------------------------------------")
print("d15N p-values")
print("----------------------------------------------------------")
pairs(all_d15N.emm, simple = "each")
sink()

# Merging SI data, metals data, and calculating k values
totmet_wide <- totmet %>% 
  pivot_longer(cols = 10:15, names_to = "element", 
               values_to = "concentration") %>%
  pivot_wider(names_from = c("Tissue", "element"), 
              values_from = "concentration", 
              id_cols = c(2:6, 8:11),
              values_fn = {mean}) %>%
  select(8, 1:6, 9:32)


totmet_isotopes <- merge(totmet_wide, sit1, all = TRUE) %>%
  mutate(Species = as.factor(Species))

totmet_isotopes <-  totmet_isotopes %>%
  mutate(K = ifelse(Species %in% 
                      c("LNSU", "LSSU", "RSSH", "LNDC"), 
                    NA, 
                    10^5 * Weight/(Length^3)),
         .before = "m_As")


  
write.csv(totmet_isotopes, "./03_incremental/metals_isotopes_K.csv")

totmet_summary

# comparing old and new data

totmet <- arrange(totmet, 1)
totmet_old <- arrange(totmet_old, 1)

totmet_long <- pivot_longer(totmet, 10:15, names_to = "element", 
                            values_to = "totmet_conc")

totmet_old_long <- pivot_longer(totmet_old, 10:15, names_to = "element", 
                            values_to = "totmet_old_conc")
totmet_new_long <- pivot_longer(totmet, 10:15, names_to = "element", 
                                values_to = "totmet_new_conc")

totmet_merged_long <- merge(totmet_old_long, totmet_long, all = TRUE)
totmet_merged_longer <- pivot_longer(totmet_merged_long, cols = c(11, 12),
                                     names_to = "dataset", 
                                     values_to = "concentration")

totmet_merged_summary <- summarySE(totmet_merged_longer, 
                                   groupvars = c("Site", "Species", "Tissue", 
                                                 "dataset", "element"), 
                                   measurevar = "concentration")

totmet_merged_sum_wide <- pivot_wider(totmet_merged_summary, 
                                      id_cols = c("Site", "Species", "Tissue", 
                                                  "element"),
                                      names_from = "dataset",
                                      values_from = "concentration")

(comparison <- ggplot(totmet_merged_long) +
  geom_point(aes(x = totmet_conc, y = totmet_old_conc)) +
  facet_wrap(~ element, scales = "free") +
  labs(x = "Concentration new Master Metals", y = "Concentration old Master Metals"))

(comparison_histo <- ggplot(totmet_merged_longer) +
    geom_histogram(aes(x = concentration)) +
    facet_grid(dataset ~ element, scales = "free") +
    labs(x = "Concentration new Master Metals", y = "Concentration old Master Metals") +
    scale_x_log10())

(comparison_summary <- ggplot(totmet_merged_sum_wide) +
    geom_point(aes(x = totmet_conc, y = totmet_old_conc, color = element)) +
    scale_y_log10() +
    scale_x_log10() +
    labs(x = "Concentration Current", y = "Concentration Draft v3.1") +
    scale_color_viridis_d(name = "Elements"))

save_plot("./03_incremental/comparison.png", comparison, base_height = 7, base_width = 10)
save_plot("./03_incremental/comparison_summary.png", comparison_summary, 
          base_height = 7, base_width = 10)

