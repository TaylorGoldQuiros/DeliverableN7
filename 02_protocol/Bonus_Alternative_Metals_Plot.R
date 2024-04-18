library(PerformanceAnalytics)

chart.Correlation(mwfll %>% filter(Species == "LL") %>% select(12:17) %>% log10(), histogram=TRUE, pch=19)
chart.Correlation(mwfll %>% filter(Species == "MWF") %>% select(12:17) %>% log10(), histogram=TRUE, pch=19)

mwfll_long <- 
  mwfll %>% 
  pivot_longer(
    12:17, 
    names_to = "element", 
    values_to = "concentration") %>%
  mutate(element = as.factor(element))

mwfll_long <- as.data.frame(mwfll_long)

mwfll.glm <- glm(concentration ~ element:Tissue:Species:Site, 
                    family = Gamma(link = "log"), 
                    data = mwfll_long)
plot(mwfll.glm)
Anova(mwfll.glm)

# Three-way interaction

(mwfll_all.emm <- emmeans(mwfll.glm, ~ element | Tissue | Site | Species,
                             type = "response"))
(mwfll_tissue.cld <- cld(mwfll_all.emm, by = c("element", "Site", "Species"),
                            alpha = 0.05, Letters = letters))
(mwfll_species.cld <- cld(mwfll_all.emm, by = c("element", "Site", "Tissue"),
                             alpha = 0.05, Letters = letters))
(mwfll_site.cld <- cld(mwfll_all.emm, by = c("element", "Tissue", "Species"),
                          alpha = 0.05, Letters = letters))

ggplot() +
  geom_point(
    data = mwfll_long, 
    aes(x = Species, y = concentration, color = Tissue),
    group = "Tissue", 
    position = position_jitterdodge(
      jitter.width = 0.7, dodge.width = 0.8, seed = 1234)
    ) +
  geom_point(
    data = as.data.frame(mwfll_all.emm), 
    aes(x = Species, y = response, color = Tissue),
    group = "Tissue", 
    position = position_dodge(width = 0.8), 
    size = 3, 
    shape = 1) +
  geom_errorbar(
    data = as.data.frame(mwfll_all.emm),
    aes(x = Species,
        ymin = lower.CL, 
        ymax = upper.CL, 
        color = Tissue),
    group = "Tissue", 
    position = position_dodge(width = 0.8), 
    width = 0)+
  scale_y_log10() +
  facet_grid(element ~ Site, scales = "free_y")

