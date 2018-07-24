# Figure 6 from Johnson et al. 2018 Nature Ecology and Evolution

library(ggplot2)

oe <- readRDS("./Data/Observed vs Expected biomass.RDS")


p <-ggplot(oe[oe$census!=1,], aes(x=observed_biomass, y=biomass, group=Site))+
  geom_point(aes(colour=survival_mode, shape=Site, size=1.5))+
  geom_line(aes(colour=survival_mode, group=site.clust))+
  scale_shape_manual(values=1:nlevels(oe$Site)) +
  scale_color_manual(values=cols, guide_legend(title="Survival mode"))+
  geom_abline(slope=1, linetype=2)+
  xlab(bquote('Observed biomass (Mg '~ ha^-1~')'))+
  ylab(bquote('Expected biomass (Mg '~ ha^-1~')'))+
  theme_bw()+
  guides(size = "none")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

p
