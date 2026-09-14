library(ggplot2)

jpeg("pam_abun.jpg", width=6.5, height=4, units='in', res=300)

pam_salmonids$Year<-as.factor(pam_salmonids$Year)


pam_abun_plot<-ggplot(pam_salmonids, aes(Year, abundance_100m, fill=Station)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  scale_fill_manual(values=c("#cccccc", "#666666")) + 
  ylab("Abundance Estimate (number/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.11, .88)) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=1.5, angle=90) +
  annotate("text", x = 3.5, y = 175, label = "'3 years'") +
  geom_vline(xintercept = 4.5, linetype="dashed", size=1.5, angle=90) +
  annotate("text", x = 4.5, y = 175, label = "'20 years'") +
  geom_errorbar(aes(ymax=stand.species.abundance.contr.ucl, ymin=stand.species.abundance.contr.lcl), size=1, width=0.25, position=position_dodge(.9)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


pam_abun_plot

dev.off()
par(mfrow=c(1,1))

#####Plot of biomass for all salmonids each year above and below diversion


library(ggplot2)

jpeg("pam_bio.jpg", width=6.5, height=4, units='in', res=300)


pam_bio_plot<-ggplot(pam_salmonids, aes(Year, biomass_100m, fill=Station)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  scale_fill_manual(values=c("#cccccc", "#666666")) + 
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.11, .88)) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=1.5, angle=90) +
  annotate("text", x = 3.5, y = 1150, label = "'3 years'") +
  geom_vline(xintercept = 4.5, linetype="dashed", size=1.5, angle=90) +
  annotate("text", x = 4.5, y = 1150, label = "'19 years'") +
  geom_errorbar(aes(ymax=stand.species.biomass.contr.ucl, ymin=stand.species.biomass.contr.lcl), size=1, width=0.25, position=position_dodge(.9)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


pam_bio_plot

dev.off()
par(mfrow=c(1,1))








#####Plot of abundance estimate for each species each year above and below diversion

library(ggplot2)

jpeg("Pam_sppabun_plot.jpg", width=6.5, height=4, units='in', res=600)

Pam_allspecies<-Pamehac_allspecies_bytype

Pam_allspecies$Year<-as.factor(Pam_allspecies$Year)

Pam_sppabun_plot<-ggplot(Pam_allspecies, aes(Year, abundance_100m)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Population Estimate (Number/100 sq. meters)") + xlab("Year") +
  facet_grid(Station~Species) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
  geom_errorbar(aes(ymax=stand.species.abundance.contr.ucl, ymin=stand.species.abundance.contr.lcl), size=1, width=0.25) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


Pam_sppabun_plot
dev.off()
par(mfrow=c(1,1))

#####test

library(ggplot2)

jpeg("Pam_sppabun_plot2.jpg", width=7, height=4, units='in', res=600)

Pam_allspecies<-Pamehac_allspecies_bytype

Pam_allspecies$Year<-as.factor(Pam_allspecies$Year)

Pam_sppabun_plot2<-ggplot(Pam_allspecies, aes(Year, abundance_100m, fill=Station)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Population Estimate (Number/100 sq. meters)") + xlab("Year") +
  scale_fill_manual(values=c("#cccccc", "#666666")) + 
  facet_grid(~Species) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
  theme(legend.title=element_blank()) +
  geom_errorbar(aes(ymax=stand.species.abundance.contr.ucl, ymin=stand.species.abundance.contr.lcl), size=1, width=0.25, position=position_dodge(.9)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

Pam_sppabun_plot2
dev.off()
par(mfrow=c(1,1))



#####Plot of biomass estimate for each species each year above and below diversion


library(ggplot2)

jpeg("Pam_sppbio_plot.jpg", width=6.5, height=4, units='in', res=600)

Pam_allspecies<-Pamehac_allspecies_bytype

Pam_allspecies$Year<-as.factor(Pam_allspecies$Year)

Pam_sppbio_plot<-ggplot(Pam_allspecies, aes(Year, biomass_100m)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  facet_grid(Station~Species) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
  theme(legend.title=element_blank()) +
  geom_errorbar(aes(ymax=stand.species.biomass.contr.ucl, ymin=stand.species.biomass.contr.lcl), size=1, width=0.25) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


Pam_sppbio_plot
dev.off()
par(mfrow=c(1,1))






















library(ggplot2)

jpeg("RB_bothsppabun_plot.jpg", width=6.5, height=4, units='in', res=600)

RB_spp_maincomp$Year<-as.factor(RB_spp_maincomp$Year)

RB_bothsppabun_plot<-ggplot(RB_spp_maincomp, aes(Year, abundance_100m)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Population Estimate (Number/100 sq. meters)") + xlab("Year") +
  facet_grid(Station~Species) +
  geom_errorbar(aes(ymax=abundance_ucl, ymin=abundance_lcl), size=1, width=0.25) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


RB_bothsppabun_plot
dev.off()
par(mfrow=c(1,1))


#####Plot of biomass estimate for each species each year in both Main Stem and Compensation Habitats

library(ggplot2)

jpeg("RB_bothsppbio_plot.jpg", width=6.5, height=4, units='in', res=600)

RB_spp_maincomp$Year<-as.factor(RB_spp_maincomp$Year)

RB_bothsppbio_plot<-ggplot(RB_spp_maincomp, aes(Year, biomass_100m)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  facet_grid(Station~Species) +
  geom_errorbar(aes(ymax=biomass_ucl, ymin=biomass_lcl), size=1, width=0.25) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


RB_bothsppbio_plot
dev.off()
par(mfrow=c(1,1))

####Plot of biomass estimate for all salmonids combined each year in both Main Stem and Compensation Habitats

library(ggplot2)

jpeg("RB_salmonidsbio_plot.jpg", width=6.5, height=4, units='in', res=600)

RB_salmonids_maincomp$Year<-as.factor(RB_salmonids_maincomp$Year)

RB_salmonidsbio_plot<-ggplot(RB_salmonids_maincomp, aes(Year, biomass_100m, fill=Station)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
  theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.85, .85)) +
  geom_errorbar(aes(ymax=biomass_ucl, ymin=biomass_lcl), size=1, width=0.30, position=position_dodge(.9)) +
  scale_fill_manual(values=c("dark grey", "light grey")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


RB_salmonidsbio_plot

dev.off()
par(mfrow=c(1,1))

####Plot of biomass estimate for all salmonids combined each year in both Main Stem and Compensation Habitats

library(ggplot2)

jpeg("RB_salmonidsabun_plot.jpg", width=6.5, height=4, units='in', res=600)

RB_salmonids_maincomp$Year<-as.factor(RB_salmonids_maincomp$Year)

RB_salmonidsabun_plot<-ggplot(RB_salmonids_maincomp, aes(Year, abundance_100m, fill=Station)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
  theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
  ylab("Population Estimate (number/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.88, .88)) +
  geom_errorbar(aes(ymax=abundance_ucl, ymin=abundance_lcl), size=1, width=0.30, position=position_dodge(.9)) +
  scale_fill_manual(values=c("dark grey", "light grey")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


RB_salmonidsabun_plot

dev.off()
par(mfrow=c(1,1))


#####Plot of Total biomass for compensation stream compared to biomass destroyed

library(ggplot2)

jpeg("RB_nonetloss_plot.jpg", width=7, height=5, units='in', res=600)

RB_nonetloss$Year<-as.factor(RB_nonetloss$Year)

RB_nonetloss_plot<-ggplot(RB_nonetloss, aes(Year, total_biomass, fill=Station)) + 
  theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
  theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
  ylab("Total Estimated Biomass (grams)") + xlab("Year") +
  theme(legend.title=element_blank()) +
 
  geom_errorbar(aes(ymax=tbiomass_ucl, ymin=tbiomass_lcl), size=1, width=0.30, position=position_dodge(.9)) +
  geom_hline(yintercept = 23940, colour="red", linetype="dashed") +
  annotate("text", x = 2.6, y = 25000, label = "'No Net Loss'", colour="red") +
  scale_fill_manual(values=c("dark grey", "red")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


RB_nonetloss_plot

dev.off()
par(mfrow=c(1,1))



