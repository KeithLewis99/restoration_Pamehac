# Created 2024-07-09
## This is just a compilation of Kristin Loughlin's code.  If the code is commented out, it is because I cannot find a corresponding figure.  If I could find a corresponding figure, I sometimes had to make changes, either because of data issues or changes in ggplot2.  Aside from minor changes in ggplot2, I have tried to document all changes.  

source("Pam_data.R")
library(ggplot2)

# all salmonids_meanwithse ----
jpeg("pam_salmonids_bio2.jpg", width=6.5, height=4, units='in', res=300)

# convert year to a factor and then do the line breaks between them 
Pam_salmonids_bio_plot <- 
  ggplot(Pam_Mean_Biomass, aes(as.factor(Year), mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  theme_bw() +  
  theme(axis.text.x  = element_text(vjust=0.2, size=12)) +
  ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.88, .88)) +
#  scale_x_break(c(1996.5, 2015.5), ticklabels = c(1990, 1996, 2016), space = 0) +
  geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="solid", linewidth=0.5) +
  theme(legend.text=element_text(size=6)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_errorbar(aes(ymax=mean-se, ymin=mean+se), linewidth=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

Pam_salmonids_bio_plot
dev.off()


#####All Salmonids Density
jpeg("pam_salmonids_den.jpg", width=6.5, height=4, units='in', res=300)

Pam_salmonids_den_plot<-ggplot(Pam_Mean_Density, aes(as.factor(Year), mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  theme_bw() +  
  theme(axis.text.x  = element_text(vjust=0.2, size=12)) +
  ylab("Density Estimate  (#/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.11, .88)) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_errorbar(aes(ymax=mean-se, ymin=mean+se), linewidth=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

Pam_salmonids_den_plot
dev.off()



# allspecies_means_and_plots_cs ----

#####Used file Output_pamehac_by_station.   This is a file of output from AMEC.efishing8cs 
##script with Carl Strube estimates - see Pam_data.R


#####PLOTS
## there are no plots by these names but they are biomass or density facetted by species

# jpeg("Pam_biose_plot.jpg", width=6.5, height=4, units='in', res=600)
# Pam_allspecies_meanbio$Year<-as.factor(Pam_allspecies_meanbio$Year)
# 
# Pam_biose_plot<-ggplot(Pam_allspecies_meanbio, aes(Year, mean, colour=type)) +
#   geom_point(size=3, position=position_dodge(1)) +
#   theme_bw() +
#   facet_grid(~Species) +
#   theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
#   ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.88, .88)) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
#   geom_errorbar(aes(ymax=mean+se, ymin=mean-se), size=1, width=0.25, position=position_dodge(1)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# # 
#  Pam_biose_plot
# dev.off()
# par(mfrow=c(1,1))

#####DENSITY PLOT

# jpeg("Pam_biose_plot.jpg", width=6.5, height=4, units='in', res=600)
# Pam_allspecies_meanden$Year<-as.factor(Pam_allspecies_meanden$Year)
# 
# Pam_dense_plot<-ggplot(Pam_allspecies_meanden, aes(Year, mean, colour=type)) + 
#   geom_point(size=3, position=position_dodge(1)) +
#   theme_bw() +  
#   facet_grid(~Species) +
#   theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
#   ylab("Density Estimate  (grams/100 sq. meters)") + xlab("Year") +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.88, .88)) +
#   theme(legend.text=element_text(size=6)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
#   geom_errorbar(aes(ymax=mean-se, ymin=mean+se), size=1, width=0.25, position=position_dodge(1)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# Pam_dense_plot



# SALMON ONLY ----
## density ----

# graph not saved
jpeg("Pam_density_plot.jpg", width=6.5, height=4, units='in', res=600)

Pam_salmondense_plot <- 
  ggplot(pam_density_allAS, aes(as.factor(Year), mean, colour=type)) + 
  geom_point(size=2, position=position_dodge(1)) +
  theme_bw() +  
  facet_grid(~Species) +
  theme(axis.text.x  = element_text(angle=45, vjust=0.2, size=12)) +
  ylab("Density Estimate  (#/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.63, .88)) +
  theme(legend.text=element_text(size=8)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  geom_errorbar(aes(ymax=mean-se, ymin=mean+se), linewidth=0.5, width=0.25, position=position_dodge(1)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
Pam_salmondense_plot
dev.off()

## biomass ----
### no biomass for some reason?????


# PLOTS BY INDIVIDUAL SPECIES ----
## AS ----

# not saved
# pam_ASbiomass_se <- subset(Pam_allspecies_meanbio, Species=="AS")
# 
# jpeg("AS_dense_plot.jpg", width=6.5, height=4, units='in', res=600)
# AS_dense_plot <- 
#   ggplot(pam_ASbiomass_se, aes(as.factor(Year), mean, colour=type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Density Estimate  (#/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.90, .88)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   geom_errorbar(aes(ymax=mean-se, ymin=mean+se), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# AS_dense_plot
# dev.off()


## ASYOY ----
# this doesn't make sense - there is no "pam_biomass_se but ignoring because she doesn't have plots for these anyway.
# pam_ASYOYbiomass_se <- subset(pam_biomass_se, Species=="ASYOY")
# 
# jpeg("ASYOY_dense_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# ASYOY_dense_plot<-ggplot(pam_ASYOYbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   scale_fill_manual(values=c("black", "grey")) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.11, .80)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# ASYOY_dense_plot
# dev.off()

#####BT

# pam_BTbiomass_se<-subset(pam_biomass_se, Species=="BT")
# 
# jpeg("BT_dense_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# BT_dense_plot<-ggplot(pam_BTbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   scale_fill_manual(values=c("black", "grey")) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.90, .88)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   
#   geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# BT_dense_plot
# dev.off()           

#####BTYOY

# pam_BTYOYbiomass_se<-subset(pam_biomass_se, Species=="BTYOY")
# 
# jpeg("BTYOY_dense_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# BTYOY_dense_plot<-ggplot(pam_BTYOYbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   scale_fill_manual(values=c("black", "grey")) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.90, .88)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   
#   geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# BTYOY_dense_plot
# dev.off()


# pamehac_ggplot_barplots ----
## abundance ----
### So something happened here - Kristin made a file called "pamehac_table_salmonid_bio.csv" and "salmonid_e.fishing.summary.data.csv" which matches the data in these figues.  However, the pam_salmonids df, which is what was in the original code I received is not summarized by year and therefore, can't be the right data set. 
### The "salmonid_e.fishing.summary.data.csv" data set has the same point estimates as the "pamehac_table_salmonid_bio.csv" but it also has the confidence intervals.  However, its not clear to me how Kristin brought in the errors.  Pretty sure that the delta method but it may have been using the AMEC code but just summarized by all salmonids....so that may be OK.  

jpeg("pam_abun.jpg", width=6.5, height=4, units='in', res=300)

#pam_salmonids$Year<-as.factor(pam_salmonids$Year)
#str(pam_salmonids)
str(pam_summary)


pam_abun_plot <-
#  ggplot(pam_salmonids, aes(Year, abundance_100m, fill=Station)) + 
  ggplot(pam_summary, aes(as.factor(Year), stand.species.abundance.contr
, fill=Station)) + 
  theme_bw() + 
  geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  scale_fill_manual(values=c("#cccccc", "#666666")) + 
  ylab("Abundance Estimate (number/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.11, .88)) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=1.5) +
  annotate("text", x = 3.5, y = 175, label = "'3 years'") +
  geom_vline(xintercept = 4.5, linetype="dashed", size=1.5) +
  annotate("text", x = 4.5, y = 175, label = "'20 years'") +
  geom_errorbar(aes(ymax=stand.species.abundance.contr.ucl, ymin=stand.species.abundance.contr.lcl), size=1, width=0.25, position=position_dodge(.9)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


pam_abun_plot

dev.off()
par(mfrow=c(1,1))

#####Plot of biomass for all salmonids each year above and below diversion
## biomass ----
### see notes above
jpeg("pam_bio.jpg", width=6.5, height=4, units='in', res=300)

pam_bio_plot <- 
  #ggplot(pam_salmonids, aes(Year, biomass_100m, fill=Station)) + 
  ggplot(pam_summary, aes(as.factor(Year), stand.species.biomass.contr, fill=Station)) + 
  theme_bw() + 
  geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=12)) +
  scale_fill_manual(values=c("#cccccc", "#666666")) + 
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.11, .88)) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=1.5) +
  annotate("text", x = 3.5, y = 1150, label = "'3 years'") +
  geom_vline(xintercept = 4.5, linetype="dashed", size=1.5) +
  annotate("text", x = 4.5, y = 1150, label = "'19 years'") +
  geom_errorbar(aes(ymax=stand.species.biomass.contr.ucl, ymin=stand.species.biomass.contr.lcl), size=1, width=0.25, position=position_dodge(.9)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


pam_bio_plot

dev.off()
par(mfrow=c(1,1))





#####Plot of abundance estimate for each species each year above and below diversion
# not one of the plots - ignore
# jpeg("Pam_sppabun_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# Pam_allspecies<-Pamehac_allspecies_bytype
# 
# Pam_allspecies$Year<-as.factor(Pam_allspecies$Year)
# 
# Pam_sppabun_plot<-ggplot(Pam_allspecies, aes(Year, abundance_100m)) + 
#   theme_bw() + geom_bar(position=position_dodge(), stat="identity") + 
#   theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
#   ylab("Population Estimate (Number/100 sq. meters)") + xlab("Year") +
#   facet_grid(Station~Species) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
#   geom_errorbar(aes(ymax=stand.species.abundance.contr.ucl, ymin=stand.species.abundance.contr.lcl), size=1, width=0.25) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# 
# Pam_sppabun_plot
# dev.off()
# par(mfrow=c(1,1))

#####test


# jpeg("Pam_sppabun_plot2.jpg", width=7, height=4, units='in', res=600)
# 
# Pam_allspecies<-Pamehac_allspecies_bytype
# 
# Pam_allspecies$Year<-as.factor(Pam_allspecies$Year)
# 
# Pam_sppabun_plot2<-ggplot(Pam_allspecies, aes(Year, abundance_100m, fill=Station)) + 
#   theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
#   theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
#   ylab("Population Estimate (Number/100 sq. meters)") + xlab("Year") +
#   scale_fill_manual(values=c("#cccccc", "#666666")) + 
#   facet_grid(~Species) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
#   theme(legend.title=element_blank()) +
#   geom_errorbar(aes(ymax=stand.species.abundance.contr.ucl, ymin=stand.species.abundance.contr.lcl), size=1, width=0.25, position=position_dodge(.9)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# Pam_sppabun_plot2
# dev.off()
# par(mfrow=c(1,1))



#####Plot of biomass estimate for each species each year above and below diversion


# jpeg("Pam_sppbio_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# Pam_allspecies<-Pamehac_allspecies_bytype
# 
# Pam_allspecies$Year<-as.factor(Pam_allspecies$Year)
# 
# Pam_sppbio_plot<-ggplot(Pam_allspecies, aes(Year, biomass_100m)) + 
#   theme_bw() + geom_bar(position=position_dodge(), stat="identity") + 
#   theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
#   ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
#   facet_grid(Station~Species) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
#   theme(legend.title=element_blank()) +
#   geom_errorbar(aes(ymax=stand.species.biomass.contr.ucl, ymin=stand.species.biomass.contr.lcl), size=1, width=0.25) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# 
# Pam_sppbio_plot
# dev.off()
# par(mfrow=c(1,1))
# 


# AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
# Rose Blanche ----
# jpeg("RB_bothsppabun_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# RB_spp_maincomp$Year<-as.factor(RB_spp_maincomp$Year)
# 
# RB_bothsppabun_plot<-ggplot(RB_spp_maincomp, aes(Year, abundance_100m)) + 
#   theme_bw() + geom_bar(position=position_dodge(), stat="identity") + 
#   theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
#   ylab("Population Estimate (Number/100 sq. meters)") + xlab("Year") +
#   facet_grid(Station~Species) +
#   geom_errorbar(aes(ymax=abundance_ucl, ymin=abundance_lcl), size=1, width=0.25) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# 
# RB_bothsppabun_plot
# dev.off()
# par(mfrow=c(1,1))

#####Plot of biomass estimate for each species each year in both Main Stem and Compensation Habitats

# 
# jpeg("RB_bothsppbio_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# RB_spp_maincomp$Year<-as.factor(RB_spp_maincomp$Year)
# 
# RB_bothsppbio_plot<-ggplot(RB_spp_maincomp, aes(Year, biomass_100m)) + 
#   theme_bw() + geom_bar(position=position_dodge(), stat="identity") + 
#   theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
#   ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
#   facet_grid(Station~Species) +
#   geom_errorbar(aes(ymax=biomass_ucl, ymin=biomass_lcl), size=1, width=0.25) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# 
# RB_bothsppbio_plot
# dev.off()
# par(mfrow=c(1,1))
# 
# ####Plot of biomass estimate for all salmonids combined each year in both Main Stem and Compensation Habitats
# 
# 
# jpeg("RB_salmonidsbio_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# RB_salmonids_maincomp$Year<-as.factor(RB_salmonids_maincomp$Year)
# 
# RB_salmonidsbio_plot<-ggplot(RB_salmonids_maincomp, aes(Year, biomass_100m, fill=Station)) + 
#   theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
#   theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
#   theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
#   theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
#   theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
#   ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.85, .85)) +
#   geom_errorbar(aes(ymax=biomass_ucl, ymin=biomass_lcl), size=1, width=0.30, position=position_dodge(.9)) +
#   scale_fill_manual(values=c("dark grey", "light grey")) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# 
# RB_salmonidsbio_plot
# 
# dev.off()
# par(mfrow=c(1,1))
# 
# ####Plot of biomass estimate for all salmonids combined each year in both Main Stem and Compensation Habitats
# 
# 
# jpeg("RB_salmonidsabun_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# RB_salmonids_maincomp$Year<-as.factor(RB_salmonids_maincomp$Year)
# 
# RB_salmonidsabun_plot<-ggplot(RB_salmonids_maincomp, aes(Year, abundance_100m, fill=Station)) + 
#   theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
#   theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
#   theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
#   theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
#   theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
#   ylab("Population Estimate (number/100 sq. meters)") + xlab("Year") +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.88, .88)) +
#   geom_errorbar(aes(ymax=abundance_ucl, ymin=abundance_lcl), size=1, width=0.30, position=position_dodge(.9)) +
#   scale_fill_manual(values=c("dark grey", "light grey")) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# 
# RB_salmonidsabun_plot
# 
# dev.off()
# par(mfrow=c(1,1))
# 
# 
# #####Plot of Total biomass for compensation stream compared to biomass destroyed
# 
# 
# jpeg("RB_nonetloss_plot.jpg", width=7, height=5, units='in', res=600)
# 
# RB_nonetloss$Year<-as.factor(RB_nonetloss$Year)
# 
# RB_nonetloss_plot<-ggplot(RB_nonetloss, aes(Year, total_biomass, fill=Station)) + 
#   theme_bw() + geom_bar(position=position_dodge(), stat="identity", colour="black") + 
#   theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
#   theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
#   theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
#   theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
#   ylab("Total Estimated Biomass (grams)") + xlab("Year") +
#   theme(legend.title=element_blank()) +
#   
#   geom_errorbar(aes(ymax=tbiomass_ucl, ymin=tbiomass_lcl), size=1, width=0.30, position=position_dodge(.9)) +
#   geom_hline(yintercept = 23940, colour="red", linetype="dashed") +
#   annotate("text", x = 2.6, y = 25000, label = "'No Net Loss'", colour="red") +
#   scale_fill_manual(values=c("dark grey", "red")) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# 
# RB_nonetloss_plot
# 
# dev.off()
# par(mfrow=c(1,1))


# Plots_for_Spawner ----
#ASYOY and AS Together

Pam_salmondense_plot <-
  ggplot(pam_density_allAS, aes(as.factor(Year), mean, fill=type)) + 
  geom_bar(position=position_dodge(0.9), stat="identity", colour="black") +
  theme_bw() +  
  facet_grid(~Species) +
  theme(axis.text.x  = element_text(angle=45, vjust=0.2, size=12)) +
  ylab("Density Estimate  (#/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.63, .88)) +
  theme(legend.text=element_text(size=8)) +
  ggtitle("Atlantic Salmon Young of the Year Density in Pamehac Brook") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_fill_manual(values=c("sky blue", "dark grey"),
                    name="",
                    breaks=c("ABOVE", "BELOW"),
                    labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  geom_errorbar(aes(ymax=mean-se, ymin=mean+se), size=0.5, width=0.25, position=position_dodge(0.9)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
Pam_salmondense_plot
#dev.off()


## ASYOY ----


jpeg("ASYOY_density_barplot.jpg", width=7, height=5, units='in', res=300)

ASYOY_Spawner <- 
  ggplot(Pam_density_allASYOY, aes(as.factor(Year), mean, fill=type)) + 
  geom_bar(position=position_dodge(0.9), stat="identity", colour="black") +
  theme_bw() +  
  theme(axis.text.x  = element_text(size=12)) +
  theme(axis.text.y  = element_text(size=12)) +
  ylab("Density Estimate  (#/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.12, .88)) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title=element_text(face="bold", size=12)) +
  annotate("text", x=3, y=58, label="Stocking of") +
  annotate("text", x=3, y=54, label="42,000 fry") +
  theme(legend.text=element_text(size=8)) +
  ggtitle("Atlantic Salmon Young of the Year Density in Pamehac Brook") + 
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_fill_manual(values=c("sky blue", "dark grey"),
                    name="",
                    breaks=c("ABOVE", "BELOW"),
                    labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  geom_errorbar(aes(ymax=mean-se, ymin=mean+se), size=0.5, width=0.25, position=position_dodge(0.9)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ASYOY_Spawner

dev.off()



##AS ----



jpeg("AS_density_barplot.jpg", width=7, height=5, units='in', res=500)

AS_Spawner <- 
  ggplot(Pam_density_AS, aes(as.factor(Year), mean, fill=type)) + 
  geom_bar(position=position_dodge(0.9), stat="identity", colour="black") +
  theme_bw() +  
  theme(axis.text.x  = element_text(size=12)) +
  theme(axis.text.y  = element_text(size=12)) +
  ylab("Density Estimate  (#/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.25, .88)) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.title=element_text(face="bold", size=12)) +
  theme(legend.text=element_text(size=8)) +
  ggtitle("Atlantic Salmon 1 Year+ Density in Pamehac Brook") + 
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_fill_manual(values=c("sky blue", "dark grey"),
                    name="",
                    breaks=c("ABOVE", "BELOW"),
                    labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  geom_errorbar(aes(ymax=mean-se, ymin=mean+se), size=0.5, width=0.25, position=position_dodge(0.9)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

AS_Spawner

dev.off()


# plotsbyspecies ----
####annotate("text", x = 1.5, y = 1000, label = "'Restoration'") +

#####All Species Together -too busy
### this is the dataset that we don't have
### this doesn't make sense - there is no "pam_biomass_se but ignoring because she doesn't have plots for these anyway.
# jpeg("Pam_biose_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# Pam_biose_plot <-
#   ggplot(pam_biomass_se, aes(Year, MeanBiomass, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(1)) +
#   theme_bw() +  
#   facet_grid(~Species) +
#   theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
#   ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.11, .88)) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
#   geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(1)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# Pam_biose_plot
# dev.off()
# par(mfrow=c(1,1))
# 
# 
# Pam_dense_plot<-ggplot(pam_biomass_se, aes(Year, MeanAbun, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(1)) +
#   theme_bw() +  
#   facet_grid(~Species) +
#   theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
#   ylab("Density Estimate  (grams/100 sq. meters)") + xlab("Year") +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.88, .88)) +
#   theme(legend.text=element_text(size=6)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
#   geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(1)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# Pam_dense_plot

###DENSITY - SALMON ONLY



####Point Graphs of Biomass Above and Below Diversion by Species with SE of Mean
#####AS

# pam_ASbiomass_se<-subset(pam_biomass_se, Species=="AS")
# 
# jpeg("AS_biose_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# AS_biose_plot<-ggplot(pam_ASbiomass_se, aes(Year, MeanBiomass, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.90, .88)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# AS_biose_plot
# dev.off()
# 
# #####ASYOY
# 
# library(ggplot2)
# 
# pam_ASYOYbiomass_se<-subset(pam_biomass_se, Species=="ASYOY")
# 
# jpeg("ASYOY_biose_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# ASYOY_biose_plot<-ggplot(pam_ASYOYbiomass_se, aes(Year, MeanBiomass, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   scale_fill_manual(values=c("black", "grey")) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.11, .80)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# ASYOY_biose_plot
# dev.off()
# 
# #####BT
# 
# pam_BTbiomass_se<-subset(pam_biomass_se, Species=="BT")
# 
# jpeg("BT_biose_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# BT_biose_plot<-ggplot(pam_BTbiomass_se, aes(Year, MeanBiomass, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   scale_fill_manual(values=c("black", "grey")) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.90, .88)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   
#   geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# BT_biose_plot
# dev.off()           
# 
# #####BTYOY
# 
# pam_BTYOYbiomass_se<-subset(pam_biomass_se, Species=="BTYOY")
# 
# jpeg("BTYOY_biose_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# BTYOY_biose_plot<-ggplot(pam_BTYOYbiomass_se, aes(Year, MeanBiomass, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   scale_fill_manual(values=c("black", "grey")) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.90, .88)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   
#   geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# BTYOY_biose_plot
# dev.off()
# 
# #######Mean Density Estimates by Species with Standard Errors
# #####AS
# 
# pam_ASbiomass_se<-subset(pam_biomass_se, Species=="AS")
# 
# jpeg("AS_dense_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# AS_dense_plot<-ggplot(pam_ASbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.90, .88)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# AS_dense_plot
# dev.off()
# 
# #####ASYOY
# 
# library(ggplot2)
# 
# pam_ASYOYbiomass_se<-subset(pam_biomass_se, Species=="ASYOY")
# 
# jpeg("ASYOY_dense_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# ASYOY_dense_plot<-ggplot(pam_ASYOYbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   scale_fill_manual(values=c("black", "grey")) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.11, .80)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# ASYOY_dense_plot
# dev.off()
# 
# #####BT
# 
# pam_BTbiomass_se<-subset(pam_biomass_se, Species=="BT")
# 
# jpeg("BT_dense_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# BT_dense_plot<-ggplot(pam_BTbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   scale_fill_manual(values=c("black", "grey")) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.90, .88)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   
#   geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# BT_dense_plot
# dev.off()           
# 
# #####BTYOY
# 
# pam_BTYOYbiomass_se<-subset(pam_biomass_se, Species=="BTYOY")
# 
# jpeg("BTYOY_dense_plot.jpg", width=6.5, height=4, units='in', res=600)
# 
# BTYOY_dense_plot<-ggplot(pam_BTYOYbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
#   geom_point(size=3, position=position_dodge(0.5)) +
#   scale_fill_manual(values=c("black", "grey")) +
#   theme_bw() +  
#   theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
#   ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
#   facet_grid(~Species) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.90, .88)) +
#   theme(legend.text=element_text(size=5)) +
#   scale_fill_discrete(name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("ABOVE", "BELOW"),
#                       labels=c("Above Diversion", "Below Diversion")) +
#   geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
#   
#   geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
#   
#   geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
# 
# BTYOY_dense_plot
# dev.off()


# END ----
# AAAA