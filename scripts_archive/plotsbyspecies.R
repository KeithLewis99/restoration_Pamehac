####annotate("text", x = 1.5, y = 1000, label = "'Restoration'") +

#####All Species Together -too busy
library(ggplot2)
jpeg("Pam_biose_plot.jpg", width=6.5, height=4, units='in', res=600)
pam_biomass_se$Year<-as.factor(pam_biomass_se$Year)
Pam_biose_plot<-ggplot(pam_biomass_se, aes(Year, MeanBiomass, colour=Type)) + 
  geom_point(size=3, position=position_dodge(1)) +
  theme_bw() +  
  facet_grid(~Species) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.11, .88)) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
  geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(1)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
Pam_biose_plot
dev.off()
par(mfrow=c(1,1))


Pam_dense_plot<-ggplot(pam_biomass_se, aes(Year, MeanAbun, colour=Type)) + 
  geom_point(size=3, position=position_dodge(1)) +
  theme_bw() +  
  facet_grid(~Species) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Density Estimate  (grams/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.88, .88)) +
  theme(legend.text=element_text(size=6)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
  geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(1)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
Pam_dense_plot

###DENSITY - SALMON ONLY



####Point Graphs of Biomass Above and Below Diversion by Species with SE of Mean
#####AS

pam_ASbiomass_se<-subset(pam_biomass_se, Species=="AS")

jpeg("AS_biose_plot.jpg", width=6.5, height=4, units='in', res=600)

AS_biose_plot<-ggplot(pam_ASbiomass_se, aes(Year, MeanBiomass, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  theme_bw() +  
  theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
  ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.90, .88)) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
  geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

AS_biose_plot
dev.off()

#####ASYOY

library(ggplot2)

pam_ASYOYbiomass_se<-subset(pam_biomass_se, Species=="ASYOY")

jpeg("ASYOY_biose_plot.jpg", width=6.5, height=4, units='in', res=600)

ASYOY_biose_plot<-ggplot(pam_ASYOYbiomass_se, aes(Year, MeanBiomass, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  scale_fill_manual(values=c("black", "grey")) +
  theme_bw() +  
  theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
  ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.11, .80)) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
  geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ASYOY_biose_plot
dev.off()

#####BT

pam_BTbiomass_se<-subset(pam_biomass_se, Species=="BT")

jpeg("BT_biose_plot.jpg", width=6.5, height=4, units='in', res=600)

BT_biose_plot<-ggplot(pam_BTbiomass_se, aes(Year, MeanBiomass, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  scale_fill_manual(values=c("black", "grey")) +
  theme_bw() +  
  theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
  ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.90, .88)) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
  
  geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(0.5)) +
           theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
           
BT_biose_plot
dev.off()           

#####BTYOY

pam_BTYOYbiomass_se<-subset(pam_biomass_se, Species=="BTYOY")

jpeg("BTYOY_biose_plot.jpg", width=6.5, height=4, units='in', res=600)

BTYOY_biose_plot<-ggplot(pam_BTYOYbiomass_se, aes(Year, MeanBiomass, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  scale_fill_manual(values=c("black", "grey")) +
  theme_bw() +  
  theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
  ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.90, .88)) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
  
  geom_errorbar(aes(ymax=MeanBiomass-SEBiomass, ymin=MeanBiomass+SEBiomass), size=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

BTYOY_biose_plot
dev.off()

#######Mean Density Estimates by Species with Standard Errors
#####AS

pam_ASbiomass_se<-subset(pam_biomass_se, Species=="AS")

jpeg("AS_dense_plot.jpg", width=6.5, height=4, units='in', res=600)

AS_dense_plot<-ggplot(pam_ASbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  theme_bw() +  
  theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
  ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.90, .88)) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
  geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

AS_dense_plot
dev.off()

#####ASYOY

library(ggplot2)

pam_ASYOYbiomass_se<-subset(pam_biomass_se, Species=="ASYOY")

jpeg("ASYOY_dense_plot.jpg", width=6.5, height=4, units='in', res=600)

ASYOY_dense_plot<-ggplot(pam_ASYOYbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  scale_fill_manual(values=c("black", "grey")) +
  theme_bw() +  
  theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
  ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.11, .80)) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
  geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ASYOY_dense_plot
dev.off()

#####BT

pam_BTbiomass_se<-subset(pam_biomass_se, Species=="BT")

jpeg("BT_dense_plot.jpg", width=6.5, height=4, units='in', res=600)

BT_dense_plot<-ggplot(pam_BTbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  scale_fill_manual(values=c("black", "grey")) +
  theme_bw() +  
  theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
  ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.90, .88)) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
  
  geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

BT_dense_plot
dev.off()           

#####BTYOY

pam_BTYOYbiomass_se<-subset(pam_biomass_se, Species=="BTYOY")

jpeg("BTYOY_dense_plot.jpg", width=6.5, height=4, units='in', res=600)

BTYOY_dense_plot<-ggplot(pam_BTYOYbiomass_se, aes(Year, MeanAbun, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  scale_fill_manual(values=c("black", "grey")) +
  theme_bw() +  
  theme(axis.text.x  = element_text( vjust=0.2, size=11)) +
  ylab("Density Estimate  (number/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.90, .88)) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5) +
  
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5) +
  
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5) +
  
  geom_errorbar(aes(ymax=MeanAbun-SEAbun, ymin=MeanAbun+SEAbun), size=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

BTYOY_dense_plot
dev.off()





