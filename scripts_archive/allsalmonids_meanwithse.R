library(ggplot2)

library(plyr)

Pam_Mean_Biomass <- ddply(pam_salmonids, c("Year", "Type"), summarise,
                      N    = length(Biomass),
                      mean = mean(Biomass),
                      sd   = sd(Biomass),
                      se   = sd / sqrt(N)
)

Pam_Mean_Biomass



jpeg("pam_salmonids_bio2.jpg", width=6.5, height=4, units='in', res=300)

Pam_salmonids_bio_plot<-ggplot(Pam_Mean_Biomass, aes(Year, mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  theme_bw() +  
  theme(axis.text.x  = element_text(vjust=0.2, size=12)) +
  ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.88, .88)) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5, angle=90) +
  theme(legend.text=element_text(size=6)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_errorbar(aes(ymax=mean-se, ymin=mean+se), size=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

Pam_salmonids_bio_plot
dev.off()


#####All Salmonids Density

Pam_Mean_Density <- ddply(pam_salmonids, c("Year", "Type"), summarise,
                          N    = length(Density),
                          mean = mean(Density),
                          sd   = sd(Density),
                          se   = sd / sqrt(N)
)

Pam_Mean_Density


jpeg("pam_salmonids_den.jpg", width=6.5, height=4, units='in', res=300)

Pam_salmonids_den_plot<-ggplot(Pam_Mean_Density, aes(Year, mean, colour=Type)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  theme_bw() +  
  theme(axis.text.x  = element_text(vjust=0.2, size=12)) +
  ylab("Density Estimate  (#/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position=c(.11, .88)) +
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 1.5, linetype="solid", size=0.5, angle=90) +
  theme(legend.text=element_text(size=5)) +
  scale_fill_discrete(name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("ABOVE", "BELOW"),
                      labels=c("Above Diversion", "Below Diversion")) +
  geom_errorbar(aes(ymax=mean-se, ymin=mean+se), size=1, width=0.25, position=position_dodge(0.5)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

Pam_salmonids_den_plot
dev.off()
