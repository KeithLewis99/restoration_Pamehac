#ASYOY and AS Together

Pam_salmondense_plot<-ggplot(pam_density_allAS, aes(Year, mean, fill=type)) + 
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
  geom_vline(xintercept = 3.5, linetype="dashed", size=0.5, angle=90) +
  geom_vline(xintercept = 4.5, linetype="dashed", size=0.5, angle=90) +
  geom_errorbar(aes(ymax=mean-se, ymin=mean+se), size=0.5, width=0.25, position=position_dodge(0.9)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
Pam_salmondense_plot
dev.off()

Pam_density_allASYOY<-subset(pam_density_allAS, Species=="ASYOY")


######ASYOY ONly
library(ggplot2)

jpeg("ASYOY_density_barplot.jpg", width=7, height=5, units='in', res=300)

ASYOY_Spawner<-ggplot(Pam_density_allASYOY, aes(Year, mean, fill=type)) + 
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





######AS ONly
library(ggplot2)

Pam_density_AS<-subset(pam_density_allAS, Species=="AS")

jpeg("AS_density_barplot.jpg", width=7, height=5, units='in', res=500)

AS_Spawner<-ggplot(Pam_density_AS, aes(Year, mean, fill=type)) + 
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




