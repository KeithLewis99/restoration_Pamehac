# Created 2024-07-09
## This is just a compliation of Kristin Loughlin's code
library(readr)
library(plyr)

# read data ----
output_pamehac_by_station <- read.csv("data/output_pamehac_by_station.csv")
str(output_pamehac_by_station)
#View(output_pamehac_by_station)

pam_salmonids <- read.csv("data/pam_salmonids.csv")
str(pam_salmonids)
#View(pam_salmonids)

pam_summary <- read.csv("data/salmonid_e.fishing.summary.data.csv")
str(pam_summary)



# data summaries ----
## output_pamehac_by_station ----
#####Used file Output_pamehac_by_station.   This is a file of output from AMEC.efishing8cs 
##script with Carl Strube estimates


# Pam_allspecies_meanbio <- ddply(output_pamehac_by_station, c("Year", "type", "Species"), summarise,
#                                 N    = length(stand.species.biomass.contr),
#                                 mean = mean(stand.species.biomass.contr),
#                                 sd   = sd(stand.species.biomass.contr),
#                                 se   = sd / sqrt(N)
# )
# 
# Pam_allspecies_meanbio
# 
# 
Pam_allspecies_meanden <- ddply(output_pamehac_by_station, c("Year", "type", "Species"), summarise,
                                N    = length(stand.species.abundance.contr),
                                mean = mean(stand.species.abundance.contr),
                                sd   = sd(stand.species.abundance.contr),
                                se   = sd / sqrt(N)
)

Pam_allspecies_meanden

# this is wonky inefficient code 
pam_density_allAS <- subset(Pam_allspecies_meanden, Pam_allspecies_meanden$Species!="BT")
# not BT
pam_density_allAS
pam_density_allAS <- subset(pam_density_allAS, pam_density_allAS$Species!="BTYOY")
###not BTYOY - Check to make sure only salmon
pam_density_allAS


Pam_density_allASYOY <- subset(pam_density_allAS, Species=="ASYOY")
Pam_density_AS <- subset(pam_density_allAS, Species=="AS")

## pam_salmonids ----
Pam_Mean_Biomass <- ddply(pam_salmonids, c("Year", "Type"), summarise,
                          N    = length(Biomass),
                          mean = mean(Biomass),
                          sd   = sd(Biomass),
                          se   = sd / sqrt(N)
)

Pam_Mean_Biomass



Pam_Mean_Density <- ddply(pam_salmonids, c("Year", "Type"), summarise,
                          N    = length(Density),
                          mean = mean(Density),
                          sd   = sd(Density),
                          se   = sd / sqrt(N)
)

Pam_Mean_Density