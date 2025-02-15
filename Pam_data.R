# Set up a project - see the below link for directions.
#https://happygitwithr.com/rstudio-git-github.html

# But basically:
# 1.	Set up a Git repo on GitHub.
# 2.	Create the project in R - New Project - Version Control - Git
# 3. type "git add -A" in the terminal
# 4.	Create a bunch of directories automatically (see below)
# 5. Copy git -ignore file

#Create a "name_dat.R" file
#put this file in the folder with the project and create the following subfolders
if(!dir.exists("archive"))dir.create("archive")
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("data_derived"))dir.create("data_derived")
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
if(!dir.exists("ms"))dir.create("ms") # manuscript
if(!dir.exists("report"))dir.create("report") #for rmd report
if(!dir.exists("refs"))dir.create("refs") #for rmd report


# Created 2024-07-09

## This is just a compliation of Kristin Loughlin's code
# libraries ----
library(readr)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)

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
                                se   = sd / sqrt(N) # these are naively calculated - see ReadMe for this project and in Rose Blanche
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


# delta method ----
# manipulations
## by species, year, and type: on station ----
## dfb - dataframe for biomass - just to make things simpler
dfb <- output_pamehac_by_station[, c(1:5, 13:15)]
dfb <- rename(dfb, bm = stand.species.biomass.contr,
              bm_ucl = stand.species.biomass.contr.ucl,
              bm_lcl = stand.species.biomass.contr.lcl)
str(dfb)


# dft - dataframe for transition
dft <- dfb |>
  group_by(Species, Year, type) |>
  mutate(bm_var = ((bm_ucl - bm)/1.96)^2, # if this were the mean, then this would be wrong but its an estimate so CIs are done with SE which is really SD.
         #bm_sd = sqrt(bm_var), this is the SD if we were dealing with means but bm is an estimate so its the SE of the estimate
         bm_se = sqrt(bm_var),
        # bm_se = sqrt(bm_var)/length(Station), # probably not needed but should have been the sqrt of length(Station)
         #bm_sd1 = (bm_ucl - bm)/1.96, # gives the same result as bm_sd
         n = length(Station),
        # pd = bm_var/length(Station)^2, # I think that his is a mistkae - this is just the partial derivative (pd) - variance and pd are multiplied in pdVar
         pd = 1/n^2,
         pdVar = bm_var*pd 
  )

# add a Before/After variable
dft$time <- NA
for(i in seq_along(dft$Year)){
  if(dft$Year[i] == 1990){
    dft$time[i] <- "Before"
  } else {
    dft$time[i] <- "After"
  }
}
str(dft, give.attr=FALSE)
#dft$time

#View(dft)

## dfd - dataframe for delta method
dfd <- dft |>
  group_by(Species, Year, type) |>
  summarise(mean = mean(bm),
         VAR = sum(pdVar), 
         # ll = mean - 1.96*(sqrt(VAR)/length(type)),
         # ul = mean + 1.96*(sqrt(VAR)/length(type))
         # for the same reason above, I think that the proper statistic to use is the standard error of the estimate, not the SE of the mean
         ll = mean - 1.96*(sqrt(VAR)),
         ul = mean + 1.96*(sqrt(VAR))
         )
dfd |> print(n = Inf)
#View(dfd)
str(dfd, give.attr=FALSE)

# so, i'm pretty sure that the above is "right" but it does lead to a lot of negative lower CIs and this is not good becaue you can't have negative fish.
## just to view
#  ggplot(dfd, aes(as.factor(Year), mean)) + 
  ggplot(dfd, aes(as.factor(Year), mean, colour=type)) + 
  geom_point(size=2, position=position_dodge(0.5)) +
  theme_bw() +  
  theme(axis.text.x  = element_text(vjust=0.2, size=12)) +
  ylab("Biomass Estimate  (grams/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside=c(.88, .88)) +
  geom_errorbar(aes(ymax=ul, ymin=ll), linewidth=1, width=0.25, position=position_dodge(0.5)) +
    geom_hline(yintercept = 0) +
  facet_grid(type~Species) + 
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

# So this plot shows that there's a lot of estimates below 0
  
dfd[dfd$Species == "AS" & dfd$Year == 1991,]
dft[dft$Species == "AS" & dft$Year == 1991,][order(dft[dft$Species == "AS" & dft$Year == 1991,]$type),]


## by year, and type: on station ----
### all salmonids


# END ----