# Summary:
# This file is a comparison of density and biomass of BT and AS in Pamehac Cove.  The data are from 2017-2020.  The data are analyzed using a zero-inflated gamma model with random effects for Year.  The residuals are checked for normality, homogeneity of variance, temporal autocorrelation, and spatial autocorrelation.  The residuals are then used to create a spatial dataset that is used to check for spatial autocorrelation.  The results are then summarized and compared to the results of Scruton et al. 1998.  The results are then plotted.

## abun.stand and bio.stand area #/area*100

# source ----
# source("Pam_abun_bio.R")
source("Pam_data_new.R")
source("Pam_fun.R")


# See Seal Cove code for original work and thoughts on this approach
# See ReadMe for thoughts on Pamehac. 

# library ----
#library(nlme)
library(glmmTMB)
library(DHARMa)
#library(readr)
#library(tidyr)
library(ggplot2)
library(cowplot)

# Scruton figs ----
# use this to compare to Scruton et al. 1998

df_a |>  
  group_by(Year, Species, type) |> 
  summarise(mean_abun = mean(abun.stand)) |>
  ggplot(aes(x = Year, y = mean_abun, group = type, fill = type)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap(~Species)

df_a |>  
  group_by(Year, Species, type) |> 
  summarise(mean_bio = mean(bio.stand)) |>
  ggplot(aes(x = Year, y = mean_bio, group = type, fill = type)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap(~Species)

# check for zeros
df_a |>
  group_by(Year, Species) |>
  summarise(a = sum(abun != 0), 
            a0 = sum(abun == 0))


# Density ----
## BT ----
### data ----
str(df_a, give.attr=FALSE)
df_aBT <- df_a[df_a$Species == "BT",]
df_aBT$int <- interaction(df_aBT$type, df_aBT$time)
plot(density(df_aBT$abun.stand, na.rm = T))
summary(df_aBT$abun.stand)
with(df_aBT, table(abun, Year))
with(df_aBT, table(abun, Station, Year))


### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Gamma is OK here because there are no zeros
#### I rand both models with a ziformula = ~ 1. bt.glmm2 was the better model but residuals were worse.  I noticed some zeros and experimented with modifying the ziformula and found that ~ time was best.  This also seemed to help the resids but nothing is significant which seems to match the BACI plots.  The main question is whether there are hidden zeros, i.e., Stations with no zeros because these don't seem to get entered in the datasets.
bt.glmm1 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), #+ (1 | Station),
  #abun.stand ~ type*time + as.numeric(Year) + (1 | Year),
  dispformula = ~ int,
  family=ziGamma(link="log"), ziformula = ~ time,
  REML = TRUE,
  data = df_aBT
)

summary(bt.glmm1)
# str(bt.glmm1)


# Fifield advised the following
## Compre the results of:
### glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
bt.glmm2 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), #+ (1 | Station),
#  abun.stand ~ type*time + as.numeric(Year) + (1 | Year),
  family=ziGamma(link="log"), ziformula = ~ time,
  REML = TRUE,
  data = df_aBT
)
summary(bt.glmm2)

# str(bt.glmm2)
anova(bt.glmm1, bt.glmm2) # this suggests that model bt.glmm2 without dispersion is better than with (bt.glmm1)


### diagnostics ----
bt.glmm2_simres <- simulateResiduals(bt.glmm2, plot = T)
# str(bt.glmm1_simres,1)
residuals(bt.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion aren't great but the  homogeneity of variance looks fine.  Fifield said that this needs to be looked at for all the usual reasons: normality and homogeneity of variance.  Tried Station as a random effect but it didn't help

# plots by themselves or by group
testUniformity(bt.glmm2_simres)
testQuantiles(bt.glmm2_simres)

plotResiduals(bt.glmm2_simres, form = df_aBT$time)
plotResiduals(bt.glmm2_simres, form = df_aBT$type)

# redundant with above
# testCategorical(bt.glmm2_simres, catPred = df_aBT$time)
# testCategorical(bt.glmm2_simres, catPred = df_aBT$type)


# dispersion/zeroinflation
testDispersion(bt.glmm2_simres)
testZeroInflation(bt.glmm2_simres) # this gives a bogus result but clearly, from the inflation model, there is no zero-inflation.


# not sure about
residuals(bt.glmm2_simres, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(residuals(bt.glmm2_simres), residuals(bt.glmm2_simres, quantileFunction = qnorm, outlierValues = c(-7,7)))
abline(coef = c(-2, 2))

### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
bt.glmm2_simres_recalc <- recalculateResiduals(bt.glmm2_simres, group = df_aBT$Year)

testTemporalAutocorrelation(bt.glmm2_simres_recalc, time = unique(df_aBT$Year))

# resids look great: conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
bt.glmm2_simres_recalcSpace <- recalculateResiduals(bt.glmm2_simres, group = df_aBT$Station)
unique(df_aBT$Station) 
#str(bt.glmm2_simres_recalcSpace)
length(bt.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(bt.glmm2_simres_recalcSpace, x = unique(df_aBT$west), y = unique(df_aBT$north))

spatialAutoCorrBase_fun(df_aBT, bt.glmm2_simres_recalcSpace)   


bt.biomass.all <- spatialData_join(df_sumBT, bt.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(bt.biomass.all)


# Diagnostics are mixed. look fantastic for this model but .  Proceed with this model (bt.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(bt.glmm2)
mean_by_site(df_sumBT, "d")
baci.plot(df_baciBT, "d")
ggsave(paste0("output/BT_density.png"), width=10, height=8, units="in")
ggplot(df_aBT, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_wrap(~Year)

confint(bt.glmm2)
tab.ci(bt.glmm2, "bt_den") 




## AS ----
### data ----
df_aAS <- df_a[df_a$Species == "AS",]
df_aAS$int <- interaction(df_aAS$type, df_aAS$time)
with(df_aAS, table(abun, Year))
plot(density(df_aAS$abun.stand, na.rm = T))


### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Gamma is OK here because there are no zeros
as.glmm1 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), #+ (1 | Station),
  dispformula = ~ int,
  family=ziGamma(link="log"), ziformula = ~ type + time,
  REML = TRUE,
  # control = glmmTMBControl(
  #   optimizer = optim,
  #   optArgs=list(method = "BFGS")),
  data = df_aAS
)

summary(as.glmm1)
# str(bt.glmm1)


# Fifield advised the following
## Compre the results of:
### glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
as.glmm2 <- glmmTMB(
  abun.stand ~ time*type + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), ziformula = ~ type + time,
  REML = TRUE,
  data = df_aAS
)
summary(as.glmm2)


anova(as.glmm1, as.glmm2)
# str(as.glmm2)
# as.glmm1 does not run - probably not enough data.  Use as.glmm2 but see how diagnostics look 


### diagnostics ----
as.glmm1_simres <- simulateResiduals(as.glmm1, plot = T)
# str(as.glmm1_simres,1)
residuals(as.glmm1_simres) 
# these are scaled residuals
# The normality/overdispersion aren't great but the  homogeneity of variance is bad.  

# plots by themselves or by group
testUniformity(as.glmm1_simres)
testQuantiles(as.glmm1_simres)

plotResiduals(as.glmm1_simres, form = df_aAS$time)
plotResiduals(as.glmm1_simres, form = df_aAS$type)
plotResiduals(as.glmm1_simres, form = df_aAS$int)
# these suggest that there may be an interaction


# dispersion/zeroinflation
testDispersion(as.glmm1_simres)
testZeroInflation(as.glmm1_simres) # this gives a bogus result but clearly, from the inflation model, there is no zero-inflation.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
as.glmm1_simres_recalc <- recalculateResiduals(as.glmm1_simres, group = df_aAS$Year)

testTemporalAutocorrelation(as.glmm1_simres_recalc, time = unique(df_aAS$Year))

# resids look great: conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
as.glmm1_simres_recalcSpace <- recalculateResiduals(as.glmm1_simres, group = df_aAS$Station)
unique(df_aAS$Station) 
#str(as.glmm2_simres_recalcSpace)
length(as.glmm1_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(as.glmm1_simres_recalcSpace, x = unique(df_aAS$west), y = unique(df_aAS$north))

spatialAutoCorrBase_fun(df_aAS, as.glmm1_simres_recalcSpace)   


as.biomass.all <- spatialData_join(df_sumAS, as.glmm1_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(as.biomass.all)


# Diagnostics look fantastic for this model.  Proceed with this model (as.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
test <- summary(as.glmm1)
test$vcov$cond
mean_by_site(df_sumAS, "d")
baci.plot(df_baciAS, "d")
ggsave(paste0("output/AS_density.png"), width=10, height=8, units="in")
ggplot(df_aAS, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_wrap(~Year)
confint(as.glmm1)[1:4, ]
confint(as.glmm2)[1:4, ]


confint(as.glmm1)
tab.ci(as.glmm1, "as_den") 



# Biomass ----
## BT ----
### data ----
with(df_aBT, table(bio, Year))

### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Gamma is OK here because there are no zeros
#### I tried a run wih ziformula = ~1 but got some patterns in resids - ziformula = ~time resolves the problem although AIC is about the same
bt_bio.glmm1 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  dispformula = ~ int,
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  data = df_aBT
)

summary(as_bio.glmm1)


bt_bio.glmm2 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), 
  ziformula = ~ time,
  REML = TRUE,
  data = df_aBT
)
summary(as_bio.glmm2)

anova(as_bio.glmm1, as_bio.glmm2)

# as_bio.glmm1 does not run - probably not enough data.  Use as_bio.glmm2 but see how diagnostics look 


### diagnostics ----
bt_bio.glmm2_simres <- simulateResiduals(bt_bio.glmm2, plot = T)
# str(as_bio.glmm1_simres,1)
residuals(bt_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion aren't great but the  homogeneity of variance is bad.  

# plots by themselves or by group
testUniformity(bt_bio.glmm2_simres)
testQuantiles(bt_bio.glmm2_simres)

plotResiduals(bt_bio.glmm2_simres, form = df_aAS$time)
plotResiduals(bt_bio.glmm2_simres, form = df_aAS$type)
# these suggest that there may be an interaction


# dispersion/zeroinflation
testDispersion(bt_bio.glmm2_simres)
testZeroInflation(bt_bio.glmm2_simres) # this gives a bogus result but clearly, from the inflation model, there is no zero-inflation.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
bt_bio.glmm2_simres_recalc <- recalculateResiduals(bt_bio.glmm2_simres, group = df_aAS$Year)

testTemporalAutocorrelation(bt_bio.glmm2_simres_recalc, time = unique(df_aAS$Year))

# resids look great: conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
bt_bio.glmm2_simres_recalcSpace <- recalculateResiduals(bt_bio.glmm2_simres, group = df_aAS$Station)
unique(df_aBT$Station) 
#str(as.glmm2_simres_recalcSpace)
length(bt_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(bt_bio.glmm2_simres_recalcSpace, x = unique(df_aBT$west), y = unique(df_aBT$north))

spatialAutoCorrBase_fun(df_aBT, bt_bio.glmm2_simres_recalcSpace)   


bt_bio.biomass.all <- spatialData_join(df_sumBT, bt_bio.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(bt_bio.biomass.all)


# Diagnostics look fantastic for this model.  Proceed with this model (as.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(bt_bio.glmm2)
mean_by_site(df_sumAS, "b")
baci.plot(df_baciAS, "b")
ggsave(paste0("output/BT_biomass.png"), width=10, height=8, units="in")
ggplot(df_aBT, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_wrap(~Year)


confint(bt_bio.glmm2)
confint(bt_bio.glmm2)[1:4, ]
tab.ci(bt_bio.glmm2, "bt_bio") 


## AS ----
### data ----
with(df_aAS, table(bio, Year))

### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Gamma is OK here because there are no zeros
#### there are problems with the temporal resids here - tried Year as a covaraite but that makes things worse - not sure what to so
as_bio.glmm1 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
#  bio.stand ~ type*time + Year +  (1 | Year), #+ (1 | Station),
  dispformula = ~ int,
  family=ziGamma(link="log"), 
  ziformula = ~1,
  REML = TRUE,
# control = glmmTMBControl(
#   optimizer = optim,
#   optArgs=list(method = "BFGS")),
  data = df_aAS
)

summary(as_bio.glmm1)


as_bio.glmm2 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
#  bio.stand ~ type*time + Year + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = df_aAS
)
summary(as_bio.glmm2)

anova(as_bio.glmm1, as_bio.glmm2)
# str(as_bio.glmm2)
# as_bio.glmm1 does not run - probably not enough data.  Use as_bio.glmm2 but see how diagnostics look 


### diagnostics ----
as_bio.glmm2_simres <- simulateResiduals(as_bio.glmm2, plot = T)


# str(as_bio.glmm1_simres,1)
residuals(as_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion aren't great but the  homogeneity of variance is bad.  

# plots by themselves or by group
testUniformity(as_bio.glmm2_simres)
testQuantiles(as_bio.glmm2_simres)

plotResiduals(as_bio.glmm2_simres, form = df_aAS$time)
plotResiduals(as_bio.glmm2_simres, form = df_aAS$type)
# these suggest that there may be an interaction


# dispersion/zeroinflation
testDispersion(as_bio.glmm2_simres)
testZeroInflation(as_bio.glmm2_simres) # this gives a bogus result but clearly, from the inflation model, there is no zero-inflation.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
as_bio.glmm2_simres_recalc <- recalculateResiduals(as_bio.glmm2_simres, group = df_aAS$Year)

testTemporalAutocorrelation(as_bio.glmm2_simres_recalc, time = unique(df_aAS$Year))

# Temporal resids are awful


### spatial independence
# recalculate resids with stations as the grouping variable
as_bio.glmm2_simres_recalcSpace <- recalculateResiduals(as_bio.glmm2_simres, group = df_aAS$Station)
unique(df_aAS$Station) 
#str(as.glmm2_simres_recalcSpace)
length(as_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(as_bio.glmm2_simres_recalcSpace, x = unique(df_aAS$west), y = unique(df_aAS$north))

spatialAutoCorrBase_fun(df_aAS, as_bio.glmm2_simres_recalcSpace)   


as_bio.biomass.all <- spatialData_join(df_sumAS, as_bio.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(as_bio.biomass.all)


# Diagnostics look fantastic for this model.  Proceed with this model (as.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(as_bio.glmm2)
mean_by_site(df_sumAS, "b")
baci.plot(df_baciAS, "b")
ggsave(paste0("output/AS_biomass.png"), width=10, height=8, units="in")
ggplot(df_aAS, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_wrap(~Year)


confint(bt_bio.glmm2)
confint(bt_bio.glmm2)[1:4, c(3, 1, 2)]
tab.ci(bt_bio.glmm2, "as_bio") 





# END ----