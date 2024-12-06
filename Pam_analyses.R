# Summary:


# source ----
# source("Pam_abun_bio.R")
source("Pam_data_new.R")


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

# Density ----
## BT ----
### data ----
str(df_a, give.attr=FALSE)
df_aBT <- df_a[df_a$Species == "BT",]
df_aBT$int <- interaction(df_aBT$type, df_aBT$time)
plot(density(df_aBT$abun.stand, na.rm = T))
summary(df_aBT$abun.stand)
with(df_aBT, table(abun, Year))

df_a |>
  group_by(Year, Species) |>
  summarise(a = sum(abun != 0), 
            a0 = sum(abun == 0))


### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Gamma is OK here because there are no zeros
bt.glmm1 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), #+ (1 | Station),
  dispformula = ~ int,
  family=ziGamma(link="log"), ziformula = ~1,
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
  family=ziGamma(link="log"), ziformula = ~,
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
abline(coef = c(1, 1))

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


# Diagnostics look fantastic for this model.  Proceed with this model (bt.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(bt.glmm2)
mean_by_site(df_sumBT, "d")
baci.plot(df_baciBT, "d")


## AS ----
### data ----
df_aAS <- df_a[df_a$Species == "AS",]
df_aAS$int <- interaction(df_aAS$type, df_aAS$time)
with(df_aAS, table(abun, Year))

### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Gamma is OK here because there are no zeros
as.glmm1 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), #+ (1 | Station),
  dispformula = ~ int,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = df_aAS
)

summary(as.glmm1)
# str(bt.glmm1)


# Fifield advised the following
## Compre the results of:
### glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
as.glmm2 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = df_aAS
)
summary(as.glmm2)

# str(as.glmm2)
# as.glmm1 does not run - probably not enough data.  Use as.glmm2 but see how diagnostics look 


### diagnostics ----
as.glmm2_simres <- simulateResiduals(as.glmm2, plot = T)
# str(as.glmm1_simres,1)
residuals(as.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion aren't great but the  homogeneity of variance is bad.  

# plots by themselves or by group
testUniformity(as.glmm2_simres)
testQuantiles(as.glmm2_simres)

plotResiduals(as.glmm2_simres, form = df_aAS$time)
plotResiduals(as.glmm2_simres, form = df_aAS$type)
# these suggest that there may be an interaction


# dispersion/zeroinflation
testDispersion(as.glmm2_simres)
testZeroInflation(as.glmm2_simres) # this gives a bogus result but clearly, from the inflation model, there is no zero-inflation.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
as.glmm2_simres_recalc <- recalculateResiduals(as.glmm2_simres, group = df_aAS$Year)

testTemporalAutocorrelation(as.glmm2_simres_recalc, time = unique(df_aAS$Year))

# resids look great: conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
as.glmm2_simres_recalcSpace <- recalculateResiduals(as.glmm2_simres, group = df_aAS$Station)
unique(df_aAS$Station) 
#str(as.glmm2_simres_recalcSpace)
length(as.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(as.glmm2_simres_recalcSpace, x = unique(df_aAS$west), y = unique(df_aAS$north))

spatialAutoCorrBase_fun(df_aAS, as.glmm2_simres_recalcSpace)   


as.biomass.all <- spatialData_join(df_sumAS, as.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(as.biomass.all)


# Diagnostics look fantastic for this model.  Proceed with this model (as.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(as.glmm2)
mean_by_site(df_sumAS, "d")
baci.plot(df_baciAS, "d")


# Biomass ----
## AS ----
### data ----
with(df_aAS, table(bio, Year))

### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Gamma is OK here because there are no zeros
as_bio.glmm1 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  dispformula = ~ int,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = df_aAS
)

summary(as_bio.glmm1)


as_bio.glmm2 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = df_aAS
)
summary(as_bio.glmm2)

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

# resids look great: conclude no temproal issues


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

# use this to compare to Scruton et al. 1998
tmp <- df_a |> group_by(Year, Species, type) |> filter(bio.stand > 0) |> summarise(mean_bio = mean(bio.stand)) 

tmp <- df_a |> group_by(Year, Species, type) |> filter(bio.stand > 0) |> summarise(mean_bio = mean(bio.stand*100)) 

tmp <- df_a |> group_by(Year, Species, type) |> summarise(mean_bio = mean(bio.stand*100)) 


ggplot(tmp, aes(x = Year, y = mean_bio, group = type, fill = type)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap(~Species)
# END ----