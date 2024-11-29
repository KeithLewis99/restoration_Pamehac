# Summary:


# source ----
source("Pam_abun_bio.R")

# See Seal Cove code for original work and thoughts on this approach
# See ReadMe for thoughts on Pamehac. 

# library ----
library(nlme)
library(glmmTMB)
library(DHARMa)
library(readr)
library(tidyr)
library(ggplot2)
library(cowplot)


# data ----
str(dft, give.attr=FALSE)
df_aBT <- df_a[df_a$Species == "BT",]
df_aBT$int <- interaction(df_aBT$type, df_aBT$time)
plot(density(df_aBT$bm))
summary(dft_BT$bm)


### Need glmmTMB to have random effects and a non-normal distribution
#### Gamma is OK here because there are no zeros
bt.glmm1 <- glmmTMB(
  bm ~ as.factor(type) + as.factor(time) + (1 | Year),
  dispformula = ~ int,
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = dft_BT
)

summary(bt.glmm1)
# str(bt.glmm1)


# Fifield advised the following
## Compre the results of:
### glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
bt.glmm2 <- glmmTMB(
  bm ~ as.factor(type) + as.factor(time) + (1 | Year),
  family=ziGamma(link="log"), ziformula = ~1,
  REML = TRUE,
  data = dft_BT
)
summary(bt.glmm2)

# str(bt.glmm2)
anova(bt.glmm1, bt.glmm2) # this suggests that model bt.glmm2 without dispersion is better than with (bt.glmm1)

## The estimates are virtually identical but the Std. Errors are slightly smaller than the model with dispersion.  
#Further, the diagnostics look better much better for this model (see below but tested separately for bt.glmm2 which is no longer in the code), especially for homogeneity of variance and this model has a much better AIC. 
# OK, the bt.glmm1 model is best.  Now, its time to see if its valid (Fifield said no need to test the less valid model at this point)?  Talked to Fifield about how to do this.  He and glmmTMB suggest using the DHARMa package

## diagnostics ----
bt.glmm1_simres <- simulateResiduals(bt.glmm1)
# str(bt.glmm1_simres,1)
plot(bt.glmm1_simres)
# The normality and homogeneity of variance look great.  Fifield said that this needs to be looked at for all the usual reasons: normality and homogeneity of variance.  But what about temporal and spatial independence? 


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
bt.glmm1_simres_recalc <- recalculateResiduals(bt.glmm1_simres, group = dft_BT$Year)
# plot(bt.glmm1_simres_recalc) Dave said that this is not required
testTemporalAutocorrelation(bt.glmm1_simres_recalc, time = unique(dft_BT$Year))

# resids look great: conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
bt.glmm1_simres_recalcSpace <- recalculateResiduals(bt.glmm1_simres, group = as.factor(bt.np$Station_new))
unique(bt.np$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
#str(bt.glmm1_simres_recalcSpace)

testSpatialAutocorrelation(bt.glmm1_simres_recalcSpace, x = unique(bt.np$X), y = unique(bt.np$Y))

spatialAutoCorrBase_fun(bt.np, bt.glmm1_simres_recalcSpace)   

bt.np.biomass.all <- spatialData_join(bt.np.biomass.station[-4,], 
                                      bt.glmm1_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(bt.np.biomass.all)


# Diagnostics look fantastic for this model.  Proceed with this model (bt.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.


summary(bt.glmm1)
mean_by_site(bt.np.biomass.station, "no", "b")
baci.plot(bt.np.biomass.baci, "b")



# END ----