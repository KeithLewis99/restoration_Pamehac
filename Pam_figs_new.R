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

# density (Fig X in Scruton)
df_a |>  
  group_by(Year, Species, type) |> 
  summarise(mean_abun = mean(abun.stand)) |>
  ggplot(aes(x = Year, y = mean_abun, group = type, fill = type)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap(~Species) + 
  theme_bw() + 
  ylab("Density Estimate  (#/100 sq. meters)") 
ggsave(paste0("output/all_density.png"), width=10, height=8, units="in")

# biomass (Fig X in Scruton)
df_a |>  
  group_by(Year, Species, type) |> 
  summarise(mean_bio = mean(bio.stand)) |>
  ggplot(aes(x = Year, y = mean_bio, group = type, fill = type)) + geom_col(position = position_dodge(width = 0.9)) +
  facet_wrap(~Species) + 
  theme_bw() + 
  ylab("Biomass Estimate  (grams/100 sq. meters)")
ggsave(paste0("output/all_biomass.png"), width=10, height=8, units="in")


# paper figs ----
## these are for the paper but need the random effects
## these are naive, i.e., don't take zeros into account nor log scale
### DEPRECATED
# df_a |>  
#   group_by(Year, Species, type) |> 
#   summarise(n = n(),
#             mean_abun = mean(abun.stand),
#             se_abun = sd(abun.stand)/n) |>
#   ggplot(aes(x = as.factor(Year), y = mean_abun, group = type, colour = type)) + 
#   geom_point(position = position_dodge(width = 0.5)) +
#   facet_wrap(~Species) + 
#   theme_bw() + 
#   ylab("Density Estimate  (#/100 sq. meters)") +
#   xlab("Year") +
#   geom_errorbar(aes(ymax=mean_abun+se_abun, ymin=mean_abun-se_abun), linewidth=1, width=0.25, position=position_dodge(0.5)) +
#   geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
#   geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
#   geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
#   theme(legend.title=element_blank()) +
#   theme(legend.position=c(.2, .88))


# this is just to see that a raw calculation makes sense with fitted values below
View(df_a |>  
       filter(Species == "BT") |>
       group_by(Year,type) |> 
       summarise(n = n(),
                 mean_abun = mean(abun.stand),
                 se_abun = sd(abun.stand)/n))

# create a df with variables, data, and predicted values
tmp <- cbind(df_aBT[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(bt.glmm2, se.fit = T))
)

# this plots converts the fitted value and se to CIs taking zeros and the link into account.  CI's are not symetrical and don't overlap zero!
ggplot(tmp, aes(x = as.factor(Year), y = exp(fit), group = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #facet_wrap(~Species) + 
  theme_bw() + 
  ylab("Density Estimate  (#/100 sq. meters)") +
  xlab("Year") +
  geom_errorbar(aes(ymax = exp(fit+se.fit*1.96), ymin = exp(fit-se.fit*1.96)), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  theme(legend.title=element_blank()) +
  #scale_fill_manual(values=c("#black","white")) +
  theme(legend.position=c(.2, .88))
ggsave(paste0("output/all_density.png"), width=10, height=8, units="in")


ggplot(tmp, aes(x = as.factor(Year), y = exp(fit), fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #facet_wrap(~Species) + 
  theme_bw() + 
  ylab("Density Estimate  (#/100 sq. meters)") +
  xlab("Year") +
  geom_errorbar(aes(ymax = exp(fit+se.fit*1.96), ymin = exp(fit-se.fit*1.96)), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  theme(legend.title=element_blank()) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) + 
  theme(legend.position=c(.85, .88))
