# source ----
# source("Pam_abun_bio.R")
source("Pam_data_new.R")
source("Pam_fun.R")


# See Seal Cove code for original work and thoughts on this approach
# See ReadMe for thoughts on Pamehac. 

# library ----
library(ggplot2)
library(cowplot)

# Scruton figs ----
# use this to compare to Scruton et al. 1998
## but I think Kristin's figures are better - use them

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

## above-below ----
# create a df with variables, data, and predicted values
btd <- cbind(df_aBT[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(bt.glmm2, se.fit = T))
)
btb <- cbind(df_aBT[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(bt_bio.glmm2, se.fit = T))
)
btyd <- cbind(df_aBTYOY[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(btyoy.glmm2, se.fit = T))
)
btyb <- cbind(df_aBTYOY[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(btyoy_bio.glmm2, se.fit = T))
)
asd <- cbind(df_aAS[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(as.glmm1, se.fit = T))
)
asb <- cbind(df_aAS[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(as_bio.glmm2, se.fit = T))
)
asyd <- cbind(df_aASYOY[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(asyoy.glmm2, se.fit = T))
)
asyb <- cbind(df_aASYOY[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(asyoy_bio.glmm2, se.fit = T))
)


## create plots ----
# this plots converts the fitted value and se to CIs taking zeros and the link into account.  CI's are not symetrical and don't overlap zero!
source("Pam_fun.R")
p1 <- above_below_year(btd, "d")
p2 <- above_below_year(btyd, "d")
p3 <- above_below_year(asd, "d")
p4 <- above_below_year(asyd, "d")

p5 <- above_below_year(btd, "b")
p6 <- above_below_year(btyd, "b")
p7 <- above_below_year(asd, "b")
p8 <- above_below_year(asyd, "b")

plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), nrow = 2)
plot_grid(p5, p6, p7, p8, nrow = 2)

