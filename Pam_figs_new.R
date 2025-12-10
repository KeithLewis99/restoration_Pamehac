# source ----
# source("Pam_abun_bio.R")
source("Pam_data_new.R")
source("Pam_fun.R")


# See Seal Cove code for original work and thoughts on this approach
# See ReadMe for thoughts on Pamehac. 

# library ----
library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)

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

p1 <- above_below_year(btd, "d", "n")
p2 <- above_below_year(btyd, "n", "y")
p3 <- above_below_year(asd, "d", "n")
p4 <- above_below_year(asyd, "n", "n")

p5 <- above_below_year(btb, "b", "n")
p6 <- above_below_year(btyb, "n", "y")
p7 <- above_below_year(asb, "b", "n")
p8 <- above_below_year(asyb, "n", "n")

plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), nrow = 2)
ggsave("output/all_density_new.png", width=10, height=8, units="in")

plot_grid(p5, p6, p7, p8, labels = c('A', 'B', 'C', 'D'), nrow = 2)
ggsave("output/all_biomass_new.png", width=10, height=8, units="in")

# salmonids - Naive ----
# note that these are NOT based on modeloutputs
#https://dataanalytics.org.uk/axis-labels-in-r-plots-using-expression/#sub_sup
## density ----
df_a |>  
  group_by(Year,type) |> 
  summarise(n = n(),
            mean_abun = mean(abun.stand),
            se_abun = sd(abun.stand)/n) |>
ggplot(aes(x = as.factor(Year), y = mean_abun, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
  ylab(expression("Density Estimate (#/100 m" ^2*")")) +
  xlab("Year") +
  geom_errorbar(aes(ymax = mean_abun+se_abun*1.96, ymin = mean_abun-se_abun*1.96), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  #theme(legend.title=element_blank()) +
  #theme(legend.position=c(.85, .88)) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) 
ggsave("output/salmonid_density_new.png", width=10, height=8, units="in")


## biomass ----
df_a |>  
  group_by(Year,type) |> 
  summarise(n = n(),
            mean_bio = mean(bio.stand),
            se_bio = sd(bio.stand)/n) |>
  ggplot(aes(x = as.factor(Year), y = mean_bio, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
#  ylab("Biomass Estimate (g/100 sq. m)") +
  ylab(expression("Biomass Estimate (g/m" ^2*")")) + 
  xlab("Year") +
  geom_errorbar(aes(ymax = mean_bio+se_bio*1.96, ymin = mean_bio-se_bio*1.96), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  #theme(legend.title=element_blank()) +
  #theme(legend.position=c(.85, .88)) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) 
ggsave("output/salmonid_biomass_new.png", width=10, height=8, units="in")


# by species - Naive ----
df_a |>  
  group_by(Year,Species, type) |> 
  summarise(n = n(),
            mean_abun = mean(abun.stand),
            se_abun = sd(abun.stand)/n) |>
  ggplot(aes(x = as.factor(Year), y = mean_abun, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
  ylab(expression("Density Estimate (#/100 m" ^2*")")) +
  xlab("Year") +
  geom_errorbar(aes(ymax = mean_abun+se_abun*1.96, ymin = mean_abun-se_abun*1.96), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  #theme(legend.title=element_blank()) +
  #theme(legend.position=c(.85, .88)) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) 
ggsave("output/salmonid_density_new.png", width=10, height=8, units="in")

# bootstrap ----

## density ----
den_ci <- read.csv("data_derived/density_ci.csv")

ggplot(den_ci, aes(x = as.factor(Year), y = mean, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
  ylab(expression("Density Estimate (#/100 m" ^2*")")) +
  xlab("Year") +
  geom_errorbar(aes(ymax = ul, ymin = ll), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
scale_fill_discrete(name="",
                    breaks=c("above", "below"),
                    labels=c("Above", "Below")) +
scale_colour_manual(values=c("black", "dark grey"),
                    name="",
                    breaks=c("above", "below"),
                    labels=c("Above", "Below")) 
ggsave("output/all_density_boot.png", width=10, height=8, units="in")

## biomass ----
bio_ci <- read.csv("data_derived/biomass_ci.csv")

ggplot(bio_ci, aes(x = as.factor(Year), y = mean, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
  ylab(expression("Biomass Estimate (g/m" ^2*")")) + 
  xlab("Year") +
  geom_errorbar(aes(ymax = ul, ymin = ll), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) 
ggsave("output/all_biomass_boot.png", width=10, height=8, units="in")


# boostrap - purrr ----
## density ----
# this was derived in GC_tables
den_ci <- read.csv("data_derived/density_ci.csv")

### in purrr ----
# split data set
den_ci_split <- den_ci |> 
  filter(Year != "Total") |>
  split(den_ci$Species)
str(den_ci_split)


plot_den <- map(names(den_ci_split), function(Species) {
  df <- den_ci_split[[Species]]  
  if(Species == "ASYOY"){
   tmp <- df[1,]
   tmp[, c(3, 5:9)] <- NA
    tmp$Year <- 1992 
    df <- rbind(df, tmp)
  }
    legend_ASYOY <- if(any(df$Species == "ASYOY"))theme(legend.position=c(0.45, 0.88),
        legend.background = element_rect(fill = "transparent", color = NA), legend.title=element_blank(),
        legend.key.size = unit(0.4, "cm"))
    legend_notASYOY <- if(any(df$Species != "ASYOY"))
        theme(legend.position= "none")
  ggplot(df, 
         aes(x = as.factor(Year), y = mean, fill = type, colour = type, shape = type)) + 
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    #facet_wrap(~Species, scales = "free_y") + 
    theme_bw(base_size = 20) + 
    ylab(expression("Density Estimate (#/100 m" ^2*")")) +
    #ylab("")+
    xlab("Year") +
    #xlab("") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_errorbar(aes(ymax = ll, 
                      ymin = ul), 
                  linewidth=1, width=0.15, position=position_dodge(0.5)) +
    # if(any(Species == "ASYOY")){
    #   theme(legend.position=c(0.25, 0.88),
    #         legend.background = element_rect(fill = "transparent", color = NA),
    #         legend.key.size = unit(0.4, "cm"))
    # } 
  #else {
  #     theme(legend.position= "none")
  #     } +
    legend_notASYOY +
    legend_ASYOY +
    geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
    geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
    geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
    scale_fill_discrete(name="",
                        breaks=c("above", "below"),
                        labels=c("Above", "Below")) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c("above", "below"),
                        labels=c("Above", "Below")) +
    scale_shape_manual(values = c(16, 16),
                       name="",
                       breaks=c("above", "below"),
                       labels=c("Above", "Below"))
})

names(plot_den) <- paste0(names(den_ci_split))
list2env(plot_den, envir = .GlobalEnv)
p1 <- plot_den$AS
p2 <- plot_den$BT
p3 <- plot_den$BTYOY
p4 <- plot_den$ASYOY
ggsave("figs/speciesBTY_density_ci.png", width=10, height=8, units="in")

### combine ----
p1_clean <- p1 + theme(axis.title = element_blank())
p2_clean <- p2 + theme(axis.title = element_blank())
p3_clean <- p3 + theme(axis.title = element_blank())
p4_clean <- p4 + theme(axis.title = element_blank())

grid_den <- plot_grid(p1_clean, 
                      p4_clean,
                      p2_clean, 
                      p3_clean, 
                      ncol = 2, align = "hv", axis = "tblr",
                      scale = 0.9,
                      labels = c("AS", "ASY","BT", "BTY"),
                      hjust = -3, 
                      vjust = 1.25)

# Add shared axis labels
final_plot_den <- ggdraw(grid_den) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(expression("Density Estimate (#/100 m" ^2*")"), x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14)
final_plot_den

save_plot("figs/species_den_ci.png", 
          final_plot_den, 
          base_height = 6, 
          base_width = 10,
          bg = "white")


## biomass ----
bio_ci <- read.csv("data_derived/biomass_ci.csv")


### in purrr ----
# split data set
bio_ci_split <- bio_ci |> 
  filter(Year != "Total") |>
  split(bio_ci$Species)
str(bio_ci_split)


plot_bio <- map(names(bio_ci_split), function(Species) {
  df <- bio_ci_split[[Species]]  
  if(Species == "ASYOY"){
    tmp <- df[1,]
    tmp[, c(3, 5:9)] <- NA
    tmp$Year <- 1992 
    df <- rbind(df, tmp)
  }
  legend_ASYOY <- if(any(df$Species == "ASYOY"))theme(legend.position=c(0.45, 0.88),
                                                      legend.background = element_rect(fill = "transparent", color = NA), legend.title=element_blank(),
                                                      legend.key.size = unit(0.4, "cm"))
  legend_notASYOY <- if(any(df$Species != "ASYOY"))
    theme(legend.position= "none")
  ggplot(df, 
         aes(x = as.factor(Year), y = mean, fill = type, colour = type, shape = type)) + 
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    theme_bw(base_size = 20) + 
    ylab(expression("Biomass Estimate (g/100 m" ^2*")")) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_errorbar(aes(ymax = ll, 
                      ymin = ul), 
                  linewidth=1, width=0.15, position=position_dodge(0.5)) +
    # theme(legend.position= "none") +
    legend_notASYOY +
    legend_ASYOY +
    geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
    geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
    geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
    scale_fill_discrete(name="",
                        breaks=c("above", "below"),
                        labels=c("Above", "Below")) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c("above", "below"),
                        labels=c("Above", "Below")) +
    scale_shape_manual(values = c(16, 16),
                       name="",
                       breaks=c("above", "below"),
                       labels=c("Above", "Below"))
})

names(plot_bio) <- paste0(names(bio_ci_split))
list2env(plot_bio, envir = .GlobalEnv)
p5 <- plot_bio$AS
p6 <- plot_bio$BT
p7 <- plot_bio$BTYOY
p8 <- plot_bio$ASYOY

ggsave("figs/speciesBTY_bio_ci.png", width=10, height=8, units="in")


### combine ----
p5_clean <- p5 + theme(axis.title = element_blank())
p6_clean <- p6 + theme(axis.title = element_blank())
p7_clean <- p7 + theme(axis.title = element_blank())
p8_clean <- p8 + theme(axis.title = element_blank())

grid_plot <- plot_grid(p5_clean, 
                       p8_clean,
                       p6_clean, 
                       p7_clean, 
                       ncol = 2, 
                       align = "hv", 
                       axis = "tblr",
                       scale = 0.9,
                       labels = c("AS", "ASY","BT", "BTY"),
                       hjust = -3, 
                       vjust = 1.25)

# Add shared axis labels
final_plot_bio <- ggdraw(grid_plot) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(expression("Biomass Estimate (g/100 m" ^2*")"), x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14)

final_plot_bio
save_plot("figs/species_bio_ci.png", 
          final_plot_bio, 
          base_height = 6, 
          base_width = 10,
          bg = "white")



# by years ----
## these are averags of the total density and biomass
## density ----
years_den_tot.ci <- read.csv("data_derived/density_all_years.csv")

p11 <- ggplot(years_den_tot.ci, aes(x = as.factor(Year), y = mean, 
                                    fill = (type), 
                                    colour = (type), 
                                    shape = (type) 
)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #facet_wrap(~Species, scales = "free_y") + 
  theme_bw(base_size = 20) + 
  ylab(expression("Density Estimate (#/100 m" ^2*")")) +
  #ylab("") +
  xlab("Year") +
  #xlab("") +
  theme(legend.position= "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_errorbar(aes(ymax = ll, 
                    ymin = ul), 
                linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  #theme(legend.position=c(.85, .88)) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_shape_manual(values = c(16, 16),
                     name="",
                     breaks=c("above", "below"),
                     labels=c("Above", "Below"))

p11
ggsave("figs/salmonids_density_ci.png", width=10, height=8, units="in")

## biomass ----
years_bio_tot.ci <- read.csv("data_derived/biomass_all_years.csv")

p12 <- ggplot(years_bio_tot.ci, aes(x = as.factor(Year), y = mean, 
                                    fill = (type), 
                                    colour = (type), 
                                    shape = (type) 
)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #facet_wrap(~Species, scales = "free_y") + 
  theme_bw(base_size = 20) + 
  ylab(expression("Biomass Estimate (g/100 m" ^2*")")) +
  #ylab("") +
  xlab("Year") +
  #xlab("") +
  theme(legend.position= "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_errorbar(aes(ymax = ll, 
                    ymin = ul), 
                linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  #theme(legend.position=c(.85, .88)) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_shape_manual(values = c(16, 16),
                     name="",
                     breaks=c("above", "below"),
                     labels=c("Above", "Below"))
p12
ggsave("figs/salmonids_biomass_ci.png", width=10, height=8, units="in")


# total biomass ----
source("Pam_data_new-v2.R")
df_bio <- df_a |>
  filter(Year != 1992 | Species != "ASYOY") |>
  group_by(Year, type, Species) |>
  summarise(bio.sum = sum(bio), bio.stand.sum = sum(bio.stand)) |>
  print(n = Inf)
df_bio |> filter(Year == 1992)

p0 <- ggplot(df_bio, aes(x = Year, y = bio.sum, group = Species, colour = Species)) +
  geom_line() +
  geom_point() +
  facet_wrap(~type) + 
  ylab("Biomass (g)") +
  theme_bw()

df_a$area
length(df_a$area)
df_bio_tot <- df_a |>
  filter(Year != 1992 | Species != "ASYOY") |>
  group_by(Year, type) |>
  summarise(bio.tot.sum = sum(bio), bio.tot.stand.sum = sum(bio.stand), area.sum = sum(area)) |>
  print(n = Inf)

# get divisor for area
df_temp <- df_a |>
  filter(Year != 1992 | Species != "ASYOY") |>
  group_by(Year, type) |>
  summarise(count = length(unique(Species)))

df_bio_tot <- left_join(df_bio_tot, df_temp, by = c("Year", "type"))
df_bio_tot <- df_bio_tot |> 
  group_by(Year, type) |>
  mutate(area.sum.scaled = area.sum/count)

ggplot(df_bio_tot, aes(x = Year, y = bio.tot.sum)) +
  geom_line() +
  geom_point() +
  facet_wrap(~type) +
  theme_bw() + 
  ylab("Biomass (g)")


# area
df_bio_tot$df_bio_stand <- df_bio_tot$bio.tot.sum/df_bio_tot$area.sum.scaled*100

# units
df_bio_tot$units <- NA
df_bio_tot$units <- ifelse(df_bio_tot$type == "above", 175, 542+449)
df_bio_tot$units[2] <- 542


p1 <- ggplot(df_bio_tot, aes(x = Year, y = bio.tot.sum,, group = type, colour = type)) +
  geom_line() +
  geom_point() +
#  facet_wrap(~type) +
  theme_bw() + 
  ylab("Biomass (g)")

p2 <- ggplot(df_bio_tot, aes(x = Year, y = df_bio_stand, group = type, colour = type)) +
  geom_line() +
  geom_point() +
#  facet_wrap(~type) +
  theme_bw() + 
  ylab("Biomass (g)/unit")

# 1.322683 fold difference in the biomass/unit
sum(df_bio_tot$df_bio_stand[9:10])/sum(df_bio_tot$df_bio_stand[1:2])
# how is biomass calculated
# df_bio_tot$bio.tot.stand.sum1 <- df_bio_tot$bio.tot.sum/df_bio_tot$units
# df_bio_tot$bio.tot.stand.sum2 <- df_bio_tot$bio.tot.stand.sum*100/df_bio_tot$units
# df_bio_tot$bio.tot.stand.sum3 <- df_bio_tot$df_bio_stand/df_bio_tot$units

# calculate kg
df_bio_tot$kg <- df_bio_tot$df_bio_stand*0.001*df_bio_tot$units
# df_bio_tot$kg_1 <- df_bio_tot$bio.tot.stand.sum*0.001*df_bio_tot$units
  
p3 <- ggplot(df_bio_tot, aes(x = Year, y = kg, group = type, colour = type)) +
  geom_line() +
  geom_point() +
  ylab("Biomass (kg)") + 
#  facet_wrap(~type) + 
  theme_bw()

# 2.865929 fold difference in kg
sum(df_bio_tot$kg[9:10])/sum(df_bio_tot$kg[1:2])

p4 <- cowplot::plot_grid(p1, p2, p3, labels = c('B', 'C', 'D'), nrow = 1)

p5 <- cowplot::plot_grid(p0, p4, labels = c('A'), nrow = 2)

ggsave(paste0("output/replacement.png"), width=10, height=8, units="in")

# END ----