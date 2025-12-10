# This was supposed to JUST create a table for the parameter estimates much like what I did in Seal Cove.  However, after the decision to just compare GC to other sites, I added tables that summarize the density/biomass by species per year. Also, bootstrap approach to get CIs around density and biomass estimates that don't overlap zero.

## using kable() and kableExtra() - see website: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Grouped_Columns__Rows


## below but here is an alternative:
###https://glin.github.io/reactable/articles/examples.html#column-formatting

### Density

# load files ----
# create pattern
temp1 = list.files(path = "output", pattern=".*_ci.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
  name = gsub(".*output/", "", x)
  name = gsub("_ci.csv", "", paste0(name, "_ci"))
  return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_sc_ci1 = (lapply(temp1, read.csv))
names(ls_sc_ci1) <- name_files(temp1)
list2env(ls_sc_ci1, envir = .GlobalEnv)

bt_den_ci$species <- "BT"
btyoy_den_ci$species <- "BTYOY"
as_den_ci$species <- "AS"
asyoy_den_ci$species <- "ASYOY"

tab_den <- rbind(bt_den_ci[1:4,-1],
                 btyoy_den_ci[1:4,-1],
                 as_den_ci[1:7, -1],
                 asyoy_den_ci[1:5, -1])

bt_bio_ci$species <- "BT"
btyoy_bio_ci$species <- "BTYOY"
as_bio_ci$species <- "AS"
asyoy_bio_ci$species <- "ASYOY"

tab_bio <- rbind(bt_bio_ci[1:4,-1],
                 btyoy_bio_ci[1:4,-1],
                 as_bio_ci[1:7, -1],
                 asyoy_bio_ci[1:5, -1])

tab <- cbind(tab_den, tab_bio[, 2:4])
tab$parm[tab$parm == "cond.(Intercept)"] <- "Int-Above"
tab$parm[tab$parm == "cond.timebefore"] <- "Time-Before"
tab$parm[tab$parm == "cond.typebelow"] <- "Type-Below"
tab$parm[tab$parm == "cond.typebelow:timebefore"] <- "Interaction"


library(kableExtra)
kbl(tab[, c(5, 1, 4, 2:3, 8, 6:7)], 
    col.names = c('spp', 'parm', 'Estimate', '2.5%', '97.5%',
                  'Estimate', '2.5%', '97.5%'),
    align = 'c', caption = "Density and Biomass CIs", digits = 3 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Density" = 3, "Biomass" = 3)) |>
  add_header_above(header = c(" " = 2, "Summer" = 6)) |>
  kable_paper()


tabC <- tab[, c(5, 1, 4, 2:3, 8, 6:7)]
tabC$ci_den <- paste0(round(tabC$Estimate, 2), 
                     " (", 
                     round(tabC$X2.5., 2), 
                     ", ",
                     round(tabC$X97.5., 2), 
                     ")")
tabC$ci_bio <- paste0(round(tabC$Estimate.1, 2), 
                     " (", 
                     round(tabC$X2.5..1, 2), 
                     ", ",
                     round(tabC$X97.5..1, 2), 
                     ")")

kbl(tabC[, c(1:2, 9,10)], 
    col.names = c('Species-age', 
                  'Parameter', 
                  'Density',
                  'Biomass'),
    align = 'c', caption = "Density and Biomass CIs", digits = 3 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, 
                              "Density" = 1, 
                              "Biomass" = 1)) |>
  #add_header_above(header = c(" " = 2, "Summer" = 6)) |>
  kable_paper()

write.csv(tabC, "output/params_all.csv")


# # density - raw values ----
# ## this is a summary of the values used in GC_analyses.R to create table
library(dplyr)
library(tidyr)
library(ggplot2)
# df_b <- read.csv("data_derived/df_b.csv")
# bt_tab <- df_b |>
#   filter(Species == "BT" & Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, Year, trt) |>
#   summarize(count = n(),
#             mean = mean(T.stand),
#             se = sd(T.stand)/sqrt(count))
# 
# bty_tab <- df_b |>
#   filter(Species == "BTYOY" & Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, Year, trt) |>
#   summarize(count = n(),
#             mean = mean(T.stand),
#             se = sd(T.stand)/sqrt(count))
# 
# as_tab <- df_b |>
#   filter(Species == "AS" & Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, Year, trt) |>
#   summarize(count = n(),
#             mean = mean(T.stand),
#             se = sd(T.stand)/sqrt(count))
# 
# #  alter because there was a significant difference in type
# asy_tab <- df_b |>
#   filter(Species == "ASYOY" & Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, Year, trt, type) |>
#   summarize(count = n(),
#             mean = mean(T.stand),
#             se = sd(T.stand)/sqrt(count))
# 
# spec_summ <- rbind(bt_tab, bty_tab, as_tab)
# spec_summ$type <- NA
# spec_summ <- spec_summ[, c(1:3, 7, 5, 6)]
# 
# spec_summ <- rbind(spec_summ, asy_tab[, -5])
# 
# 
# ## totals ----
# tot_tab <- df_b |>
#   filter(Species != "ASYOY", Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, trt) |>
#   summarize(count = n(),
#             mean = mean(T.stand),
#             se = sd(T.stand)/sqrt(count))
# tot_tab$type <- NA
# tot_tab <- tot_tab[, c(1:2, 6, 4, 5)]
# 
# totASY_tab <- df_b |>
#   filter(Species == "ASYOY", Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, trt, type) |>
#   summarize(count = n(),
#             mean = mean(T.stand),
#             se = sd(T.stand)/sqrt(count))
# tot_all_tab <- rbind(tot_tab, totASY_tab[, -4])
# 
# 
# tmp <- pivot_wider(spec_summ,
#                    id_cols = c(Species, trt, type),
#                    names_from = c(Year),
#                    values_from = c(mean, se)
# )
# str(tmp)
# tmp <- tmp[, c(1:3, 4, 9, 5, 10, 6, 11, 7, 12, 8, 13)]
# 
# tab_den <- left_join(tmp, tot_all_tab, by = c("Species", "trt", "type"))
# tab_den$Species <- factor(tab_den$Species, levels = c("BT", "BTYOY", "AS", "ASYOY"))
# tab_den <- tab_den[order(tab_den$Species),]
# 
# 
# write.csv(tab_den, "data_derived/density_table.csv")
# 
# df_den <- rbind(spec_summ, tot_all_tab)
# ggplot(df_den, aes(x = Year, y = mean)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   facet_grid(Species ~ trt, scales = "free_y")
# 
# 
# 
# # biomass - raw values ----
# ## this is a summary of the values used in GC_analyses.R to create table
# 
# spp_bio_tab <- df_b |>
#   filter(Species != "ASYOY" & Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, Year, trt) |>
#   summarize(count = n(),
#             mean = mean(B.stand),
#             se = sd(B.stand)/sqrt(count))
# 
# as_bio_tab <- df_b |>
#   filter(Species == "ASYOY" & Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, Year, trt, type) |>
#   summarize(count = n(),
#             mean = mean(B.stand),
#             se = sd(B.stand)/sqrt(count))
# 
# 
# 
# spp_bio_tab$type <- NA
# spp_bio_tab <- spp_bio_tab[, c(1:3, 7, 5, 6)]
# 
# spec_bio_summ <- rbind(spp_bio_tab, as_bio_tab[, -5])
# 
# 
# # totals ----
# tot_bio_tab <- df_b |>
#   filter(Species != "ASYOY", Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, trt) |>
#   summarize(count = n(),
#             mean = mean(B.stand),
#             se = sd(B.stand)/sqrt(count))
# tot_bio_tab$type <- NA
# tot_bio_tab <- tot_bio_tab[, c(1:2, 6, 4, 5)]
# 
# totASY_bio_tab <- df_b |>
#   filter(Species == "ASYOY", Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Species, trt, type) |>
#   summarize(count = n(),
#             mean = mean(B.stand),
#             se = sd(B.stand)/sqrt(count))
# 
# tot_all_bio_tab <- rbind(tot_bio_tab, totASY_bio_tab[, -4])
# 
# 
# tmp_bio <- pivot_wider(spec_bio_summ,
#                        id_cols = c(Species, trt, type),
#                        names_from = c(Year),
#                        values_from = c(mean, se)
# )
# str(tmp_bio)
# tmp_bio <- tmp_bio[, c(1:3, 4, 9, 5, 10, 6, 11, 7, 12, 8, 13)]
# 
# tab_bio <- left_join(tmp_bio, tot_all_bio_tab, by = c("Species", "trt", "type"))
# tab_bio$Species <- factor(tab_bio$Species, levels = c("BT", "BTYOY", "AS", "ASYOY"))
# tab_bio <- tab_bio[order(tab_bio$Species),]
# 
# write.csv(tab_bio, "data_derived/biomass_table.csv")
# 
# df_bio <- rbind(spec_bio_summ, tot_all_bio_tab)
# ggplot(df_bio, aes(x = Year, y = mean)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   ylab("Biomass") +
#   facet_grid(Species ~ trt, scales = "free_y")
# 
# # make table ----
# tab_den
# tab_bio
# library(kableExtra)
# 
# tab_all <- left_join(tab_den, tab_bio, by = c("Species", "trt", "type"))
# str(tab_all, give.attr = F)
# str(tab_den, give.attr = F)

# kbl(tab_den,
#     col.names = c('spp', 'trt', 'type',
#                   'mean', 'se',
#                   'mean', 'se',
#                   'mean', 'se',
#                   'mean', 'se',
#                   'mean', 'se',
#                   'total', 'se'),
#     align = 'c', caption = "Density", digits = 2 ) |>
#   collapse_rows(valign = "top",
#                 latex_hline = "major") |>
#   add_header_above(header = c(" " = 3, "2004" = 2, "2005" = 2, "2006" = 2, "2009" = 2, "2016" = 2, "total" = 2)) |>
#   add_header_above(header = c(" " = 3, "Density" = 12)) |>
#   kable_paper()
# 
# 
# 
# 
# kbl(tab_bio,
#     col.names = c('spp', 'trt', 'type',
#                   'mean', 'se',
#                   'mean', 'se',
#                   'mean', 'se',
#                   'mean', 'se',
#                   'mean', 'se',
#                   'total', 'se'),
#     align = 'c', caption = "Density", digits = 2 ) |>
#   collapse_rows(valign = "top",
#                 latex_hline = "major") |>
#   add_header_above(header = c(" " = 3, "2004" = 2, "2005" = 2, "2006" = 2, "2009" = 2, "2016" = 2, "total" = 2)) |>
#   add_header_above(header = c(" " = 3, "Biomass" = 12)) |>
#   kable_paper()
# 
# # 

# Bootstrap ----
## density - CI ----

# Hmisc - ben bolker approach
#https://stackoverflow.com/questions/38554383/bootstrapped-confidence-intervals-with-dplyr
library(Hmisc)


spp_den.ci <- df_a |> 
  filter(!(Year == 1992 & Species == "ASYOY")) |>
  group_by(Species, Year, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
# this works.  The means are the same as if calculated in dplyr and the CI's make sense relative to the means.

spp_den.ci$ci <- paste0("(", round(spp_den.ci$ll, 1), ", ", round(spp_den.ci$ul, 1), ")")
spp_den.ci


### total density ----
spp_den_tot.ci <- df_a |>
  group_by(Species, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
# this works.  The means are the same as if calculated in dplyr and the CI's make sense relative to the means.

spp_den_tot.ci$ci <- paste0("(", round(spp_den_tot.ci$ll, 1), ", ", round(spp_den_tot.ci$ul, 1), ")")
spp_den_tot.ci



### all density ----
all_den_ci <- rbind(spp_den.ci, spp_den_tot.ci)
all_den_ci$Year <- ifelse(is.na(all_den_ci$Year), "Total", all_den_ci$Year)
all_den_ci$mci <- paste(round(all_den_ci$mean, 2), "",  all_den_ci$ci)

write.csv(all_den_ci, "data_derived/density_ci.csv")

# all_den_ci_tab <- pivot_wider(all_den_ci,
#                               id_cols = c(Species, type),
#                               names_from = c(Year),
#                               values_from = c(mean, ci)
# )
# 
# 
# #View(all_den_ci_tab)
# all_den_ci_tab <- all_den_ci_tab[, c(1:2, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 8, 14)]
# #all_den_ci_tab <- all_den_ci_tab |>
# # rename(mean_tot = mean_NA, ci_tot = ci_NA)
# 
# all_den_ci_tab$Species <- factor(all_den_ci_tab$Species, levels = c("BT", "BTYOY", "AS", "ASYOY"))
# all_den_ci_tab <- all_den_ci_tab[order(all_den_ci_tab$Species),]
# str(all_den_ci_tab, give.attr = F)
# write.csv(all_den_ci_tab, "data_derived/density_ci_tab.csv")
# 
# # make table
# library(kableExtra)
# kbl(all_den_ci_tab,
#     col.names = c('spp', 'type',
#                   'mean 1990', 'ci 1990',
#                   'mean 1991', 'ci 1991',
#                   'mean 1992', 'ci 1992',
#                   'mean 1996', 'ci 1996',
#                   'mean 2016', 'ci 2016',
#                   'mean total', 'ci total'),
#     align = 'c', caption = "Density CIs", digits = 2 ) |>
#   collapse_rows(valign = "top",
#                 latex_hline = "major") |>
#   add_header_above(header = c(" " = 2, "1990" = 2, "1991" = 2, "1992" = 2, "1996" = 2, "2016" = 2, "total" = 2)) |>
#   add_header_above(header = c(" " = 2, "Density" = 12)) |>
#   kable_paper()

# combine mean and ci
all_den_ci_tabC <- pivot_wider(all_den_ci,
                               id_cols = c(Species, type),
                               names_from = c(Year),
                               values_from = c(mci)
)

all_den_ci_tabC$Species <- factor(all_den_ci_tabC$Species, levels = c("BT", "BTYOY", "AS", "ASYOY"))
all_den_ci_tabC <- all_den_ci_tabC[order(all_den_ci_tabC$Species),]
str(all_den_ci_tabC, give.attr = F)
write.csv(all_den_ci_tabC, "data_derived/density_ci_tabC.csv")

# make table
library(kableExtra)
kbl(all_den_ci_tabC,
    col.names = c('spp', 'type',
                  '1990',
                  '1991',
                  '1992',
                  '1996',
                  '2016',
                  'total'),
    align = 'c', caption = "Density CIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  #add_header_above(header = c(" " = 3, "2004" = 1, "2005" = 1, "2007" = 1, "2009" = 1, "2016" = 1, "total" = 1)) |>
  add_header_above(header = c(" " = 2, "Year" = 6)) |>
  kable_paper()


### density by year -----
### I COPIED THIS FROM GRANITE CANAL AND THINK THAT I MODIFIED THE BELOW BUT FORGOT TO DELETE IT
#### I'm not sure if this is really needed, i.e., this does not match Kristin's figures which are for the whole year; but there is a reason that you can't match.  I could so a sum of all fish or biomass in a year but then that is what I would be stuck with - a number.  This is not awful but there would be no confidence intervals around it.  Also, i'm not sure what the point of it is.  A yearly trend in fish and biomass.  Largely driven by ASY.
# year_den_tot.ci <- df_b |>
#   filter(Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Year, trt, type) |>
#   #need to get a total here
#   do(data.frame(rbind(Hmisc::smean.cl.boot(.$T.stand)))) |>
#   rename(mean = Mean, ll = Lower, ul = Upper)
# 
# year_den_tot.ci$ci <- paste0("(", round(year_den_tot.ci$ll, 1), ", ", round(year_den_tot.ci$ul, 1), ")")
# year_den_tot.ci
# 
# write.csv(year_den_tot.ci, "data_derived/density_all_year.csv")




## biomass - CI ----
spp_bio.ci <- df_a |>
  filter(!(Year == 1992 & Species == "ASYOY")) |>
  group_by(Species, Year, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)

spp_bio.ci$ci <- paste0("(", round(spp_bio.ci$ll, 1), ", ", round(spp_bio.ci$ul, 1), ")")
spp_bio.ci
# spp_bio.ci$mci <- paste(round(spp_bio.ci$mean, 2), "",  spp_bio.ci$ci)


### total biomass ----
spp_bio_tot.ci <- df_a |>
  group_by(Species, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)

spp_bio_tot.ci$ci <- paste0("(", round(spp_bio_tot.ci$ll, 1), ", ", round(spp_bio_tot.ci$ul, 1), ")")
spp_bio_tot.ci
# spp_bio_tot.ci$mci <- paste(round(spp_bio_tot.ci$mean, 2), "",  spp_bio_tot.ci$ci)



### all biomass ----
all_bio_ci <- rbind(spp_bio.ci, spp_bio_tot.ci)
all_bio_ci$Year <- ifelse(is.na(all_bio_ci$Year), "Total", all_bio_ci$Year)
all_bio_ci$mci <- paste(round(all_bio_ci$mean, 2), "",  all_bio_ci$ci)
write.csv(all_bio_ci, "data_derived/biomass_ci.csv")

# all_bio_ci_tab <- pivot_wider(all_bio_ci,
#                               id_cols = c(Species, trt, type),
#                               names_from = c(Year),
#                               values_from = c(mean, ci)
# )
# #View(all_den_ci_tab)
# all_bio_ci_tab <- all_bio_ci_tab[, c(1:3, 4, 10, 5, 11, 6, 12, 7, 13, 8, 14, 9, 15)]
# #all_bio_ci_tab <- all_bio_ci_tab |>
# # rename(mean_tot = mean_NA, ci_tot = ci_NA)
# str(all_bio_ci_tab, give.attr = F)
# all_bio_ci_tab$Species <- factor(all_bio_ci_tab$Species, levels = c("BT", "BTYOY", "AS", "ASYOY"))
# all_bio_ci_tab <- all_bio_ci_tab[order(all_bio_ci_tab$Species),]
# 
# write.csv(all_bio_ci_tab, "data_derived/biomass_ci_tab.csv")
# 
# # make table
# kbl(all_bio_ci_tab,
#     col.names = c('spp', 'trt', 'type',
#                   'mean 2004', 'ci 2004',
#                   'mean 2005', 'ci 2005',
#                   'mean 2007', 'ci 2007',
#                   'mean 2009', 'ci 2009',
#                   'mean 2016', 'ci 2016',
#                   'mean total', 'ci total'),
#     align = 'c', caption = "Biomass CIs", digits = 2 ) |>
#   collapse_rows(valign = "top",
#                 latex_hline = "major") |>
#   add_header_above(header = c(" " = 3, "2004" = 2, "2005" = 2, "2007" = 2, "2009" = 2, "2016" = 2, "total" = 2)) |>
#   add_header_above(header = c(" " = 3, "Biomass" = 12)) |>
#   kable_paper()


# combine mean and ci
all_bio_ci_tabC <- pivot_wider(all_bio_ci,
                               id_cols = c(Species,  type),
                               names_from = c(Year),
                               values_from = c(mci)
)
#View(all_den_ci_tab)
all_bio_ci_tabC$Species <- factor(all_bio_ci_tabC$Species, levels = c("BT", "BTYOY", "AS", "ASYOY"))
all_bio_ci_tabC <- all_bio_ci_tabC[order(all_bio_ci_tabC$Species),]

write.csv(all_bio_ci_tabC, "data_derived/biomass_ci_tabC.csv")

# make table
kbl(all_bio_ci_tabC,
    col.names = c('spp', 'type',
                  '1990',
                  '1991',
                  '1992',
                  '1996',
                  '2016',
                  'total'),
    align = 'c', caption = "Biomass CIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  #  add_header_above(header = c(" " = 3, "2004" = 1, "2005" = 1, "2007" = 1, "2009" = 1, "2016" = 1, "total" = 1)) |>
  add_header_above(header = c(" " = 2, "Year" = 6)) |>
  kable_paper()


### biomass by year -----
### I COPIED THIS FROM GRANITE CANAL AND THINK THAT I MODIFIED THE BELOW BUT FORGOT TO DELETE IT

# year_bio_tot.ci <- df_b |>
#   filter(Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Year, trt, type) |>
#   
#   do(data.frame(rbind(Hmisc::smean.cl.boot(.$B.stand)))) |>
#   rename(mean = Mean, ll = Lower, ul = Upper)
# 
# year_bio_tot.ci$ci <- paste0("(", round(year_bio_tot.ci$ll, 1), ", ", round(year_bio_tot.ci$ul, 1), ")")
# year_bio_tot.ci
# 
# 
# write.csv(year_bio_tot.ci, "data_derived/biomass_all_year.csv")


# bootstrap - totals ----
### get cde from 

## density by year -----
source("Pam_data_new-v2.R")
df_tot <- df_a |>  
  filter(!(Year == 1992 & Species == "ASYOY")) |>
  group_by(Year, Station, area, time, type) |>
  summarise(T_sum = sum(abun),
            B_sum = sum(bio)) |>
  mutate(T_sum.stand = T_sum/area*100,
         B_sum.stand = B_sum/area*100)
df_tot

years_den_tot.ci <- df_tot |>
  group_by(Year, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$T_sum.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
write.csv(years_den_tot.ci, "data_derived/density_all_years.csv")

## biomass by year -----
years_bio_tot.ci <- df_tot |>
  group_by(Year, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$B_sum.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
write.csv(years_den_tot.ci, "data_derived/biomass_all_years.csv")
# END ----