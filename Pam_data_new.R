# the purpose of this file is to take what was done in scratch pad and make it available for general use for Carle STrub, unmarked, and Bayesian analyses
# Source ----
source("Pam_fun.R")

# library ----
library(tidyr)
library(dplyr)
library(ggplot2)

# import ----
# import files in catch and convert to proper format
#create a pattern and bind directory to pattern
temp = list.files(path = "data/year_summaries/", pattern="Pamehac_.*_by_station.csv$", full.names = T)

# import files as a list
ls_pam = (lapply(temp, read.csv))
str(ls_pam)
str(ls_pam,1)
str(ls_pam[1])

# standardize names
names(ls_pam) <- c("1990", "1991", "1992", "1996", "2016")

# change 1990 to character variable 
ls_pam[["1990"]]$Station <- as.character(ls_pam[["1990"]]$Station)

# create either a large dataframe and then do some summaries
# as above, create summaries for FSA
df_all <- bind_rows(ls_pam)

# standardize data across years
##
## remove SITE
for(i in seq_along(df_all$Station)){
  df_all$Station[i] <- gsub("SITE\\s", paste0("\\6"), df_all$Station[i])
}
## remove "space"
unique(df_all$Station)
for(i in seq_along(df_all$Station)){
  df_all$Station[i] <- gsub("*\\s", paste0("\\1"), df_all$Station[i])
}
unique(df_all$Station)

## remove "space" for Species
for(i in seq_along(df_all$Species)){
  df_all$Species[i] <- gsub("*\\s", paste0("\\1"), df_all$Species[i])
}
unique(df_all$Species)

#write.csv(df_all, "derived_data/df_all.csv")


# sum previous catch ----
## first, create a table for T
df_sum <- df_all |>
  group_by(Year, Species, Station, Sweep) |>
  summarise(bio.sum = sum(Weight.g), abun = n()) 
str(df_sum, give.attr = F)

# calculate sum of previous catch
df_sum$spc <- NA

# calculate spc and flag sites without a Sweep == 1; more negative means more Sweeps before fish is found
df_sum <- df_sum |>
  group_by(Year, Species, Station) |>
  #summarise(min = min(Sweep))
  # case_when is vectorized if-else: so when Sweep ==1, spc is 0, when Sweep ==2 & there is a Sweep ==1, abundance, else -1, when Sweep ==3, if there is a Sweep ==1, sum Sweep 1 & 2, else -2, etc.
  mutate(spc = case_when(
    Sweep == 1 ~ 0,
    Sweep == 2 ~ ifelse(any(Sweep == 1), abun[Sweep ==1], -1),
    Sweep == 3 ~ ifelse(any(Sweep == 1), sum(c(abun[Sweep == 1 | Sweep == 2])), -2),
    Sweep == 4 ~ ifelse(any(Sweep == 1), sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3])), -3),
    Sweep == 5 ~ ifelse(any(Sweep == 1), sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3 | Sweep == 4])), -4)
  ))

#View(arrange(df_sum, Year, Station,  Sweep, Species))
str(df_sum, give.attr = F)


# in the original data, when no fish is caught, there is no row.  Therefore, in order to add a Sweep == 1 where abundance == 0, need a subset where 1st sweep != 0; it didn't need to be minimum but then its consistent
tmp <- df_sum |>
  group_by(Year, Species, Station) |>
  filter(!any(Sweep == 1)) |>
  slice_min(Sweep)

#View(tmp)

# create a df from above, remove values, and add Sweep ==1 with bio.sum/abun = 0 and spc == NA; then bind
df_tmp <- tmp[1:nrow(tmp),]
df_tmp[, c("Sweep", "bio.sum", "abun", "spc")] <- NA
df_tmp$Sweep[1:nrow(df_tmp[])] <- 1
df_tmp$bio.sum[1:nrow(df_tmp[])] <- 0
df_tmp$abun[1:nrow(df_tmp[])] <- 0
df_tmp

# this is the original df but with a Sweep 1 with abun = 0 for rows where 1st sweep > 1
df_sum <- bind_rows(df_sum, df_tmp) |>
  arrange(Year, Species, Station, Sweep)
str(df_sum, give.attr = F)
#View(df_sum)
#write.csv(df_sum, "derived_data/df_sum.csv")

# now for when Sweep == 1 is True but there is a missing sweep - don't need this bc you are only using the first value but it will make the spc graphs a bit hard to interpret
# test <- df_sum |>
#     #filter(na.omit) 
#     filter(!is.na(spc)) |>  
#     mutate(Verify = Sweep - lag(Sweep, default = 0) == 1) |>
#     filter(Verify == FALSE) 
# 
# test1 <- left_join(df_sum, test[, c(1:4, 8)], by = c("Year", "Species", "Station", "Sweep"))
#View(test1)  

## spc plot ----
### year by spp

p <- ggplot(
  df_sum |> filter(Species == "AS"|Species == "ASYOY"),
  aes(x = spc, y = abun, 
      group = Station, fill = Station,
      text = paste("SPC: ", spc, "\n",
                   "Abund: ", abun, "\n",
                   "Stn: ", Station, "\n",
                   "Sweep: ", Sweep,
                   sep = "")
  )) +
  geom_point() +
  geom_path() +
  facet_grid(Year ~ Species)

p
plotly:: ggplotly(p, tooltip = "text")


### subset
p <- ggplot(
  #df_sum[df_sum$Species == "AS",], 
  #df_sum[df_sum$Species == "AS" & df_sum$Station == 8,], 
  df_sum[df_sum$Species == "BTYOY" & df_sum$Year == 1990,], 
  aes(x = spc, y = abun, 
      group = Station, fill = Station,
      text = paste("SPC: ", spc, "\n",
                   "Abund: ", abun, "\n",
                   "Stn: ", Station, "\n",
                   "Sweep: ", Sweep,
                   sep = "")
  )) +
  geom_point() +
  geom_path()

p

#write.csv(df_sum[df_sum$Species == "AS" & df_sum$Year == 2016,], 
      #"derived_data/spc_example.csv"
plotly::ggplotly(p, tooltip = "text")

# summary stats ----
## totals ----
length(unique(df_sum$Year))
length(unique(df_sum$Station))

# total year:spp:site:catch
nrow(df_sum)

# total year:spp:site
df_sum |> group_by(Year, Species, Station) |> 
  summarise (catch_num = n()) |> 
  ungroup() |>
  summarise(tot = n())

# zeros - 17 of these
df_sum |>
  filter(Sweep == 1 & abun == 0)

## tables ----
with(df_all, table(Station, Species))
with(df_all, table(Station, Sweep, Species))
with(df_all, table(Station, Sweep, Species, Year))
with(df_all[df_all$Year == 2016,], table(Station, Sweep, Species, Year))


## number of sweeps per station
df_sum |>
  group_by(Year, Species, Station) |>
  summarize(Sweeps = length(Sweep)) |>
  pivot_wider(id_cols = c(Year, Species), 
              names_from = Station, values_from = Sweeps)

# 4-5 passes
df_all$pass_no <-NA

# summarize just 4-5-pass sites and had fish
df_4_5pass <- df_all |>
  group_by(Year, Species, Station) |>
  mutate(pass_no = ifelse(max(Sweep )<=3, 3, 5)) |>
  ungroup() |>
  filter(pass_no == 5) |> 
  group_by(Year, Species, Station, Sweep) |>
  summarize(count = n()) |>
  pivot_wider(names_from = Sweep, values_from = count, values_fill = 0)

str(df_4_5pass, give.attr = F)

# tabulate by Year:Species:Station and so that catches are individual columns - required for FSA::removal

# NOTE: WHEN USING:
### filter(length(Sweep) > 1 & Sweep <= 3) - this give sites where there were at least 2 sweeps but excludes 4 and 5 - this is inappropriate for any analysis involving a catchability estimate.  
### filter(Sweep <= 3)would be appropriate for using T
### filter(!(is.na(`2`) & is.na(`3`))) - without this, you still get one catch value

df_tab1 <- df_sum |>
  group_by(Year, Species, Station) |>
  filter(length(Sweep) > 1 & Sweep <= 3) |>
  ungroup() |>
  # pivot_wider(id_cols = c(Year, Species, Station), 
  #             names_from = Sweep, values_from = abun) |> 
  pivot_wider(id_cols = c(Year, Species, Station),
              names_from = Sweep, values_from = c(abun, bio.sum)) |> #bio.sum abun
#  filter(!(is.na(`2`) & is.na(`3`))) 
  filter(!(is.na(`abun_2`) & is.na(`abun_3`))) 
df_tab1[df_tab1$Species == "BT" & df_tab1$Year == 1996,]

df_tab1 |> print(n = Inf)

df_tab1 |>
  group_by(Station, Species, Year) |>
  filter(Station %in% c("5", "5A", "5B", "8", "8A")) |> 
  print(n = Inf)
unique(df_tab1$Station)

# temp is same as df_tab1 but without the last filter
## the query above does not get rid of stations with captures on Sweep 1 (or 2 or 3) & 4 or 5 (all three have captures on sweep 3)
anti_join(df_4_5pass, df_tab1,  by = c('Year', 'Species', 'Station'))



# sum by year and species
df_tab2 <- df_sum |>
  group_by(Year, Species, Station) |>
  pivot_wider(id_cols = c(Year, Species, Station), 
              names_from = Sweep, values_from = abun) #bio.sum abun
str(df_tab2, give.attr = F)
#View(df_tab2)


# this includes the three from the anti_join above plus 13 more sites with only catches on the first sweep - this adds up to 140 so all good.
df_aj2 <- anti_join(df_tab2, df_tab1, by = c('Year', 'Species', 'Station'))


# sum catches by year and species
df_tab2 |>
  group_by(Year, Species) |>
  summarise(sum_c1 = sum(`1`, na.rm = T),
            sum_c2 = sum(`2`, na.rm = T),
            sum_c3 = sum(`3`, na.rm = T)
  )

# sum catches by species for 3 passes - both of this and the below sum to 2547 which is the sum of df_all
df_tab2 |>
  group_by(Species) |>
  summarise(sum_c1 = sum(`1`, na.rm = T),
            sum_c2 = sum(`2`, na.rm = T),
            sum_c3 = sum(`3`, na.rm = T)
  )

df_sum |> group_by(Species) |> filter(Sweep ==4 | Sweep ==5) |> summarise(sum = sum(abun))

df_tab2 |> filter(`1` == 0)
df_tab1 |> filter(abun_1 == 0)

# sum by catch for Year and Species 3 passes - this sum so 2495 which matches Excel
df_tab_T <- df_sum |>
  group_by(Year, Species) |>
  filter(Sweep <= 3) |>
  summarize(T = sum(abun)) |>
#  ungroup() |>
  pivot_wider(id_cols = c(Year), 
              names_from = Species, 
              values_from = T) 
df_tab_T # this is right

# as above but summation of total sites and catches by year and species - but this is only for the site where fish were caught
df_tab_T <- df_sum |>
  group_by(Year, Species) |>
  filter(Sweep <= 3) |>
  summarize(T = sum(abun),
            n = n_distinct(Station)) |>
  #  ungroup() |>
  pivot_wider(id_cols = c(Year), 
              names_from = Species, 
              values_from = c(T, n)) 
df_tab_T
str(df_tab_T, give.attr = F)
#write.csv(df_tab_T[, c(1, 6, 2, 7, 3, 8, 4, 9, 5)], "derived_data/df_tab_T.csv")

# this summary is for all sites that were fished irregardless of whether they had fish or not.  The assumption here is that there were 5 passes in 1990, 1991, 1996 but three in 1992 and 2016.  Scruton says minimum of 4 passes in 1990-1996 but if this is the case, 1992 had none.  To determine if 4 or 5 would require returning to original data - not sure if this is worth it.  
df_tab3 <- 
  df_tab2 |>
  mutate_at(c(5, 6), ~replace_na(.,0)) |>
  mutate(`4` = case_when(
    Year == 1990 | Year == 1991 | Year == 1996 
    ~ ifelse(is.na(`4`), 0 , `4`))) |>
  mutate(`5` = case_when(
    Year == 1990 | Year == 1991 | Year == 1996 
    ~ ifelse(is.na(`5`), 0 , `5`))) |> 
  print(n = 140)
#View(test)
str(df_tab3, give.attr = F)
nrow(df_tab3)
#View(df_tab3 |> filter(!is.na(`1`)) |> count(`1`))
sum(rowSums(!is.na(df_tab3[,4:8])))
write.csv(df_tab3, "derived_data/df_tab3.csv")

# pool ----
# pool by Year and Station - this is for the Cote method
df_tab_pool <- df_tab2 |>
  group_by(Year, Station) |>
  summarise(`1` = sum(`1`, na.rm = T), 
            `2` = sum(`2`, na.rm = T), 
            `3` = sum(`3`, na.rm = T),
            `4` = sum(`3`, na.rm = T),
            `5` = sum(`3`, na.rm = T))
df_tab_pool |> print(n = Inf)

df_tab_pool_spc <- df_sum |>
  group_by(Year, Station, Sweep) |>
  summarise(bio.sum = sum(bio.sum), abun = sum(abun)) |>
  mutate(spc = case_when(
    Sweep == 1 ~ 0,
    Sweep == 2 ~ abun[Sweep ==1],
    Sweep == 3 ~ sum(c(abun[Sweep == 1 | Sweep == 2])),
    Sweep == 4 ~ sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3])),
    Sweep == 5 ~ sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3 | Sweep == 4]))
))
df_tab_pool_spc

p <- ggplot(
  df_tab_pool_spc,
  aes(x = spc, y = abun, 
      group = Station, fill = Station,
      text = paste("SPC: ", spc, "\n",
                   "Abund: ", abun, "\n",
                   "Stn: ", Station, "\n",
                   "Sweep: ", Sweep,
                   sep = "")
  )) +
  geom_point() +
  geom_path() +
  facet_grid(Year ~ Station)

p

# sum catches by year and species - from Pam_data.R
## DEPRECATE? - need to check this - just commenting out for now
# df_abun <- output_pamehac_by_station[, c(1:2, 3:4, 9)]
# str(df_abun)
# df_abun[df_abun$Species == "ASYOY" & df_abun$Year == 1991,]
# df_abun[df_abun$Station == "SITE 3",]
# 
# df_tab2 <- df_abun |> 
#   select(Species, Year, Station, abundance.caught) |>
#   pivot_wider(names_from = Station, values_from = abundance.caught) |>
#   arrange(Species) |>
#   print(n = Inf)


# for analysis ----
tmp1 <- 
  df_tab1 |>
  group_by(Year, Species, Station) |>
  pivot_longer(
    cols = starts_with("abun"),
    values_to = "abun") |>
  summarize(abun = sum(abun, na.rm = T))

tmp2 <- 
  df_tab1 |>
  group_by(Year, Species, Station) |>
  pivot_longer(
    cols = starts_with("bio"),
    values_to = "bio") |>
  summarize(bio = sum(bio, na.rm = T))

# df_a for analysis
df_a <- full_join(tmp1, tmp2, by = c("Year", "Species", "Station"))
df_a |> print(n = Inf)

df_a$time <- NA
df_a$type <- NA

df_a <- df_a |>
  mutate(time = if_else(Year == 1990, "before", "after")) |>
  mutate(type = if_else(Station == "6"|Station == "7", "above", "below"))
df_a$time <- as.factor(df_a$time)
df_a$type <- as.factor(df_a$type)


str(df_a, give.attr=FALSE)  
#1 = 269
#2 = 188
#3 = 137
#4  = 160
#5  = 100 
#5A = 108
#6  = 84
#7  = 96
#8 = 111
#8A = 103
station <- c("1", "2", "3", "4", "5", "5A", "6", "7", "8", "8A")
area <- c(269, 188, 137, 160, 100, 108, 84, 96, 111, 103)

df_area <- data.frame(station, area)

df_a <- left_join(df_a, df_area, by = c("Station" = "station"))
df_a <- df_a |>
  group_by(Year, Species, Station) |>
  mutate(abun.stand = abun/area, bio.stand = bio/area) |>
  filter(!Station %in% c("5B", "9"))  # what about "5A", "8A", and "9"


unique(df_a$Station)


# lat_long ----
df_loc <- read.csv("data/waypoints.csv")
station_way <- c("4", "7b", "6b", "7", "12", "3b", "3", "5A", "5b", "5", "6", "8A", "8", "1", "2")
df_loc <- cbind(df_loc, station_way)
str(df_loc)



df_a <- left_join(df_a, df_loc, by = c("Station" = "station_way"))


# summaries ----

df_sumBT <- tab_type(df_a, "BT", abun.stand)
df_baciBT <- tab_baci(df_a, "BT", abun.stand)

df_sumAS <- tab_type(df_a, "AS", abun.stand)
df_baciAS <- tab_baci(df_a, "AS", abun.stand)

tab_type(df_a, "BTYOY", abun.stand)
tab_baci(df_a, "BTYOY", abun.stand)

tab_type(df_a, "ASYOY", abun.stand)
tab_baci(df_a, "ASYOY", abun.stand)

# END ----
