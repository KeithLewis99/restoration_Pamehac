# the purpose of this file is to take what was done in scratch pad and make it available for general use for Carle STrub, unmarked, and Bayesian analyses.

# revised from Pam_data_new.R to incorporate the changes that were made for Granite Canal - see GC_data_new.R
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
unique(df_all$Station)

## CLEAN UP ----
## standardize data across years
## remove SITE from df_all$Station

for(i in seq_along(df_all$Station)){
  df_all$Station[i] <- gsub("SITE\\s", paste0("\\6"), df_all$Station[i])
}

## remove "space" from df_all$Station
unique(df_all$Station)
for(i in seq_along(df_all$Station)){
  df_all$Station[i] <- gsub("*\\s", paste0("\\1"), df_all$Station[i])
}
unique(df_all$Station)
unique(df_all$Species)

## remove "space" for Species
for(i in seq_along(df_all$Species)){
  df_all$Species[i] <- gsub("*\\s", paste0("\\1"), df_all$Species[i])
}
unique(df_all$Species)

#write.csv(df_all, "data_derived/df_all.csv")


# sum previous catch ----
## first, create a table for abun = T (total catch) and biomass
df_sum <- df_all |>
  group_by(Year, Species, Station, Sweep) |>
  summarise(bio.sum = sum(Weight.g), abun = n()) 
str(df_sum, give.attr = F)

# write.csv(df_sum, "data_derived/df_sum.csv")

# grid ----
# create dataset with all possible combinations of the following variables
year <- as.character(unique(df_sum$Year))
station <- unique(df_sum$Station)
species <- c("AS", "ASYOY", "BT", "BTYOY")
sweep <- c(1:max(df_sum$Sweep))

# make grid
df_grid <- expand.grid(Year = year, 
                       Species = species,
                       Station = station,
                       Sweep = sweep) |> 
  arrange(Year, Species, Station, Sweep)
#str(df_grid)

# write.csv(df_grid, "data_derived/df_grid.csv")

# edit grid ----

# get max by Year and Station
df_sweep <- df_sum |> 
  group_by(Year, Station) |>
  summarise(max_sweep = max(Sweep)) |> 
  pivot_wider(id_cols = Year,
              names_from = Station,
              values_from = max_sweep)

# edit grid
# remove the structural zeros, i.e., sites that weren't fished
df_grid1 <- df_grid |>
  filter(!((Year == 1990) & 
             (Station == "5A" | Station == "5B" |
                Station == "9" | Station == "3" | Station == "8A")) &
           !(Year == 1991 & (Station == "4" | 
                               Station == "3" |
                               Station == "8A")) &
           !(Year == 1992 &
               (Station == "5A" | 
                  Station == "5B" |
                  Station == "8A"
               )) &
           !((Year == 1996 | Year == 2016) &
               (Station == "5B" | Station == "9"))
  )

#str(df_grid1, give.attr = F)
df_grid1$Year <- as.integer(as.character(df_grid1$Year))
df_grid1$Sweep <- as.integer(df_grid1$Sweep)
# write.csv(df_grid1, "data_derived/df_grid1.csv")
#str(df_sum, give.attr = F)


# join grid and summary ----
# now, join the two dataframes - sites with no fish caught are NA
df_all1 <- full_join(df_grid1, df_sum, by = c("Year", "Species", "Station", "Sweep")) |>
  arrange(Year, Species, Station, Sweep)
#str(df_all1, give.attr = F)
nrow(df_all1 |> filter(Sweep > 3)) # 360 rows with Sweep > 3 + 540 = 900

# write.csv(df_all1, "data_derived/df_all.csv")


# get max Sweep ----
# this is for below where I remove the extra sweeps
df_stn_tag <- df_all1 |>
  group_by(Year) |>
  filter(!is.na(abun)) |>
  summarise(tag = max(Sweep)) |>
  print(n = Inf)

# this may be redundant with above
df_stn_tag_all <- df_all1 |>
  group_by(Year, Station) |>
  filter(!is.na(abun)) |>
  summarise(tag = max(Sweep)) |>
  print(n = Inf)
plot(density(df_stn_tag_all$tag))


# join all and tags
df_all2 <- full_join(df_all1, df_stn_tag, by = c("Year")) |>
  arrange(Year, Species, Station, Sweep) |>
  filter( Sweep <= tag) #!is.na(abun) &


# replace NA with zero
df_all2 <- df_all2 |>
  replace_na(list(bio.sum = 0, abun = 0)) 

# write.csv(df_all2, "data_derived/df_all2.csv")



# spc ----
# calculate sum of previous catch
df_all2$spc <- NA

# calculate spc and flag sites without a Sweep == 1; more negative means more Sweeps before fish is found
df_all2 <- df_all2 |>
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

#View(arrange(df_all2, Year, Station,  Sweep, Species))
str(df_all2, give.attr = F)


# in the original data, when no fish is caught, there is no row.  Therefore, in order to add a Sweep == 1 where abundance == 0, need a subset where 1st sweep != 0; it didn't need to be minimum but then its consistent
# df_tmp <- df_sum |>
#   group_by(Year, Species, Station) |>
#   filter(!any(Sweep == 1)) |>
#   slice_min(Sweep)

#View(tmp)

# create a df from above, remove values, and add Sweep ==1 with bio.sum/abun = 0 and spc == NA; then bind
#df_tmp <- tmp[1:nrow(tmp),]
# df_tmp[, c("Sweep", "bio.sum", "abun", "spc")] <- NA
# df_tmp$Sweep[1:nrow(df_tmp[])] <- 1
# df_tmp$bio.sum[1:nrow(df_tmp[])] <- 0
# df_tmp$abun[1:nrow(df_tmp[])] <- 0
# df_tmp
# 
# # this is the original df but with a Sweep 1 with abun = 0 for rows where 1st sweep > 1
# df_sum <- bind_rows(df_sum, df_tmp) |>
#   arrange(Year, Species, Station, Sweep)
# str(df_sum, give.attr = F)
# #View(df_sum)
# #write.csv(df_sum, "data_derived/df_sum.csv")
# 
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
#  df_sum |> filter(Species == "AS"|Species == "ASYOY"),
  df_all2 |> filter(Species == "BT"|Species == "BTYOY"),
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
  df_all2[df_all2$Species == "BTYOY" & df_all2$Year == 1990,], 
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
      #"data_derived/spc_example.csv"
plotly::ggplotly(p, tooltip = "text")



# for analysis ----
df_a <- df_all2 |> 
  filter(Sweep <= 3) |>
  group_by(Year, Station, Species) |>
  summarise(abun = sum(abun),
            bio = sum(bio.sum))
  

## zeros ----

### This yields 175 rows (5-Years x 4-Species x 12 Stations = 240)  - (11 Station:Years where no fish were caught x 4 Speices = 44) - these are structural "zeros" and therefore, don't need to be in teh analysis.
## Station 3 has 2 years with no fish, Stn 4 has 1, Stn 5A has 2 and Stn 8A has 3 = 8.  Further, Stn 9 has 3 Years but will probably not be included in the analysis 8+ 3 = 11.
## But, 8A's only two years,BTYOY & ASYOY is not included???? (-2)
## But 5B has only fish in 1991  and only BTYOY (= 4). So 240 - (4*15) = 180 
## to check, open csv below and create a pivot table (row == Year & Species, Column == Station, cell = count(abun))

# this is also a bit of bad programming but correcting for earlier efforts
#df_a <- df_all2
# df_a <- df_a |> 
#   ungroup() |>
#   complete(Year, Species, Station) |>
#   filter(!(Year == 1990 & Station %in% c(3, 9, "5A", "5B", "8A")) &
#            !(Year == 1991 & Station %in% c(3, 4, "8A")) &
#            !(Year == 1992 & Station %in% c("5A", "5B", "8A")) &
#            !(Year == 1996 & Station %in% c("5B", 9)) &
#            !(Year == 2016 & Station %in% c("5B", 9))
#          ) |>
#            mutate_at(c("abun", "bio"), ~replace_na(.,0))
# str(df_a, give.attr = F)
#write.csv(df_a, "data_derived/df_a2.csv")

# to check that the expansion using "complete" worked
unique(df_a$Station)
df_a |> filter(Year == 1996 & Station == "8A")
df_a |> filter(Station == "5B")
df_a |> filter(Station == "9")

## variables ----
df_a$time <- NA
df_a$type <- NA

df_a <- df_a |>
  mutate(time = if_else(Year == 1990, "before", "after")) |>
  mutate(type = if_else(Station == "6"|Station == "7", "above", "below"))
df_a$time <- as.factor(df_a$time)
df_a$type <- as.factor(df_a$type)

str(df_a, give.attr=FALSE)  


## area ----
### from Pamehac_2016_efishing.xlsx but this differs from Pamehac_1996_bystation.xlsx - see also Pamehac_*_by_station.csv for 1990, 1991, and 1992.
#### folder "C:\Users\lewiske\Documents\CAFE\projects\restoration\PamehacDepletion\data\year_summaries"
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
area_2016 <- c(269, 188, 137, 160, 100, 108, 84, 96, 111, 103)
area_1990 <- c(102, 100, NA, 100, 220, NA, 100, 102, 286, NA)
area_1991 <- c(98, 108, NA, NA, 252, 123, 101, 119, 115, NA) # 5B = 80, 9 = 113
area_1992 <- c(218, 124, 233, 176, 213, NA, 116, 115, 198, NA) # 9 = 190
area_1996 <- c(131, 101, 109, 102, 115, 131, 125, 105, 111, 104)
df_area <- data.frame(station, area_1990, area_1991, area_1992, area_1996, area_2016)
year <- c(1990, 1991, 1992, 1996, 2016)
years <- rep(year, 10)


df_area <- df_area |>
  pivot_longer(
  cols = starts_with("area"),
  values_to = "area") |>
  bind_cols(year = years)

df_a <- left_join(df_a, df_area[,-2], by = c("Station" = "station", "Year" = "year"))
df_a <- df_a |>
  group_by(Year, Species, Station) |>
  mutate(abun.stand = abun/area*100, bio.stand = bio/area*100) #

unique(df_a$Station)


## lat_long ----
df_loc <- read.csv("data/waypoints.csv")
station_way <- c("4", "7b", "6b", "7", "12", "3b", "3", "5A", "5b", "5", "6", "8A", "8", "1", "2")
df_loc <- cbind(df_loc, station_way)
str(df_loc)

df_a <- left_join(df_a, df_loc, by = c("Station" = "station_way")) |>
  filter(!Station %in% c("5B", "9"))  # remove 5B and 9 for good  BUT what about "5A", "8A", and "9"
str(df_a, give.attr=F)
#write.csv(df_a, "data_derived/df_a3.csv")




# summaries ----

df_sumBT <- tab_type(df_a, "BT", abun.stand)
df_baciBT <- tab_baci(df_a, "BT", abun.stand)
df_bio_sumBT <- tab_type(df_a, "BT", bio.stand)
df_bio_baciBT <- tab_baci(df_a, "BT", bio.stand)


df_sumAS <- tab_type(df_a, "AS", abun.stand)
df_baciAS <- tab_baci(df_a, "AS", abun.stand)
df_bio_sumAS <- tab_type(df_a, "AS", bio.stand)
df_bio_baciAS <- tab_baci(df_a, "AS", bio.stand)

df_sumBTYOY <- tab_type(df_a, "BTYOY", abun.stand)
df_baciBTYOY <- tab_baci(df_a, "BTYOY", abun.stand)
df_bio_sumBTYOY <- tab_type(df_a, "BTYOY", bio.stand)
df_bio_baciBTYOY <- tab_baci(df_a, "BTYOY", bio.stand)

df_sumASYOY <- tab_type(df_a, "ASYOY", abun.stand)
df_baciASYOY <- tab_baci(df_a, "ASYOY", abun.stand)
df_bio_sumASYOY <- tab_type(df_a, "ASYOY", bio.stand)
df_bio_baciASYOY <- tab_baci(df_a, "ASYOY", bio.stand)


# summary stats ----
## totals ----
length(unique(df_all2$Year))
length(unique(df_all2$Station))

# total year:spp:site:catch
nrow(df_sum)
nrow(df_all2)

# total year:spp:site
df_all2 |> group_by(Year, Species, Station) |> 
  #df_sum |> group_by(Year, Species, Station) |> 
  summarise (catch_num = n()) |> 
  ungroup() |>
  summarise(tot = n())

# zeros - 17 of these
df_sum |>
  filter(Sweep == 1 & abun == 0)
df_all2 |>
  filter(Sweep == 1 & abun == 0)

## tables ----
with(df_all2, table(Station, Species))
with(df_all2, table(Station, Sweep, Species))
with(df_all2, table(Station, Sweep, Species, Year))
with(df_all2[df_all2$Year == 2016,], table(Station, Sweep, Species, Year))


## number of sweeps per station
df_all2 |>
  group_by(Year, Species, Station) |>
  summarize(Sweeps = length(Sweep)) |>
  pivot_wider(id_cols = c(Year, Species), 
              names_from = Station, values_from = Sweeps)

# 4-5 passes
df_all2$pass_no <-NA

# summarize just 4-5-pass sites that had fish
df_4_5pass <- df_all2 |>
  group_by(Year, Species, Station) |>
  mutate(pass_no = ifelse(max(Sweep )<=3, 3, 5)) |>
  ungroup() |>
  filter(pass_no == 5) |> # this gets stations with pass 4 or 5
  group_by(Year, Species, Station, Sweep) |>
  summarize(count = n()) |>
  pivot_wider(names_from = Sweep, values_from = count, values_fill = 0)

str(df_4_5pass, give.attr = F)

# tabulate by Year:Species:Station and so that catches are individual columns - required for FSA::removal

# NOTE: WHEN USING:
### filter(length(Sweep) > 1 & Sweep <= 3) - this give sites where there were at least 2 sweeps but excludes 4 and 5 - this is inappropriate for any analysis involving a catchability estimate.  
### filter(Sweep <= 3)would be appropriate for using T
### filter(!(is.na(`2`) & is.na(`3`))) - without this, you still get one catch value

df_tab1 <- df_all2 |>
  group_by(Year, Species, Station) |>
  filter(length(Sweep) >= 1 & Sweep <= 3) |>
  ungroup() |>
  # pivot_wider(id_cols = c(Year, Species, Station), 
  #             names_from = Sweep, values_from = abun) |> 
  pivot_wider(id_cols = c(Year, Species, Station),
              names_from = Sweep, values_from = c(abun, bio.sum), values_fill = 0) #bio.sum abun
#  filter(!(is.na(`2`) & is.na(`3`))) 
#filter(!(is.na(`abun_2`) & is.na(`abun_3`))) 
# mutate_at(c(4:9), ~replace_na(.,0))
df_tab1[df_tab1$Species == "BT" & df_tab1$Year == 1996,]
df_tab1[df_tab1$Species == "AS" & df_tab1$Year == 1990,]
df_sum[df_sum$Species == "AS" & df_sum$Year == 1990,]

df_tab1 |> print(n = Inf)

df_tab1 |>
  group_by(Station, Species, Year) |>
  filter(Station %in% c("5", "5A", "5B", "8", "8A")) |> 
  print(n = Inf)
unique(df_tab1$Station)

# temp is same as df_tab1 but without the last filter
## the query above does not get rid of stations with captures on Sweep 1 (or 2 or 3) & 4 or 5 (all three have captures on sweep 3)
anti_join(df_4_5pass, df_tab1,  by = c('Year', 'Species', 'Station'))



# abundance by year and species
df_tab2 <- df_all2 |>
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
df_tab_T1 <- df_sum |>
  group_by(Year, Species) |>
  filter(Sweep <= 3) |>
  summarize(T = sum(abun),
            n = n_distinct(Station)) |>
  #  ungroup() |>
  pivot_wider(id_cols = c(Year), 
              names_from = Species, 
              values_from = c(T, n)) 
df_tab_T1
str(df_tab_T, give.attr = F)
#write.csv(df_tab_T[, c(1, 6, 2, 7, 3, 8, 4, 9, 5)], "data_derived/df_tab_T.csv")

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
#write.csv(df_tab3, "data_derived/df_tab3.csv")

## for FSA ----
# this would be what is used in FSA - not doing it but keeping for consisttency
tmp1 <-  df_tab1 |>
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
# df_a <- full_join(tmp1, tmp2, by = c("Year", "Species", "Station"))
# df_a |> print(n = Inf)
# str(df_a, give.attr = F)
#write.csv(df_a, "data_derived/df_a1.csv")

## zeros and >30 ----
# proportion > 30
nrow(df_a) # 168 Year:Species:Stations

# 25 Year:Species:Stations > 30
df_a |>
  filter(abun >= 30) |>
  nrow()


# hurdle check
df_all2 |>
  group_by(Year, Station)|>
  filter(Sweep <= 3) |>
  summarise(sum_zero = sum(abun)) |>
  ungroup() |>
  filter(sum_zero == 0)

# 41 Stations had no fish after 3 sweeps
tmp1 <- df_all2 |>
  group_by(Year, Species, Station)|>
  filter(Sweep <= 3) |>
  summarise(sum_zero = sum(abun)) |>
  filter(sum_zero == 0) |>
  print(n = Inf)


# 40 had no fish ever
tmp2 <- df_all2 |>
  group_by(Year, Species, Station)|>
  summarise(sum_zero = sum(abun)) |>
  filter(sum_zero == 0) |>
  print(n = Inf)

# these stations had fish on Sweep > 3
# all had fish on 4th or 5th sweep and n = 1 | n = 2; so 5 out of 41 are not real zeros out of 336 samples
anti_join(tmp1, tmp2, by = c("Year", "Species", "Station"))

df_all2 |> filter(Year == 1996 & Species == "BT" & Station == "4")


# impacts of zeros beyond 3 Sweeps
# 208 Year, Species, Station, Sweeps or
# 104 Year, Species, Station
df_all2 |>
  filter(Sweep > 3) |>
  nrow()

# 
df_all2 |>
  filter(Sweep > 3) |>
  group_by(Year, Species, Station) |>
  summarise(count = n(),
            abun = sum(abun)) |>
  filter(abun >= 3)


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

# zeros
df_all2 |> filter(Sweep <=3) |> nrow()


# for Paul ----
## van-dam bates
df_tmp <- read.csv("data/output_pamehac_by_station.csv")

df_tmp_mean <- df_tmp |>
  filter(Species == "BT" & Year == 1992) |> 
  summarize(mean = mean(abundance.caught),
            mean_cs = mean(species.abundance.contributions)
  )

df_tmp |>
  filter(Species == "BT" & Year == 1992) |>
  select(Station, abundance.caught, species.abundance.contributions, species.abundance.contributions.lcl, species.abundance.contributions.ucl) |>
  ggplot() +
  geom_point(aes(x = Station, y = species.abundance.contributions), color = "red") +
  geom_errorbar(aes(x = Station, ymin = species.abundance.contributions.lcl, ymax = species.abundance.contributions.ucl), color = "red") +
  geom_point(aes(x = Station, y = abundance.caught)) +
  geom_hline(yintercept = df_tmp_mean$mean, color = "blue") + 
  geom_hline(yintercept = df_tmp_mean$mean_cs, color = "black") + 
  ylab("Abundance") + 
  theme_bw()


# END ----
