# scratch pad

# Origins ----
## test the impact of 1990 BT

source("amec.efishingcs.r")
amec.efishing8cs("data/year_summaries/Pamehac_1990_by_station") # this the data Kristin used and reproduces Kritin's results in ouptu_pamehac_by_station.csv

amec.efishing8cs("data/test1") # this gives completely different output - not even close

amec.efishing8cs("data/test2")# then, BT data but without sweeps 4 and 5. Some of these are close are the same but SITE 5 is very different.

# re-running this because something very wonky with the output ito SITE 8
amec.efishing8cs("data/year_summaries/Pamehac_2016_by_station")


# tabulate ----
# this is to tabulate the number of fish per pass by year and species - trying to figure out why Pamehac produces wild CIs when RB doesn't

df_tab <- output_pamehac_by_station[, c(1:2, 3:4, 6)]
str(df_tab)

library(tidyr)
df_tab |>
  select(Species, Year, Station, biomass.caught) |>
  pivot_wider(names_from = Station, values_from = biomass.caught) |>
  arrange(Species) |>
  print(n = Inf)



df_abun <- output_pamehac_by_station[, c(1:2, 3:4, 9)]
str(df_abun)
df_abun[df_abun$Species == "ASYOY" & df_abun$Year == 1991,]
df_abun[df_abun$Station == "SITE 3",]

df_tab1 <- df_abun |> 
  select(Species, Year, Station, abundance.caught) |>
  pivot_wider(names_from = Station, values_from = abundance.caught) |>
  arrange(Species) |>
  print(n = Inf)
View(df_tab1)

sum(is.na(df_tab1))/(20*12) # this is about the same as RB which suggests that NAs aren't the issue - but this is for the table and NAs are inserted because ......[check this].  But doing this off df_abun
sum(is.na(df_abun$abundance.caught))/(20*12) # no sites with no fish

temp <- subset(df_abun, abundance.caught < 10)
length(temp$abundance.caught)  # this is about the same as RB which suggests that low counts aren't to blame - but see below

# now, to look at the variances
str(dft, give.attr = F)
View(dft[, c(1:2, 4, 6, 10)])
df_tab2 <- dft |> 
  ungroup() |>
  select(Species, Year, Station, bm_var) |>
  pivot_wider(names_from = Station, values_from = bm_var) |>
  arrange(Species, Year) |>
  print(n = Inf)
sum(is.na(df_tab2))/(20*12) # this is about the same as RB which suggests that NA in variance is not the issue

length(df_tab2[df_tab2 == 0])/(20*12) # so, about 20% of the data set is 0 vs none for RB.  Between 0 and NA, about 1/2 of the sites have no variance to work with.  

length(dft$bm_var[dft$bm_var > 100 & !is.na(dft$bm_var)]) # length >10 = 52, >50 = 30, >100 = 24 - so, there are a lot more Species:Year:Sites with very high variance in Pam than RB.
View(dft[dft$bm_var > 100 & !is.na(dft$bm_var),])


# now, looking at abundance
df_abun[df_abun$abundance.caught >= 30 & !is.na(df_abun$abundance.caught), ]
length(df_abun$abundance.caught[df_abun$abundance.caught <= 5 & !is.na(df_abun$abundance.caught)])/(20*12) # > 30 = 11, > 20 = 17, > 10 = 29, > 5 = 0.4

# FSA ----
library(FSA)
library(dplyr)
pm90 <- read.csv("data/year_summaries/Pamehac_1990_by_station.csv")

# originally did this with biomass function removal only works on abundance
pm90_sum <- pm90 |>
  group_by(Species, Station, Sweep) |>
  summarise(bio.sum = sum(Weight.g), abun = n()) 
pm90_sum |> print(n = Inf)

# removal for one species and station
pm90_as <- pm90_sum |>
  ungroup() |>
  filter(Species == "AS" & Station == 6) |>
  select(Sweep, bio.sum, abun)
pm90_as


removal(pm90_as$bio.sum, method = "CarleStrub", just.ests = T)
removal(pm90_as$abun, method = "CarleStrub", just.ests = T)

# removal for one species and all stations
library(tidyr)
pm90_tab <- pm90_sum |>
  filter(Species == "AS") |>
  pivot_wider(id_cols = c(Species, Station), 
              names_from = Sweep, values_from = bio.sum)
  #pivot_wider(id_cols = c(Species, Station),
   #           names_from = Sweep, values_from = abun)

apply(pm90_tab[c(1, 3), c(-1:-2)], MARGIN=1, FUN = removal, method = "CarleStrub", just.ests = T)


# removal for all species and all stations
pm90_tab1 <- pm90_sum |>
  pivot_wider(id_cols = c(Species, Station), 
              names_from = Sweep, values_from = ) #bio.sum

res <- apply(pm90_tab1[c(1, 3, 5:7, 9:13, 16:17), c(-1:-2)], MARGIN=1, FUN = removal, method = "CarleStrub", just.ests=T)

res <- data.frame(spp = pm90_tab1$Species[c(1, 3, 5:7, 9:13, 16:17)],
                  sta = pm90_tab1$Station[c(1, 3, 5:7, 9:13, 16:17)], 
                  t(res))
res
res[res$spp == "AS",]

# END ----