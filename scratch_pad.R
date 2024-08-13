# scratch pad

# Origins ----
## test the impact of 1990 BT

source("amec.efishingcs.r")
amec.efishing8cs("data/year_summaries/Pamehac_1990_by_station") # this the data Kristin used and reproduces Kritin's results in ouptu_pamehac_by_station.csv

amec.efishing8cs("data/test1") # this gives completely different output - not even close

amec.efishing8cs("data/test2")# then, BT data but without sweeps 4 and 5. Some of these are close are the same but SITE 5 is very different.


# tabulate ----
# this is to tabulate the number of fish per pass by year and species


df_tab <- output_pamehac_by_station[, c(1:2, 3:4, 6)]
str(df_tab)

library(tidyr)
df_tab |>
  select(Species, Year, Station, biomass.caught) |>
  pivot_wider(names_from = Station, values_from = biomass.caught) |>
  arrange(Species) |>
  print(n = Inf)
