# this file is to try and determine WTF did Scruton to get the numbers he did in the 1998 paper
## I was not successful in reproducing these values - see ReadMe.

# Start ----
source("Pam_data_new.R")
library(FSA)

# create data sets ----
## to run through FSA::removal

# pool above and below by species
df_tab1a <- df_tab1 |>
  mutate(type = if_else(Station == "6"|Station == "7", "above", "below")) |>
  group_by(Year, Species, type) |>
  summarise(sum_abun1 = sum(abun_1),
            sum_abun2 = sum(abun_2),
            sum_abun3 = sum(abun_3))

df_tab1a

# pool above and below - salmonids
df_tab1b <- df_tab1 |>
  mutate(type = if_else(Station == "6"|Station == "7", "above", "below")) |>
  group_by(Year, type) |>
  summarise(sum_abun1 = sum(abun_1),
            sum_abun2 = sum(abun_2),
            sum_abun3 = sum(abun_3))

df_tab1b

# create area data set
df_area_sum <- df_area |> mutate(type = if_else(station == "6"|station == "7", "above", "below")) |>
  group_by(year, type) |>
  summarise(sum_area = sum(area, na.rm = TRUE))

## bind area ----
### join data sets to area to scale estimates
df_tab1 <- left_join(df_tab1, df_area, by = c("Year" = "year", "Station" = "station"))

df_tab1a <- left_join(df_tab1a, df_area_sum, by = c("Year" = "year", "type" = "type"))
  
df_tab1b <- left_join(df_tab1b, df_area_sum, by = c("Year" = "year", "type" = "type"))


# removal estimates 

## microfish
## by station and species
res_list <- apply(df_tab1[, c(4:6)], MARGIN=1, FUN = removal, method = "Burnham") # takes NA's

# pool above and below stations by species
res_list <- apply(df_tab1a[, c(4:6)], MARGIN=1, FUN = removal, method = "Burnham") # takes NA's

# pool above and below - salmonids
res_list <- apply(df_tab1b[, c(3:5)], MARGIN=1, FUN = removal, method = "Burnham") # takes NA's

# this works but only when three catches so maybe that is fine - filter above on this.
out <- as.data.frame(matrix(NA, length(res_list), 11))
colnames(out) <- c("c1",  "c2", "c3","k",  "T", "X", 
                   "No",  "No.se", "No.LCI", 
                   "No.UCI", "p"
)


# format res_list so that its readable
for(i in seq_along(res_list)){
  if(length(res_list[[i]]$catch) ==3){
    out[i,] <- round(c(res_list[[i]]$catch, 
                       res_list[[i]]$int, 
                       res_list[[i]]$est[1:5]), 2)
    
  } else if (names(res_list[[i]]$catch[2]) == 2 & 
             length(res_list[[i]]$catch) == 2){
    out[i,c(1:2, 4:11)] <- round(c(res_list[[i]]$catch, 
                                   res_list[[i]]$int, 
                                   res_list[[i]]$est[1:5]), 2)
  } else if (names(res_list[[i]]$catch[2]) == 3 & 
             length(res_list[[i]]$catch) == 2){
    out[i,c(1, 3:11)] <- round(c(res_list[[i]]$catch, 
                                 res_list[[i]]$int, 
                                 res_list[[i]]$est[1:5]), 2)
  }
}


# output ----
## output of removal estimates for different data sets above
### may need to tweak these a bit to get them to work
x <- df_tab3
x <- df_tab1
x <- df_tab1a
x <- df_tab1b
# bind year, species and station to res_list
out <- cbind(year = x$Year, 
             spp = x$Species, 
             sta = x$Station,
            # area = x$sum_area,
             #area = x$area,
#             type = x$type,
             out)
# df_tab1 
out1 <- out |> 
  group_by(year, spp, sta) |>
  mutate(NO_stand = No/area*100)
out1[out1$year == 1996, c(1:7, 9, 11, 16)]

# df_tab1a
out1 <- out |> 
  group_by(year, spp, type) |>
  mutate(NO_stand = No/area*100, 
         T_stand = T/area*100)

out1[, c(1:4, 9, 11, 16)]  
View(out1[, c(1:7, 9, 11, 16)] |> arrange(spp, type, year)) 
View(out1[, c(1:4, 9, 11, 16)] |> arrange(spp, type, year)) 
write.csv(out1[, c(1:7, 9, 11, 16)], "out1.csv")


ggplot(out1, aes(x = year, NO_stand, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~spp)

ggplot(out1, aes(x = year, T_stand, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~spp)

# plot out1 with variables year and T_stand with geom_col but filter out year 2016
out1 |> 
  filter(year < 2016) |>
  ggplot(aes(x = year, NO_stand, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~spp)

out1 |> 
  filter(year < 2016) |>
ggplot(aes(x = year, T_stand, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~spp)

# df_tab1b
out1 <- out |> 
  group_by(year, type) |>
  mutate(NO_stand = No/area*100)

out1[, c(1:3, 8, 10, 15)]  
str(out, give.attr=F)

#View(out)
head(out)
nrow(out |> filter(T < 30)) # 97 of 124
nrow(out |> filter(T < 20)) # 85 of 124
nrow(out |> filter(T < 10)) # 57 of 124
nrow(out |> filter(T < 5)) # 33 of 124

# density of total catch
plot(density(out$T))


p <- ggplot(out, aes(x = T, y = No, group = as.factor(spp), colour = spp)) +
  geom_point()
p

# tabulations ----
table(out$year, out$spp, out$T)
table(out$year, out$spp)


# if df_tab3, use this to compare to Scruton et al. 1998
# this is the mean
out_sum <- out |>
  mutate(type = if_else(sta == "6"|sta == "7", "above", "below")) |>
  group_by(year, spp, type) |>
  summarize(meanNo = mean(No))

ggplot(out_sum, aes(x = year, meanNo, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~spp)

#out$bio <- NA
str(out, give.attr = F)
str(df_a, give.attr = F)

#library(tidyr)
pivot_wider(out[out$year == 1991, c(2:3, 10)], names_from = "sta", values_from = c("No"))

df_area |> filter(year == 1991)



# attempt II ----
# tmp3 <- df_a |>
#   group_by(Year, Species, type) |>
#   summarise(mean_bio = mean(bio))
# tmp3
# str(out, give.attr = F)
# str(df_a, give.attr = F)
# unique(df_a$Station)
# 
# tmp4 <- left_join(out, tmp3, by = c("year" = "Year", "spp" = "Species", "type" = "type"))
# unique(tmp5$sta)


tmp5 <- left_join(df_a, out, by = c("Year" = "year", "Species" = "spp",  "Station" = "sta")) |>
  select(Year, Species, Station, T, No, abun, bio, area, type, abun.stand, bio.stand) |>
  mutate_at(c(4:5), ~replace_na(.,0)) |>
  # mutate(abun.stand = abun/area,
  #        bio.stand = bio/area) 
  mutate(adj.bio.stand = (bio/T)*No/area)

head(tmp5)
View(tmp5)
str(tmp5, give.attr = F)

unique(out$sta)
unique(tmp5$Station)

# this is good - keep this but it won't replicate Scruton
tmp6 <- tmp5 |>
  group_by(Year, Species, type) |>
  #filter(abun.stand > 0 & bio.stand > 0) |>
  summarise(mean.abun.stand = mean(abun.stand),
            mean.bio.stand = mean(bio.stand),
            sum.bio.stand = sum(bio.stand)
            )
tmp6
str(tmp6, give.attr = F)
tmp5 |>
  filter(Species == "AS")

tmp6 |>
  filter(Species == "AS")

ggplot(tmp6, aes(x = Year, mean.abun.stand, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~Species)

ggplot(tmp6, aes(x = Year, mean.bio.stand, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~Species)

ggplot(tmp6, aes(x = Year, sum.bio.stand, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~Species)

tmp6 |>
  filter(Species == "AS") |>
  ggplot(aes(x = Year, mean.bio.stand, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~Species)

tmp6 |>
  filter(Species == "AS") |>
  ggplot(aes(x = Year, mean.bio.stand, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~Species)



tmp7 <- tmp5 |>
  group_by(Year, Species, type) |>
  #filter(abun.stand > 0 & bio.stand > 0) |>
  summarise(sum.abun = sum(abun),
            sum.N = sum(No),
            sum.bio = sum(bio),
            sum.area = sum(area)
)

tmp8 <- tmp7 |>
  mutate(density = sum.abun/sum.area*100,
         densityN = sum.N/sum.area*100,
         biomass = sum.bio/sum.area*100)

tmp8
tmp8 |>
  #filter(Species == "AS") |>
  ggplot(aes(x = Year, density, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~Species)



# with by_type ----
## just want to compare how the different approaches work with the same data set
removal(c(10, 10, 3), method = "Burnham")$est
removal(c(10, 10, 3), method = "Burnham", CIMicroFish = TRUE)$est
removal(c(10, 10, 3), method = "CarleStrub")$est

27/230*100 # this gets closer to Scruton's values but still way, way off  (11.7 v. 20+'


removal(c(30, 10, 3), method = "Burnham")$est
removal(c(30, 10, 3), method = "CarleStrub")$est

# END ----