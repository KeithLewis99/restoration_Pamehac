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
# figuring out how FSA::removal works and how to get the output
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
              names_from = Sweep, values_from = bio.sum) #abun

# remove the rows with NA in first sweep or only one sweep
res <- apply(pm90_tab1[c(1, 3, 5:7, 9:13, 16:17), c(-1:-2)], MARGIN=1, FUN = removal, method = "CarleStrub", just.ests=T)

res <- data.frame(spp = pm90_tab1$Species[c(1, 3, 5:7, 9:13, 16:17)],
                  sta = pm90_tab1$Station[c(1, 3, 5:7, 9:13, 16:17)], 
                  t(res))
res
res[res$spp == "AS",]




# large variance ----
library(FSA)
library(dplyr)
pm16 <- read.csv("data/year_summaries/Pamehac_2016_by_station.csv")
# summarize abundance and biomass
pm16_sum <- pm16 |>
  group_by(Species, Station, Sweep) |>
  summarise(bio.sum = sum(Weight.g), abun = n()) 

# create a data set for FSA::removal for all species and all stations - not that this is for abundance - if you change to biomass, you need to change some code below so that its apples:apples 
library(tidyr)
pm16_tab1 <- pm16_sum |>
  pivot_wider(id_cols = c(Species, Station), 
              names_from = Sweep, values_from = abun) #bio.sum abun
pm16_tab1 |> print(n = Inf)

# remove the rows with NA in first sweep or only one sweep - this is manual - need to change subset below if you change the data set
## just estimates
res <- apply(pm16_tab1[c(1:20, 29:30), c(-1:-2)], MARGIN=1, FUN = removal, method = "CarleStrub", just.ests=T)

res <- data.frame(spp = pm16_tab1$Species[c(1:20, 29:30)],
                  sta = pm16_tab1$Station[c(1:20, 29:30)], 
                  t(res))
res
res[res$spp == "AS",]

## get all output from "removal" and put in list for comparisions and to figure out why some of the variances are so huge
res_list <- apply(pm16_tab1[c(1:20, 29:30), c(-1:-2)], MARGIN=1, FUN = removal, method = "CarleStrub") # 
res_list[[1]]


# had to drop the last row bc only 2 passes
## This was an attempt to deal with the unequal capture rates but both the Moran and Schnute methods produce UCIs that are infinite and adjusting Tmult doesn't really seem to help.
# res_list <- apply(pm16_tab1[c(1:20, 29), c(-1:-2)], MARGIN=1, FUN = removal, method = "Moran", just.est = T, Tmult = 20) # CarleStrub



# create a matrix to be populated by output from res_list
out <- as.data.frame(matrix(NA, 10, 11))
colnames(out) <- c("c1",  "c2", "c3","k",  "T", "X", 
                   "No",  "No.se", "No.LCI", 
                   "No.UCI", "p"
                   )
# loop to extract output from res_list
for(i in seq_along(res_list[1:10])){
  out[i,] <- round(c(res_list[[i]]$catch, 
                     res_list[[i]]$int, 
                     res_list[[i]]$est[1:5]), 2)
  
  #return(out)
}
out
out <- cbind(sta = pm16_tab1$Station[c(1:10)], out)

# create some plots that I thought would be interesting
with(out, plot(p, No.se)) # negative exponential relationship between capture probability and variance
with(out, plot(X/No, p)) #   positive linear relationship which suggests that when first and second capture are large, p is high
with(out, plot(c3/T, p)) # negative linear relationship - suggests that the lower the last catch is relative to total catch, then lower p and therefore, lower the variance.  
# which is why 8A has such large variance.  The p is very low which means high variance (in part) and p is low because of the ratio of c3 to total catch (all else being equal)


# equal capture ----
## there are two assumptions here - equal catchability in all sampling efforts and equal capture among groups.
## plot ----
# decrease in abundance with sweep
library(ggplot2)
p <- ggplot(pm16_sum, 
            aes(x = as.factor(Sweep), y = abun, 
                group = Station, fill = Station,
                    text = paste("Sweep: ", Sweep, "\n",
              #"Abund: ", abun, "\n",
              "Biomass: ", bio.sum, "\n",
              "Stn: ", Station, 
              sep = ""))) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(Species),
             )
plotly::ggplotly(p, tooltip = "text")


# plots as recommended in Eg. 7.6 in Lockwood 2000 to demonstrate equal catchability
## spc = sum of previous catch
### calculations of sum of previous catches below
pm16_sum$spc <- rep(NA, nrow(pm16_sum))

for(i in seq_along(pm16_sum$Sweep)){
  if(pm16_sum$Sweep[i] == 1){
    pm16_sum$spc[i] <- 0
  } else if(pm16_sum$Sweep[i] == 2) {
    #  z[i] <- cumsum(x[i-1])
    pm16_sum$spc[i] <- pm16_sum$abun[i-1]
  } else {
    pm16_sum$spc[i] <- sum(pm16_sum$abun[(i-2):(i-1)])
  }
}

p <- ggplot(pm16_sum[pm16_sum$Species == "AS",], 
            aes(x = spc, y = abun, 
                group = Station, fill = Station,
                text = paste("SPC: ", spc, "\n",
                   "Abund: ", abun, "\n",
                   "Stn: ", Station, "\n",
                   "Sweep: ", Sweep,
                   sep = "")
          )) +
  geom_point() +
  geom_line() 
#  geom_smooth(method='lm', se = F) 
#  facet_wrap(vars(Species))

p
plotly::ggplotly(p, tooltip = "text")



## GF ----
# goodness of fit as recommended in Lockwood 2000
## The test statistic χ 2 from Equation (15) is compared with chi-square (0.95), Table 2, with k-2 degrees of freedom (df). Note that two degrees of freedom are lost because N is estimated (Snedecor and Cochran 1991:77). If 2 χ 2 < χ(0.95), probability of capture did not differ significantly (at the 95% level of certainty) between passes; if χ 2 ≥ χ(0.95) , then probability of capture was significantly different with 95% certainty (don't think this last part is right.

out$GF <- with(out, round((c1 - (No*p))^2/No*p + 
                 (c2 - No*(1-p)*p)^2/(No*(1-p)*p) +
                 (c3 - (No*(1-p)^2*p))^2/(No*(1-p)^2*p)
  ,4)             
)
out

temp <- 10

# a comparison of the GF statistics with the sum of previous catches plots gives good correspondence but the plots tell you why there is a lack of fit.

pchisq(0.03478419, 2)
pchisq(out$GF[1], 2)


# matches Lockwood 2000 more or less - i confirmed that the small discrepancies are due to rounding error by Lockwood
N = 546
p = 0.5496
c1 = 300
c2 = 130
c3 = 69
(c1 - (N*p))^2/N*p + 
  (c2 - N*(1-p)*p)^2/(N*(1-p)*p) +
  (c3 - (N*(1-p)^2*p))^2/(N*(1-p)^2*p)

1-pchisq(1.2813, 2) #- this is the pvalue
qchisq(1-0.5269498, 2) #- returns the test statistic
qchisq(0.95, 1) #- gives the critical test


# Seber Goodness of Fit test for three passes
cs_gf <- function(N, p, c1, c2, c3){
  (c1 - (N*p))^2/N*p + 
    (c2 - N*(1-p)*p)^2/(N*(1-p)*p) +
    (c3 - N*(1-p)^2*p)^2/(N*(1-p)^2*p)
  }


# test 8A
N = 1426
p = 0.1
c1 = 150
c2 = 103
c3 = 133
cs_gf(N, p, c1, c2, c3)


# test 1
i = 2
N = out$No[i]
p = out$p[i]
c1 = out$c1[i]
c2 = out$c2[i]
c3 = out$c3[i]
cs_gf(N, p, c1, c2, c3)

1-pchisq(7.656638, 2) #- this is the pvalue
qchisq(0.95, 1) #- gives the critical test



pm16_sum[pm16_sum$Species == "AS",] |> print(n=Inf)


## equal capture - groups ----
ggplot(res, aes(x = sta, y = p, group = spp)) +
  geom_point() + 
  geom_errorbar(aes(ymax = p.UCI, ymin=p.LCI), width = 0.25) +
  facet_grid(~spp)


pm16_tab2 <- pm16_tab1 |>
  group_by(Station) |>
  summarise(s1 = sum(`1`, na.rm = T), s2 = sum(`2`, na.rm = T), s3 = sum(`3`, na.rm = T))
pm16_tab2

res1 <- apply(pm16_tab2[, -1], MARGIN=1, FUN = removal, method = "CarleStrub", just.ests=T)  

res1 <- data.frame(sta = pm16_tab2$Station, 
                  t(res1))
res1


# capture prob
ggplot(res1, aes(x = sta, y = p)) +
  geom_point() + 
  geom_errorbar(aes(ymax = p.UCI, ymin=p.LCI), width = 0.25)



pm16_sum$spc <- rep(NA, nrow(pm16_sum))

# fairly sure this only works in ideal circumstances
# for(i in seq_along(pm16_sum$Sweep)){
#   if(pm16_sum$Sweep[i] == 1){
#     pm16_sum$spc[i] <- 0
#   } else if(pm16_sum$Sweep[i] == 2) {
#     #  z[i] <- cumsum(x[i-1])
#     pm16_sum$spc[i] <- pm16_sum$abun[i-1]
#   } else {
#     pm16_sum$spc[i] <- sum(pm16_sum$abun[(i-2):(i-1)])
#   }
# }

pm16_sum_comb <- pm16_sum |>
  group_by(Station, Sweep) |>
  summarise(sum_abun = sum(abun), sum_spc = sum(spc))

p <- ggplot(pm16_sum_comb, 
            aes(x = sum_spc, y = sum_abun, 
                group = Station, fill = Station,
                text = paste("SPC: ", sum_spc, "\n",
                             "Abund: ", sum_abun, "\n",
                             "Stn: ", Station, "\n",
                             "Sweep: ", Sweep,
                             sep = "")
            )) +
  geom_point() +
  geom_line() 
#  geom_smooth(method='lm', se = F) 
#  facet_wrap(vars(Species))

p
plotly::ggplotly(p, tooltip = "text")


# just a scratch to figure out how to do cumsum and sum properly
x <- 1:10
cumsum(x[1:3])
z <- rep(NA, 10)
i = 10
for(i in seq_along(x)){
  if(i == 1){
    z[i] <- 0
  } else if(i > 2) {
    #  z[i] <- cumsum(x[i-1])
    z[i] <- sum(x[c(1:i-1)])
  }
}
z
z[3]

cumsum(x[c(1:i-1)])
sum(x[c(1:i-1)])




#  mutate(counter = if_else(Sweep == 1, 0, 1)) |>
#mutate(spc = if_else(Sweep == 1, 0, lag(abun)) # sum previous catch
#mutate(spc = if_else(Sweep == 1, 0, sum(lag(abun, Sweep - 1)))) # sum previous catch
#mutate(spc = case_when(Sweep == 1, 0, lag(abun, Sweep-1)) # sum previous catch

sum(abun[1:n-1])
pm16_sum
pm16_sum |> print(n = Inf)

sum(lag(pm16_sum$abun[1:3], as.numeric(pm16_sum$Sweep[1:3])))
lag(pm16_sum$abun[2], pm16_sum$Sweep[1])

lag(as.numeric(pm16_sum$abun[1:3]), n= 2)
str(pm16_sum)

# import ----
# import files in catch and convert to proper format
#create a pattern and bind directory to pattern
temp = list.files(path = "data/year_summaries/", pattern="Pamehac_.*_by_station.csv$", full.names = T)

# import files as a list
ls_pam = (lapply(temp, read.csv))
str(ls_pam)
str(ls_pam,1)
str(ls_pam[1])

names(ls_pam) <- c("1990", "1991", "1992", "1996", "2016")
 ls_pam[1][[1]]$Station
 ls_pam[1]$'1990'$Station
 ls_pam$'1990'$Station
 ls_pam[[1]]$Station # gets to contents directly
 ls_pam[["1990"]]$Station

# change 1990# change 1990# change 1990 to character variable 
 ls_pam[["1990"]]$Station <- as.character(ls_pam[["1990"]]$Station)



# create either a large dataframe and then do some summaries
# as above, create summaries for FSA
# 
str(ls_pam[1])
library(dplyr)
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


# sum previous catch ----
## first, create a table for T
df_sum <- df_all |>
  group_by(Year, Species, Station, Sweep) |>
  summarise(bio.sum = sum(Weight.g), abun = n()) 

# calculate sum of previous catch
df_sum$spc <- NA

write.csv2()


# calculate spc and flag sites without a Sweep == 1
df_sum <- df_sum |>
  group_by(Year, Species, Station) |>
  #summarise(min = min(Sweep))
  mutate(spc = case_when(
    Sweep == 1 ~ 0,
    Sweep == 2 ~ ifelse(any(Sweep == 1), abun[Sweep ==1], -1),
    Sweep == 3 ~ ifelse(any(Sweep == 1), sum(c(abun[Sweep == 1 | Sweep == 2])), -2),
    Sweep == 4 ~ ifelse(any(Sweep == 1), sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3])), -3),
    Sweep == 5 ~ ifelse(any(Sweep == 1), sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3 | Sweep == 4])), -4)
))

View(df_sum)


# in order to add a Sweep == 1 where abundance == 0, need a subset where 1st sweep != 0; it didn't need to be minimum but then its consistent
test <- df_sum |>
  group_by(Year, Species, Station) |>
  filter(!any(Sweep == 1)) |>
  slice_min(Sweep)

View(test)

# create a df from above, remove values, and add Sweep ==1 with bio.sum/abun = 0 and spc == NA; then bind
df_tmp <- test[1:nrow(test),]
df_tmp[, c("Sweep", "bio.sum", "abun", "spc")] <- NA
df_tmp$Sweep[1:nrow(df_tmp[])] <- 1
df_tmp$bio.sum[1:nrow(df_tmp[])] <- 0
df_tmp$abun[1:nrow(df_tmp[])] <- 0
df_tmp

# this is the original df but with a Sweep 1 with abun = 0 for rows where 1st sweep > 1
df_sum <- bind_rows(df_sum, df_tmp) |>
  arrange(Year, Species, Station, Sweep)

#View(df_sum)

# now for when Sweep == 1 is True but there is a missing sweeps - don't need this bc you are only using the first value but it will make the spc graphs a bit hard to interpret
# test <- df_sum |>
#     #filter(na.omit) 
#     filter(!is.na(spc)) |>  
#     mutate(Verify = Sweep - lag(Sweep, default = 0) == 1) |>
#     filter(Verify == FALSE) 
# 
# test1 <- left_join(df_sum, test[, c(1:4, 8)], by = c("Year", "Species", "Station", "Sweep"))
#View(test1)  

## spc plot ----
### year by spp ----
library(ggplot2)
p <- ggplot(
            df_sum,
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

### subset ----
p <- ggplot(
  #df_sum[df_sum$Species == "AS",], 
  #df_sum[df_sum$Species == "AS" & df_sum$Station == 8,], 
  df_sum[df_sum$Species == "AS" & df_sum$Year == 2016,], 
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

plotly:: ggplotly(p, tooltip = "text")

## GF ----
## need to set up for CS and then calculate GF based on L243

# but first, let's get some summary stats
## number of sweeps per station
library(tidyr)
df_sum |>
  group_by(Year, Species, Station) |>
  summarize(Sweeps = length(Sweep)) |>
  pivot_wider(id_cols = c(Year, Species), 
              names_from = Station, values_from = Sweeps)

# zeros - 17 of these
df_sum |>
  filter(Sweep == 1 & abun == 0)



# tabulate so that catches are columns - required for FSA::removal
library(tidyr)
df_tab1 <- df_sum |>
  group_by(Year, Species, Station) |>
  #filter(Sweep <=3 & length(Sweep) > 1 | is.na(Sweep == 2)) |>
#  filter(length(Sweep) > 1 & Sweep <= 3) |>
  filter(length(Sweep) > 1 & Sweep <= 3) |>
#& !is.null(Sweep == 2) | length(Sweep) > 1 & !is.null(Sweep == 3)) |>
  ungroup() |>
  pivot_wider(id_cols = c(Year, Species, Station), 
              names_from = Sweep, values_from = abun) |> #bio.sum abun
  filter(!(is.na(`2`) & is.na(`3`))) 
df_tab1 |> print(n = Inf)

library(FSA)
#res_list <- apply(df_tab1[c(1:2, 4:10), c(4:6)], MARGIN=1, FUN = removal, method = "CarleStrub") # 
res_list <- apply(df_tab1[, c(4:6)], MARGIN=1, FUN = removal, method = "CarleStrub") # takes NA's


# this works but only when three catches so maybe that is fine - filter above on this.
out <- as.data.frame(matrix(NA, length(res_list), 11))
colnames(out) <- c("c1",  "c2", "c3","k",  "T", "X", 
                   "No",  "No.se", "No.LCI", 
                   "No.UCI", "p"
)

# loop to extract output from res_list when catch 2 or catch 3 is NA
# for(i in seq_along(res_list)){
#   if(length(res_list[[i]]$catch) ==3){
#     out[i,] <- round(c(res_list[[i]]$catch, 
#                        res_list[[i]]$int, 
#                        res_list[[i]]$est[1:5]), 2)
#     
#   }
# }
# out
# out <- cbind(sta = pm16_tab1$Station[c(1:10)], out)


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


out <- cbind(year = df_tab1$Year, 
             spp = df_tab1$Species, 
             sta = df_tab1$Station, 
             out)

View(out)
head(out)
out |> filter(spp == "AS" & year == 2016)


### GF calc ----

out$GF <- with(out, round((c1 - (No*p))^2/No*p + 
                            (c2 - No*(1-p)*p)^2/(No*(1-p)*p) +
                            (c3 - (No*(1-p)^2*p))^2/(No*(1-p)^2*p)
                          ,4)             
)


1-pchisq(1.2813, 2) #- this is the pvalue
qchisq(1-0.5269498, 2) #- returns the test statistic
qchisq(0.95, 1) #- gives the critical test 3.84

# total year:spp:site:catch
nrow(df_sum)

# total year:spp:site
df_sum |> group_by(Year, Species, Station) |> 
  summarise (catch_num = n()) |> 
  ungroup() |>
  summarise(tot = n())

length(unique(df_sum$Year))
length(unique(df_sum$Station))

# sum catches by year and species
df_tab1 |>
  group_by(Year, Species) |>
  summarise(sum_c1 = sum(`1`, na.rm = T),
            sum_c2 = sum(`2`, na.rm = T),
            sum_c3 = sum(`3`, na.rm = T)
            )

# sum catches by year
df_tab1 |>
  group_by(Species) |>
  summarise(sum_c1 = sum(`1`, na.rm = T),
            sum_c2 = sum(`2`, na.rm = T),
            sum_c3 = sum(`3`, na.rm = T)
  )


# filter out sites with only 1 catch or where c2 & c3 == NA
nrow(out)
out |> filter(GF > qchisq(0.95, 1)) # 10 sites don't make GF with T > 30 on 5 sites 
nrow(out |> filter(GF > qchisq(0.95, 1)))
out |> filter(T < 30)
nrow(out |> filter(T < 30)) # 97 of 124
nrow(out |> filter(T < 20)) # 85 of 124
nrow(out |> filter(T < 10)) # 57 of 124

# density of total catch
plot(density(out$T))


p <- ggplot(out, aes(x = T, y = No, group = as.factor(spp), colour = spp)) +
  geom_point()
p


## Hedger ----
### from Hedger et al

# calibration site
df_cal <- out |>
  filter(X > 30 & GF < qchisq(0.95, 1)) |>
  group_by(year) |>
  filter(length(year) >3)
df_cal |> print(n = Inf)


df_cal <- out |>
  group_by(sta, spp) |>
  filter(n() > 3) |>
  arrange(spp, sta, year) #|>
#  ungroup() |>
#  filter(X > 20 & GF < qchisq(0.95, 1)) 
df_cal |> print(n = Inf)
View(df_cal)

df_var_test <- out |>
  group_by(spp, sta) |>
  summarise(var = var(T))

df_var_test |> print(n = Inf)


### Prob capture[i] = number captured[i]/T - number captured[i-1]
### then, apply this from the calibration stream to all sites to get N = number captured[1]/prob capture.  



# view
library(tidyr)
df_view <- df_sum |>
  filter(Species == "AS") |>
  pivot_wider(id_cols = c(Year, Species, Sweep),
              names_from = Station, 
              values_from =abun) 
df_view |> print(n=Inf)

# calculate T in order to find site with greatest variance
df_tabT <- df_sum |>
  summarise(T = sum(abun)) |> 
  pivot_wider(id_cols = c(Year, Species),
              names_from = Station, 
              values_from =T)

df_tabT |> print(n=Inf)
str(df_tabT, give.attr = F)

df_tabT[df_tabT$Species == "AS",]
vt <- c(2, "NA", 2, 21, 36)
vt <- df_tabT[df_tabT$Species == "AS", 3]
var(vt, na.rm = T)

###### CHECK THIS####
# check that these are actually the variances of T - they are
## and update this as its deprecated
df_var <- df_tabT |>
  group_by(Species) |>
  summarise(across(!c(Year), .f = var, na.rm=T)) 
df_var

# this is the calibration site, i.e, what site has the max variance
df_max <- df_var |> 
  pivot_longer(!Species, names_to = "Station", values_to = "Variance") |>
  group_by(Species) |>
  slice(which.max(Variance))
df_max

# use these to see if sites in df_max are suitable
target <- c(6, 8, 9)
out |> filter(sta %in% c(6, 8, 9))

species <- "BTYOY"
out |> filter(sta == 9 & spp == species)
df_tabT |> filter(Species == species) |>
  select(Year, Species, `9`)

p <- ggplot(
  #df_sum,
  #df_sum[df_sum$Species == "AS",], 
  df_sum[df_sum$Species == "BTYOY" & df_sum$Station == 6,], 
  #df_sum[df_sum$Species == "AS" & df_sum$Year == 2016,], 
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
  facet_wrap(vars(Year))
p






# sample data ----
a <- sort(rep(1:4, 3))
b <- rep(seq(1,3), 4)
c <- rep(c(10, 5, 1), 4)

df <- as.data.frame(cbind(a, b, c))
df[-4,]
df[12,3] <- NA


# END ----