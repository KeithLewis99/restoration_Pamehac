# How TF did Scruton get the numbers he did in the 1998 paper
source("Pam_data_new.R")

### CS ----
library(FSA)
# microfish
res_list <- apply(df_tab1[, c(4:6)], MARGIN=1, FUN = removal, method = "Burnham") # takes NA's

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

x <- df_tab3 # df_tab1
# bind year, species and station to res_list
out <- cbind(year = x$Year, 
             spp = x$Species, 
             sta = x$Station, 
             out)

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


# END ----