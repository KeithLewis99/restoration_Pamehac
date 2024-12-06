# How TF did Scruton get the numbers he did
source("Pam_data_new.R")

### CS ----
library(FSA)
# microfish
res_list <- apply(df_tab3[, c(4:6)], MARGIN=1, FUN = removal, method = "Burnham") # takes NA's

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
out_sum <- out |>
  mutate(type = if_else(sta == "6"|sta == "7", "above", "below")) |>
  group_by(year, spp, type) |>
  summarize(meanNo = mean(No))

ggplot(out_sum, aes(x = year, meanNo, group = type, fill = type)) + geom_col(position = position_dodge()) + facet_wrap(~spp)

out$bio <- NA
str(out, give.attr = F)

tmp3 <- df_a |>
  group_by(Year, Species, type) |>
  summarise(mean_bio = mean(bio))
tmp3

tmp4 <- left_join(out, tmp3, by = c("year" = "Year", "spp" = "Species", "type" = "type"))

# END ----