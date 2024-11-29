# this is to do the Carle Strub estimates and get T (total Catch) as well as diagnostics.
source("Pam_data_new.R")

### CS ----
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

# bind year, species and station to res_list
out <- cbind(year = df_tab1$Year, 
             spp = df_tab1$Species, 
             sta = df_tab1$Station, 
             out)

#View(out)
head(out)
# out |> filter(spp == "AS" & year == 2016)
# 
# 
# ### GF calc ----
# out$GF <- with(out, round((c1 - (No*p))^2/No*p + 
#                             (c2 - No*(1-p)*p)^2/(No*(1-p)*p) +
#                             (c3 - (No*(1-p)^2*p))^2/(No*(1-p)^2*p)
#                           ,4)             
# )
# str(out)
# write.csv(out, "derived_data/FSA_output.csv")
# dchisq(1.2813, 2) ## What is the likelihood of this value
# pchisq(1.2813, 2) # probability of this value or less - cumulative density
# 1-pchisq(1.2813, 2) #- this is the pvalue
# qchisq(1-0.5269498, 2) #- returns the test statistic
# qchisq(0.95, 1) #- gives the critical test 3.84 - this is right - it should be one df because k-2: 2 df are lost because N is estimated - see Locwood2000
# 
# 
# # filter out sites with only 1 catch or where c2 & c3 == NA
# nrow(out)
# out |> filter(GF > qchisq(0.95, 1)) # 10 sites don't make GF with T > 30 on 5 sites 
# nrow(out |> filter(GF > qchisq(0.95, 1)))
# out |> filter(T < 30)
nrow(out |> filter(T < 30)) # 97 of 124
nrow(out |> filter(T < 20)) # 85 of 124
nrow(out |> filter(T < 10)) # 57 of 124
nrow(out |> filter(T < 5)) # 33 of 124

# density of total catch
plot(density(out$T))
 
 
p <- ggplot(out, aes(x = T, y = No, group = as.factor(spp), colour = spp)) +
  geom_point()
p
# 
# 
# ## GF - pool ----
# res_list_pool <- apply(df_tab_pool[, c(3:5)], MARGIN=1, FUN = removal, method = "CarleStrub") # takes NA's
# 
# 
# # this works but only when three catches so maybe that is fine - filter above on this.
# out_pool <- as.data.frame(matrix(NA, length(res_list_pool), 11))
# colnames(out_pool) <- c("c1",  "c2", "c3","k",  "T", "X", 
#                         "No",  "No.se", "No.LCI", 
#                         "No.UCI", "p"
# )
# 
# 
# for(i in seq_along(res_list_pool)){
#   if(length(res_list_pool[[i]]$catch) ==3){
#     out_pool[i,] <- round(c(res_list_pool[[i]]$catch, 
#                             res_list_pool[[i]]$int, 
#                             res_list_pool[[i]]$est[1:5]), 2)
#     
#   } else if (names(res_list_pool[[i]]$catch[2]) == 2 & 
#              length(res_list_pool[[i]]$catch) == 2){
#     out_pool[i,c(1:2, 4:11)] <- round(c(res_list_pool[[i]]$catch, 
#                                         res_list_pool[[i]]$int, 
#                                         res_list_pool[[i]]$est[1:5]), 2)
#   } else if (names(res_list_pool[[i]]$catch[2]) == 3 & 
#              length(res_list_pool[[i]]$catch) == 2){
#     out_pool[i,c(1, 3:11)] <- round(c(res_list_pool[[i]]$catch, 
#                                       res_list_pool[[i]]$int, 
#                                       res_list_pool[[i]]$est[1:5]), 2)
#   }
# }
# 
# 
# out_pool <- cbind(year = df_tab_pool$Year, 
#                   sta = df_tab_pool$Station, 
#                   out_pool)
# 
# #View(out_pool)
# head(out_pool)
# 
# 
# out_pool$GF <- with(out_pool, round((c1 - (No*p))^2/No*p + 
#                                       (c2 - No*(1-p)*p)^2/(No*(1-p)*p) +
#                                       (c3 - (No*(1-p)^2*p))^2/(No*(1-p)^2*p)
#                                     ,4)             
# )
# 
# out_pool
# qchisq(0.95, 1) #- gives the critical test 3.84
# 
# 
# # for pooled
# # filter out sites with only 1 catch or where c2 & c3 == NA
# nrow(out_pool)
# out_pool |> filter(GF > qchisq(0.95, 1)) # 6 sites don't make GF with T > 30 on 3 sites 
# nrow(out_pool |> filter(GF > qchisq(0.95, 1)))
# out_pool |> filter(T < 30)
# nrow(out_pool |> filter(T < 30)) # 19 of 45
# nrow(out_pool |> filter(T < 20)) # 12 of 45
# nrow(out_pool |> filter(T < 10)) # 6 of 45


# density of total catch
plot(density(out_pool$T))


p <- ggplot(out_pool, aes(x = T, y = No)) +
  geom_point()
p

# tabulations ----
table(out$year, out$spp, out$T)



# END ----