
#library(purrr)
## calculate densities
# 1. Load the file and assign it to a variable name of your choice
my_data <- readRDS("Guillaume_hierarchical_modelling/ef_removal_results_d.rds")

# 2. View the contents in a spreadsheet-style grid tab
str(my_data)
View(my_data)

chains_m12_zip <- do.call(rbind, my_data$m12_zip_samples)

# ---- Labels (inherited from M11) ----
sl_levels <- c("AS 1+", "ASYOY 0+", "BT 1+", "BTYOY 0+")
year_levels <- c(1990, 1991, 1992, 1996, 2016)

# --- Group-mean density with proper CrIs ---
#     Average d[i] across sites within each year × species × trt
#     at the MCMC iteration level, then take quantiles.

m12_zip_trt_density <- purrr::map_dfr(year_levels, function(yr) {
  purrr::map_dfr(sl_levels, function(sp) {
    purrr::map_dfr(c(0L, 1L), function(t) {
      
      idx <- which(ef_raw$year == yr & ef_raw$species == sp & dat_m13$treat == t)
      if (length(idx) == 0) return(NULL)
      
      # Extract d[i] chains for all units in this group
      d_chains <- sapply(idx, function(i) chains_m12_zip[, paste0("d[", i, "]")])
      
      # Handle single-site groups (sapply returns vector, not matrix)
      group_mean <- if (is.matrix(d_chains)) rowMeans(d_chains) else d_chains
      q <- unname(quantile(group_mean, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
      
      tibble(
        year    = yr,
        species = sp,
        trt     = t,
        n_sites = length(idx),
        q2.5 = q[1], q25 = q[2], q50 = q[3], q75 = q[4], q97.5 = q[5]
      )
    })
  })
})


## beta-delta graphs