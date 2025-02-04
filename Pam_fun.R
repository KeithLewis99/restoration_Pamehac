
spatialAutoCorrBase_fun <- function(x, y, xtextRes = 0.005, xtextSite = -0.005){
  col = colorRamp(c("red", "white", "blue"))(y$scaledResiduals)
  plot(x$west, x$north, type = 'n')
  points(x = unique(x$west), y = unique(x$north), col = rgb(col, maxColorValue = 255))
  text(unique(x$west)+xtextRes, unique(x$north), labels = unique(x$Station), cex = 0.5)
  text(unique(x$west)+xtextSite, unique(x$north), labels = y$scaledResiduals, cex = 0.5)
}




# xlim=c(345448.6, 345970.3)
#' spatialData_join
#'
#' @param x data set with N, mean, sd, and se for Seal Cove sites
#' @param y - residuals calculated in DHARMa for a glmmTMB object
#' @param z - the coordinates (lat/long) for the Seal Cove sites
#'
#' @return a dataframe with the above dataframes properly bound together
#' @export
#'
#' @examples bt.np.biomass.all <- spatialData_join(bt.np.biomass.station[-4,], bt.glmm1_simres_recalcSpace, coords.np)
#' 
spatialData_join <- function(x, y, z){
  #browser()
  #z <- as.data.frame(matrix(NA, nrow(x), 2)) %>% rename(Station = V1, scResid = temp1)
  temp1 <- y$scaledResiduals # get resids
  group1 <- unique(y$group) # establish the order of resids - see scratch_pad and https://github.com/florianhartig/DHARMa/issues/359 
  temp2 <- as.data.frame(cbind(as.character(group1),temp1)) %>% rename(Station = V1, scResid = temp1)
  z1 <- left_join(temp2, x, by = "Station")
  z2 <- left_join(z1, z, by = c("Station" = "station_way"))
}





#' spatialAutoCorrGG_fun
#'
#' @param x - a dataframe produced by spatialData_join
#' @param xtextRes - a graphical term to move the residual value away from the point
#' @param xtextSite - a graphical term to move the residual value away from the point
#'
#' @return a ggplot of lat v long for Seal Cove sites.  alpha numerics to the right are the Seal Cove station and to the left are the scaled residuals (see spatialAutoCorr_fun above for an explanation).  
#' @export
#'
#' @examples spatialAutoCorrGG_fun(bt.np.biomass.all)
#' 
spatialAutoCorrGG_fun <- function(x, xtextRes = -0.005, xtextSite = 0.005) {
  #browser()
  ggplot(x, aes(x = west, y = north, size = mean)) +
    geom_point() +
    #xlim(345498.6, 345920.3) +
    geom_text(aes(label = Station), check_overlap = T, nudge_x = xtextSite, size = 3) +
    geom_text(aes(label = scResid), nudge_x = xtextRes, size = 3)
}


# x = data, y = non-pool, pool, lunker, z = biomass v density
#' Title
#'
#' @param x 
#' @param y filter variable: pools or non-pools - pool == "yes", non-pool == "no, LUNKER == "lunker"
#' @param z filter variable: density or biomass, density == "d", biomass == "b"
#'
#' @return a ggplot with Seal Cove station on the x-axis and density/biomass on the y-axis with the means and standard deviations by Control and Impact for non-pools, Before-After for Pools, and Lunker/no Lunker for the pools on Impact sites.
#' @export
#'
#' @examples mean_by_site(bt.np.biomass.station, "no", "b")
mean_by_site <- function(x, z){
  #browser()
    ggplot(x, aes(as.factor(Station), mean)) +
      geom_point(size=4, position=position_dodge(1)) +
      theme_bw() +
      theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
      facet_grid(~factor(type, levels = c("above", "below"))) +
#      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
      {if (z == "b"){
        ylab("Mean Biomass Estimate (g/100 sq. m)")
      } else if (z == "d"){
        ylab("Density Estimate (#/100 sq. m)")
      }
      } +
      xlab("Station") +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_blank())
}


#' baci.plot
#' for non-pools only
#' @param x - dataframe with the mean and standard deviations and errors for biomass or density by Treatment (Control:Impact) and Time (Before:After)
#' @param z filter variable: response variable - density or biomass, density == "d", biomass == "b"
#' @return a 
#' @export 
#'
#' @examples baci.plot(bt.np.biomass.baci, "b")
baci.plot <- function(x, z){
  p1 <- ggplot(x, aes(as.factor(Station), mean)) +
    geom_point(size=4, position=position_dodge(1)) +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
    #facet_grid(Treatment ~ Time) +
    facet_grid(forcats::fct_rev(type) ~ forcats::fct_rev(time)) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.5, position=position_dodge(1)) +
    {if (z == "b"){
      ylab("Mean Biomass Estimate (g/100 sq. m)")
    } else if (z == "d"){
      ylab("Density Estimate (#/100 sq. m)")
    }
    } +
    xlab("Station") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())
  return(p1)
}


#' fig.data
#'
#' @param df original dataframe from K. Loughlin with all associated variables for Stations by year, species, treatment, type, and response variables (Density_100, Biomass_100)
#' @param pl filter variable: pools or non-pools - pool == "Yes", non-pool == "No"
#' @param sp filter variable: species BT = brook trout, AS = atlantic salmon, YOY = young of year
#' @param var filter variable: response variable -  density("Biomass_100") or biomass ("Biomass_100")
#'
#'
#' @return a ggplot with Seal Cove station on the x-axis and density/biomass on the y-axis with the means and standard deviations.  Figures are facted by  by Treatment(Control:Impact) and Time(Before:After)
#' @export
#'
#' @examples bt.bio.np.summ <- fig.data(edited_cs_estimates_by_site, "No", "BT", Biomass_100)

fig.data <- function(df, pl, sp, var){
  #browser()
  if(pl == "No"){
    tmp <- df |>
      filter(Pool == pl & Species == sp) |>
      group_by(Year, Type) |>
      summarise(N  = length({{var}}),
                mean = mean({{var}}),
                sd   = sd({{var}}),
                se   = sd / sqrt(N))
    tmp$Type <- as.factor(tmp$Type)
    levels(tmp$Type) <- c("Compensation", "Control", "Compensation", "Downstream")
  } else if (pl == "Yes") {
    tmp <- df |>
      filter(Pool == pl & Species == sp) |>
      group_by(Year, Type) |>
      droplevels() |>
      summarise(N  = length({{var}}),
                mean = mean({{var}}),
                sd   = sd({{var}}),
                se   = sd / sqrt(N)
      )
    
    # creates proper levels 
    tmp$Type <- as.factor(tmp$Type)
    tmp <- droplevels(tmp)
    levels(tmp$Type) <- c("Compensation", "Compensation")
    # creates NA's for 1990
    tmp_row <- tmp[1,]
    tmp_row[1,] <- NA
    tmp_row$Year <- 1990
    tmp_row$Type <- "Compensation"
    tmp_row$Type <- as.factor(tmp_row$Type)
    tmp_row$Year <- as.factor(tmp_row$Year)
    tmp <- rbind(tmp, tmp_row)
  }
  return(tmp)
}




#' fig.np
#'
#' @param df dataframe with mean, sd, and se by year and Type (Control, Compensation, Downstream).  Note that Downstream is another set of Controls added later
#'
#' @return a ggplot with Seal Cove station on the x-axis and density/biomass on the y-axis with the means and standard deviations.  Figures are facted by  by Treatment(Control:Impact) and Time(Before:After)
#' @export
#'
#' @examples p1 <- fig.np(bt.bio.np.summ)
fig.np <- function(df){
  p1 <- ggplot(df, aes(as.factor(Year), mean)) + 
    geom_point(size=4, position=position_dodge(1)) +
    theme_bw() + 
    theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
    facet_grid(~Type) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
    #ylab("Mean Biomass Estimate (g/100 sq. m)") +
    ylab("") +
    xlab("Year") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()) +
    geom_vline(xintercept = 3, linetype = 3)
  return(p1)
}

# p2 <- ggplot(tmp1, aes(as.factor(Year), mean)) + 
#   geom_point(size=4, position=position_dodge(1)) +
#   theme_bw() + 
#   theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
#   facet_grid(~Type) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.5, position=position_dodge(1)) +
#   #  ylab("Mean Biomass Estimate (g/100 sq. m)") +
#   ylab("") +
#   xlab("Year") +
#   theme(panel.grid.minor=element_blank(),
#         panel.grid.major=element_blank()) +
#   geom_vline(xintercept = 3, linetype = 3)


tab_type <- function(df, species, metric){
  #browser()
  df_sum <- df |>
    filter(Species == {{species}}) |>
    group_by(Station, type) 
  df_test <- df_sum |>
    summarise(N = n(),
              mean = mean({{ metric }}, na.rm = T),
              sd = sd({{metric}})
              )
  return(df_test)    
}



tab_baci <- function(df, species, metric){
  #browser()
  df_sum <- df |>
    filter(Species == {{species}}) |>
    group_by(Station, time, type) 
  df_test <- df_sum |>
    summarise(N = n(),
              mean = mean({{ metric }}, na.rm = T),
              sd = sd({{metric}})
    )
  return(df_test)    
}




#' Confidence Interval table
#'
#' @param df dataframe
#' @param name name of the file to be exported, e.g. bt_den for brook trout density or bt for brook trout biomass
#'
#' @return a csv file with the confidence intervals for the glmmTMB object to be used in Overview.Rmd
#' @export
#'
#' @examples
tab.ci <- function(df, name){
  #browser()
  df_ciout <- as.data.frame(confint(df))
  df_ciout <- cbind(parm = rownames(df_ciout), data.frame(df_ciout, row.names = NULL)) 
  colnames(df_ciout)[2] <- "2.5%"
  colnames(df_ciout)[3] <- "97.5%"
  write.csv(df_ciout, paste0("output/", name, "_ci.csv"))
}


#' above_below_year
#'
#' @param df dataframe of year, type, time, abundance
#' @param z filter variable: response variable - density or biomass, density == "d", biomass == "b"
#'
#' @return
#' @export
#'
#' @examples
above_below_year <- function(df, z){
p1 <- ggplot(df, aes(x = as.factor(Year), y = exp(fit), fill = type, colour = type)) + 
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    #facet_wrap(~Species) + 
    theme_bw() + 
    {if (z == "b"){
      ylab("Biomass Estimate (g/100 sq. m)")
    } else if (z == "d"){
      ylab("Density Estimate (#/100 sq. m)")
    }
    } +
  xlab("Year") +
    geom_errorbar(aes(ymax = exp(fit+se.fit*1.96), ymin = exp(fit-se.fit*1.96)), linewidth=1, width=0.15, position=position_dodge(0.5)) +
    geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
    geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
    geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
    theme(legend.title=element_blank()) +
    scale_fill_discrete(name="",
                        breaks=c("above", "below"),
                        labels=c("Above", "Below")) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c("above", "below"),
                        labels=c("Above", "Below")) + 
    theme(legend.position=c(.85, .88))
  return(p1)
}
