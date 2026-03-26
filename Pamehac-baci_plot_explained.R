# BACI plot explained

df <- data.frame(
  time = c("A", "A", "B", "B"), # after - before
  trt = c("B", "A", "B", "A"), # above - below
  rv = c(5, 3, 5, 2)
)
df


ggplot(df, aes(x = reorder(time, desc(time)), y = rv)) +
  geom_point() +
  geom_segment(aes(x = 1, xend = 2, y = 5, yend = 5), 
               color = "blue", linetype = "dashed", size = 1) +  # First horizontal segment
  geom_segment(aes(x = 1, xend = 2, y = 2, yend = 3), 
               color = "red", linetype = "solid", size = 1) +    # Second horizontal segment
  labs(
    title = "BACI Plot showing relationship between \n parameters and variables for Pamehac \n with biomass interaction",
    x = "Time",
    y = "Response Variable"
  ) +
  ylim(0, 7) +
  theme_minimal() +
  annotate("text", x = 2.3, y = 3, label = "Below") +
  annotate("text", x = 2.3, y = 5, label = "Above") +
  annotate("text", x = 2, y = 2, label = "Intercept + \n type") +
  annotate("text", x = 2, y = 4.5, label = "Intercept") +
  annotate("text", x = 1, y = 5.5, label = "Intercept + time") +
  annotate("text", x = 1, y = 3, label = "Intercept + type \n + time + trt:time")

ggsave("output/plot_baci_explained.png", width=10, height=8, units="in")


# BACI demo ----
before <- rep("B", 3)
after <- rep("A", 5)
time <- c(before, after)
time_all <- rep(time, 4)
trt <- sort(rep(c("C", "I"), 16))

rpois_1 <- rnorm(8, 32, 2)
rpois_2 <- rnorm(8, 30, 3)
rpois_3 <- rnorm(3, 29, 3)
rpois_4 <- rnorm(3, 28, 2)
rpois_5 <- rnorm(5, 15, 3)
rpois_6 <- rnorm(5, 16, 2)

rv <- c(rpois_1, rpois_2, rpois_3, rpois_5, rpois_4, rpois_6)

df <- cbind(time_all, trt, rv)


ggplot(df, aes(x = , y = rv, )) +
  geom_point() +
  geom_segment(aes(x = 1, xend = 2, y = 5, yend = 5), 
               color = "blue", linetype = "dashed", size = 1) +  # First horizontal segment
  geom_segment(aes(x = 1, xend = 2, y = 2, yend = 3), 
               color = "red", linetype = "solid", size = 1) +    # Second horizontal segment
  labs(
    title = "BACI Plot showing relationship between \n parameters and variables for Pamehac \n with biomass interaction",
    x = "Time",
    y = "Response Variable"
  )





library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Design parameters ----
n_before <- 3
n_after  <- 5

time_points <- seq_len(n_before + n_after)

period <- factor(
  ifelse(time_points <= n_before, "Before", "After"),
  levels = c("Before", "After")
)

# Means by site and period
mean_ctrl_before <- 30
mean_ctrl_after  <- 30
mean_imp_before  <- 30
mean_imp_after   <- 16

sd_common <- 3  # tweak for more/less variability

# ---- Simulate data ----
dat <- tibble(
  Time   = rep(time_points, times = 2),
  Period = rep(period, times = 2),
  Site   = rep(c("Control", "Impact"), each = length(time_points))
) |>
  mutate(
    mu = case_when(
      Site == "Control" & Period == "Before" ~ mean_ctrl_before,
      Site == "Control" & Period == "After"  ~ mean_ctrl_after,
      Site == "Impact"  & Period == "Before" ~ mean_imp_before,
      Site == "Impact"  & Period == "After"  ~ mean_imp_after
    ),
    Value = rnorm(n = n(), mean = mu, sd = sd_common)
  )

# ---- Plot ----
ggplot(dat, aes(x = Time, y = Value, color = Site)) +
  geom_vline(xintercept = n_before + 0.5, linetype = "dashed", color = "grey40") +
  annotate("text", x = n_before + 0.5, y = max(dat$Value) + 1,
           label = "Before / After", vjust = 0, hjust = 0.5, size = 3.5, color = "grey20") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Control" = "#1f77b4", "Impact" = "#d62728")) +
  scale_x_continuous(breaks = time_points) +
  labs(
    title = "Simple BACI Design: Control vs Impact",
    subtitle = paste0(n_before, " Before periods and ", n_after, " After periods"),
    x = "Time",
    y = "Response",
    color = "Site"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )
