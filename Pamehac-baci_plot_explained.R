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
