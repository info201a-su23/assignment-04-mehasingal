ggplot(sorted_data, aes(x = year, y = black_to_white_ratio, color = state)) +
  geom_line() +
  labs(title = "Black to White Inmate Population Ratio in Top 10 States Over Time",
       x = "Year",
       y = "Black to White Ratio",
       color = "State") +
  theme_minimal()
