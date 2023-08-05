# graph 2 - correlation between average population size and black to white ratio
library(ggplot2)
library(dplyr)

prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")

average_data <- prison %>%
  group_by(state) %>%
  summarize(
    average_prison_pop = mean(total_prison_pop, na.rm = TRUE),
    black_to_white_ratio = sum(black_prison_pop, na.rm = TRUE) / sum(white_prison_pop, na.rm = TRUE)
  )

filtered_average_data <- average_data %>%
  filter(!is.na(average_prison_pop) & !is.na(black_to_white_ratio))

chart_2 <- ggplot(filtered_average_data, aes(x = average_prison_pop, y = black_to_white_ratio)) +
  geom_point() +
  labs(title = "Correlation between Average Prison Population Size and Average Black to White Ratio",
       x = "Average Prison Population Size",
       y = "Average Black to White Ratio") +
  theme_minimal()

print(chart_2)

