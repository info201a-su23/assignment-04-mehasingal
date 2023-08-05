library(ggplot2)
library(dplyr)

prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")


# find the states with the top 10 highest Black to White population ratios

average_prison_ratios <- prison %>%
  group_by(state) %>%
  summarize(
    black_to_white_ratio = sum(black_prison_pop, na.rm = TRUE) / sum(white_prison_pop, na.rm = TRUE)
  )

# sorting in descending order, with the highest ratios at the top and filtering
# for the top 10 states
sorted_ratio_states <- average_prison_ratios %>%
  arrange(desc(black_to_white_ratio)) %>%
  head(10)

# Preparing data for graphs

# extracting only the states from the previously created top 10 states
top_10 <- sorted_ratio_states$state

# finding the average ratios for all states in data set, including population
average_prison_ratios_all <- prison %>%
  group_by(state, year, .groups = "drop") %>%
  summarize(
    average_prison_pop = mean(total_prison_pop, na.rm = TRUE),
    black_to_white_ratio = sum(black_prison_pop, na.rm = TRUE) / sum(white_prison_pop, na.rm = TRUE)
  )

# filtering out the states with missing ratios/populations
filtered_data <- average_prison_ratios_all %>%
  filter(!is.na(average_prison_pop) & !is.na(black_to_white_ratio))

# arranging the data to be in descending order of average prison population sizes 
# and filtering to only include the top 10 states with the highest ratios
sorted_data <- filtered_data %>%
  filter(state %in% top_10) %>%
  arrange(desc(average_prison_pop))


chart_1 <- ggplot(sorted_data, aes(x = year, y = black_to_white_ratio, color = state)) +
  geom_line() +
  labs(title = "Black to White Inmate Population Ratio in Top 10 States Over Time",
       x = "Year",
       y = "Black to White Ratio",
       color = "State") +
  theme_minimal()

print(chart_1)
