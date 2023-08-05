# Loading the dataset

prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")

# Number of observations

prison_obs <- nrow(prison)

# Number of variables 

prison_vars <- ncol(prison)

# load DPLYR package 

library(dplyr)

# total average White population in prisons for whole data set

total_average_white_prison_pop <- mean(prison$white_prison_pop, na.rm = TRUE)

# total average Black population in prisons for whole data set

total_average_black_prison_pop <- mean(prison$black_prison_pop, na.rm = TRUE)

# variable of interest --> proportion of Black population in prisons

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

# filtering the top ratio state
highest_ratio_state <- head(sorted_ratio_states, 1)

# state with the highest Black to White population ratio
highest_state <- highest_ratio_state$state

# highest average Black to White population ratio
highest_ratio <- highest_ratio_state$black_to_white_ratio


