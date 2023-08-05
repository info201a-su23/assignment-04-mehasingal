library(ggplot2)
library(dplyr)

prison <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true")

us_map <- map_data("state")

state_mapping <- data.frame(
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  state_full = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

sorted_states <- prison %>%
  group_by(state) %>%
  summarize(
    black_to_white_ratio = sum(black_prison_pop, na.rm = TRUE) / sum(white_prison_pop, na.rm = TRUE)
  ) %>%
  arrange(desc(black_to_white_ratio))

black_white_ratios_mapped <- sorted_states %>%
  left_join(state_mapping, by = c("state" = "state_abbr")) %>%
  rename(region = state_full) %>%
  mutate(region = tolower(region))

# Merge the mapped black_white_ratios dataframe with the state_shape dataframe
merged_data <- us_map %>%
  left_join(black_white_ratios_mapped, by = "region")

# Define a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

final_map <- ggplot(merged_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_to_white_ratio),
    color = "white", # show state outlines
    linewidth = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(title = "Geographical Distribution of Black to White Prison Population Ratios", fill = "Black to White Ratio") +
  blank_theme # variable containing map styles (defined in next code snippet)

print(final_map)

  

