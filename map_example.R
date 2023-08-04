
us_map <- map_data("state")

state_mapping <- data.frame(
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  state_full = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

black_white_ratios_mapped <- sorted_states %>%
  left_join(state_mapping, by = c("state" = "state_abbr")) %>%
  rename(region = state_full) %>%
  mutate(region = tolower(region))
  
# Inspect the black_white_ratios_mapped dataframe
print(black_white_ratios_mapped)

# Merge the mapped black_white_ratios dataframe with the state_shape dataframe
merged_data <- us_map %>%
  left_join(black_white_ratios_mapped, by = "region")
  #filter(!is.na(black_to_white_ratio))

# Inspect the merged_data dataframe
print(merged_data)

ggplot(merged_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_to_white_ratio),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Black to White Ratio")
  

