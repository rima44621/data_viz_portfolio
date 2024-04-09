install.packages("tidyverse")
install.packages("RColorBrewer")
install.packages("patchwork")
install.packages("ggplot2")
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(ggplot2)

spotify_songs <- tibble(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv'))

# MALE RAPPER DOMINATION
# CREATING THE BAR PLOT
# Subsetting the data for male rappers
male_rappers <- c("Kanye West", "Eminem", "Kendrick Lamar", "Drake")
male_rappers_data <- spotify_songs %>%
  filter(`track_artist` %in% male_rappers)
# Filters the data to include only the specified male rappers

# Grouping the data by artist and calculating the mean track popularity for each artist
artist_popularity_summary <- male_rappers_data %>%
  group_by(`track_artist`) %>%
  summarize(mean_popularity = mean(`track_popularity`))

# Plotting a bar chart with annotations 
male_rapper_bar <- ggplot(artist_popularity_summary, aes(x = reorder(`track_artist`, mean_popularity), y = mean_popularity, fill = `track_artist`)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  geom_text(aes(label = round(mean_popularity, 1)), size = 4.5, colour = "darkred", fontface = "bold",nudge_y = 2 ) + # Annotations 
  scale_fill_brewer(palette = "Paired", direction = -1) +
  labs(title = "Track Popularity of Male Rappers",
       x = "Rapper",
       y = "Mean Track Popularity") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(colour = "darkred", size = 16, face = "bold", family = "Georgia"),
        axis.text.x = element_text(angle = 45, hjust = 0.6, colour = "black", size =11), 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = "Georgia"))  
#Geom_text adds text annotations (mean popularity values) to the bars, with a vertical nudge of 2 units
#scale_fill_brewer sets the color palette for the bars to the "Paired" palette from RColorBrewer

#CREATING THE LINE PLOT
# Converting release date to date format and extracting the year
male_rappers_data <- male_rappers_data %>%
  mutate(release_date = as.Date(`track_album_release_date`, "%Y-%m-%d")) %>%
  mutate(year = lubridate::year(release_date))

#Grouping the data by artist and year and calculating the mean track_popularity for each rapper in each year
artist_year_popularity_summary <- male_rappers_data %>%
  group_by(`track_artist`, year) %>%
  summarize(mean_popularity = mean(`track_popularity`, na.rm = TRUE))

# Creating a line plot
line_plot <- ggplot(artist_year_popularity_summary, aes(x = year, y = mean_popularity, color = `track_artist`, group = `track_artist`)) +
  geom_line(size = 1.2) + 
  geom_text(aes(label = year), size = 2, colour = "darkred", fontface = "bold", nudge_y = 6, hjust = 0.5 ) +
  geom_point(size = 2.5) +
  xlim(min(artist_year_popularity_summary$year, na.rm = TRUE), max(artist_year_popularity_summary$year, na.rm = TRUE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size =11),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", family = "Georgia"),
    axis.title = element_text(colour = "darkred", size = 16, face = "bold", family = "Georgia"), 
    strip.text = element_text(size = 12, face = "bold"),  
    strip.background = element_rect(fill = "white", color = "darkred")
  ) +
  labs(
    title = "Track Popularity of Male Rappers Over Time",
    x = "Year",
    y = "Mean Track Popularity",
    color = "Artist"
  )+
  facet_wrap(~`track_artist`, ncol = 2, scales = "fixed") + # Creates facets for each artist, arranged in 2 columns with fixed scales
  scale_color_brewer(palette = "Paired", direction = -1)

# Combining the plots
combined_plot <- male_rapper_bar / line_plot +
  plot_layout(ncol = 2, widths = c(1, 2)) + # Arranges the plots horizontally, with the line plot twice as wide as the bar plot
  plot_annotation(tag_levels = "A") # Adds panel labels to the combined plot

# Display the combined plot
combined_plot
