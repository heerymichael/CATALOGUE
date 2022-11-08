
library(tidyverse)
library(ggimage)

# Importing basic data with urls

sample_of_players <- nflreadr::load_rosters("season" = 2022) %>% 
  filter(team == "GB" & position == "WR") %>% 
  select(full_name,
         headshot_url) %>% 
  mutate(y_position = row_number())


# I need a script that creates a face-centered circle crop of the images contained in th
# headshot_url column with the locations contained in a new column that i can use 
# within the the image arguyment of geom_image so I can add a number of circle cropped 
# headshots at one time

ggplot() +
  geom_image(data = sample_of_players,
             aes(x = 1,
                 y = y_position,
                 image = headshot_url),
             asp = 1.618,
             size = 0.1)
             