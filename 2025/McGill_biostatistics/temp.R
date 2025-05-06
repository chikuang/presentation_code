pacman::p_load(tidyr, dplyr, ggplot2, readr, lubridate)
library(gganimate)
# install.packages("gifski")
# install.packages("magick")

theme_set(
  theme_classic() +
    theme(
      plot.title = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18),
      axis.text.y = element_blank()
    )
)


# Plot the one NBA game 

source("./2025/util_NBA.R")

# One game plot -----------------------------------------------------------
filename <- "./2025/McGill_biostatistics/data/nba_2019.csv"
ff <- read_csv(file = filename, col_names = T) %>% filter(game_num == 170) %>% 
  time_conversion() %>% avg_tp()

ggplot(ff, aes(x = time, y = home_WP)) + geom_step(col = "red") +
  geom_point(col = "black") +
  xlab("In-game time elapsed (seconds)") + ylab("Home team \n winning probability") + 
  theme(axis.title = element_text(size = rel(1.5))) +
  xlim(0, 3000)

# make animation

p <- ggplot(ff, aes(x = time, y = home_WP)) + 
  geom_step(col = "red") +
  geom_point(col = "black") +
  xlab("In-game time elapsed (seconds)") + 
  ylab("Home team \n winning probability") + 
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.5))) +
  xlim(0, 3000) +
  
  # Add animation: reveal over time
  transition_reveal(time)

# Animate
animate(p, nframes = 100, fps = 10, width = 800, height = 600)
anim_save("./2025/McGill_biostatistics/winning_probability_animation.gif", p)



library(gifski)

# Create the animation object
anim <- animate(p, nframes = 100, fps = 10, width = 800, height = 600, renderer = gifski_renderer())

# Save the animation
anim_save("./2025/McGill_biostatistics/winning_probability_animation.gif", animation = anim)


## Black point only


# Create animation with only black points, no red lines
p_points <- ggplot(ff, aes(x = time, y = home_WP)) + 
  geom_point(col = "black", size = 2) +
  geom_step(col = "red") +
  xlab("In-game time elapsed (seconds)") + 
  ylab("Home team \n winning probability") + 
  theme_minimal() +
  theme(axis.title = element_text(size = rel(1.5))) +
  xlim(0, 3000) +
  
  # Animate points appearing sequentially
  transition_reveal(time)

# Render animation
anim_points <- animate(p_points, nframes = 100, fps = 10, width = 800, height = 600, renderer = gifski_renderer())

# Save animation
anim_save("./2025/McGill_biostatistics/winning_probability_points_only.gif", animation = anim_points)


# accumilated


library(ggplot2)
library(gganimate)
library(dplyr)

# Example: make sure your data has no missing 'time'
ff <- ff %>%
  arrange(time) %>%
  mutate(frame_id = row_number())

# Important check
if (!"frame_id" %in% colnames(ff)) {
  stop("The 'frame_id' column was not created. Check your data!")
}

# Build the plot
p_points <- ggplot(ff, aes(x = time, y = home_WP)) + 
  geom_point(color = "black", size = 3) +
  # xlab("In-game time elapsed (seconds)") + 
  # ylab("Home team \n winning probability") + 
  # theme(axis.title = element_text(size = rel(1.5))) +
  xlim(0, 3000) +
  transition_manual(frame_id, cumulative = TRUE)  # <- keep old points

# Animate
anim_points <- animate(
  p_points, 
  nframes = max(ff$frame_id, na.rm = TRUE), 
  fps = 10, width = 400, height = 400, 
  renderer = gifski_renderer()
)
anim_points
# # Save animation
# anim_save("./2025/McGill_biostatistics/winning_probability_points_accumulate.gif", animation = anim_points)

