pacman::p_load(tidyr, dplyr, ggplot2, readr, lubridate)
theme_set(
  theme_minimal() +
    theme(
      plot.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 18),
      axis.text.y = element_blank()
    )
)


# Plot the one NBA game 

source("./2025/McGill_biostatistics/util_NBA.R")

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
