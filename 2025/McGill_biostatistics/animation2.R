library(ggplot2)
library(gganimate)
library(dplyr)

# Define custom breaks and labels
breaks <- c(360, 1080, 1800, 2520)
labels <- c("Q1", "Q2", "Q3", "Q4")

# Prepare data
ff <- ff %>%
  arrange(time) %>%
  mutate(frame_id = row_number())

# Extract the final point (time = 2880), but replicate across all frames
final_point <- ff %>%
  filter(time == 2880) %>%
  slice(rep(1, max(ff$frame_id, na.rm = TRUE))) %>%
  mutate(frame_id = 1:max(ff$frame_id, na.rm = TRUE))

# Build the plot
p_points <- ggplot(ff, aes(x = time, y = home_WP)) + 
  # Regular black points, animated over time
  geom_point(color = "black", size = 2) +
  
  # Vertical dotted lines at quarters
  geom_vline(xintercept = c(720, 1440, 2160), linetype = "dotted") +
  
  # Big blue X, repeated in all frames
  geom_point(data = final_point, 
             aes(x = time, y = home_WP, group = frame_id),
             color = "blue", shape = 4, size = 6, stroke = 2, inherit.aes = FALSE) +
  
  xlab("Basketball Game") + 
  ylab("Winning Prob.") + 
  
  # Customize x-axis
  scale_x_continuous(breaks = breaks, labels = labels, limits = c(0, 3000)) +
  theme(axis.title = element_text(size = rel(3))) +
  
  # Animate black points accumulating over time
  transition_manual(frame_id, cumulative = TRUE)

# Animate
anim_points <- animate(
  p_points, 
  nframes = max(ff$frame_id, na.rm = TRUE), 
  fps = 15, width = 500, height = 400, 
  renderer = gifski_renderer()
)
anim_points
# Save animation
anim_save("./2025/McGill_biostatistics/winning_probability_points_accumulate.gif", animation = anim_points)