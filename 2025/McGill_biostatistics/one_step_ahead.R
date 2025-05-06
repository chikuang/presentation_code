library(ggplot2)
library(dplyr)

theme_set(
  theme_void() +
    theme(
      plot.title = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = 16),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank()
    )
)

# Example data: 30 time points, binary outcome, random forecasts
set.seed(536)
forecast_df <- data.frame(
  time = 1:30,
  forecast_prob = runif(30, 0, 1),
  outcome = sample(0:1, 30, replace = TRUE)
)

# First 5 observations (red)
observed_df <- forecast_df %>% filter(time <= 5)

# Blue X points (move up slightly on y-axis)
blue_x_df <- forecast_df %>% filter(time <= 6) |> 
  mutate(y_x = 1.15)  

# Next 5 observations (6–10, orange)
next_obs <- forecast_df %>% filter(time >= 6 & time <= 6)

# Plot
p <- ggplot() +
  # Red points for first 5 observations, same size as orange circles
  # geom_line(data = observed_df,
  #            aes(x = time, y = forecast_prob),
  #            color = "red", size = 2) +
  geom_point(data = observed_df,
             aes(x = time, y = forecast_prob),
             color = "black", size = 6, stroke = 2) +
  # Big blue X slightly above red points
  geom_point(data = blue_x_df,
             aes(x = time, y = y_x),
             shape = 4, size = 6, stroke = 2, color = "blue") +
  
  # Big orange O on observations 6–10
  geom_point(data = next_obs,
             aes(x = time, y = forecast_prob),
             shape = 1, size = 6, stroke = 2, color = "orange") +
  
  # Vertical dotted line at x = 5.5
  geom_vline(xintercept = 5.5,
             linetype = "dotted", color = "black", size = 1) +
  
  # Horizontal separator line above red/orange area
  geom_hline(yintercept = 1.05, linetype = "dashed", color = "gray40") +
  
  ylim(0, 1.2) # + xlim(1, )

# Print plot
print(p)