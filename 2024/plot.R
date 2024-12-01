# Load necessary packages
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate all points (200 points in total)
n_total_points <- 200
time <- runif(n_total_points, min = 0, max = 10)  # No sorting
error <- rnorm(n_total_points, mean = 0, sd = 0.5)  # Add some noise
outcome <- cos(time) + 0.1 * time + error  # Cyclic pattern with slight drift

# Create a complete dataset and shuffle the order of points
complete_data <- data.frame(
  Time = time,
  Time_new = time / max(time),  # Normalize time to [0, 1]
  Outcome = outcome
)
shuffled_data <- complete_data[sample(1:n_total_points), ]  # Randomize the order of points

# Data for 10 points
data_10 <- shuffled_data[1:4, ]

# Data for 200 points
data_200 <- shuffled_data[1:200, ]

# Plot with 10 points
plot_10 <- ggplot() +
  geom_point(data = data_10, aes(x = Time_new, y = Outcome), color = "blue", size = 2) +
  theme_void()  # Remove axis and labels

# Save plot with 10 points
ggsave("plot_with_10_points.png", plot = plot_10, width = 6, height = 4, dpi = 100)
plot_10
# Plot with 200 points
plot_200 <- ggplot() +
  geom_point(data = data_200, aes(x = Time_new, y = Outcome), color = "blue", size = 2) +
  theme_void()  # Remove axis and labels

# Save plot with 200 points
ggsave("plot_with_200_points.png", plot = plot_200, width = 6, height = 4, dpi = 100)


error <- rnorm(n_total_points, mean = 0, sd = 0.5)  # Add some noise
outcome <- cos(time) + 0.1 * time + error




# Create a few lines ------------------------------------------------------


n_total_points <- 100
n_lines <- 5  # Number of lines to display
time <- seq(0, 10, length.out = n_total_points)  # Generate time points

# Create multiple lines with random perturbations
lines_data <- do.call(rbind, lapply(1:n_lines, function(i) {
  error <- rnorm(n_total_points, mean = 0, sd = 0.1)  # Add some noise
  outcome <- 0.3*runif(1, -1, 1) + cos(time) + 0.01 * time + error
  data.frame(Time = time, Outcome = outcome, LineID = as.factor(i))
}))

# Plot the lines
p <- ggplot(lines_data, aes(x = Time, y = Outcome, color = LineID, group = LineID)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") 
p
