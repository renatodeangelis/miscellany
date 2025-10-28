# Load libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(101)

# Parameters
n <- 100  # Number of time points
x <- 1:n

# Sloping boundaries with more distance between y_max and y_min
m_max <- 3/2 # Slope of y_max
m_min <- 3/2 # Slope of y_min
y_max <- m_max * x + 20   # Adjusted to increase distance between y_max and y_min
y_min <- m_min * x - 20    # Adjusted to increase distance between y_max and y_min

# Generate time series with fluctuations
generate_bounded_series <- function(n, y_min, y_max, slope, offset, noise_sd = 6) {
  y <- numeric(n)
  y[1] <- (y_min[1] + y_max[1]) / 2 + offset  # Start in the middle of the bounds, with offset
  for (t in 2:n) {
    y[t] <- y[t - 1] + slope + rnorm(1, mean = 0, sd = noise_sd)  # Add fluctuations
    y[t] <- pmin(pmax(y[t], y_min[t]), y_max[t])  # Keep within bounds
  }
  return(y)
}

# Adjusted slopes and offsets for distinct series (ensuring they stay between the bounds)
y_it <- generate_bounded_series(n, y_min, y_max, slope = 1.2, offset = 3)
y_jt <- generate_bounded_series(n, y_min, y_max, slope = 1.0, offset = 1)

# Create a dataframe for plotting
data <- data.frame(
  x = rep(x, 2),
  value = c(y_it, y_jt),
  series = rep(c("y_it", "y_jt"), each = n)
)

# Create a dataframe for boundaries
boundaries <- data.frame(
  x = x,
  y_max = y_max,
  y_min = y_min
)

# Plot
ggplot() +
  geom_line(data = boundaries, aes(x = x, y = y_max), color = "black") +
  geom_line(data = boundaries, aes(x = x, y = y_min), color = "black") +
  geom_line(data = data, aes(x = x, y = value, color = series), size = 1) +
  annotate("text", x = max(x) * 0.95, y = max(y_max), label = expression(W[max]), hjust = 0, vjust = -0.5) +
  annotate("text", x = max(x) * 0.95, y = max(y_min), label = expression(W[min]), hjust = 0, vjust = 2) +
  annotate("text", x = max(x), y = y_it[n], label = expression(w[it]), color = "blue", size = 4, fontface = "italic", hjust = -0.1) + 
  annotate("text", x = max(x), y = y_jt[n], label = expression(w[jt]), color = "red", size = 4, fontface = "italic", hjust = -0.1) + 
  labs(
    x = expression(W[t]),
    y = expression(W[t+1]),
    title = "Stable Wealth Inequality in a Growing Economy"
  ) +
  scale_x_continuous(limits = c(0, n)) +
  scale_y_continuous(limits = c(min(y_min) - 1, max(y_max) + 1)) +
  scale_color_manual(values = c("blue", "red")) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),          # Remove numerical labels on both axes
    axis.line = element_line(color = "black", size = 0.5), # Add axis lines
    axis.title.x = element_text(hjust = 1, vjust = 0),  # Move x-axis label to the end
    axis.title.y = element_text(angle = 0, hjust = 1, vjust = 1),  # Move y-axis label to the top
    panel.grid = element_blank(),         # Remove grid lines for a cleaner look
    legend.position = "none",
    plot.margin = margin(10, 40, 10, 10)
  )
