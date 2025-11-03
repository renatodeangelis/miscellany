# Load libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(101)

# Parameters
n <- 100  # Number of time points
x <- 1:n

# Sloping boundaries
m_max <- 1.5 # Slope of y_max (positive slope)
m_min <- -0.7 # Slope of y_min (downward sloping)
y_max <- m_max * x + 2
y_min <- m_min * x + 10

# Generate time series
generate_bounded_series <- function(n, y_min, y_max, slope, offset, noise_sd = 1) {
  y <- numeric(n)
  y[1] <- (y_min[1] + y_max[1]) / 2 + offset  # Start in the middle of the bounds, with offset
  for (t in 2:n) {
    y[t] <- y[t - 1] + slope + rnorm(1, mean = 0, sd = noise_sd)
    y[t] <- pmin(pmax(y[t], y_min[t]), y_max[t])  # Keep within bounds
  }
  return(y)
}

# Time series: y_it (upward slope) and y_jt (downward slope)
y_it <- generate_bounded_series(n, y_min, y_max, slope = 1.1, offset = 1.5)  # Steeper upward slope
y_jt <- generate_bounded_series(n, y_min, y_max, slope = -0.4, offset = 0.5)  # Downward slope

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
  annotate("text", x = max(x), y = y_min[n], label = "Y[min]", parse = TRUE, hjust = 0, vjust = -0.5) +
  annotate("text", x = max(x), y = y_max[n], label = "Y[max]", parse = TRUE, hjust = 0, vjust = -0.5) +
  annotate("text", x = max(x), y = y_it[n], label = "y[it]", parse = TRUE,
           color = "blue", size = 4, fontface = "italic", hjust = -0.1) +
  annotate("text", x = max(x), y = y_jt[n], label = "y[jt]", parse = TRUE,
           color = "red", size = 4, fontface = "italic", hjust = -0.1) +
  labs(
    x = expression(Y[t]),
    y = expression(Y[t+1]),
    title = "Growing Economy with Decline for the Less Advantaged"
  ) +
  scale_x_continuous(limits = c(10, n)) +
  scale_color_manual(values = c("blue", "red")) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),          # Remove numerical labels on both axes
    axis.line = element_line(color = "black", size = 0.5), # Add axis lines
    axis.title.x = element_text(hjust = 1, vjust = 0),  # Move x-axis label to the end
    axis.title.y = element_text(hjust = 1, vjust = 0),  # Move y-axis label to the top
    panel.grid = element_blank(),         # Remove grid lines for a cleaner look
    legend.position = "none",
    plot.margin = margin(10, 40, 10, 10)
  )
