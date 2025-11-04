library(ggplot2)
library(dplyr)

set.seed(5)

n = 100
time = 1:n

y_min <- 2
y_max <- 8

generate_bounded_series <- function(n, y_min, y_max) {
  y <- numeric(n)
  y[1] <- runif(1, y_min, y_max) # Initial value within bounds
  for (t in 2:n) {
    y[t] <- y[t - 1] + rnorm(1, mean = 0, sd = 0.1) # Small random change
    y[t] <- pmin(pmax(y[t], y_min), y_max) # Keep within bounds
  }
  return(y)
}

y_it <- generate_bounded_series(n, y_min, y_max)
y_jt <- generate_bounded_series(n, y_min, y_max)

data <- data.frame(
  time = rep(1:n, 2),
  value = c(y_it, y_jt),
  series = rep(c("y_it", "y_jt"), each = n)
)

ggplot(data, aes(x = time, y = value, color = series)) +
  geom_line(size = 1) + 
  geom_hline(yintercept = y_min, color = "black") +
  geom_hline(yintercept = y_max, color = "black") +
  annotate("text", x = max(data$time), y = y_min, label = "W[min]", parse = TRUE, hjust = 0, vjust = -0.5) +
  annotate("text", x = max(data$time), y = y_max, label = "W[max]", parse = TRUE, hjust = 0, vjust = -0.5) +
  annotate("text", x = max(data$time), y = y_it[n], label = "w[it]", parse = TRUE,
           color = "royalblue", size = 5, fontface = "italic", hjust = -0.1) +
  annotate("text", x = max(data$time), y = y_jt[n], label = "w[jt]", parse = TRUE,
           color = "maroon", size = 5, fontface = "italic", hjust = -0.1) +
  labs(
    x = expression(W[t]),
    y = expression(W[t+1]),
    title = "Stationary Economy with Permanent Inequality"
  ) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(values = c("royalblue", "maroon")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),          # Remove numerical labels on both axes
    axis.line = element_line(color = "black", size = 0.5), # Add axis lines
    axis.title.x = element_text(hjust = 1, vjust = 0),  # Move x-axis label to the end
    axis.title.y = element_text(angle = 0, hjust = 1, vjust = 1),  # Move y-axis label to the top
    panel.grid = element_blank(),         # Remove grid lines for a cleaner look
    legend.position = "none",
    plot.margin = margin(10, 40, 10, 10),
    plot.title = element_text(size = 18)
  )


