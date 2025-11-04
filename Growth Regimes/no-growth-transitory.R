library(ggplot2)
library(dplyr)

set.seed(111)

n = 100
time = 1:n

y_min <- 2
y_max <- 5

generate_bounded_series <- function(n, y_min, y_max) {
  y <- numeric(n)
  y[1] <- runif(1, y_min, y_max) # Initial value within bounds
  for (t in 2:n) {
    y[t] <- y[t - 1] + rnorm(1, mean = 0, sd = 0.2) # Small random change
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
  annotate("text", x = max(data$time), y = y_it[n], label = "W[it]", parse = TRUE,
           color = "royalblue", size = 5, fontface = "italic", hjust = -0.1) +
  annotate("text", x = max(data$time), y = y_jt[n], label = "W[jt]", parse = TRUE,
           color = "maroon", size = 5, fontface = "italic", hjust = -0.1) +
  labs(
    x = expression(W[t]),
    y = expression(W[t+1]),
    title = "Stationary Economy Where All Inequality is Transitory"
  ) +
  scale_y_continuous(limits = c(0, 7)) +
  scale_color_manual(values = c("royalblue", "maroon")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.title.x = element_text(hjust = 1, vjust = 0),
    axis.title.y = element_text(angle = 0, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 40, 10, 10),
    plot.title = element_text(size = 18)
  )


