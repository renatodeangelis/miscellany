library(tidyverse)

billionaires = read_csv("billionaires.csv") |>
    select(-rank, -image, -previous_worth) |>
    filter(country == "United States")

topline = billionaires |>
    slice(1:20) |>
    summarise(total_worth = sum(current_worth)) |>
    print()

# Combined net worth of top 20 billionaires is 2779 billion (or 2.779 trillion)

# Histogram of 100 richest billionaires

billionaires |>
    slice(1:100) |>
    ggplot(aes(x = current_worth)) +
    geom_histogram(bins = 100, fill = "steelblue", color = "black") +
    theme_minimal() +
    theme(panel.grid = element_line()) +
    labs(x = "Net Worth (Billions USD)",
        y = "Count", 
        title = "Distribution of the 100 Richest U.S. Billionaires",
        caption = "Source: Forbes Real-Time Billionaires List") +
    scale_x_continuous(breaks = c(seq(0, 10, 10), seq(50, 400, 50)))
