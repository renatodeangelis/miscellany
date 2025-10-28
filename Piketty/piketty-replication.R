library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("~/Downloads/NievasPiketty2025")

for_wealth = read_xlsx("NievasPiketty2025AppendixHistorical.xlsx",
                       sheet = "I1a", skip = 3) |>
  select(year:`South & South-East Asia`) |>
  pivot_longer(
    cols = `World`:`South & South-East Asia`,
    names_to = "region",
    values_to = "count")

plot1 = ggplot(for_wealth, aes(x = year, y = count, color = region)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

net_cur_acc = for_wealth = read_xlsx("NievasPiketty2025AppendixHistorical.xlsx",
                                     sheet = "H1a", skip = 3) |>
  select(year:`South & South-East Asia`) |>
  pivot_longer(
    cols = `World`:`South & South-East Asia`,
    names_to = "region",
    values_to = "count")

plot2 = ggplot(net_cur_acc, aes(x = year, y = count, color = region)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")
