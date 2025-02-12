library(tidyverse)
library(paletteer)

billionaires = read_csv("billionaires.csv") |>
    select(-rank, -image, -previous_worth) |>
    filter(country == "United States")

topline = billionaires |>
    slice(1:20) |>
    summarise(total_worth = sum(current_worth)) |>
    print()

# Combined net worth of top 20 billionaires is 2779 billion (or 2.779 trillion)

# Histogram of 100 richest billionaires

silicon_valley <- c(
  "Elon Musk",         # Tesla, SpaceX (SV-adjacent)
  "Mark Zuckerberg",   # Facebook (Meta)
  "Larry Page",        # Google (Alphabet)
  "Sergey Brin",       # Google (Alphabet)
  "Bill Gates",        # Microsoft
  "Melinda French Gates", # Microsoft, investments
  "Eric Schmidt",      # Google (Alphabet)
  "Dustin Moskovitz",  # Facebook (Meta)
  "Peter Thiel",       # Facebook, investments
  "Jan Koum",          # WhatsApp
  "Brian Armstrong",   # Coinbase (Cryptocurrency)
  "John Doerr",        # Venture Capital (Kleiner Perkins)
  "David Duffield",    # Workday, PeopleSoft (Business Software)
  "Lin Bin",           # Xiaomi (Smartphones, partly SV-linked)
  "Adam Foroughi",     # AppLovin (Marketing software, mobile games)
  "Jay Chaudhry",      # Zscaler (Security software)
  "Michael Dell",      # Dell Technologies
  "Robert Pera",       # Ubiquiti (Wireless networking)
  "Henry Nicholas III",# Broadcom (Semiconductors)
  "Jensen Huang"       # Nvidia (Semiconductors)
)

wall_street <- c(
  "Stephen Schwarzman",  # Blackstone (Private Equity)
  "Ken Griffin",         # Citadel (Hedge Funds)
  "Jeff Yass",           # Susquehanna International Group (Trading, Investments)
  "Steve Cohen",         # Point72, formerly SAC Capital (Hedge Funds)
  "David Tepper",        # Appaloosa Management (Hedge Funds)
  "Ray Dalio",           # Bridgewater Associates (Hedge Funds)
  "Israel Englander",    # Millennium Management (Hedge Funds)
  "Henry Kravis",        # KKR (Private Equity)
  "George Roberts",      # KKR (Private Equity)
  "Leon Black",          # Apollo Global Management (Private Equity)
  "Josh Harris",         # Apollo Global Management, 76ers (Private Equity)
  "Robert F. Smith",     # Vista Equity Partners (Private Equity)
  "James Simons",        # Renaissance Technologies (Hedge Funds) *not in list but notable*
  "Arthur Dantchik"      # Susquehanna International Group (Trading, Investments)
)

billionaires |>
    slice(1:100) |>
    mutate(industry = case_when(name %in% silicon_valley ~ "Silicon Valley",
                                name %in% wall_street ~ "Wall Street",
                                TRUE ~ "Other")) |>
    ggplot(aes(x = current_worth, fill = industry)) +
    geom_dotplot(binwidth = 6, dotsize = 1, method = "histodot",
                 stackgroups = TRUE) +
    theme(panel.grid = element_line()) +
    labs(x = "Net Worth (Billions USD)", y = "Count",
        title = "Distribution of the 100 Richest U.S. Billionaires",
        caption = "Source: Forbes Real-Time Billionaires List",
        guide = guide_legend(reverse = TRUE)) +
    scale_fill_paletteer_d("nbapalettes::heat_vice", direction = -1) +
    scale_x_continuous(breaks = c(10, 50, 100, 200, 300, 400),
                       labels = c("10", "50", "100", "200", "300", "400"))
