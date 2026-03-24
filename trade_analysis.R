

# Read in dataset
library(tidyverse)

trades_raw <- read.csv("nba_trades.csv")

player_omit <- c(101108,201144,203486)
date_omit <- as.Date("2026-02-01")

trades <- trades_raw |>
    mutate(trade_date = as.Date(trade_date)) |>
    group_by(playerid) |>
    arrange(desc(trade_date)) |>
    slice_head(n=1) |>
    ungroup() |>
    select(trade_date,player,playerid,trade_new_team) |>
    filter(!(playerid %in% player_omit)) |>
    filter(!(trade_date < date_omit))

view(trades)
write.csv(trades, "nba_trades_cleaned.csv", row.names = FALSE)
