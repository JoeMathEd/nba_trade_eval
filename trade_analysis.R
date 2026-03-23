

# Read in dataset
library(tidyverse)

trades_raw <- read.csv("nba_trades.csv")

player_omit <- c(101108,201144,203486)

trades <- trades_raw |>
    mutate(trade_date = as.Date(trade_date)) |>
    group_by(playerid) |>
    arrange(desc(trade_date)) |>
    slice_head(n=1) |>
    ungroup() |>
    select(trade_date,player,playerid,trade_new_team) |>
    filter(!(playerid %in% player_omit))

view(trades)
