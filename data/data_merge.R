library(tidyverse)

file_5man_edited <- "nba5man_HandEdited_CLEAN_Jan26_2026 .csv"
file_players <- "df_data.csv"


df_5man <- read_csv(file_5man_edited) |> 
  select(- c(lineup_ok, lineup_matches, lineup_remainder) ) |> 
  pivot_longer(starts_with("man"), names_to = "lineup_man", values_to = "player_last_name") |> 
  mutate(lineup_man = gsub("man", "", lineup_man),
         player_last_name = stringi::stri_trans_general(player_last_name, id = "Latin-ASCII")
         ) |> 
  relocate(lineup_man, player_last_name, .after = lineup_num)

df_players <- read_csv(file_players) |> 
  select( - contains("fantasy_pts"))   |> 
  mutate(player_last_name = word(player_name, -1),
         player_last_name = gsub("\\-.*", "", player_last_name),
         player_last_name = str_remove_all(player_last_name, "[[:digit:][:punct:]]"),
         player_last_name = stringi::stri_trans_general(player_last_name, id = "Latin-ASCII")
  ) |> 
  relocate(player_last_name, .after = player_id) |> 
  rename( team_abbr = team_abbreviation  ) 

# are there any overlapping variables?
names(df_players)



intersect(names(df_5man),names(df_players))


# merge data

df_merged <-df_5man |> 
  left_join(df_players)  


df_merged |> 
  filter(is.na(player_id)) |> 
  select(team_abbr, lineup, player_last_name) |> 
  filter(!duplicated(player_last_name)) |> 
  mutate(player_id = NA) |> 
  write_csv("fixes_player_last_name.csv", na = "")



