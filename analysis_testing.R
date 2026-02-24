
library(tidyverse)

# Load the data
df <- read_csv("df_FINAL.csv")

# Calculate team-level performance
team_performance <- df |> 
  distinct(team_abbr, 
           lineup, minutes, 
           off_pts, def_pts, offorb, deforb, off_3pt,
           offts,offtov,defts,deftov,ast) |> 
  group_by(team_abbr) |> 
  summarise(
    avg_off_rtg = weighted.mean(off_pts, minutes),
    avg_def_rtg = weighted.mean(def_pts, minutes),
    avg_reb_rate = weighted.mean(offorb + deforb, minutes),
    avg_3pt_freq = weighted.mean(off_3pt, minutes),
    avg_offtov_freq = weighted.mean(offtov,minutes),
    avg_ts_pct  = weighted.mean(offts, minutes),
    avg_opp_ts   = weighted.mean(defts, minutes),
    avg_forced_tov = weighted.mean(deftov, minutes),
    avg_ast_freq = weighted.mean(ast,minutes),
    total_min = sum(minutes)
  )

# Define category weights (perceived importance for trades)
category_weights <- tibble(
  category = c("z_off", "z_def", "z_reb", "z_3pt","z_ast"),
  weight = c(0.25, 0.25, 0.15, 0.20, 0.15)
)

# Calculate normalized team needs
team_analysis <- team_performance |> 
  mutate(
    z_off = (as.numeric(scale(avg_off_rtg)) + as.numeric(scale(avg_ts_pct)))/2,
    z_def = ((as.numeric(-scale(avg_def_rtg))* -1) + 
    (as.numeric(scale(avg_opp_ts))*-1)/2), 
    z_reb = as.numeric(scale(avg_reb_rate)),
    z_3pt = as.numeric(scale(avg_3pt_freq)),
    z_ast = (as.numeric(scale(avg_ast_freq))+ 
    (as.numeric(scale(avg_offtov_freq))* -1))/2,
  ) |> 
  pivot_longer(cols = starts_with("z_"), names_to = "category", values_to = "perf_z") |> 
  left_join(category_weights, by = "category") |> 
  mutate(
    need_score = -perf_z,
    weighted_need = need_score * weight,
    need_label = case_when(
      category == "z_off" ~ "Scoring/Offense",
      category == "z_def" ~ "Interior/Perimeter Defense",
      category == "z_reb" ~ "Rebounding",
      category == "z_3pt" ~ "3pt Shooting",
      category == "z_ast" ~ "Point Guard Play"
    )
  )

# Summarize team needs
team_trade_summary <- team_analysis |> 
  group_by(team_abbr) |> 
  summarise(
    need_urgency = round(sum(weighted_need), 3),
    top_need = need_label[which.max(need_score)],
    top_need_cat = category[which.max(need_score)],
    team_baseline_z = perf_z[which.max(need_score)]
  )

# Define "Untouchables" to filter the potential trade pool
untouchables <- df |>
  filter(!duplicated(player_id)) |>
  group_by(team_abbr) |>
  mutate(is_team_leader = (pts == max(pts))) |>
  ungroup() |>
  filter(pts_rank <= 15 | plus_minus_rank <= 15 | age <= 20
         | age >= 35 | is_team_leader) |>
  pull(player_name) |>
  unique()

untouchables

# Extract pool of potential trade targets (excluding untouchables)
player_pool <- df |> 
  distinct(player_name, team_abbr, pts, reb, stl, blk, fg3m, ast, tov, min) |> 
  filter(!(player_name %in% untouchables)) |>  # remove untouchables
  mutate(
    pts_pm = pts / min,
    reb_pm = reb / min,
    def_pm = (stl + blk) / min,
    three_pm = fg3m / min,
    ast_pm = ast / min,
    tov_pm = tov / min,
  ) |> 
  mutate(
    pz_off = as.numeric(scale(pts_pm)),
    pz_reb = as.numeric(scale(reb_pm)),
    pz_def = as.numeric(scale(def_pm)),
    pz_3pt = as.numeric(scale(three_pm)),
    pz_ast = (as.numeric(scale(ast_pm))+(as.numeric(scale(tov_pm))*-1))/2
  )

# special function to find trade prospects (and estimated "lift")
find_prospects <- function(need_cat, current_team, team_z, pool) {
  player_stat_col <- case_when(
    need_cat == "z_off" ~ "pz_off",
    need_cat == "z_def" ~ "pz_def",
    need_cat == "z_reb" ~ "pz_reb",
    need_cat == "z_3pt" ~ "pz_3pt",
    need_cat == "z_ast" ~ "pz_ast"
  )
  
  prospects <- pool |> 
    filter(team_abbr != current_team) |> 
    mutate(lift = !!sym(player_stat_col) - team_z) |> 
    arrange(desc(!!sym(player_stat_col))) |> 
    head(3) |> 
    mutate(label = paste0(player_name, " (+", round(lift, 2), " lift)")) |> 
    pull(label)
  
  
  if(length(prospects) == 0) {
    prospects_out <- "No targets identified" 
    }
  else{
    prospects_out <- paste(prospects, collapse = "; ")
  }
  
  return(prospects_out)
}

# generate trade report
final_trade_report <- team_trade_summary |> 
  rowwise() |> 
  mutate(
    trade_targets = find_prospects(top_need_cat, 
                                             team_abbr, 
                                             team_baseline_z, 
                                             player_pool)
  ) |> 
  select(team_abbr, 
         need_urgency, 
         top_need, 
         trade_targets) |> 
  arrange(desc(need_urgency))

# Output
View(final_trade_report)

write.csv(untouchables,"untouchables",row.names = FALSE)
write.csv(final_trade_report, "trade_prospects_out_testing.csv", row.names = FALSE)


# --- New Section: Top 10 Targets per Category ---

# Define the mapping of z-score columns to readable labels
category_map <- tibble(
  category_z = c("pz_off", "pz_reb", "pz_def", "pz_3pt", "pz_ast"),
  label = c("Scoring_Offense", "Rebounding", "Defense", "3pt_Shooting", "Playmaking")
)

# Identify top 10 players per category
top_10_by_category <- player_pool |>
  pivot_longer(
    cols = c(pz_off, pz_reb, pz_def, pz_3pt, pz_ast),
    names_to = "category_z",
    values_to = "z_score"
  ) |>
  left_join(category_map, by = "category_z") |>
  group_by(label) |>
  slice_max(order_by = z_score, n = 10, with_ties = FALSE) |>
  select(Category = label, Player = player_name, Team = team_abbr, Z_Score = z_score) |>
  arrange(Category, desc(Z_Score))

# Save to CSV
write.csv(top_10_by_category, "top_10_targets_by_category.csv", row.names = FALSE)
