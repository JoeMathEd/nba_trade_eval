library(tidyverse)

# 1. Load the data
df <- read_csv("df_FINAL.csv")

# 2. Calculate team-level performance
team_performance <- df |> 
  distinct(team_abbr, 
           lineup, minutes, 
           off_pts, def_pts, offorb, deforb, off_3pt,
           offts, offtov, defts, deftov, ast, stl, blk) |> 
  group_by(team_abbr) |> 
  summarise(
    avg_off_rtg = weighted.mean(off_pts, minutes),
    avg_3pt_freq = weighted.mean(off_3pt, minutes),
    avg_pointplay_freq = weighted.mean((ast - offtov), minutes),
    avg_ts_pct = weighted.mean(offts, minutes),
    avg_reb_rate = weighted.mean(offorb + deforb, minutes),
    
    # Interior Defense Components (Efficiency prevention + Rim Protection)
    avg_opp_ts = weighted.mean(defts, minutes),
    avg_blocks = weighted.mean(blk, minutes),
    
    # Perimeter Defense Components (Ball Pressure + Turnovers)
    avg_forced_tov = weighted.mean(deftov, minutes),
    avg_steals = weighted.mean(stl, minutes),
    
    total_min = sum(minutes)
  )

# 3. Define category weights
category_weights <- tibble(
  category = c("z_off", "z_int_def", "z_perim_def", "z_reb", "z_3pt", "z_ast"),
  weight = c(0.25, 0.10, 0.10, 0.15, 0.20, 0.20)
)

# 4. Calculate normalized team needs
team_analysis <- team_performance |> 
  mutate(
    z_off = (as.numeric(scale(avg_off_rtg)) + as.numeric(scale(avg_ts_pct))) / 2,
    
    # Interior: High blocks is good (+), High opponent TS is bad (-)
    z_int_def = (as.numeric(scale(avg_blocks)) + (as.numeric(scale(avg_opp_ts)) * -1)) / 2,
    
    # Perimeter: High steals is good (+), High forced turnovers is good (+)
    z_perim_def = (as.numeric(scale(avg_steals)) + as.numeric(scale(avg_forced_tov))) / 2,
    
    z_reb = as.numeric(scale(avg_reb_rate)),
    z_3pt = as.numeric(scale(avg_3pt_freq)),
    z_ast = as.numeric(scale(avg_pointplay_freq))
  ) |>
  pivot_longer(cols = starts_with("z_"), names_to = "category", values_to = "perf_z") |> 
  left_join(category_weights, by = "category") |> 
  mutate(
    need_score = -perf_z,
    weighted_need = need_score * weight,
    need_label = case_when(
      category == "z_off" ~ "Scoring/Offense",
      category == "z_int_def" ~ "Interior Defense",
      category == "z_perim_def" ~ "Perimeter Defense",
      category == "z_reb" ~ "Rebounding",
      category == "z_3pt" ~ "3pt Shooting",
      category == "z_ast" ~ "Point Guard Play"
    )
  )

# 5. Summarize team needs
team_trade_summary <- team_analysis |> 
  group_by(team_abbr) |> 
  summarise(
    need_urgency = max(weighted_need),
    top_need = need_label[which.max(weighted_need)],
    top_need_cat = category[which.max(weighted_need)],
    team_baseline_z = perf_z[which.max(weighted_need)]
  )

# 6. Define "Untouchables"
untouchables <- df |>
  filter(!duplicated(player_id)) |>
  group_by(team_abbr) |>
  mutate(is_team_leader = (pts == max(pts))) |>
  ungroup() |>
  filter(pts_rank <= 15 | plus_minus_rank <= 15 | age <= 20 | age >= 35 | is_team_leader) |>
  pull(player_name) |>
  unique()

# 7. Extract player pool with fixed typos and logic
# 7. Extract player pool (Aggregated to 1 row per player)
player_pool <- df |> 
  group_by(player_name, team_abbr, player_id) |> 
  summarise(
    # Sum up totals across all lineups
    total_pts = sum(pts, na.rm = TRUE),
    total_reb = sum(reb, na.rm = TRUE),
    total_stl = sum(stl, na.rm = TRUE),
    total_blk = sum(blk, na.rm = TRUE),
    total_fg3m = sum(fg3m, na.rm = TRUE),
    total_ast = sum(ast, na.rm = TRUE),
    total_tov = sum(tov, na.rm = TRUE),
    total_deftov = sum(deftov, na.rm = TRUE),
    total_min = sum(min, na.rm = TRUE),
    # Average opponent efficiency faced
    avg_defts = mean(defts, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  filter(!(player_name %in% untouchables), total_min > 50) |> # Filter out low minute players
  mutate(
    pts_pm = total_pts / total_min,
    reb_pm = total_reb / total_min,
    three_pm = total_fg3m / total_min,
    ast_pm = total_ast / total_min,
    tov_pm = total_tov / total_min,
    stl_pm = total_stl / total_min,
    blk_pm = total_blk / total_min,
    deftov_pm = total_deftov / total_min
  ) |> 
  mutate(
    pz_off = as.numeric(scale(pts_pm)),
    pz_reb = as.numeric(scale(reb_pm)),
    pz_int_def = (as.numeric(scale(blk_pm)) + (as.numeric(scale(avg_defts)) * -1)) / 2,
    pz_perim_def = (as.numeric(scale(stl_pm)) + as.numeric(scale(deftov_pm))) / 2,
    pz_3pt = as.numeric(scale(three_pm)),
    pz_ast = (as.numeric(scale(ast_pm)) + (as.numeric(scale(tov_pm)) * -1)) / 2
  )

# 8. Prospect function
find_prospects <- function(need_cat, current_team, team_z, pool) {
  player_stat_col <- case_when(
    need_cat == "z_off" ~ "pz_off",
    need_cat == "z_int_def" ~ "pz_int_def",
    need_cat == "z_perim_def" ~ "pz_perim_def",
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
  
  if(length(prospects) == 0) return("No targets identified")
  return(paste(prospects, collapse = "; "))
}

# 9. Generate final trade report
final_trade_report <- team_trade_summary |> 
  rowwise() |> 
  mutate(
    trade_targets = find_prospects(top_need_cat, team_abbr, team_baseline_z, player_pool)
  ) |> 
  select(team_abbr, need_urgency, top_need, trade_targets) |> 
  arrange(desc(need_urgency))

# 10. Top 10 Targets per Category
category_map <- tibble(
  category_z = c("pz_off", "pz_reb", "pz_int_def", "pz_perim_def", "pz_3pt", "pz_ast"),
  label = c("Scoring_Offense", "Rebounding", "Interior_Defense", "Perimeter_Defense", "3pt_Shooting", "Playmaking")
)

top_10_by_category <- player_pool |>
  pivot_longer(
    cols = c(pz_off, pz_reb, pz_int_def, pz_perim_def, pz_3pt, pz_ast),
    names_to = "category_z",
    values_to = "z_score"
  ) |>
  left_join(category_map, by = "category_z") |>
  group_by(label) |>
  slice_max(order_by = z_score, n = 10, with_ties = FALSE) |>
  select(Category = label, Player = player_name, Team = team_abbr, Z_Score = z_score) |>
  arrange(Category, desc(Z_Score))

# --- File Outputs ---
write.csv(final_trade_report, "trade_prospects_out_testing.csv", row.names = FALSE)
write.csv(top_10_by_category, "top_10_targets_by_category.csv", row.names = FALSE)