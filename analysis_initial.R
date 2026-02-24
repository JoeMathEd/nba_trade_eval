
library(tidyverse)

# Load the data
df <- read_csv("df_FINAL.csv")

# Calculate team-level performance
team_performance <- df |> 
  distinct(team_abbr, 
           lineup, minutes, 
           off_pts, def_pts, offorb, deforb, off_3pt) |> 
  group_by(team_abbr) |> 
  summarise(
    avg_off_rtg = weighted.mean(off_pts, minutes),
    avg_def_rtg = weighted.mean(def_pts, minutes),
    avg_reb_rate = weighted.mean(offorb + deforb, minutes),
    avg_3pt_freq = weighted.mean(off_3pt, minutes),
    total_min = sum(minutes)
  )

# Define category weights (perceived importance for trades)
category_weights <- tibble(
  category = c("z_off", "z_def", "z_reb", "z_3pt"),
  weight = c(0.40, 0.40, 0.10, 0.10) 
)

# Calculate normalized team needs
team_analysis <- team_performance |> 
  mutate(
    z_off = as.numeric(scale(avg_off_rtg)),
    z_def = as.numeric(-scale(avg_def_rtg)), 
    z_reb = as.numeric(scale(avg_reb_rate)),
    z_3pt = as.numeric(scale(avg_3pt_freq))
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
      category == "z_3pt" ~ "3pt Shooting"
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
  filter(!duplicated( player_id) ) |>
  group_by(team_abbr) |>
  mutate(is_team_leader = (pts == max(pts))) |>
  ungroup() |>
  filter(pts_rank <= 15 | plus_minus_rank <= 15 | age >= 35 | is_team_leader ) |>
  pull(player_name) |>
  unique()

untouchables

# Extract pool of potential trade targets (excluding untouchables)
player_pool <- df |> 
  distinct(player_name, team_abbr, pts, reb, stl, blk, fg3m, min) |> 
  filter(!(player_name %in% untouchables)) |>  # remove untouchables
  mutate(
    pts_pm = pts / min,
    reb_pm = reb / min,
    def_pm = (stl + blk) / min,
    three_pm = fg3m / min
  ) |> 
  mutate(
    pz_off = as.numeric(scale(pts_pm)),
    pz_reb = as.numeric(scale(reb_pm)),
    pz_def = as.numeric(scale(def_pm)),
    pz_3pt = as.numeric(scale(three_pm))
  )

# special function to find trade prospects (and estimated "lift")
find_prospects <- function(need_cat, current_team, team_z, pool) {
  player_stat_col <- case_when(
    need_cat == "z_off" ~ "pz_off",
    need_cat == "z_def" ~ "pz_def",
    need_cat == "z_reb" ~ "pz_reb",
    need_cat == "z_3pt" ~ "pz_3pt"
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

write.csv(final_trade_report, "trade_prospects_out.csv", row.names = FALSE)


# 1. Prepare the data
team_analysis %>%
  select(category, need_score)  %>%
  ggplot(aes(x = need_score)) +
  geom_histogram(aes(y = ..density..),      # Scale histogram to density
                 bins = 30, 
                 fill = "steelblue", 
                 color = "white") +
  geom_density(color = "red", size = 1) +   # Add the density curve
  facet_wrap(~category, scales = "free") +  # Create a grid for each column
  theme_minimal() +
  labs(title = "Histograms and Density Functions",
       x = "Value",
       y = "Density")


library(corrr)

cor_matrix <- team_analysis %>%
  select(team_abbr, category, need_score) %>%
  pivot_wider(names_from = category, values_from = need_score) %>%
  # 2. Calculate correlations
  correlate() %>%
  # 3. Optional: Make it pretty
  shave() %>% # Removes the redundant upper triangle
  fashion()   # Formats numbers for easy reading

print(cor_matrix)
