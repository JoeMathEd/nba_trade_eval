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
    avg_pointplay_freq = weighted.mean((ast - offtov), minutes),
    
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

    z_ast = as.numeric(scale(avg_pointplay_freq))
  ) |>
  pivot_longer(cols = starts_with("z_"), names_to = "category", values_to = "perf_z") |> 
  left_join(category_weights, by = "category") |> 
  mutate(
    need_score = -perf_z,
    weighted_need = need_score * weight,
    need_label = case_when(
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
    total_ast = sum(ast, na.rm = TRUE),
    total_tov = sum(tov, na.rm = TRUE),
    total_min = sum(min, na.rm = TRUE),
    # Average opponent efficiency faced
    avg_defts = mean(defts, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  filter(!(player_name %in% untouchables), total_min > 50) |> # Filter out low minute players
  mutate(
    ast_pm = total_ast / total_min,
    tov_pm = total_tov / total_min,
  ) |> 
  mutate(
    pz_ast = (as.numeric(scale(ast_pm)) + (as.numeric(scale(tov_pm)) * -1)) / 2
  )

# 8. Prospect function
find_prospects <- function(need_cat, current_team, team_z, pool) {
  player_stat_col <- case_when(
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
  #Make sure to include this and below
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




#APP STUFF

library(shiny)
library(tidyverse)
library(corrr)
library(DT)
library(bslib)

# -------------------------------------------------------------------------
# GLOBAL FUNCTIONS & SETUP
# -------------------------------------------------------------------------
find_prospects <- function(need_cat, current_team, team_z, pool) {
  # Map internal category names to player-level Z-score columns
  player_stat_col <- case_when(

    need_cat == "z_ast" ~ "pz_ast"
  )
  
  if (is.na(player_stat_col)) return("Category mapping error")

  prospects <- pool |> 
    filter(team_abbr != current_team) |> 
    mutate(lift = !!sym(player_stat_col) - team_z) |> 
    arrange(desc(!!sym(player_stat_col))) |> 
    head(3) |> 
    mutate(
      img_url = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", player_id, ".png"),
      label = paste0(
        "<div style='display: inline-block; text-align: center; margin-right: 15px;'>",
        "<img src='", img_url, "' style='height: 60px; object-fit: cover;' onerror=\"this.src='https://stats.nba.com/media/img/no-headshot.png'\" alt='", player_name, "'><br>",
        "<span style='font-size: 0.85em; font-weight: bold;'>", player_name, "</span><br>",
        "<span style='font-size: 0.8em; color: gray;'>(+", round(lift, 2), ")</span>",
        "</div>"
      )
    ) |> 
    pull(label)
  
  if(length(prospects) == 0) return("No targets identified")
  return(paste(prospects, collapse = ""))
}

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Advanced Basketball Trade Analytics",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Analysis Controls",
    h5("Category Weights"),
    sliderInput("w_off", "Offense Weight", min = 0, max = 1, value = 0.25, step = 0.05),
    sliderInput("w_int_def", "Interior Defense", min = 0, max = 1, value = 0.10, step = 0.05),
    sliderInput("w_perim_def", "Perimeter Defense", min = 0, max = 1, value = 0.10, step = 0.05),
    sliderInput("w_ast", "Point Guard Play", min = 0, max = 1, value = 0.20, step = 0.05),
    sliderInput("w_reb", "Rebounding Weight", min = 0, max = 1, value = 0.15, step = 0.05),
    sliderInput("w_3pt", "3PT Shooting Weight", min = 0, max = 1, value = 0.20, step = 0.05),
    
    hr(),
    h5("Filter Results"),
    selectInput("filter_need", "Filter by Top Need Outcome:",
                choices = c("All", "Scoring/Offense", "Interior Defense", 
                            "Perimeter Defense", "Rebounding", "3pt Shooting", "Point Guard Play"),
                selected = "All")
  ),
  
  navset_card_underline(
    nav_panel("Trade Report", DTOutput("trade_table")),
    nav_panel("Correlation Analysis", 
              plotOutput("cor_plot", height = "400px"),
              hr(),
              h5("Correlation Matrix"),
              verbatimTextOutput("cor_matrix_text"))
  )
)

# -------------------------------------------------------------------------
# SERVER
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(file.exists("df_FINAL.csv")) 
    read_csv("df_FINAL.csv", show_col_types = FALSE)
  })
  
  team_performance <- reactive({
    raw_data() |> 
      distinct(team_abbr, lineup, minutes, off_pts, def_pts, offorb, deforb, off_3pt,
               offts, offtov, defts, deftov, ast, stl, blk) |> 
      group_by(team_abbr) |> 
      summarise(
        
        avg_pointplay = weighted.mean(ast - offtov, minutes, na.rm=TRUE),
        .groups = "drop"
      )
  })
  
  category_weights <- reactive({
    tibble(
      category = c("z_off", "z_int_def", "z_perim_def", "z_reb", "z_3pt", "z_ast"),
      weight = c(input$w_off, input$w_int_def, input$w_perim_def, input$w_reb, input$w_3pt, input$w_ast) 
    )
  })
  
  team_analysis <- reactive({
    team_performance() |> 
      mutate(
       
        z_ast = as.numeric(scale(avg_pointplay))
      ) |> 
      pivot_longer(cols = starts_with("z_"), names_to = "category", values_to = "perf_z") |> 
      left_join(category_weights(), by = "category") |> 
      mutate(
        need_score = -perf_z,
        weighted_need = need_score * weight,
        need_label = case_when(
          category == "z_ast" ~ "Point Guard Play"
        )
      )
  })
  
  team_trade_summary <- reactive({
    team_analysis() |> 
      group_by(team_abbr) |> 
      summarise(
        # Urgency is based on the maximum weighted gap (the deepest hole)
        need_urgency = round(max(weighted_need), 3),
        top_need = need_label[which.max(weighted_need)],
        top_need_cat = category[which.max(weighted_need)],
        team_baseline_z = perf_z[which.max(weighted_need)],
        .groups = "drop"
      )
  })
  
  # 6. Untouchables & Player Pool (Static)
  player_pool <- reactive({
    df <- raw_data()
    
    # Define untouchables (Keep existing logic)
    untouchables <- df |>
      filter(!duplicated(player_id)) |>
      group_by(team_abbr) |>
      mutate(is_team_leader = (pts == max(pts, na.rm=TRUE))) |>
      ungroup() |>
      filter(pts_rank <= 15 | plus_minus_rank <= 15 | age <= 20 | age >= 35 | is_team_leader) |>
      pull(player_name) |> unique()
    
    # AGGREGATE HERE to prevent repeated players
    df |> 
      group_by(player_name, player_id, team_abbr) |> 
      summarise(
        ast = sum(ast, na.rm=TRUE),
        tov = sum(tov, na.rm=TRUE),
        min = sum(min, na.rm=TRUE),
        .groups = "drop"
      ) |> 
      filter(!(player_name %in% untouchables), min > 50) |> 
      mutate(
        ast_pm = ast/min, tov_pm = tov/min,
        deftov_pm = deftov/min
      ) |> 
      mutate(
        pz_ast = (as.numeric(scale(ast_pm)) + (as.numeric(scale(tov_pm) * -1))) / 2
      )
  })
  
  final_trade_report <- reactive({
    report <- team_trade_summary() |> 
      rowwise() |> 
      mutate(trade_targets = find_prospects(top_need_cat, team_abbr, team_baseline_z, player_pool())) |> 
      ungroup() |> 
      select(team_abbr, need_urgency, top_need, trade_targets) |> 
      arrange(desc(need_urgency))
    
    if (input$filter_need != "All") report <- report |> filter(top_need == input$filter_need)
    return(report)
  })
  
  output$trade_table <- renderDT({
    datatable(final_trade_report(), 
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames = FALSE, escape = FALSE,
              colnames = c("Team", "Need Urgency", "Top Need", "Suggested Targets"))
  })
  
  cor_data <- reactive({
    team_analysis() |>
      select(team_abbr, category, weighted_need) |>
      pivot_wider(names_from = category, values_from = weighted_need) |>
      select(-team_abbr) |> correlate(quiet = TRUE)
  })
  
  output$cor_plot <- renderPlot({ cor_data() |> rplot() + theme_minimal() })
  output$cor_matrix_text <- renderPrint({ cor_data() |> shave() |> fashion() })
}