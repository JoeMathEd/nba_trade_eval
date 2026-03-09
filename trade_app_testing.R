
# # Download and install packages if not already installed
# packages <- c("shiny", "tidyverse", "corrr", "DT", "bslib")
# installed_packages <- rownames(installed.packages())
# for (pkg in packages) {
#   if (!(pkg %in% installed_packages)) {
#     install.packages(pkg)
#   }
# }
# Load required libraries
library(shiny)
library(tidyverse)
library(corrr)
library(DT)
library(bslib)

# -------------------------------------------------------------------------
# GLOBAL FUNCTIONS & SETUP
# -------------------------------------------------------------------------
# Helper function to find trade prospects
find_prospects <- function(need_cat, current_team, team_z, pool) {
  player_stat_col <- case_when(
    need_cat == "z_off" ~ "pz_off",
    need_cat == "z_perim" ~ "pz_perim",
    need_cat == "z_inter" ~ "pz_inter",
    need_cat == "z_reb" ~ "pz_reb",
    need_cat == "z_3pt" ~ "pz_3pt",
    need_cat == "z_ast" ~ "pz_ast"
  )
  
  prospects <- pool |> 
    filter(team_abbr != current_team) |> 
    mutate(lift = !!sym(player_stat_col) - team_z) |> 
    arrange(desc(!!sym(player_stat_col))) |> 
    head(3) |> 
    mutate(
      # Construct the image URL
      img_url = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", player_id, ".png"),
      
      # Build an HTML block for each prospect
      label = paste0(
        "<div style='display: inline-block; text-align: center; margin-right: 15px;'>",
        "<img src='", img_url, "' style='height: 60px; object-fit: cover;' alt='", player_name, "'><br>",
        "<span style='font-size: 0.85em; font-weight: bold;'>", player_name, "</span><br>",
        "<span style='font-size: 0.8em; color: gray;'>(+", round(lift, 2), ")</span>",
        "</div>"
      )
    ) |> 
    pull(label)
  
  if(length(prospects) == 0) {
    prospects_out <- "No targets identified" 
  } else {
    prospects_out <- paste(prospects, collapse = "")
  }
  
  return(prospects_out)
}

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Basketball Trade Prospects Analysis",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Analysis Controls",
    
    h5("Category Weights"),
    sliderInput("w_off", "Offense Weight", min = 0, max = 1, value = 0.20, step = 0.05),
    sliderInput("w_perim", "Perimeter Defense Weight", min = 0, max = 1, value = 0.20, step = 0.05),
    sliderInput("w_inter", "Interior Defense Weight", min = 0, max = 1, value = 0.20, step = 0.05),
    sliderInput("w_reb", "Rebounding Weight", min = 0, max = 1, value = 0.10, step = 0.05),
    sliderInput("w_3pt", "3PT Shooting Weight", min = 0, max = 1, value = 0.10, step = 0.05),
    sliderInput("w_ast", "Point Guard Play", min = 0, max = 1, value = 0.20, step = 0.05),

    
    hr(),
    
    h5("Filter Results"),
    selectInput("filter_need", "Filter by Top Need Outcome:",
                choices = c("All", "Scoring/Offense", "Interior Defense", 
                            "Perimeter Defense", "Rebounding", "3pt Shooting","Point Guard Play"),
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
  
  # 1. Load Data
  raw_data <- reactive({
    req(file.exists("df_FINAL.csv")) 
    read_csv("df_FINAL.csv", show_col_types = FALSE)
  })
  
  # 2. Base Team Performance (Static across weight changes)
  team_performance <- reactive({
    raw_data() |> 
      distinct(team_abbr, lineup, minutes, off_pts, def_pts, offorb, deforb, off_3pt, deftov, defts
      , ast, offtov) |> 
      group_by(team_abbr) |> 
      summarise(
        avg_off_rtg = weighted.mean(off_pts, minutes, na.rm=TRUE),
        avg_perim_def = weighted.mean(deftov, minutes, na.rm=TRUE),
        avg_inter_def = weighted.mean(defts, minutes, na.rm=TRUE),
        avg_reb_rate = weighted.mean(offorb + deforb, minutes, na.rm=TRUE),
        avg_3pt_freq = weighted.mean(off_3pt, minutes, na.rm=TRUE),
        avg_pointplay = weighted.mean(ast - offtov, minutes, na.rm=TRUE),

        total_min = sum(minutes, na.rm=TRUE),
        .groups = "drop"
      )
  })
  
  # 3. Dynamic Weights based on UI Sliders
  category_weights <- reactive({
    tibble(
      category = c("z_off","z_perim", "z_inter", "z_reb", "z_3pt", "z_ast"),
      weight = c(input$w_off, input$w_perim, input$w_inter, input$w_reb, input$w_3pt, input$w_ast) 
    )
  })
  
  # 4. Team Analysis (Reactive to Weights)
  team_analysis <- reactive({
    team_performance() |> 
      mutate(
        z_off = as.numeric(scale(avg_off_rtg)),
        z_perim = as.numeric(scale(avg_perim_def)),
        z_inter = as.numeric(scale(avg_inter_def)),
        z_reb = as.numeric(scale(avg_reb_rate)),
        z_3pt = as.numeric(scale(avg_3pt_freq)),
        z_ast = as.numeric(scale(avg_pointplay))
      ) |> 
      pivot_longer(cols = starts_with("z_"), names_to = "category", values_to = "perf_z") |> 
      left_join(category_weights(), by = "category") |> 
      mutate(
        need_score = -perf_z,
        weighted_need = need_score * weight,
        need_label = case_when(
          category == "z_off" ~ "Scoring/Offense",
          category == "z_perim" ~ "Perimeter Defense",
          category == "z_inter" ~ "Interior Defense",
          category == "z_reb" ~ "Rebounding",
          category == "z_3pt" ~ "3pt Shooting",
          category == "z_ast" ~ "Point Guard Play"
        )
      )
  })
  
  # 5. Team Trade Summary
  team_trade_summary <- reactive({
    team_analysis() |> 
      group_by(team_abbr) |> 
      summarise(
        need_urgency = round(sum(weighted_need), 3),
        top_need = need_label[which.max(weighted_need)],
        top_need_cat = category[which.max(weighted_need)],
        team_baseline_z = perf_z[which.max(weighted_need)],
        .groups = "drop"
      )
  })
  
  # 6. Untouchables & Player Pool (Static)
  player_pool <- reactive({
    df <- raw_data()
    
    untouchables <- df |>
      filter(!duplicated(player_id)) |>
      group_by(team_abbr) |>
      mutate(is_team_leader = (pts == max(pts, na.rm=TRUE))) |>
      ungroup() |>
      filter(pts_rank <= 15 | plus_minus_rank <= 15 | age >= 35 | is_team_leader) |>
      pull(player_name) |>
      unique()
    
    df |> 
      # Added player_id here to pass it to the function
      # distinct(player_name, player_id, team_abbr, pts, reb, stl, blk, fg3m, ast, offtov, min) |> 
      # filter(!(player_name %in% untouchables)) |>  
      filter(!(player_name %in% untouchables)) |>  
      group_by(player_id, player_name, team_abbr) |> 
      summarise(
        across(c(pts, reb, stl, blk, fg3m, ast, offtov, min), sum, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        pts_pm = pts / min,
        reb_pm = reb / min,
        perim_pm = stl / min,
        inter_pm = blk / min,
        three_pm = fg3m / min,
        ast_pm = ast / min,
        tov_pm = offtov / min
      ) |> 
      mutate(
        pz_off = as.numeric(scale(pts_pm)),
        pz_reb = as.numeric(scale(reb_pm)),
        pz_perim = as.numeric(scale(perim_pm)),
        pz_inter = as.numeric(scale(inter_pm)),
        pz_3pt = as.numeric(scale(three_pm)),
        pz_ast = (as.numeric(scale(ast_pm)) + (as.numeric(scale(tov_pm)) * -1)) / 2
      )
  })
  
  # 7. Final Trade Report
  final_trade_report <- reactive({
    pool <- player_pool()
    
    report <- team_trade_summary() |> 
      rowwise() |> 
      mutate(
        trade_targets = find_prospects(top_need_cat, team_abbr, team_baseline_z, pool)
      ) |> 
      ungroup() |> 
      select(team_abbr, need_urgency, top_need, trade_targets) |> 
      arrange(desc(need_urgency))
    
    # Apply UI Filter
    if (input$filter_need != "All") {
      report <- report |> filter(top_need == input$filter_need)
    }
    
    return(report)
  })
  
  # 8. Render Outputs
  output$trade_table <- renderDT({
    datatable(final_trade_report(), 
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames = FALSE,
              escape = FALSE, # Allows HTML rendering for images
              colnames = c("Team", "Need Urgency", "Top Need", "Suggested Targets"))
  })
  
  # Calculate Correlation Matrix (Reactive)
  cor_data <- reactive({
    team_analysis() |>
      select(team_abbr, category, weighted_need) |>
      pivot_wider(names_from = category, values_from = weighted_need) |>
      select(-team_abbr) |> 
      correlate(quiet = TRUE)
  })
  
  # Render Correlation Plot
  output$cor_plot <- renderPlot({
    cor_data() |> 
      rplot() +
      theme_minimal() +
      labs(title = "Category Needs Correlation Network")
  })
  
  # Render Correlation Text Table
  output$cor_matrix_text <- renderPrint({
    cor_data() |> 
      shave() |> 
      fashion()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)