library(shiny)
library(tidyverse)
library(corrr)
library(DT)
library(bslib)

# -------------------------------------------------------------------------
# GLOBAL FUNCTIONS
# -------------------------------------------------------------------------
find_prospects <- function(need_cat, current_team, team_z, pool) {
  if (is.null(pool) || nrow(pool) == 0) return("No players available")
  
  player_stat_col <- case_when(
    need_cat == "z_off"   ~ "pz_off",
    need_cat == "z_perim" ~ "pz_perim",
    need_cat == "z_inter" ~ "pz_inter",
    need_cat == "z_reb"   ~ "pz_reb",
    need_cat == "z_3pt"   ~ "pz_3pt",
    need_cat == "z_ast"   ~ "pz_ast"
  )
  
  prospects <- pool |> 
    filter(team_abbr != current_team) |> 
    mutate(lift = !!sym(player_stat_col) - team_z) |> 
    arrange(desc(!!sym(player_stat_col))) |> 
    head(5) |> 
    mutate(
      img_url = paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", player_id, ".png"),
      label = paste0(
        "<div style='display: inline-block; text-align: center; margin-right: 15px;'>",
        "<img src='", img_url, "' style='height: 60px; object-fit: cover;' alt='", player_name, "'><br>",
        "<span style='font-size: 0.85em; font-weight: bold;'>", player_name, "</span><br>",
        "<span style='font-size: 0.8em; color: gray;'>(+", round(lift, 2), ")</span>",
        "</div>"
      )
    ) |> 
    pull(label)
  
  if(length(prospects) == 0) return("No targets identified")
  paste(prospects, collapse = "")
}

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Basketball Trade Prospects Analysis",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    title = "Analysis Controls",
    
    h5("Strategy Presets"),
    selectInput("preset", "Choose a Model:",
                choices = c("Custom", "Balanced", "Small Ball", "Lockdown Defense", "Pure Scoring", "Glass Cleaners")),
    
    hr(),
    h5("Category Importance (0-10)"),
    sliderInput("w_off", "Offense", min = 0, max = 10, value = 5, step = 1),
    sliderInput("w_perim", "Perimeter Defense", min = 0, max = 10, value = 5, step = 1),
    sliderInput("w_inter", "Interior Defense", min = 0, max = 10, value = 5, step = 1),
    sliderInput("w_reb", "Rebounding", min = 0, max = 10, value = 5, step = 1),
    sliderInput("w_3pt", "3PT Shooting", min = 0, max = 10, value = 5, step = 1),
    sliderInput("w_ast", "Point Guard Play", min = 0, max = 10, value = 5, step = 1),
    
    actionButton("reset_weights", "Reset to Balanced", 
                 icon = icon("rotate-left"), 
                 class = "btn-outline-secondary btn-sm w-100"),
    
    hr(),
    h5("Normalized Weighting"),
    uiOutput("weight_display"),
    
    hr(),
    h5("Filter Results"),
    selectInput("filter_need", "Filter by Top Need Outcome:",
                choices = c("All", "Scoring/Offense", "Interior Defense", 
                            "Perimeter Defense", "Rebounding", "3pt Shooting","Point Guard Play"),
                selected = "All")
  ),
  navset_card_underline(
    nav_panel("Trade Report", DTOutput("trade_table")),
    nav_panel("Correlation", plotOutput("cor_plot")),
    nav_panel("Trade Deadline Comparison", DTOutput("deadline_results"))
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

  
  # 2. Preset Logic
  presets <- list(
    "Balanced"         = c(5, 5, 5, 5, 5, 5),
    "Small Ball"       = c(7, 8, 2, 2, 10, 9),
    "Lockdown Defense" = c(3, 10, 10, 7, 3, 4),
    "Pure Scoring"     = c(10, 3, 2, 3, 9, 7),
    "Glass Cleaners"   = c(4, 4, 8, 10, 4, 4)
  )
  
  observeEvent(input$preset, {
    if (input$preset == "Custom") return()
    vals <- presets[[input$preset]]
    updateSliderInput(session, "w_off", value = vals[1])
    updateSliderInput(session, "w_perim", value = vals[2])
    updateSliderInput(session, "w_inter", value = vals[3])
    updateSliderInput(session, "w_reb", value = vals[4])
    updateSliderInput(session, "w_3pt", value = vals[5])
    updateSliderInput(session, "w_ast", value = vals[6])
  })
  
  observe({
    current_vals <- c(input$w_off, input$w_perim, input$w_inter, input$w_reb, input$w_3pt, input$w_ast)
    isolate({
      if (input$preset != "Custom") {
        target_vals <- presets[[input$preset]]
        if (!isTRUE(all.equal(current_vals, target_vals))) {
          updateSelectInput(session, "preset", selected = "Custom")
        }
      }
    })
  })
  
  observeEvent(input$reset_weights, {
    updateSelectInput(session, "preset", selected = "Balanced")
  })

  category_weights <- reactive({
    vals <- c(z_off=input$w_off, z_perim=input$w_perim, z_inter=input$w_inter, 
              z_reb=input$w_reb, z_3pt=input$w_3pt, z_ast=input$w_ast)
    total <- sum(vals)
    norm <- if(total==0) rep(1/6, 6) else vals/total
    tibble(category=names(vals), weight=norm, label=c("Offense", "Perim Def", "Inter Def", "Reb", "3PT", "PG Play"))
  })

  # Team Analysis Logic from analysis_initial_testing.R
  team_analysis <- reactive({
    cw <- category_weights()
    raw_data() |> 
      distinct(team_abbr, lineup, minutes, off_pts, deftov, defts, offorb, deforb, off_3pt, ast, offtov) |> 
      group_by(team_abbr) |> 
      summarise(
        avg_off = weighted.mean(off_pts, minutes),
        avg_perim = weighted.mean(deftov, minutes), 
        avg_int = weighted.mean(defts, minutes),
        avg_reb = weighted.mean(offorb + deforb, minutes),
        avg_3pt = weighted.mean(off_3pt, minutes),
        avg_ast = weighted.mean(ast - offtov, minutes), .groups="drop"
      ) |>
      mutate(across(c(avg_off, avg_perim, avg_int, avg_reb, avg_3pt, avg_ast), ~as.numeric(scale(.x)), .names = "z_{.col}")) |>
      rename(z_off=z_avg_off, z_perim=z_avg_perim, z_inter=z_avg_int, z_reb=z_avg_reb, z_3pt=z_avg_3pt, z_ast=z_avg_ast) |>
      pivot_longer(cols=starts_with("z_"), names_to="category", values_to="perf_z") |>
      left_join(cw, by="category") |>
      mutate(weighted_need = (-perf_z) * weight,
             need_label = case_when(
               category=="z_off"~"Scoring/Offense", category=="z_perim"~"Perimeter Defense", 
               category=="z_inter"~"Interior Defense", category=="z_reb"~"Rebounding", 
               category=="z_3pt"~"3pt Shooting", category=="z_ast"~"Point Guard Play"))
  })

  team_trade_summary <- reactive({
    team_analysis() |> 
      group_by(team_abbr) |>
      summarise(need_urgency = round(sum(weighted_need), 3),
                idx = which.max(weighted_need), 
                top_need = need_label[idx], # Matches script: which.max(need_score)
                top_need_cat = category[idx], 
                team_baseline_z = round(perf_z[idx],2), .groups="drop")
  })

  # Fixed Player Pool - Matches script "Untouchables" logic
  player_pool <- reactive({
    df <- raw_data()
    
    untouchable_ids <- df |>
      filter(!duplicated(player_id)) |>
      group_by(team_abbr) |>
      mutate(is_team_leader = (pts == max(pts))) |>
      ungroup() |>
      filter(pts_rank <= 15 | plus_minus_rank <= 15 | age >= 35 | age <= 20 | is_team_leader) |>
      pull(player_id) |> unique()

    df |> 
      filter(!(player_id %in% untouchable_ids)) |> 
      group_by(player_id, player_name, team_abbr) |> 
      summarise(across(c(pts, reb, stl, blk, fg3m, ast, offtov, min), sum, na.rm = TRUE), .groups = "drop") |>
      filter(min > 50) |>
      mutate(
        pz_off   = as.numeric(scale(pts/min)), 
        pz_reb   = as.numeric(scale(reb/min)), 
        pz_perim = as.numeric(scale(stl/min)),
        pz_inter = as.numeric(scale(blk/min)), 
        pz_3pt   = as.numeric(scale(fg3m/min)), 
        # Matches script pz_ast calculation
        pz_ast   = (as.numeric(scale(ast/min)) + (as.numeric(scale(offtov/min)) * -1)) / 2
      )
  })

  output$trade_table <- renderDT({
    pool <- player_pool()
    report <- team_trade_summary() |> 
      rowwise() |> 
      mutate(trade_targets = find_prospects(top_need_cat, team_abbr, team_baseline_z, pool)) |> 
      ungroup() |> 
      select(
        Team = team_abbr, 
        `Need Urgency` = need_urgency, 
        `Top Need (Weighted)` = top_need, 
        `Current Team Z` = team_baseline_z, # This shows the raw performance
        `Trade Targets` = trade_targets
      ) |> 
      arrange(desc(`Need Urgency`))
    
    datatable(report, 
              options = list(pageLength = 30, autoWidth = TRUE), 
              rownames = FALSE, 
              escape = FALSE) |>
      formatStyle('Current Team Z', 
                  color = styleInterval(0, c('red', 'green'))) # Optional: Red if below avg, green if above
  })
  
  output$weight_display <- renderUI({
    w <- category_weights()
    tags$div(style = "font-size: 0.85em; color: #555;",
             lapply(1:6, function(i) tags$div(tags$span(w$label[i]), tags$span(style="float:right; font-weight:bold;", paste0(round(w$weight[i]*100, 1), "%")), tags$br())))
  })

  output$deadline_results <- renderDT({
    df_trades <- read.csv("nba_trades.csv")
    # array that pulls all nba playerids
    player_pool_raw <- 
    # The new pool will be playerids in both player_pool_raw & df_trades
    player_pool <- filter(!(player_pool) %in% df_trades)

    # Display each NBA team, who they "should" have traded for,
    # then who they did trade for (if Applicable)




  })
}

shinyApp(ui, server)

# Nothing is appearing in Correlation Tab