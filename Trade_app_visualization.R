library(shiny)
library(tidyverse)
library(corrr)
library(DT)
library(bslib)
library(plotly)
library(beeswarm)
library(shinyWidgets)

# -------------------------------------------------------------------------
# GLOBAL SETTINGS & FUNCTIONS
# -------------------------------------------------------------------------
# Using a list to prevent the jsonlite "asJSON" warning
metric_choices <- list(
  "Offense" = "z_off",
  "Perimeter Defense" = "z_perim",
  "Interior Defense" = "z_inter",
  "Rebounding" = "z_reb",
  "3PT Shooting" = "z_3pt",
  "Point Guard Play" = "z_ast"
)

find_prospects <- function(need_cat, current_team, team_z, pool) {
  if (is.null(pool) || nrow(pool) == 0) return("No players available")
  
  # Map the team-need category to the player-stat category
  player_stat_col <- case_when(
    need_cat == "z_off"   ~ "pz_off",
    need_cat == "z_perim" ~ "pz_perim",
    need_cat == "z_inter" ~ "pz_inter",
    need_cat == "z_reb"   ~ "pz_reb",
    need_cat == "z_3pt"   ~ "pz_3pt",
    need_cat == "z_ast"   ~ "pz_ast",
    TRUE ~ "pz_off"
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
  title = "NBA Trade Analysis Dashboard",
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
    actionButton("reset_weights", "Reset to Balanced", icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm w-100"),
    hr(),
    h5("Weight Distribution"),
    uiOutput("weight_display")
  ),
  navset_card_underline(
    nav_panel("Trade Report", DTOutput("trade_table")),
    nav_panel("Deadline Comparison", DTOutput("deadline_results")),
    nav_panel("Correlations", 
              plotOutput("cor_plot", height = "500px"),
              hr(),
              verbatimTextOutput("cor_matrix_text")),
    nav_panel("League Distributions", 
              card(
                card_header("Team Performance Distributions"),
                radioGroupButtons(
                  inputId = "target_var", label = NULL, 
                  choices = metric_choices, selected = "z_off",
                  justified = TRUE, checkIcon = list(yes = icon("check"))
                ),
                plotlyOutput("teamPlot", height = "600px")
              )
    )
  )
)

# -------------------------------------------------------------------------
# SERVER
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 1. Load Data with requirement checks
  raw_data <- reactive({
    req(file.exists("df_FINAL.csv"))
    read_csv("df_FINAL.csv", show_col_types = FALSE)
  })

  # 2. Centralized Team Data Processing
  team_summary_data <- reactive({
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
      # FIXED: Using {.col} inside across for proper naming
      mutate(across(starts_with("avg_"), ~as.numeric(scale(.x)), 
                    .names = "z_{str_remove(.col, 'avg_')}")) |>
      mutate(url_team = paste0("https://a.espncdn.com/i/teamlogos/nba/500/", team_abbr, ".png"))
  })
  
  # 3. Presets
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

  category_weights <- reactive({
    vals <- c(z_off=input$w_off, z_perim=input$w_perim, z_inter=input$w_inter, 
              z_reb=input$w_reb, z_3pt=input$w_3pt, z_ast=input$w_ast)
    total <- sum(vals)
    norm <- if(total==0) rep(1/6, 6) else vals/total
    tibble(category=names(vals), weight=norm, label=c("Offense", "Perim Def", "Inter Def", "Reb", "3PT", "PG Play"))
  })

  team_analysis <- reactive({
    cw <- category_weights()
    team_summary_data() |>
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
                top_need = need_label[idx], 
                top_need_cat = category[idx], 
                team_baseline_z = round(perf_z[idx],2), .groups="drop")
  })

  # 4. Player Pool (Fixing the Duplicate Issue)
  player_pool <- reactive({
    df <- raw_data()
    
    untouchable_ids <- df |>
      distinct(player_id, .keep_all = TRUE) |>
      group_by(team_abbr) |>
      mutate(is_team_leader = (pts == max(pts))) |>
      ungroup() |>
      filter(pts_rank <= 15 | plus_minus_rank <= 15 | age >= 35 | age <= 20 | is_team_leader) |>
      pull(player_id) |> unique()

    df |> 
      filter(!(player_id %in% untouchable_ids)) |> 
      # Grouping by ID to ensure distinct players
      group_by(player_id, player_name, team_abbr) |> 
      summarise(across(c(pts, reb, stl, blk, fg3m, ast, offtov, min), sum, na.rm = TRUE), .groups = "drop") |>
      filter(min > 50) |>
      mutate(
        pz_off   = as.numeric(scale(pts/min)), 
        pz_reb   = as.numeric(scale(reb/min)), 
        pz_perim = as.numeric(scale(stl/min)),
        pz_inter = as.numeric(scale(blk/min)), 
        pz_3pt   = as.numeric(scale(fg3m/min)), 
        pz_ast   = (as.numeric(scale(ast/min)) + (as.numeric(scale(offtov/min)) * -1)) / 2
      )
  })

  # 5. Outputs
  output$trade_table <- renderDT({
    pool <- player_pool()
    report <- team_trade_summary() |> 
      rowwise() |> 
      mutate(trade_targets = find_prospects(top_need_cat, team_abbr, team_baseline_z, pool)) |> 
      ungroup() |> 
      select(Team = team_abbr, `Need Urgency` = need_urgency, `Top Need` = top_need, 
             `Stat Z` = team_baseline_z, `Targets` = trade_targets) |> 
      arrange(desc(`Need Urgency`))
    
    datatable(report, options = list(pageLength = 30, autoWidth = TRUE), rownames = FALSE, escape = FALSE)
  })

  output$deadline_results <- renderDT({
    req(file.exists("nba_trades_cleaned.csv"))
    df_trades <- read_csv("nba_trades_cleaned.csv", show_col_types = FALSE) |> 
      mutate(playerid = as.character(playerid)) # Ensure type match for join
    
    pool_df <- player_pool() |> mutate(player_id = as.character(player_id))
    stat_lookup <- c("z_off"="pz_off", "z_perim"="pz_perim", "z_inter"="pz_inter", "z_reb"="pz_reb", "z_3pt"="pz_3pt", "z_ast"="pz_ast")

    trade_comparison <- team_trade_summary() |> 
      rowwise() |> 
      mutate(
        target_col = stat_lookup[top_need_cat],
        trade_targets = find_prospects(top_need_cat, team_abbr, team_baseline_z, pool_df),
        actual_trades = {
          col_name <- target_col
          base_z <- team_baseline_z
          df_trades |> 
            filter(trade_new_team == team_abbr) |> 
            distinct(playerid, .keep_all = TRUE) |>
            inner_join(pool_df, by = c("playerid" = "player_id")) |> 
            mutate(lift = get(col_name) - base_z,
                   label = paste0("<div style='display:inline-block; text-align:center; margin-right:10px;'><img src='https://cdn.nba.com/headshots/nba/latest/1040x760/", playerid, ".png' style='height:50px;'><br><b>", player, "</b><br>(", sprintf("%+.2f", lift), ")</div>")) |> 
            pull(label) |> paste(collapse = "")
        }
      ) |> ungroup() |> select(Team = team_abbr, `Need` = top_need, `Suggested` = trade_targets, `Actual` = actual_trades)

    datatable(trade_comparison, options = list(pageLength = 30, autoWidth = TRUE), rownames = FALSE, escape = FALSE)
  })

  output$cor_plot <- renderPlot({
    cd <- team_analysis() |>
      select(team_abbr, category, weighted_need) |>
      pivot_wider(names_from = category, values_from = weighted_need) |>
      select(-team_abbr) |> correlate(quiet = TRUE)
    
    cat_map <- c("z_off"="Offense", "z_perim"="Perim Def", "z_inter"="Inter Def", "z_reb"="Reb", "z_3pt"="3PT", "z_ast"="PG Play")
    long_cor <- cd |> pivot_longer(-term, names_to = "variable", values_to = "correlation") |>
      filter(!is.na(correlation)) |>
      mutate(term = factor(term, levels = names(cat_map), labels = cat_map),
             variable = factor(variable, levels = names(cat_map), labels = cat_map)) |>
      filter(as.numeric(term) < as.numeric(variable))
    
    ggplot(long_cor, aes(x = variable, y = term, fill = correlation)) +
      geom_tile(color = "white") + geom_text(aes(label = sprintf("%.2f", correlation))) +
      scale_fill_gradient2(low = "#e67e22", mid = "white", high = "#2ecc71", midpoint = 0, limit = c(-1, 1)) +
      theme_minimal() + theme(axis.title = element_blank()) + coord_fixed()
  })

  output$teamPlot <- renderPlotly({
    df <- team_summary_data() 
    req(input$target_var)
    
    df_plot <- df |>
      mutate(current_z = .data[[input$target_var]],
             swarm_y = swarmy(current_z, 0, xsize = 2, cex = 2, side = 1, priority = "random")$y,
             tooltip = paste0("<b>", team_abbr, "</b><br>Z-Score: ", round(current_z, 2)))

    nba_logos <- lapply(1:nrow(df_plot), function(i) {
      list(source = df_plot$url_team[i], xref = "x", yref = "y", x = df_plot$current_z[i], y = df_plot$swarm_y[i],
           sizex = 0.45, sizey = 0.45, xanchor = "center", yanchor = "middle")
    })
    
    var_label <- names(metric_choices)[unlist(metric_choices) == input$target_var]
    
    plot_ly(df_plot, x = ~current_z, y = ~swarm_y, type = 'scatter', mode = 'markers',
            marker = list(size = 30, opacity = 0), text = ~tooltip, hoverinfo = 'text') |>
      layout(title = paste("League Distribution:", var_label),
             xaxis = list(title = "Z-Score", range = c(-3.5, 3.5), zeroline = TRUE),
             yaxis = list(visible = FALSE, range = c(-1, max(df_plot$swarm_y) + 1)),
             images = nba_logos)
  })

  output$weight_display <- renderUI({
    w <- category_weights()
    tags$div(style = "font-size: 0.85em; color: #555;",
             lapply(1:6, function(i) tags$div(tags$span(w$label[i]), tags$span(style="float:right; font-weight:bold;", paste0(round(w$weight[i]*100, 1), "%")), tags$br())))
  })
}

shinyApp(ui, server)


# THis is what I got the AI to generate
# However it messed up the other sectons, so I just need to integrate the two