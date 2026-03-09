library(shiny)
library(tidyverse)
library(corrr)
library(DT)
library(bslib)

# -------------------------------------------------------------------------
# GLOBAL FUNCTIONS & SETUP
# -------------------------------------------------------------------------
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
    nav_panel("Correlation Analysis", 
              plotOutput("cor_plot", height = "500px"), # Slightly taller for heatmap
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
  
  # 3. Dynamic Normalized Weights
  category_weights <- reactive({
    raw_vals <- c(z_off = input$w_off, z_perim = input$w_perim, z_inter = input$w_inter, 
                  z_reb = input$w_reb, z_3pt = input$w_3pt, z_ast = input$w_ast)
    total <- sum(raw_vals)
    norm_vals <- if(total == 0) rep(1/6, 6) else raw_vals / total
    tibble(
      category = names(raw_vals),
      weight = norm_vals,
      label = c("Offense", "Perim Def", "Inter Def", "Reb", "3PT", "PG Play")
    )
  })
  
  output$weight_display <- renderUI({
    w <- category_weights()
    tags$div(style = "font-size: 0.85em; color: #555;",
             lapply(1:nrow(w), function(i) {
               tags$div(tags$span(style="float:left;", w$label[i]),
                        tags$span(style="float:right; font-weight:bold;", paste0(round(w$weight[i]*100, 1), "%")),
                        tags$br())
             })
    )
  })
  
  # 4. Processing Logic
  team_performance <- reactive({
    raw_data() |> 
      distinct(team_abbr, lineup, minutes, off_pts, def_pts, offorb, deforb, off_3pt, deftov, defts, ast, offtov) |> 
      group_by(team_abbr) |> 
      summarise(across(c(off_pts, deftov, defts, offorb, deforb, off_3pt, ast, offtov), 
                       ~weighted.mean(.x, minutes, na.rm=TRUE)), .groups = "drop") |>
      mutate(avg_reb_rate = offorb + deforb, avg_pointplay = ast - offtov)
  })
  
  team_analysis <- reactive({
    
    team_performance() |> 
      mutate(across(c(off_pts, deftov, defts, avg_reb_rate, off_3pt, avg_pointplay), 
                    ~as.numeric(scale(.x)), .names = "z_{.col}")) |>
      rename(z_off = z_off_pts, z_perim = z_deftov, z_inter = z_defts, z_reb = z_avg_reb_rate, z_3pt = z_off_3pt, z_ast = z_avg_pointplay) |>
      pivot_longer(cols = starts_with("z_"), names_to = "category", values_to = "perf_z") |> 
      left_join(category_weights(), by = "category") |> 
      mutate(need_score = -perf_z, 
             weighted_need = need_score * weight,
             need_label = case_when(category == "z_off" ~ "Scoring/Offense", category == "z_perim" ~ "Perimeter Defense",
                                    category == "z_inter" ~ "Interior Defense", category == "z_reb" ~ "Rebounding",
                                    category == "z_3pt" ~ "3pt Shooting", category == "z_ast" ~ "Point Guard Play"))
  })
  
  team_trade_summary <- reactive({
    team_analysis() |> group_by(team_abbr) |> 
      summarise(need_urgency = round(sum(weighted_need), 3), top_need = need_label[which.max(weighted_need)],
                top_need_cat = category[which.max(weighted_need)], team_baseline_z = perf_z[which.max(weighted_need)], .groups = "drop")
  })
  
  player_pool <- reactive({
    df <- raw_data()
    untouchables <- df |> group_by(team_abbr) |> filter(pts == max(pts, na.rm=TRUE) | pts_rank <= 15) |> pull(player_name) |> unique()
    df |> filter(!(player_name %in% untouchables)) |>  
      group_by(player_id, player_name, team_abbr) |> 
      summarise(across(c(pts, reb, stl, blk, fg3m, ast, offtov, min), sum, na.rm = TRUE), .groups = "drop") |>
      mutate(pz_off = as.numeric(scale(pts/min)), pz_reb = as.numeric(scale(reb/min)), pz_perim = as.numeric(scale(stl/min)),
             pz_inter = as.numeric(scale(blk/min)), pz_3pt = as.numeric(scale(fg3m/min)), pz_ast = as.numeric(scale((ast-offtov)/min)))
  })
  
  final_trade_report <- reactive({
    pool <- player_pool()
    report <- team_trade_summary() |> rowwise() |> 
      mutate(trade_targets = find_prospects(top_need_cat, team_abbr, team_baseline_z, pool)) |> ungroup() |> 
      select(team_abbr, need_urgency, top_need, trade_targets) |> arrange(desc(need_urgency))
    if (input$filter_need != "All") report <- report |> filter(top_need == input$filter_need)
    report
  })
  
  # 5. Render Outputs
  output$trade_table <- renderDT({
    datatable(final_trade_report(), options = list(paging = FALSE), rownames = FALSE, escape = FALSE)
  })
  
  # CORRELATION MATRIX CALCULATIONS
  cor_data <- reactive({
    team_analysis() |>
      select(team_abbr, category, weighted_need) |>
      pivot_wider(names_from = category, values_from = weighted_need) |>
      select(-team_abbr) |> 
      correlate(quiet = TRUE)
  })
  
  # FIXED CORRELATION PLOT: Upper Triangular Heatmap Tiles
  output$cor_plot <- renderPlot({
    cd <- cor_data()
    
    # Human-readable labels for the plot
    cat_map <- c("z_off" = "Offense", "z_perim" = "Perim Def", "z_inter" = "Inter Def", 
                 "z_reb" = "Rebounding", "z_3pt" = "3PT", "z_ast" = "PG Play")
    
    long_cor <- cd |> 
      pivot_longer(-term, names_to = "variable", values_to = "correlation") |>
      filter(!is.na(correlation)) |>
      mutate(
        term = factor(term, levels = names(cat_map), labels = cat_map),
        variable = factor(variable, levels = names(cat_map), labels = cat_map)
      ) |>
      # Filter for Upper Triangular logic (Row index < Col index)
      filter(as.numeric(term) < as.numeric(variable))
    
    ggplot(long_cor, aes(x = variable, y = term, fill = correlation)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", correlation)), color = "black", size = 4) +
      scale_fill_gradient2(low = "#e67e22", mid = "white", high = "#2ecc71", 
                           midpoint = 0, limit = c(-1, 1), name="Correlation") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "right"
      ) +
      coord_fixed()
  })
  
  output$cor_matrix_text <- renderPrint({
    cor_data() |> shave() |> fashion()
  })
}

shinyApp(ui = ui, server = server)