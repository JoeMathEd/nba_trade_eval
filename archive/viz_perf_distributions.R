# visualize distributions of performance metrics in the app

# preliminaries (mirrors app, but not reactive)
library(tidyverse)

library(plotly) # for interactive plots
library(beeswarm) # to adjust points that are close

raw_data <- read_csv("df_FINAL.csv", show_col_types = FALSE)

cw <- tibble(category=paste0("z_", c("off", "perim", "inter",
                                     "reb", "3pt", "ast")),
       weight=rep(1/6, 6), 
       label=c("Offense", "Perim Def", "Inter Def", "Reb", "3PT", "PG Play"))

# Team Analysis Logic from analysis_initial_testing.R
team_analysis <- raw_data |> 
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


df_teams <- team_analysis |>
  select(team_abbr, starts_with("avg")) |> 
  distinct()  |> 
  mutate(
    url_team = paste0("https://a.espncdn.com/i/teamlogos/nba/500/", 
                      team_abbr, ".png"),
    swarm_y = swarmy(avg_off, 0, 
                     xsize =5, cex = 5, side = 1, priority = "random")$y
  ) |> 

  mutate(tooltip_html  = paste0(
  "<b>Team:</b> ", team_abbr, "<br>",
  "<b>Offense:</b> ", round(avg_off,1), "<br>",
  "<b>Perimeter Defense:</b> ", round(avg_perim,1), "<br>")
)

# list of logo images
nba_logos <- lapply(1:nrow(df_teams), function(i) {
  list(
    source = df_teams$url_team[i],
    xref = "x",
    yref = "y",
    x = df_teams$avg_off[i],
    y = df_teams$swarm_y[i],
    sizex = 1.5,  # Size in axis units (adjust based on your x-axis scale)
    sizey = 1.5,  # Size in axis units (adjust based on your y-axis scale)
    xanchor = "center",
    yanchor = "middle"
  )
})


plot_teams <- plot_ly(
  data = df_teams, 
  x = ~avg_off, 
  y = ~swarm_y, 
  type = 'scatter', 
  mode = 'markers',
  # Create invisible markers that are large enough to act as our hover/click targets
  marker = list(size = 35, opacity = 0), 
  text = ~tooltip_html, 
  hoverinfo = 'text'
) %>% 
  layout(
    title = "NBA Team Performance - Offense",
    xaxis = list(title = "Offensive Metric", range = c(100, 140)),
    # yaxis = list(title = "Whatever", 
    #              range = c(min(df_teams$swarm_y) - .5,
    #                        max(df_teams$swarm_y)+.5)),
    yaxis = list(visible = FALSE),
    images = nba_logos 
  )

# Display the plot
plot_teams

