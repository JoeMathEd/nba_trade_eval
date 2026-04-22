# NBA Trade Analysis

## Overview
* Runs a Shiny app that allows users to explore NBA teams trade urgency, trade needs, reccomended trade targets, and the impact of trades on team performance
* Users can view each team need and reccomended player to trade for
* Users can adjust sliders to weight different factors such as Offense, 3pt shooting, and Defense to see how it affects the reccomended trade
* Comparison tab of reccomended trades vs actual trades at the NBA trade deadline
* Tab showing distributions of teams for each factor (offense, 3pt shooting, defense)
* Untouchables are players that cannot be traded for, will not appear in reccomended trades

## Data Collection
* Lineup data was scraped from Databallr.com and cleaned using R
* Player data collected from ballr package in R
* Two datasets were merged together to create a comprehensive dataset for analysis (data_merge.R)
* Majority of data cleaning was using R, however some was hand-cleaned

## Analysis 
* Integrate NBA statistical categories (Offense, Perimeter Defense, Interior Defense, 3PT Shooting, Point Guard Play)
* Utilize Z-scores to determine team performance and player lift
* Determine Trade Urgency using weighted Z-scores

## Prerequisites
* install.packages(c("shiny", "tidyverse", "corrr", "DT", "bslib", "plotly", "beeswarm", "shinyWidgets"))

## How to Run
* Clone the repository
* Ensure you have all Prerequisites
* Run OFFICIAL_TRADE_APP.R
* Run "shinyApp(ui, server)" in console

## Credits
* Braden Udager (https://www.linkedin.com/in/braden-udager/)
* Joe Champion

