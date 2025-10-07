library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)

jordan_career <- read.csv("jordan_career.csv")
lebron_career <- read.csv("lebron_career.csv")
jordan_playoffs <- read.csv("jordan_playoffs.csv")
lebron_playoffs <- read.csv("lebron_playoffs.csv")

# This function calculates the win percentage by season. This does grouping by season and counting the games played and wins to get a win percentage.
calculate_win_percentage_by_season <- function(data) {
  data %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"),
           year = year(date),
           season = ifelse(month(date) < 7, # this is length of NBA season
                           paste(year - 1, year, sep = "-"),
                           paste(year, year + 1, sep = "-"))) %>%
    group_by(season) %>%
    summarize(games_played = n(), 
              wins = sum(grepl("W", result)),
              win_percentage = (wins / games_played) * 100) %>%
    arrange(season)
}

# calculates the win percentages for each players career
jordan_season_stats <- calculate_win_percentage_by_season(jordan_career)
lebron_season_stats <- calculate_win_percentage_by_season(lebron_career)

# Add championships and finals results for each player
jordan_finals <- data.frame(
  year = c(1991, 1992, 1993, 1996, 1997, 1998),
  result = c("W", "W", "W", "W", "W", "W")
)

lebron_finals <- data.frame(
  year = c(2007, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020),
  result = c("L", "L", "W", "W", "L", "L", "W", "L", "L", "W")
)

# Our ui for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Jordan vs LeBron Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Career and Playoff Stats", tabName = "career_playoff", icon = icon("chart-bar")),
      menuItem("Win Percentage by Season", tabName = "win_percentage", icon = icon("chart-line")),
      menuItem("Point Distribution", tabName = "points_distribution", icon = icon("chart-pie")),
      menuItem("Summary", tabName = "summary", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              h2("LeBron James vs. Michael Jordan: WHO DA GOAT???"),
              p("This dashboard provides a statistical comparison between two basketball legends using data from both their regular-season and playoff games."),
              h3("Dataset Overview"),
              p("Data includes per-game statistics includes points, assists, rebounds, and performance metrics for both players, across regular season and playoff games."),
              h3("Research Questions"),
              p("Who has better individual statistics?"),
              p("Who has the greater win percentage?"),
              p("How do their scoring distributions compare?"),
              h3("Player Backgrounds"),
              h4("Michael Jordan"),
              p("6× NBA Champion, 6× Finals MVP, 5× NBA MVP, 10× Scoring Champion, 14× All-Star, Career Points: 32,292, Seasons Played: 15."),
              h4("LeBron James"),
              p("4× NBA Champion, 4× Finals MVP, 4× NBA MVP, All-Time Scoring Leader, 19× All-Star, Career Points: Over 38,000 (and counting), Seasons Played: 20+.")
      ),
      tabItem(tabName = "career_playoff",
              h2("Career and Playoff Statistics"),
              selectInput("stat_choice", "Choose a Statistic:", choices = c("Points" = "pts", "Assists" = "ast", "Rebounds" = "trb", "Blocks" = "blk", "Steals" = "stl")),
              fluidRow(
                column(6, plotOutput("jordan_stat_plot")),
                column(6, plotOutput("lebron_stat_plot"))
              ),
              fluidRow(
                column(6, uiOutput("jordan_stat_text")),
                column(6, uiOutput("lebron_stat_text"))
              )
      ),
      tabItem(tabName = "win_percentage",
              h2("Win Percentage by Season"),
              selectInput("win_player_choice", "Choose a Player:", choices = c("Michael Jordan", "LeBron James")),
              plotOutput("win_percentage_plot")),
      tabItem(
        tabName = "points_distribution",
        h2("Distribution of Points Scored Per Game"),
        selectInput("distribution_player_choice", "Choose a Player:", choices = c("Michael Jordan", "LeBron James")),
        sliderInput("bins", "Number of Bins:", min = 5, max = 50, value = 20),
        plotOutput("points_distribution_plot")
      ),
      tabItem(tabName = "summary",
              h2("Summary"),
              h3("Project Overview"),
              p("This project compares two basketball legends, Michael Jordan and LeBron James, by analyzing their individual statistics, win percentages, and scoring distributions."),
              h3("Research Questions and Answers"),
              h4("Who has better individual statistics?"),
              p("Michael Jordan leads in points per game. LeBron James leads in assists and rebounds."),
              h4("Who has the greater win percentage?"),
              p("Michael Jordan has a greater win percentage, and an undefeated record in the Finals."),
              h4("How do their scoring distributions compare?"),
              p("Jordan’s scoring distribution shows more high-scoring games. LeBron’s distribution is broader, showing a more consistent contribution across games."),
              h3("Reasons to Choose Either Player"),
              p("Jordan: If your're looking for pure dominance in a concentrated time, an undefeated Finals record, and cultural impact."),
              p("Lebron: If you value all-around skill, longevity, and adaptability across eras")
      )
    )
  )
)

# Server logic for dashboard
server <- function(input, output) {
  
  # Function to create a bar plot showing average stat comparisons for career vs playoffs
  generate_stat_plot <- function(player_career, player_playoffs, stat, player_name, colors, y_max) {
    career_avg <- mean(player_career[[stat]], na.rm = TRUE) # calculates career and playoff average for the chosen NBA stat
    playoff_avg <- mean(player_playoffs[[stat]], na.rm = TRUE)
    
    data <- data.frame(
      Type = c("Career", "Playoffs"),
      Average = c(career_avg, playoff_avg)
    )
    
    ggplot(data, aes(x = Type, y = Average, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste(player_name, "- Career vs. Playoff", input$stat_choice),
           x = "Type", y = paste("Average", input$stat_choice)) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      ylim(0, y_max)
  }
  
  # Render Jordan's plot based on the chosen stat
  output$jordan_stat_plot <- renderPlot({
    stat <- input$stat_choice
    # Find the max value for each stat to determine a Y-axis
    jordan_stat_max <- max(mean(jordan_career[[stat]], na.rm = TRUE), mean(jordan_playoffs[[stat]], na.rm = TRUE), na.rm = TRUE)
    lebron_stat_max <- max(mean(lebron_career[[stat]], na.rm = TRUE), mean(lebron_playoffs[[stat]], na.rm = TRUE), na.rm = TRUE)
    y_max <- max(jordan_stat_max, lebron_stat_max)   
    
    generate_stat_plot(jordan_career, jordan_playoffs, stat, "Michael Jordan", c("Career" = "red", "Playoffs" = "black"), y_max)
  })
  
  # Render LeBron's plot
  output$lebron_stat_plot <- renderPlot({
    stat <- input$stat_choice
    jordan_stat_max <- max(mean(jordan_career[[stat]], na.rm = TRUE), mean(jordan_playoffs[[stat]], na.rm = TRUE), na.rm = TRUE)
    lebron_stat_max <- max(mean(lebron_career[[stat]], na.rm = TRUE), mean(lebron_playoffs[[stat]], na.rm = TRUE), na.rm = TRUE)
    y_max <- max(jordan_stat_max, lebron_stat_max)  
    
    generate_stat_plot(lebron_career, lebron_playoffs, stat, "LeBron James", c("Career" = "purple", "Playoffs" = "gold"), y_max)
  })
  
  # Generate text output for Michael Jordan's stats
  output$jordan_stat_text <- renderUI({
    stat <- input$stat_choice
    career_avg <- mean(jordan_career[[stat]], na.rm = TRUE)
    playoff_avg <- mean(jordan_playoffs[[stat]], na.rm = TRUE)
    tagList(
      tags$p(paste("Michael Jordan's Career", stat, "average:", round(career_avg, 2))),
      tags$p(paste("Michael Jordan's Playoff", stat, "average:", round(playoff_avg, 2)))
    )
  })
  
  # Generate text output for LeBron James's stats
  output$lebron_stat_text <- renderUI({
    stat <- input$stat_choice
    career_avg <- mean(lebron_career[[stat]], na.rm = TRUE)
    playoff_avg <- mean(lebron_playoffs[[stat]], na.rm = TRUE)
    tagList(
      tags$p(paste("LeBron James's Career", stat, "average:", round(career_avg, 2))),
      tags$p(paste("LeBron James's Playoff", stat, "average:", round(playoff_avg, 2)))
    )
  })
  
  # Render the win-percentage plot by season for the players
  output$win_percentage_plot <- renderPlot({
    season_stats <- if (input$win_player_choice == "Michael Jordan") {
      jordan_season_stats
    } else {
      lebron_season_stats
    }
    
    finals <- if (input$win_player_choice == "Michael Jordan") {
      jordan_finals
    } else {
      lebron_finals
    }
    
    ggplot(season_stats, aes(x = season, y = win_percentage, group = 1)) +
      geom_line(color = ifelse(input$win_player_choice == "Michael Jordan", "red", "purple"), size = 1) +
      geom_point(color = ifelse(input$win_player_choice == "Michael Jordan", "red", "purple"), size = 2) +
      geom_text(data = finals, aes(x = as.character(year), y = 50, label = result),
                color = "blue", size = 5, inherit.aes = FALSE) +
      labs(title = paste(input$win_player_choice, "- Win Percentage by Season"),
           x = "Season", y = "Win Percentage") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the points distribution plot
  output$points_distribution_plot <- renderPlot({
    player_data <- if (input$distribution_player_choice == "Michael Jordan") {
      jordan_career
    } else {
      lebron_career
    }
    max_points <- max(c(jordan_career$pts, lebron_career$pts), na.rm = TRUE)
    ggplot(player_data, aes(x = pts)) +
      geom_histogram(
        bins = input$bins,
        fill = ifelse(input$distribution_player_choice == "Michael Jordan", "red", "purple"),
        color = "white"
      ) +
      labs(title = paste(input$distribution_player_choice, "- Points Scored Distribution"),
           x = "Points per Game", y = "Frequency") +
      xlim(0, max_points) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
