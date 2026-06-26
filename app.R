library(shiny)
library(bslib)
library(tidyverse)
library(plotly)

# Load data
games <- read_csv("data/Games.csv") %>%
  filter(homeScore > 0, awayScore > 0) %>%
  mutate(
    home_team = paste(hometeamCity, hometeamName),
    away_team = paste(awayteamCity, awayteamName),
    year = year(gameDate)
  )

# Get filter choices
all_teams <- sort(unique(c(games$home_team, games$away_team)))
game_types <- sort(unique(games$gameType))
year_range <- range(games$year, na.rm = TRUE)

# UI
ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#6366f1",
    base_font = font_google("Inter"),
    heading_font = font_google("Outfit")
  ),
  title = "NBA Scorigami Dashboard",
  sidebar = sidebar(
    title = "Controls",
    selectInput("team", "Filter by Team (Home or Away):", choices = c("All Teams", all_teams), selected = "All Teams"),
    selectInput("game_type", "Filter by Game Type:", choices = c("All Types", game_types), selected = "All Types"),
    sliderInput("years", "Year Range:", min = year_range[1], max = year_range[2], value = year_range, sep = ""),
    hr(),
    markdown("
      ### What is Scorigami?
      **Scorigami** is a concept documenting final score combinations that have occurred for the first time in a sport's history.
      
      *Use the filters above to dynamically update the matrix below.*
    ")
  ),
  
  card(
    card_header("Interactive Scorigami Matrix (Hover to inspect scores)", class = "d-flex justify-content-between align-items-center"),
    plotlyOutput("scorigami_plot", height = "650px")
  ),
  
  card(
    card_header("Active Filter Summary"),
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Total Games Played",
        value = textOutput("total_games"),
        theme = "primary"
      ),
      value_box(
        title = "Unique Scores",
        value = textOutput("unique_scores"),
        theme = "info"
      ),
      value_box(
        title = "Home Win Rate",
        value = textOutput("home_win_rate"),
        theme = "success"
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive dataset based on filters
  filtered_games <- reactive({
    df <- games
    
    # Filter by team
    if (input$team != "All Teams") {
      df <- df %>% filter(home_team == input$team | away_team == input$team)
    }
    
    # Filter by game type
    if (input$game_type != "All Types") {
      df <- df %>% filter(gameType == input$game_type)
    }
    
    # Filter by years
    df <- df %>% filter(year >= input$years[1] & year <= input$years[2])
    
    df
  })
  
  # Reactive calculations for value boxes
  output$total_games <- renderText({
    nrow(filtered_games())
  })
  
  output$unique_scores <- renderText({
    filtered_games() %>% distinct(homeScore, awayScore) %>% nrow()
  })
  
  output$home_win_rate <- renderText({
    df <- filtered_games()
    if (nrow(df) == 0) return("0.0%")
    rate <- sum(df$homeScore > df$awayScore) / nrow(df) * 100
    sprintf("%.1f%%", rate)
  })
  
  # Render Plotly Matrix
  output$scorigami_plot <- renderPlotly({
    df <- filtered_games()
    
    if (nrow(df) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0, label = "No games match the selected filters.") +
          theme_void()
      )
    }
    
    # Compute counts and details for tooltip
    score_counts <- df %>%
      group_by(homeScore, awayScore) %>%
      summarize(
        n = n(),
        first_date = format(min(gameDate), "%b %d, %Y"),
        last_date = format(max(gameDate), "%b %d, %Y"),
        sample_teams = paste0(first(home_team), " vs ", first(away_team)),
        .groups = "drop"
      ) %>%
      mutate(
        tooltip_text = paste0(
          "Score: ", homeScore, " - ", awayScore, "\n",
          "Occurrences: ", n, "\n",
          "First Achieved: ", first_date, "\n",
          "Most Recent: ", last_date, "\n",
          "Example: ", sample_teams
        )
      )
    
    p <- ggplot(score_counts, aes(x = homeScore, y = awayScore)) +
      geom_tile(aes(fill = n, text = tooltip_text), width = 0.95, height = 0.95) +
      geom_abline(intercept = 0, slope = 1, color = "white", linewidth = 1) +
      scale_fill_viridis_c(
        option = "inferno",
        trans = "log10",
        name = "Count",
        breaks = c(1, 10, 100),
        labels = c("1", "10", "100+")
      ) +
      scale_x_continuous(limits = c(15, 195), expand = c(0, 0)) +
      scale_y_continuous(limits = c(15, 195), expand = c(0, 0)) +
      labs(
        x = "Home PTS",
        y = "Visitor PTS"
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#EAEAEA", color = NA),
        panel.grid.major = element_line(color = "white", linewidth = 0.3),
        panel.grid.minor = element_line(color = "white", linewidth = 0.1),
        plot.background = element_rect(fill = "white", color = NA),
        aspect.ratio = 1
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 50, r = 50, b = 50, t = 50),
        xaxis = list(scaleanchor = "y", scaleratio = 1)
      )
  })
}

shinyApp(ui, server)
