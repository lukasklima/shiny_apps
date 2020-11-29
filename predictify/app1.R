library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

# load the soccer prediction data
file <- "https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"
football <- read_csv(file = file)

# define variables used in the app
leagues <- unique(football$league) %>% sort()

date_max <- max(football$date)

# Define UI for application that shows football match predictions
ui <- fluidPage(
   
   # Application title
   titlePanel("538 Football Predictions"),
   
   tabsetPanel(
     
     # Define tabpanel for the table
     tabPanel("Games",
              
              # Sidebar with filtering options
              sidebarLayout(
                sidebarPanel(
                  
                  # Wellpanel for filtering options
                  wellPanel(
                    
                    # Wellpanel header
                    h2("Filters"),
                    
                    # Select league
                    selectizeInput(
                      inputId = "league_g", label = "Choose a League",
                      choices = c("ALL", leagues), selected = "ALL"
                    ),
                    
                    # Select team
                    uiOutput("teams_g"),
                    
                    # Select starting date to be plotted
                    dateInput(
                      inputId = "start_date_g", label = "Starting Date",
                      min = today() - 1, max = max(football$date)
                    ),
                    
                    # Select end date to be plotted
                    uiOutput("end_date_g"),
                    
                    # Select minimum probability 
                    sliderInput(inputId = "prob", label = "Minimum Probability", min = 0,
                                max = 100, value = 0, step = 1, post = "%")
                  )
                ),
                
                mainPanel(
                  DT::dataTableOutput("table")
                )
              )
     ),
     
     tabPanel("Team",
              
              # Sidebar with filtering options
              sidebarLayout(
                sidebarPanel(
                  
                  # Wellpanel for filtering options
                  wellPanel(
                    
                    # Wellpanel header
                    h2("Filters"),
                    
                    # Select league
                    selectizeInput(
                      inputId = "league", label = "Choose a League",
                      choices = c("ALL", leagues), selected = "ALL"
                    ),
                    
                    # Select team
                    uiOutput("teams"),
                    
                    # Select starting date to be plotted
                    dateInput(
                      inputId = "start_date", label = "Starting Date",
                      min = today() - 1, max = max(football$date)
                    ),
                    
                    # Select end date to be plotted
                    uiOutput("end_date"),
                    
                    tags$small("* can be max 60 days from start date")
                  )
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  plotlyOutput("plot")
                )
              )
     )
   )
)
                  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ## Table tab
  # reactive team selection
  output$teams_g <- renderUI({
    football_league <- football
    if (input$league_g != "ALL") {
      football_league <- football_league %>%
        filter(league == input$league_g)
    }
    teams <- c("ALL", unique(c(football_league$team1, football_league$team1))) %>% sort()
    selectizeInput(
      inputId = "team_g", label = "Choose a Team",
      choices = teams, selected = "ALL"
    )
  })
  
  # Make reactive ending date selection
  output$end_date_g <- renderUI({
    min_end_date <- input$start_date_g
    max_end_date <- date_max
    current_selection <- date_max
    dateInput(
      inputId = "end_date_g", label = "End Date",
      value = current_selection, min = min_end_date, max = max_end_date
    )
  })
  
  # Make reactive data for table
  football_games <- reactive({
    football %>% 
    filter(team1 == input$team_g | team2 == input$team_g) %>%
    select(date, league, team1, team2, prob1:probtie) %>%
    arrange(date) %>% 
    filter(date >= input$start_date_g, date <= input$end_date_g) %>% 
    filter(prob1 >= input$prob )
  })
  
  # Make datatable output
  output$table <- renderDataTable({
    DT::datatable(data = football_games(), caption = "Outcome probabilities",
                  colnames = c("Date", "League", "Home Team", "Away Team", "Home Win", "Away Win", "Tie")
                  ) %>% 
      DT::formatPercentage(5:7, 2)
  })
  
  ## Plot tab
  # Make reactive team selection
  output$teams <- renderUI({
    football_league <- football
    if (input$league != "ALL") {
      football_league <- football_league %>%
        filter(league == input$league)
    }
    teams <- unique(c(football_league$team1, football_league$team1)) %>% sort()
    selectizeInput(
      inputId = "team", label = "Choose a Team",
      choices = teams, selected = "Ajax"
    )
  })
  
  # Make reactive ending date selection
  output$end_date <- renderUI({
    min_end_date <- input$start_date
    max_end_date <- input$start_date + 60
    current_selection <- input$start_date + 14
    dateInput(
      inputId = "end_date", label = "End Date",
      value = current_selection, min = min_end_date, max = max_end_date
    )
  })
  
  # Make reactive data.frame
  football_filtered <- reactive({
    football %>% 
    subset(team1 == input$team | team2 == input$team) %>%
    select(date, league, team1, team2, prob1:probtie) %>%
    mutate(site = ifelse(input$team == team1, "Home", "Away"),
           opponent = ifelse(input$team == team1, team2, team1),
           Win = ifelse(site == "Home", prob1, prob2),
           Lose = ifelse(site == "Home", prob2, prob1),
           Draw = probtie,
           team = input$team) %>% 
    select(-c(team1:probtie)) %>% 
    gather(key = "outcome", value = "probability", Win, Lose, Draw) %>% 
    arrange(date) %>% 
      filter(date >= input$start_date, date <= input$end_date)
  })
  
  # Make plot
  output$plot <- renderPlotly({
    plot_ly(data = football_filtered(),
            x = ~date,
            y = ~probability,
            hoverinfo = "text", 
            text = ~paste("P:", round(probability, 2), "<br>",
                          "Opponent:", opponent, "<br>",
                          "Site:", site, "<br>",
                          league)) %>% 
      add_markers(symbol = ~factor(site), hoverinfo = "none") %>% 
      add_lines(color = ~fct_rev(outcome), colors = c("#66DF90", "#D15656", "#6692DF")) %>%
      layout(xaxis = list(title = "Date", tickangle = 45, type = "date",
                          tickformat = "%d %B (%a)<br>%Y"),
             yaxis = list(title = "Probability"),
             title = input$team,
             hovermode = "compare")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)