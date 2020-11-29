library(shiny)
library(tidyverse)

# load data
load("Data/userData_shiny_ass3.RData")

# factorize domain and grade
userData$domain_name <- factor(userData$domain_name)
userData$grade <- factor(userData$grade,
                         levels = 3:8,
                         labels = c(paste("Grade", 3:8)))

# get unique grade names
grades <- unique(userData$grade) %>% sort()

# get unique domain names
domains <- unique(userData$domain_name)


# Define UI for oefenweb application
ui <- fluidPage(
  
  # Application title
  titlePanel(title = "Q-Score Comparison App", windowTitle = "Oefenweb App"),
  
  # Define sidebarlayout
  sidebarLayout(
    sidebarPanel(
      
      # Selectize input for domain to plot
      selectizeInput(inputId = "domain", label = "Domain",
                     choices = domains),
      
      # Checkbox input for the grades to plot
      checkboxGroupInput(inputId = "grade", label = "Show Grades:", 
                         choices = grades,
                         selected = "Grade 3"),
      
      # Make action button to select all grades
      actionButton("all", label = "Select all"),
      
      # Make action button to select all grades
      actionButton("none", label = "Select none")
      
    ),
    
    # Define mainpanel
    mainPanel(
      
      # Define tabset layout
      tabsetPanel(
        
        # Tab for plot all
        tabPanel(title = "All",
                 plotOutput("plot_all")
        ),
        
        # Tab for plot female 
        tabPanel(title = "Female",
                 # Show boxplot
                 plotOutput("plot_f")
        ),
       
         # Tab for plot male 
        tabPanel(title = "Male",
                 # Show boxplot
                 plotOutput("plot_m")
        )
      )
    )
  )
)

# Define server logic required to draw the plots
server <- function(input, output, session) {
  
  # Define action button updates to grade selection all
  observeEvent(input$all, {
    x <- as.character(unique(userData$grade))
    updateCheckboxGroupInput(session, inputId = "grade",
                             selected = x)
  })
  
  # Define action button updates to grade selection none
  observeEvent(input$none, {
    updateCheckboxGroupInput(session, inputId = "grade",
                             selected = character(0))
  })
  
  # Make reactive data frame based on input selection from UI
  data <- reactive({
    userData %>% 
      filter(domain_name == input$domain, grade %in% factor(input$grade))
  })
  
  # Make plot specifications
  l <- labs(y = "Quantile Score")
  
  th <- theme_minimal() +
    theme(axis.title.x = element_blank())
  
  # Make plotoutput all
  output$plot_all <- renderPlot({
    ggplot(data = data(), mapping = aes(x = grade, y = q_score)) +
      geom_boxplot() +
      l +
      th
  })
  
  # Make plotoutput female
  output$plot_f <- renderPlot({
    ggplot(data = subset(data(), gender == "f"), mapping = aes(x = grade, y = q_score)) +
      geom_boxplot() +
      l +
      th
    
  })
  
  # Make plotoutput male
  output$plot_m <- renderPlot({
    ggplot(data = subset(data(), gender == "m"), mapping = aes(x = grade, y = q_score)) +
      geom_boxplot() +
      l +
      th
  })
}

# Run the App
shinyApp(ui = ui, server = server)