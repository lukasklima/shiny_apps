library(shiny)
library(tidyverse)
library(ggrepel)

## load summary data here
load("summarised_data.RData")
data <- summarised_data
data$grade <- factor(data$grade)
data <- data %>%
  mutate(correct_count = word_count - wrong_count)

# make data in long format to plot correct/incorrect responses
data <- data %>%
  gather(key = "count_type", value = "count", wrong_count, correct_count)

# add variable for starting letter
data <- data %>%
  mutate(starting_letter = str_sub(correct_answer, start = 1, end = 1))


## load translation data
load("translation.RData")

# join with translation
data <- data %>%
  left_join(translation)

# sort data by rating
data <- data %>%
  arrange(rating)

data$correct_answer <- factor(data$correct_answer, levels = unique(data$correct_answer))

  
## load log answers data here
load("answer_data.RData")
data_box <- answer_item_tibble
data_box$grade <- factor(data_box$grade)
data_box$correct_answered <- factor(data_box$correct_answered)

# join with translation
data_box <- data_box %>%
  left_join(translation)


## define variables
rating_min <- round(min(data$rating), 0)
rating_max <- round(max(data$rating), 0)
wordlength_max <- max(data$word_length)
wordlength_min <- min(data$word_length)


## Define UI for duckify application
ui <- fluidPage(

   # Application title
   titlePanel(title = "Ducktator - Oefenweb", windowTitle = "Duckify ME"),

   # Tabsetpanel layer
   tabsetPanel(

     # Tabpanel for summary inspection
     tabPanel(title = "Summary",

              plotOutput("plot_sum"),

              hr(),

              fluidRow(
                column(6,

                       # Checkbox input for the grades to display
                       checkboxGroupInput(inputId = "grade_s", label = "Choose the grades to display",
                                          choices = list("Grade 1" = 1, "Grade 2" = 2, "Grade 3" = 3, "Grade 4" = 4,
                                                         "Grade 5" = 5, "Grade 6" = 6, "Grade 7" = 7, "Grade 8" = 8),
                                          selected = 1),

                       # Make action button to select all grades
                       actionButton("all_s", label = "Select all"),

                       # Make action button to select no grades
                       actionButton("none_s", label = "Select none")
                ),

                column(6,
                       # Starting letter select input
                       selectizeInput(inputId = "starting_letter", label = "Choose a starting letter",
                                      choices = c("ALL", letters), selected = "a"),

                       # Wordlength input
                       sliderInput("wordlength_range", label = "Wordlength Range", min = wordlength_min,
                                   max = wordlength_max, value = c(wordlength_min, wordlength_max),
                                   dragRange = TRUE, step = 1),

                       # Difficulty rating input
                       sliderInput("rating_range_s", label = "Difficulty Range", min = rating_min,
                                   max = rating_max, value = c(rating_min, rating_max), round = -2,
                                   dragRange = TRUE)
                )
              )
     ),

     # Tabpanel for single word inspection
     tabPanel(title = "Single Word",

              # Define sidebarlayout
              sidebarLayout(

                # Define sidebarpanel
                sidebarPanel(
                    wellPanel(
                    # Checkbox input for the grades to display
                      checkboxGroupInput(inputId = "grade", label = "Choose the grades to display",
                                         choices = list("Grade 1" = 1, "Grade 2" = 2, "Grade 3" = 3, "Grade 4" = 4,
                                                        "Grade 5" = 5, "Grade 6" = 6, "Grade 7" = 7, "Grade 8" = 8),
                                         selected = 1),

                      # Make action button to select all grades
                      actionButton("all_w", label = "Select all"),

                      # Make action button to select no grades
                      actionButton("none_w", label = "Select none")
                    ),

                    wellPanel(
                      # Difficulty rating range
                      sliderInput("rating_range", label = "Difficulty Range", min = rating_min,
                                  max = rating_max, value = c(rating_min, rating_max), round = -2,
                                  dragRange = TRUE),

                      # Selectize for specific word
                      uiOutput("word_ui")
                    ),

                    wellPanel(
                      strong("Response Time Plot"),

                      # Checkbox for comparing response time for correct/incorrect/all
                      checkboxInput(inputId = "resp_compare", label = "Compare correct/incorrect answers",
                                    value = FALSE)
                    )
                ),

                # Define main panel
                mainPanel(

                  # Make tabset layout
                  tabsetPanel(
                    tabPanel(title = "Accuracy",

                             # Show accuracy plot
                             plotOutput("plot_single_acc"),
                             
                             fluidRow(
                               column(6,
                                      h3("Item Difficulty Rating"),
                                      textOutput("rating")
                               ),
                               
                               column(6, 
                                      h3("Levenshtein Distance"),
                                      textOutput("dist1")
                               )
                             ),
                             
                             br(),
                             br(),
                             # output for answers given data table 
                             DT::dataTableOutput("answers")
                    ),

                    tabPanel(title = "Response Time",

                             # Show boxplot
                             plotOutput("plot_single_resp"),
                             
                             fluidRow(
                               column(6,
                                      h3("Item Difficulty Rating"),
                                      textOutput("rating2")
                               ),
                               
                               column(6, 
                                      h3("Levenshtein Distance"),
                                      textOutput("dist2")
                               )
                             ),
                             
                             br(),
                             br(),
                             # output for answers given data table 
                             DT::dataTableOutput("answers2")
                             
                    )
                  )
                )
              )
     ),

     tabPanel(title = "Levenshtein Distance",

              # Define sidebarlayout
              sidebarLayout(

                # Define sidebarpanel
                sidebarPanel(
                  wellPanel(
                    # Checkbox input for the grades to display
                    checkboxGroupInput(inputId = "grade_m", label = "Choose the grades to display",
                                       choices = list("Grade 1" = 1, "Grade 2" = 2, "Grade 3" = 3, "Grade 4" = 4,
                                                      "Grade 5" = 5, "Grade 6" = 6, "Grade 7" = 7, "Grade 8" = 8),
                                       selected = 1),

                    # Make action button to select all grades
                    actionButton("all_m", label = "Select all"),

                    # Make action button to select no grades
                    actionButton("none_m", label = "Select none")
                  )
                ),

                mainPanel(
                  tabsetPanel(
                    tabPanel(title = "Rating",
                             # show rating plot
                             plotOutput("plot_mod_rating")
                    ),

                    tabPanel(title = "Response Time",
                             # show response time plot
                             plotOutput("plot_mod_resp")
                    )
                  )
                )
              )
     )
   )
)


        
# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## Summary tabpanel
  # Define action button updates to grade selection all
  observeEvent(input$all_s, {
    x <- as.character(unique(data$grade))
    updateCheckboxGroupInput(session, inputId = "grade_s",
                             selected = x)
  })

  # Define action button updates to grade selection none
  observeEvent(input$none_s, {
    updateCheckboxGroupInput(session, inputId = "grade_s",
                             selected = character(0))
  })

  # Make reactive data for plot_sum
  data_sum <- reactive({
    if (input$starting_letter != "ALL") {
    data %>%
      filter(starting_letter == input$starting_letter,
             grade %in% factor(input$grade_s),
             word_length >= input$wordlength_range[1], word_length <= input$wordlength_range[2],
             rating >= input$rating_range_s[1], rating <= input$rating_range_s[2])
    } else if (input$starting_letter == "ALL") {
      data %>%
        filter(grade %in% factor(input$grade_s),
               word_length >= input$wordlength_range[1], word_length <= input$wordlength_range[2],
               rating >= input$rating_range_s[1], rating <= input$rating_range_s[2])
    }
  })

  # make summary plot output
  output$plot_sum <- renderPlot({
    ggplot(data = data_sum(),
           mapping = aes(x = correct_answer, y = count, fill = fct_rev(count_type))) +
      geom_col() +
      theme_minimal() +
      scale_fill_discrete(labels = c("incorrect", "correct"), guide = guide_legend(reverse = FALSE)) +
      labs(x = "Grade", y = "Frequency", fill = "Answer", caption = "sorted by difficulty rating") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
            axis.ticks.x = element_line(colour = "#5E5A5A", size = 1),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14),
            legend.position = "top",
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 13),
            plot.caption = element_text(size = 13))
  })


  ## Single word tabpanel
  # Update checkboxgroup selection based on summary tabpanel selection
  observe({
    x <- input$grade_s
    updateCheckboxGroupInput(session, inputId = "grade",
                             selected = x)
  })

  # Select all action button grades
  observeEvent(input$all_w, {
    x <- as.character(unique(data$grade))
    updateCheckboxGroupInput(session, inputId = "grade",
                             selected = x)
  })

  # Define action button updates to grade selection none
  observeEvent(input$none_w, {
    updateCheckboxGroupInput(session, inputId = "grade",
                             selected = character(0))
  })

  # Make reactive word selection defined by difficulty range
  output$word_ui <- renderUI({
    words <- summarised_data %>%
      filter(rating >= input$rating_range[1], rating <= input$rating_range[2]) %>%
      distinct(correct_answer)
    selectizeInput(inputId = "word", label = "Choose a specific word",
                   choices = words, selected = "act")
  })

  # Make reactive data frame by word selection for acc plot
  data_word <- reactive({
    data %>%
      filter(correct_answer == input$word, grade %in% factor(input$grade))
  })

  # Make reactive data frame by word selection for resp box plot
  data_resp <- reactive({
    data_box %>%
      filter(correct_answer == input$word, grade %in% factor(input$grade))
  })

  # Make output for difficulty rating
  output$rating <- renderText({
    rating <- unique(data_word()[, "rating"])
    as.numeric(rating)
  })

  output$rating2 <- renderText({ 
    rating <- unique(data_word()[, "rating"])
    as.numeric(rating)
  })
  
  # Make output for translation distance
  output$dist1 <- renderText({ 
    rating <- unique(data_word()[, "stringdist"])
    as.numeric(rating)
  })
  
  output$dist2 <- renderText({ 
    rating <- unique(data_word()[, "stringdist"])
    as.numeric(rating)
  })
  
  # Make datatable output for answers given
  data_dt <- reactive({
    data_resp() %>%
      count(answer) %>%
      arrange(desc(n)) %>% 
      select(Answer = answer, Frequency = n)
  })
  
  output$answers <- DT::renderDataTable({
    DT::datatable(data = data_dt(), caption = "Answers given and their frequency")
  })
  
  output$answers2 <- DT::renderDataTable({
    DT::datatable(data = data_dt(), caption = "Answers given and their frequency")
  })
  
  # Make single word accuracy plot output
  output$plot_single_acc <- renderPlot({
    ggplot(data = data_word(), mapping = aes(
      x = grade, y = count, fill = fct_rev(count_type))) +
        geom_col() +
      theme_minimal() +
      labs(x = "Grade", y = "Frequency", fill = "Answer", title = input$word) +
      scale_fill_discrete(labels = c("incorrect", "correct"), guide = guide_legend(reverse = FALSE)) +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "top",
            legend.title = element_text(size = 16), 
            legend.text = element_text(size = 13),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14))
  })
  
  # Make single word boxplot output for response time
  output$plot_single_resp <- renderPlot({
    p <- ggplot(data = data_resp()) +
      labs(x = "Grade", y = "Response Time (sec)", title = input$word) +
      theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          legend.position = "top",
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 13),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14))
    
    # summarised when compare checkbox is FALSE
    if (input$resp_compare == FALSE)
      p <- p + geom_boxplot(mapping = aes(
        x = grade, y = response_in_milliseconds / 1000))
    
    # fill color split between correct/incorrect when compare checkbox is TRUE
    if (input$resp_compare == TRUE)
      p <- p + geom_boxplot(mapping = aes(
        x = grade, y = response_in_milliseconds / 1000, fill = correct_answered)) +
        scale_fill_discrete(labels = c("incorrect", "correct"), guide = guide_legend(reverse = FALSE)) +
        labs(fill = "Answer")
    
    print(p)
  })

  ## Model tabpanel
  # Define action button updates to grade selection all
  observeEvent(input$all_m, {
    y <- 1:8
    updateCheckboxGroupInput(session, inputId = "grade_m",
                             selected = y)
  })
  
  # Define action button updates to grade selection none
  observeEvent(input$none_m, {
    updateCheckboxGroupInput(session, inputId = "grade_m",
                             selected = character(0))
  })
  
  # Define data for model plot rating
  data_mod_rating <- reactive({
    data %>%
      filter(grade %in% factor(input$grade_m))
  })
  
  # Define data for model plot resp time
  data_mod_resp <- reactive({
    data_box %>%
      filter(grade %in% factor(input$grade_m))
  })
  
  # Define model plot rating
  output$plot_mod_rating <- renderPlot({
    ggplot(data = data_mod_rating(), mapping = aes(
      x = dist_cat, y = as.numeric(rating))) +
      geom_boxplot() +
      labs(x = "Levenshtein Distance", y = "Rating") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14))
  })
  
  # Define model plot resp time
  output$plot_mod_resp <- renderPlot({
    ggplot(data = data_mod_resp(), mapping = aes(
      x = grade, y = response_in_milliseconds / 1000, fill = dist_cat)) +
      geom_boxplot() +
      labs(x = "Grade", y = "Response Time (sec)", fill = "Levenshtein Distance") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14),
            legend.position = "top",
            legend.title = element_text(size = 16), 
            legend.text = element_text(size = 13)
            )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)