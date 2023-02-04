library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)

data = read.csv("top50spotify.csv")

data$year = as.factor(data$year)

# Define UI for application that plots features of top 50 spotify songs --------
ui <- fluidPage(
  
  
  theme = shinythemes::shinytheme("lumen"),

    # Application title
    titlePanel("Top 50 Songs on Spotify from 2010-2019"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          
          selectInput(inputId = "x", 
                      label = "X-axis:",
                      choices = c("duration",
                                  "beats per minute" = "bpm",
                                  "energy"), 
                      selected = "duration"),
          
          selectInput(inputId = "y", 
                      label = "Y-axis:",
                      choices = c("Popularity" = "popular"), 
                      selected = "popular"),
          
          selectInput(inputId = "z", 
                      label = "Color by:",
                      choices = "genre",
                      selected = "genre"),
          
          checkboxGroupInput(inputId = "selected_type",
                             label = "Select Song Genre",
                             choices = c("dance pop", "pop", "hip hop", "big room"
                                         , "latin", "canadian pop", 
                                         "barbadian pop", "boy band", "electropop"),
                             selected = c("dance pop", "pop", "hip hop", 
                                          "big room"
                                          , "latin", "canadian pop", 
                                          "barbadian pop", "boy band", 
                                          "electropop")),
          
          hr(),
          
          selectInput(inputId = "x1", 
                      label = "X-axis:",
                      choices = c("loudness",
                                  "liveness",
                                  "Danceability" = "dance", 
                                  "valence", 
                                  "acoustics",
                                  "speechiness"), 
                      selected = "duration"),
          
          selectInput(inputId = "y1", 
                      label = "Y-axis:",
                      choices = c("Popularity"= "popular"), 
                      selected = "popular"),
          
          hr(),
          
          selectInput(inputId = "x2", 
                      label = "X-axis:",
                      choices = "year", 
                      selected = "popular"),
          
          hr(),
          
          checkboxInput(inputId = "show_data",
                        label = "Show data table",
                        value = TRUE)
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput(outputId = "scatterplot"),
          br(),        # a little bit of visual separation
          
          # Print number of obs plotted ---------------------------------
          uiOutput(outputId = "n"),
          br(), br(),    # a little bit of visual separation
          
          plotOutput(outputId = "bargraph"),
          
          br(), br(),
          
          plotOutput(outputId = "violin"),
          
          br(), br(),
          
          # Show data table ---------------------------------------------
          DT::dataTableOutput(outputId = "data"),
          
          br(), br(),
          
          downloadButton(outputId = "downloadData", label = "Download", class = NULL)
          
        )
    )
)

# Define server 
server <- function(input, output) {
  
  # Create a subset of data filtering for selected title types ------
  data_subset <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(data, genre %in% input$selected_type)
  })
  
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = data_subset(), aes_string(x = input$x, y = input$y,
                                              color = input$z)) +
      geom_point(size = 2) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = toTitleCase(str_replace_all(input$y, "_", " ")))
  })
  
  output$bargraph <- renderPlot({
    ggplot(data = data_subset(), aes_string(x = input$x1, y = input$y1
    )) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(x = toTitleCase(str_replace_all(input$x1, "_", " ")),
           y = toTitleCase(str_replace_all(input$y1, "_", " ")))
  })
  
  output$violin <- renderPlot({
    ggplot(data = data_subset(), aes_string(x=input$x2, y= input$y1,
                                            fill = input$x2)) + 
      geom_violin() +
      labs(x = toTitleCase(str_replace_all(input$x2, "_", " ")),
           y = toTitleCase(str_replace_all(input$y1, "_", " ")))
  })
  
  # Print number of movies plotted ----------------------------------
  output$n <- renderUI({
    types <- data_subset()$genre %>% 
      factor(levels = input$selected_type) 
    counts <- table(types)
    
    HTML(paste("There are", counts, input$selected_type, "songs in this dataset. <br>"))
  })
  
  # Print data table if checked -------------------------------------
  output$data <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = data[, 1:15], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
      },
       content = function(con) {
        write.csv(data, con)
    }
    )
}

    

# Run the application 
shinyApp(ui = ui, server = server)
