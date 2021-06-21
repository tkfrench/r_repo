library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(DT)
library(here)

#load(here::here('apps', 'movies', 'movies.RData'))
load(file="../../data/movies.Rdata")

min_year <- min(movies$thtr_rel_year)
max_year <- max(movies$thtr_rel_year)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 2,
                 selectInput(
                   inputId = "i_title_type",
                   label = "Title type:",
                   choices = c("All", unique(as.character(movies$title_type))),
                   selected = "All"
                 ),
                 
                 selectInput(
                   inputId = "i_genre",
                   label = "Genre:",
                   choices = c("All", unique(as.character(movies$genre))),
                   selected = "All"
                 ),
                 
                 selectInput(
                   inputId = "i_studio",
                   label = "Studio:",
                   choices = c("All", unique(as.character(movies$studio))),
                   selected = "All"
                 ),
                 
                 selectInput(
                   inputId = "i_mpaa_rating",
                   label = "MPAA rating:",
                   choices = c("All", unique(as.character(movies$mpaa_rating))),
                   selected = "All"
                 ),
                 
                 sliderInput(
                   inputId = "i_year",
                   label = "Year", min = min_year, max = max_year,
                   sep = "",
                   value = c(1995, 2010)
                   
                 ),
                 
                 br(), 
                 
                 actionButton('select', 'Select'),
                 
                 hr(),
                 
                 
                 downloadButton("download", "Download results")
                 
    ),
    
    mainPanel(width = 10,
        #      DT::dataTableOutput(outputId = "mtable"),
              DT::dataTableOutput(outputId = "m_mean_rt"),
              plotOutput(outputId =  "plot_1"), 
              textOutput("text1")
    )
  )
)



server <- function(input, output) {
  
  filtered_title_type <- reactive({
    if(input$i_title_type == "All"){
      movies
    } else {
      movies %>%
        filter(title_type == input$i_title_type)
    }
  })
  
  filtered_genre <- reactive({
    if(input$i_genre == "All"){
      filtered_title_type() 
    } else {
      filtered_title_type() %>% 
        filter(genre == input$i_genre )
    }
  })
  
  filtered_studio <- reactive({
    if(input$i_studio == "All"){
      filtered_genre()
    } else {
      filtered_genre() %>% 
        filter(studio == input$i_studio)
    }
  })
  
  filtered_rating <- reactive({
    if(input$i_mpaa_rating == "All"){
      filtered_studio()
    } else {
      filtered_studio() %>% 
        filter(mpaa_rating == input$i_mpaa_rating)
    }
  })
  
  filtered_year <- reactive({
    filtered_rating() %>% 
      filter(thtr_rel_year >= input$i_year[1] & thtr_rel_year <= input$i_year[2]) %>% 
      #select(title:thtr_rel_year)
      select(title:audience_score)
  })
  
  fully_filtered <- eventReactive(input$select, {
    filtered_year()
  })
  
  output$mtable <- DT::renderDataTable({
    DT::datatable(data = fully_filtered(), options = list(pageLength = 10),
                  rownames = FALSE, class = 'display', escape = FALSE)
    
  })
################################## 
  # Out a mean
  output$m_mean_rt <- DT::renderDataTable({
    DT::datatable(data = fully_filtered() %>% summarize(Avg_rt = mean(runtime, na.rm=T)), options = list(pageLength = 10),
                  rownames = T, class = 'display', escape = FALSE)
  })

  # Out a plot
  output$plot_1 <-renderPlot({
   fully_filtered() %>% ggplot(aes(x=audience_score, y=imdb_rating)) + 
      geom_point() + 
      geom_smooth()
  })
  
  # Out text
  output$text1 <-renderText({paste("Number of records: ",nrow(fully_filtered() ))
  }) 
  
  output$download <- downloadHandler(
    filename = function() {
      "movie-results.csv"
    },
    content = function(con) {
      write.csv(fully_filtered(), con)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)