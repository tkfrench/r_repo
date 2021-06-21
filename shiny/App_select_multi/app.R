# https://stackoverflow.com/questions/39798042/r-shiny-how-to-use-multiple-inputs-from-selectinput-to-pass-onto-select-optio
library(shiny)
library(dplyr)
data(mtcars)

ui <- fluidPage(
  titlePanel("MTCARS"),
  selectInput("Columns","Columns",
              names(mtcars), multiple = TRUE),
  verbatimTextOutput("dfStr")
)

server <- function(input, output) {
  Dataframe2 <- reactive({
    mtcars[,input$Columns] 
  })
  output$dfStr <- renderPrint({
    str(Dataframe2())
  })
}

shinyApp(ui, server)