ui = fluidPage(
  titlePanel("Multiple reactive values"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "column1", "Reac1", letters, selected = "a"),
      selectInput(inputId = "column2", "Reac2", letters, selected = "a")
    ),
    mainPanel(
      verbatimTextOutput("txt1"),
      verbatimTextOutput("txt2")
    )
  )
)

server = function(input, output, session) {
  reac <- reactiveValues()
  #reac2 <- reactiveValues(qweqwe = 0)
  
  # If any inputs are changed, set the redraw parameter to FALSE
  
  observe({ 
    reac$asdasd =  input$column1
  })  
  observe({ 
    reac$qweqwe = input$column2
  }) 
  
  # Only triggered when the copies of the inputs in reac are updated
  # by the code above
  output$txt1 <- renderPrint({
    print('output 1')
    print(paste(reac$asdasd, 'reac1'))  
  })
  
  output$txt2 <- renderPrint({ 
    print('output2') 
    print(paste(reac$qweqwe, 'reac2')) 
  }) 
  
}
shinyApp(ui, server)