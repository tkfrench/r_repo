# tib <- as_tibble(sample_n(ex_df, 40))
library(shinyWidgets)
library(tidyverse)

DIM_FACILITY <- read_csv("../../data/DIM_FACILITY.csv")
# add some rows
DIM_FACILITY <- DIM_FACILITY %>% mutate(alpha=0.05, wt_LOS=0, wt_Readmit=0) %>% select(alpha, wt_LOS, wt_Readmit, everything())


shinyApp(
  ui = pageWithSidebar(
    headerPanel("Selection Template"),
    sidebarPanel(
      helpText("FILTERS: Menu includes multi-select and cascading selection properties."),
      
      selectizeGroupUI(
        id = "my-filters",
        inline = FALSE,
        params = list(
          STATE_CD                 = list(inputId = "STATE_CD", title = "Sate", placeholder = 'select'),
          CLINICAL_SERVICE_AREA_NM = list(inputId = "CLINICAL_SERVICE_AREA_NM", title = "Clinical Sercive Area", placeholder = 'select'),
          CLINICAL_REGION_NM       = list(inputId = "CLINICAL_REGION_NM", title = "Clinical Region", placeholder = 'select'),
          FACILITY_NM              = list(inputId = "FACILITY_NM", title = "Facility", placeholder = 'select'),
          FACILITY_TYPE_CD         = list(inputId = "FACILITY_TYPE_CD", title = "Facility Type", placeholder = 'select')
        )
      ),
      helpText("PARAMETERS:"),
      radioButtons("alpha", h6("Alpha"), choices = list("0.10" = 0.10, "0.05" = 0.05, "0.01" = 0.01,"0.001" = 0.001  ),selected = 0.10),
      sliderInput("wt_LOS", h6("WT LOS"),     min = 0, max = 10, value = 0, step=1),
      sliderInput("wt_Readmit", h6("WT Readmit"), min = 0, max = 10, value = 0, step=1),
    ),
    
    mainPanel(
      h3("A Plot"),
      plotOutput("plot"),
      h3("A Table, top 3 rows"),
      tableOutput("table")
    )
  ),
  
  server = function(input, output, session) {
    
   data_trans <- reactive({
     DIM_FACILITY %>% mutate(alpha = input$alpha, wt_LOS = input$wt_LOS, wt_Readmit= input$wt_Readmit)
   })
    
    res_mod <- callModule(
      module = selectizeGroupServer,
      id = "my-filters",
      #data = DIM_FACILITY,
      data = data_trans,
      vars = c("STATE_CD", "CLINICAL_SERVICE_AREA_NM", 
               "CLINICAL_REGION_NM", "FACILITY_NM", "FACILITY_TYPE_CD")
    )
    
    output$table <- renderTable({
      head(res_mod(), 3)
    },striped = TRUE, bordered = TRUE,  
    hover = TRUE, spacing = 'xs', digits = 0, 
    na = 'missing')
    
    output$plot <- renderPlot({res_mod() %>% ggplot(aes(x=REGION_CD)) + geom_bar()
      
    })    


    
  },
  

  options = list(height = 500)
)