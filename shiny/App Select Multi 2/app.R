# tib <- as_tibble(sample_n(ex_df, 40))
library(shinyWidgets)
library(tidyverse)

DIM_FACILITY <- read_csv("../../data/DIM_FACILITY.csv")


shinyApp(
  ui = pageWithSidebar(
    headerPanel("Facility Selection"),
    sidebarPanel(
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
      )
    ),
    
    mainPanel(
      tableOutput("table")
    )
  ),
  
  server = function(input, output, session) {
    
    res_mod <- callModule(
      module = selectizeGroupServer,
      id = "my-filters",
      data = DIM_FACILITY,
      vars = c("STATE_CD", "CLINICAL_SERVICE_AREA_NM", 
               "CLINICAL_REGION_NM", "FACILITY_NM", "FACILITY_TYPE_CD")
    )
    
    output$table <- renderTable({
      res_mod()
    })
    
  },
  
  options = list(height = 500)
)