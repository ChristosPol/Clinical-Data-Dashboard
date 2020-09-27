library("shiny")
library("readr")

# Load esmo scorecards
ESMO_Scorecards_all_cancers <- read_csv("/media/chris/DATA/Documents/Clinical_Data_Dashboard/Shiny/ESMO_Scorecards_all_cancers.csv")

tumor_types <- data.frame(type = ESMO_Scorecards_all_cancers$Tumour.Type,
                          sub = ESMO_Scorecards_all_cancers$Tumour.Sub.type)

# Set up the layout
ui <- fluidPage(
  tags$h1("Clinical Data Dashboard"),
  selectInput(inputId = "tum_type",
              label = "Select Tumor type",
              choices =  unique(tumor_types$sub)),
  fluidRow(
    column(2, tableOutput('table'))
  )
)

# Inputs and outputs
server <- function(input, output) {
  
  data <- reactive( {
    subset(ESMO_Scorecards_all_cancers,ESMO_Scorecards_all_cancers$Tumour.Sub.type %in%
             input$tum_type)
  })
  output$table <- renderTable( {
    data()
    })
}

# Launch the app
shinyApp(ui = ui, server = server)
