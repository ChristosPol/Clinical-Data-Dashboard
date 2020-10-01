library("shiny")
library("readr")

# Load esmo scorecards
ESMO_Scorecards_all_cancers <- read_csv("ESMO_Scorecards_all_cancers.csv")
View(ESMO_Scorecards_all_cancers)
tumor_types <- data.frame(type = ESMO_Scorecards_all_cancers$Tumour.Type,
                          sub = ESMO_Scorecards_all_cancers$Tumour.Sub.type)

# Set up the layout
ui <- fluidPage(
  tags$h1("Clinical Data Dashboard"),
  tabsetPanel(
        tabPanel(title ="Data Table",
        selectInput(inputId = "tum_type",
                    label = "Select Tumor type",
                    choices =  unique(tumor_types$sub)),
        fluidRow(
          column(2, tableOutput('table'))
    )
    ),
    tabPanel(title ="Data Visualization",
             plotOutput("Normal")
             )
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
  output$Normal <- renderPlot({
    hist(rnorm(400), breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard normal distribution")
  })
}

# Launch the app
shinyApp(ui = ui, server = server)

