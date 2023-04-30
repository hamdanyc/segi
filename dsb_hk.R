library(shiny)
library(dplyr)
library(ggplot2)

# Load data
housekeeping_data <- read.csv("data/housekeeping.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Housekeeping Report"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sort_by", "Sort By:",
                  choices = c("Room Number", "Floor Number", "Room Type", "Cleanliness Status", "Last Cleaned On", "Next Cleaning Due"),
                  selected = "Room Number")
    ),
    mainPanel(
      dataTableOutput("housekeeping_table")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Calculate housekeeping report
  housekeeping_report <- reactive({
    if (input$choices == "Room Number") {
      housekeeping_data %>%
        # arrange(!!sym(input$sort_by)) %>%
        # mutate(Last_Cleaned_On = as.Date(Last_Cleaned_On), Next_Cleaning_Due = as.Date(Next_Cleaning_Due)) %>%
        select(Room_Number, Floor_Number, Room_Type, Cleanliness_Status, Last_Cleaned_On, Next_Cleaning_Due)
    }
  })
  
  # Render housekeeping table
  output$housekeeping_table <- renderDataTable({
    housekeeping_report()
  })
}

# Run the app
shinyApp(ui, server)
