library(shiny)
library(dplyr)
library(ggplot2)

# Load data
sales_data <- read.csv("data/fb.csv")

# Define UI
ui <- fluidPage(
  titlePanel("F&B Department Sales Report"),
  sidebarLayout(
    sidebarPanel(
      selectInput("time_period", "Select Time Period:",
                  choices = c("Daily", "Weekly", "Monthly"),
                  selected = "Monthly")
    ),
    mainPanel(
      plotOutput("sales_chart")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Calculate sales figures
  sales_figures <- reactive({
    if (input$time_period == "Daily") {
      sales_data %>%
        group_by(Category, Event_Date) %>%
        summarise(Total_Sales = sum(Price)) %>%
        filter(Event_Date == Sys.Date())
    } else if (input$time_period == "Weekly") {
      sales_data %>%
        group_by(Category, week = lubridate::week(Event_Date)) %>%
        summarise(Total_Sales = sum(Price)) %>%
        filter(week == lubridate::week(Sys.Date()))
    } else {
      sales_data %>%
        group_by(Category, month = lubridate::month(Event_Date)) %>%
        summarise(Total_Sales = sum(Price))
    }
  })
  
  # Render sales chart
  output$sales_chart <- renderPlot({
    ggplot(sales_figures(), aes(x = Category, y = Total_Sales, fill = Category)) +
      geom_col() +
      labs(title = paste("Total Sales by Category -", input$time_period),
           x = "Category", y = "Total Sales",
           fill = "Category")
  })
}

# Run the app
shinyApp(ui, server)
