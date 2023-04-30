library(shiny)
library(dplyr)
library(ggplot2)

# Load data
inventory_data <- read.csv("data/fb_inv.csv")

# Define UI
ui <- fluidPage(
  titlePanel("F&B Department Inventory Report"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sort_by", "Sort By:",
                  choices = c("Item Name", "Inventory Level", "Inventory Value"),
                  selected = "Item Name")
    ),
    mainPanel(
      plotOutput("inventory_chart")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Calculate inventory levels and values
  inventory_levels <- reactive({
    inventory_data %>%
      group_by(Item_Name) %>%
      summarise(Inventory_Level = sum(Quantity), Inventory_Value = sum(Quantity * Price))
  })
  
  # Render inventory chart
  output$inventory_chart <- renderPlot({
    ggplot(inventory_levels(), aes(x = reorder(Item_Name, Inventory_Level), y = Inventory_Level)) +
      geom_bar(stat = "identity", fill = "dodgerblue4") +
      labs(title = "Current Inventory Levels by Menu Item",
           x = "Menu Item", y = "Inventory Level") +
      geom_text(aes(label = Inventory_Level), vjust = -0.5, size = 4) +
      coord_flip() +
      theme_bw()
  })
}

# Run the app
shinyApp(ui, server)
