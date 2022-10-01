# fileWidget.R

ui <- fluidPage(
  
  # Copy the line below to make a file upload manager
  fileInput("file", label = h3("File input")),
  
  hr(),
  fluidRow(column(4, verbatimTextOutput("value")))
  
)

server <- function(input, output) {
  
  # You can access the value of the widget with input$file, e.g.
  output$value <- renderPrint({
    str(input$file)
  })
  
}