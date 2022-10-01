# fbWeb.R

# Init ----

library(shiny)
library(tidyverse)
library(RColorBrewer)

bln <- c("Jan","Feb","Mac","Apr","Mei","Jun",
         "Jul","Ogos","Sep","Okt","Nov","Dis")

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Food and Beverage Dashboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
 
      # Input: Selector dataset to plot ----
      selectInput("mth", "Pilih bulan:",
                  bln),

      # Download button ----
      tags$hr(),
      downloadButton("report", "Muat turun laporan")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot of the requested variable ----
      textOutput("mth"),
      tableOutput("tb")
      ),
  )
)

# Define server logic ----
server <- function(input, output) {
  # read data file ----
  setwd("~/wisma_perwira")
  
  att_tb <- reactive(readxl::read_excel("fbtable.xlsx", sheet = "att",
                                        col_types = c("date",replicate(16,"numeric"))))
  exp_tb <-  reactive(readxl::read_excel("fbtable.xlsx", sheet = "exp",
                                         col_types = c("date","text","numeric","text","numeric")))
  part_tb <- reactive(readxl::read_excel("fbtable.xlsx", sheet = "psm",
                                         col_types = c("date","text","numeric","numeric")))
  bqt_tb <- reactive(readxl::read_excel("fbtable.xlsx", sheet = "bqt",
                               col_types = c("date","text","text","text","text","text","numeric",
                                             "numeric","text")))
  # display table ----
  # output$tb <- renderTable({
  #   tb <- part_tb()
  # })
  
  # Choose report mth ----
  # i <- reactive({
  #   match(input$mth,bln)
  # })
  
  imth <- renderText({
    i <-  match(input$mth,bln)
  })
 
  # Download Handler -----
  
  output$report <- downloadHandler(
    
    file <-  "report.docx",
    content <-  function(file) {
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      # pass table to report
      att_tb <- att_tb()
      bqt_tb <- bqt_tb()
      exp_tb <- exp_tb()
      part_tb <- part_tb()
      i <- imth()

      tempReport <- file.path(tempdir(), "report.Rmd")
      # tempData <- file.path(tempdir(), "f&b.RData")
      file.copy("fbreport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass dataset to Rmd document
      params <- list(att.tb = att_tb,
                     bqt.tb = bqt_tb,
                     exp.tb = exp_tb,
                     part.tb = part_tb,
                     bln = i)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)