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
      
      # Input: Select a file ----
      fileInput("file", "Fail Data"),
 
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
  xl <- reactive(
    req(input$file$datapath)
  )
  
  att_tb <- reactive(readxl::read_excel(xl(), sheet = "att",
                                        col_types = c("date",replicate(16,"numeric"))))
  exp_tb <-  reactive(readxl::read_excel(xl(), sheet = "exp",
                                         col_types = c("date","text","numeric","text","numeric")))
  bqt_tb <- reactive(readxl::read_excel(xl(), sheet = "bqt",
                               col_types = c("date","text","text","text","text","text","numeric",
                                             "numeric","numeric","text")))
  df_tb <- readxl::read_excel("patt.xlsx", col_types = c("skip","text","text"))
  
  # display table ----
  output$tb <- renderTable({
    # count no rows
    att.cnt <- nrow(att_tb())
    exp.cnt <- nrow(exp_tb())
    # part.cnt <- nrow(part_tb())
    bqt.cnt <- nrow(bqt_tb())
    df.cnt <- nrow(df_tb)
    tb <- tibble("Attendance" = att.cnt, "Expenses" = exp.cnt,
                 "Banquet" = bqt.cnt, "Kategori" = df.cnt) %>% 
      tidyr::gather("Data","Bilangan")
  })
  
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
      i <- imth()
 
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("fbreport.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass dataset to Rmd document
      params <- list(att.tb = att_tb,
                     bqt.tb = bqt_tb,
                     exp.tb = exp_tb,
                     df.tb = df_tb,
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