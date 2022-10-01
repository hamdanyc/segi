# tsWeb.R

# Init ----

library(shiny)
library(tidyverse)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

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
      selectInput("dataset", "Saring mengikut:",
                  c("Keseluruhan" ,"Pekerja" )),
  
      # Input: Selector for details plot ----
      selectInput("varset", "Pilih (Kategori):",
                  c("Waktu biasa",
                    "Lebih-masa",
                    "Sakit",
                    "Cuti-rehat")),

      # Download button ----
      tags$hr(),
      downloadButton("report", "Muat turun laporan")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot of the requested variable ----
      plotOutput("tsPlot"),
      tableOutput("tb")
      ),
  )
)

# Define server logic ----
server <- function(input, output) {
  # read data file ----
  df <- reactive({
    tbs <- readxl::excel_sheets(input$file$datapath)
    emp_ts <- lapply(tbs, function(x) readxl::read_xlsx(input$file$datapath,
                                                        range = "D38:G38",
                                                        col_names = c("rh","ot","sl","vc"),
                                                        col_types = "numeric",
                                                        sheet = x))
    
    emp_tb <- dplyr::bind_rows(lapply(emp_ts, as.data.frame.list)) %>% 
      cbind("emp"=tbs)
    return(emp_tb)
  })
  
  # Time Sheet Summary ----
  tf <- reactive({
    emp_tb <- df()
    
    # Total Reg hr by employee
    # chart total (hour) by type ----
    ts <- emp_tb %>%
      summarise("Waktu-biasa" = sum(rh), "Lebih-masa" = sum(ot),
                "Sakit" = sum(sl), "Cuti-rehat" = sum(vc))

    # gather col into rows
    ts <- ts %>% tidyr::gather(key = "type")
    return(ts)
  })
  
  # Choose dataset and variable ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Keseluruhan" = tf(),
           "Pekerja" = df())
  })
  
  varInput <- reactive({
    emp_tb <- df()
    switch(input$varset,
           "Waktu biasa" = emp_tb$rh,
           "Lebih-masa" = emp_tb$ot,
           "Sakit" = emp_tb$sl,
           "Cuti-rehat" = emp_tb$vc)
  })
  
  # plot ----
  output$tsPlot <- renderPlot({
    
    emp_tb <- df()
    ts <- tf()
    dataset <- datasetInput()
    
    type <- switch(input$dataset,
                   "Keseluruhan" = ts$type,
                   "Pekerja" = emp_tb$emp)
    
    value <- varInput()
    
    ggplot(dataset, aes(x = type, fill = type, na.rm = TRUE, y=value)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=format(value,digits = 3,big.mark=",")), vjust=1.6, color="white",
                size=3.5) +
      xlab("Type") + ylab("Hour") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  })
  
  # display table ----
  output$tb <- renderTable({
    vs <- datasetInput()
    tb_set <- function(vs){
      emp_tb <- df()
      names(emp_tb) <- c("Waktu-biasa","Lebih-masa","Sakit","Cuti-rehat","Nama")
      switch(input$varset,
             "Waktu biasa" = emp_tb[,c("Nama","Waktu-biasa")],
             "Lebih-masa" = emp_tb[,c("Nama","Lebih-masa")],
             "Sakit" = emp_tb[,c("Nama","Sakit")],
             "Cuti-rehat" = emp_tb[,c("Nama","Cuti-rehat")])
    }
    switch(input$dataset,
           "Keseluruhan" = tf(),
           "Pekerja" = tb_set())
  })
  
  # Download Handler -----
  
  output$report <- downloadHandler(
    
    file = "report.docx",
    content = function(file) {
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      emp_tb <- df()
      tempReport <- file.path(tempdir(), "report.Rmd")
      tempData <- file.path(tempdir(), "f&b.RData")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass dataset to Rmd document
      params <- list(emp.tb=emp_tb)
      
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