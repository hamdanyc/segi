# fbtime.R
# use with tsproc.R

# Init ----

library(shiny)
library(tidyverse)
library(rdrop2)
library(lubridate)
library("xlsx")

bln <- factor(c("JAN","FEB","MAC","APR","MEI","JUN",
                "JUL","OGOS","SEP","OKT","NOV","DIS"),
              levels = c("JAN","FEB","MAC","APR","MEI","JUN",
                         "JUL","OGOS","SEP","OKT","NOV","DIS"))

options(digits = 0)

# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Food and Beverage Time Sheet"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            # fileInput("file", "Fail Data"),
            
            # Input: Selector dataset to plot ----
            # selectInput("mth", "Pilih bulan:",
            #             bln),
            
            # Input: Selector dataset form df ----
            h3("Nama"),
            uiOutput("id")
            
            # Download button ----
            # tags$hr(),
            # downloadButton("report", "Muat turun laporan")
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            h3("Month (hr)"),
            tableOutput("mth"),
            h3("Total (hr)"),
            tableOutput("all")
        ),
    )
)

server <- function(input, output) {
    
    # read data file ----
    # get file from drop
    token <- readRDS("token.rds")
    drop_download(path = "/wp/fbtime.RData",overwrite = TRUE,dtoken = token)
    load("fbtime.RData")

    output$id <- renderUI({
        df <- att_ts %>%
            distinct(Name)
        selectInput("id", "Name", df$Name)
     })
    
    # output$columns = renderUI({
    #     mydata = get(input$dataset)
    #     selectInput('columns2', 'Columns', names(mydata))
    # })
    
    imth <- renderText({
        i <-  match(input$mth,bln)
    })
    
    output$nama <- reactive({
        df <- att_ts %>%
            distinct(Name) %>% 
            filter(Name == input$id)
        nama <- df$Name
    })
    
    # display table ----
    output$tb <- renderTable({
        # select ts for id
        att_ts %>%
            mutate("Mth" = bln[month(Dt)]) %>% 
            filter(Name == input$id) %>%
            filter(Mth == bln[imth()]) %>% 
            group_by(Name) %>%
            summarise( "OT" = case_when(sum(Hr,na.rm = TRUE) < 104 ~ sum(Hr,na.rm = TRUE),
                                        TRUE ~ 104),
                       "CH" = case_when(OT >= 104 ~ sum(Hr,na.rm = TRUE) - 104,
                                        TRUE ~ 0) %>% 
                           sum(na.rm = TRUE))
    })
    
    # display table ----
    output$mth <- renderTable({
        # summary all
        att_ts %>%
            filter(Name == input$id) %>%
            mutate("Mth" = bln[month(Dt)]) %>%
            group_by(Mth,Name) %>%
            summarise( "OT" = case_when(sum(Hr,na.rm = TRUE) < 104 ~ sum(Hr,na.rm = TRUE),
                                        TRUE ~ 104),
                       "CH" = case_when(OT >= 104 ~ sum(Hr,na.rm = TRUE) - 104,
                                        TRUE ~ 0) %>% 
                           sum(na.rm = TRUE))
    },digits = 0)
    
    # display table ----
    output$all <- renderTable({
        # summary all
        ts <- att_ts %>%
            filter(Name == input$id) %>%
            mutate("Mth" = bln[month(Dt)]) %>%
            group_by(Mth,Name) %>%
            summarise( "OT" = case_when(sum(Hr,na.rm = TRUE) < 104 ~ sum(Hr,na.rm = TRUE),
                                        TRUE ~ 104),
                       "CH" = case_when(OT >= 104 ~ sum(Hr,na.rm = TRUE) - 104,
                                        TRUE ~ 0) %>% 
                           sum(na.rm = TRUE))
        ts[,c("OT","CH")] %>% 
            colSums() %>% 
            t()
        
    },digits = 0)
 
}

shinyApp(ui = ui, server = server)