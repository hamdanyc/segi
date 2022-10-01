# cuti/app.R

library(shiny)
library(dplyr)
library(readr)

# load(file = "cuti.RData", envir = environment())

cuti <- read_csv("cuti.csv")
st_al <- read_csv("st_al.csv")
cg <- read_csv("cg.csv")
rod <- read_csv("rod.csv")
names(cuti) <- c("Id","Nama","Tkh Khidmat","Cuti Ke hadapan","Kelayakan")
names(st_al) <- c("Nama","Tkh Cuti")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Kelayakan Cuti Tahun"),

    # Sidebar with elements input/output 
    sidebarLayout(
        sidebarPanel(
            selectInput("name", "Nama:",
                        choices = cuti$Nama),
            checkboxGroupInput("ct_type", "Jenis Cuti:",
                        c("Tahunan" = "al",
                          "ROD" = "rod",
                          "Gantian" = "cg"),selected = "al"),
            textInput("al_add", "Permohonan Cuti", lubridate::today()),
            actionButton("go", "Tambah", class = "btn-success"),
            actionButton("save", "Simpan", class = "btn-success")
        ),

        # Show panel
        mainPanel(
            tabsetPanel(
                tabPanel("Profail", tableOutput("al_tb")),
                tabPanel("Kelayakan, Cuti Diambil & Baki", tableOutput("al_st")),
                tabPanel("ROD", tableOutput("rod")),
                tabPanel("Cuti Gantian", tableOutput("cg")),
                tabPanel("Cuti/ROD/Gantian", tableOutput("al"))
            ),
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$al_tb <- renderTable(digits = 0,{
        cuti[,1:5]
    })
    
    output$al_st <- renderTable(digits = 0,{
        if (input$ct_type == "al") {
            tb() %>% inner_join(cuti) %>% 
                group_by(Nama) %>%
                summarise("Kelayakan"=first(Kelayakan), "Jlh cuti"=n(), "Baki" = first(Kelayakan)-n())
        }
        else if (input$ct_type == "rod") {
            tb() %>% inner_join(cuti) %>% 
                group_by(Nama) %>%
                summarise("Kelayakan"=first(Kelayakan), "ROD"=n(), "Jlh Keseluruhan" = first(Kelayakan)+ROD)
        }
        else if (input$ct_type == "cg") {
            tb() %>% inner_join(cuti) %>% 
                group_by(Nama) %>%
                summarise("Kelayakan"=first(Kelayakan), "Jlh Cuti Gantian"=n())
        }

    })
    
    output$rod <- renderTable({
        tro <- rod %>% group_by(Nama) %>%
            summarise("ROD" = n())
        tro[tro$Nama == input$name,]
        
    })
    
    output$cg <- renderTable({
        tcg <- cg %>% group_by(Nama) %>% 
            summarise("Cuti Gantian" = n())
        tcg[tcg$Nama == input$name,]
    })
    
    # select db
    tb <- reactive({
        if(input$ct_type == "al"){st_al}
        else if(input$ct_type == "rod"){rod}
        else if(input$ct_type == "cg"){cg}
    })
    
    output$al <- renderTable({
        tb <- tb()
        tb[tb$Nama == input$name,]
    })
    
    observeEvent(input$go, {
        tb <- tb()
        tmp <- tibble::add_row(tb,Nama=input$name,`Tkh Cuti`=as.character(input$al_add))
        st_al <- tmp
        tb(st_al)
    }, once = TRUE)

    observeEvent(input$save, {
        write_csv(tibble::as_tibble(tb()),"st_al.csv")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
