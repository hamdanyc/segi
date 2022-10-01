# dropBox.R

library(rdrop2)
library(shiny)

drop_auth()

# save authentication ----
# token <- drop_auth()
# saveRDS(token, file = "token.rds")

token <- readRDS("token.rds")
drop_dir(path = "", recursive = FALSE, include_media_info = FALSE,
         include_deleted = FALSE, include_has_explicit_shared_members = FALSE,
         include_mounted_folders = TRUE, limit = NULL, cursor = FALSE,
         dtoken = token) %>% View()

drop_download(path = "/wp/fbtable.xlsx",overwrite = TRUE)
df_tb <- readxl::read_excel("fbtable.xlsx")

ui <- fluidPage(
  tableOutput("table") 
)

server <- function(input, output, session) {
  output$table <- renderTable( {
    df
  })
}

shinyApp(ui, server)
