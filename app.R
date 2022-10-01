library(shiny)
library(tidyverse)
library(lubridate)
library(flexdashboard)

ui <- fluidPage(
    gaugeOutput("am"),
    gaugeOutput("wed"),
    gaugeOutput("mmr"),
    gaugeOutput("smr")
)

server <- function(input, output) {
    
    # get file from drop ----
    token <- readRDS("token.rds")
    drop_download(path = "/wp/fbtable.xlsx",overwrite = TRUE,dtoken = token)
    att_tb <- readxl::read_excel("fbtable.xlsx", sheet = "att",
                                 col_types = c("date",replicate(16,"numeric")))
    exp_tb <-  readxl::read_excel("fbtable.xlsx", sheet = "exp",
                                  col_types = c("date","text","numeric","text","numeric"))
    bqt_tb <- readxl::read_excel("fbtable.xlsx", sheet = "bqt",
                                 col_types = c("date","text","text","text","text","text","numeric",
                                               "numeric","numeric","text"))
    df_tb <- readxl::read_excel("patt.xlsx", col_types = c("skip","text","text"))
    
    # Fun chart ----
    tschart <- function(df, xvar = "MGU", yvar = "RD",
                        fillvar = "factor(MGU)") {
        ggplot(df,
               aes_string(x = xvar, y = yvar, fill = fillvar, na.rm = TRUE),
               environment = environment()) + 
            geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
            geom_text(aes_string(label=format(yvar,digits = 3,big.mark=",")), hjust=0.1,
                      vjust=0.5, color="black",size=3.5) +
            xlab("Minggu") + ylab(yvar) +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
            guides(fill=guide_legend(title="Minggu"))
    }
    
    # Fun type ----
    patt <- function(x){
        df_tb %>% 
            filter(cat == x) %>% 
            select(item)
    }
    
    # pattern
    sta <- patt("sta")$item
    bev  <- patt("bev")$item
    smr <- patt("smr")$item
    wed <- patt("wed")$item
    mmr <- patt("mmr")$item
    
    # Fun type ----
    type <- function(txt,pattern){
        txt <- tolower(txt)
        term <- str_replace_all(txt, " ","|")
        lapply(term, function(x) str_detect(pattern, x)) %>% unlist()
    }
    
    # banquet summary ----
    bqt.sum <- bqt_tb %>%
        filter(month(DATE) == 9) %>% 
        mutate("MGU" = isoweek(DATE),
               "JENIS" = sapply(EVENT, function (t){
                   case_when (any(type(t,smr)) ~ "Seminar/Mesyuarat",
                              any(type(t,wed)) ~ "Majlis Perkahwinan",
                              any(type(t,mmr)) ~ "MMR",
                              TRUE ~ "Am")
               }),
               "RATE" = case_when(JENIS == "MMR" ~ 9,
                                  TRUE ~ 8)) %>%
        group_by(JENIS) %>%
        summarise("PENDAPATAN" = sum(REVENUE, na.rm=TRUE),
                  "PERBELANJAAN" = sum(PSM * RATE * 8, na.rm=TRUE))
    
    bqt.type <- bqt.sum %>% 
        group_by(JENIS) %>% 
        summarise("JLH" = sum(PENDAPATAN))
    
    bqt.net <- sum(bqt.sum$PENDAPATAN) - sum(bqt.sum$PERBELANJAAN)

    output$am = renderGauge({
        gauge(bqt.sum$PENDAPATAN[1], 
              min = 0, 
              max = 150000,
              label = bqt.sum$JENIS[1],
              sectors = gaugeSectors(success = c(50000, 150000), 
                                     warning = c(10000, 49999),
                                     danger = c(0, 9000)))
    })
    
    # Wedding
    output$wed = renderGauge({
        gauge(bqt.sum$PENDAPATAN[2], 
              min = 0, 
              max = 150000,
              label = bqt.sum$JENIS[2],
              sectors = gaugeSectors(success = c(50000, 150000), 
                                     warning = c(10000, 49999),
                                     danger = c(0, 9000)))
    })
    
    # mmr
    output$mmr = renderGauge({
        gauge(bqt.sum$PENDAPATAN[3], 
              min = 0, 
              max = 150000,
              label = bqt.sum$JENIS[3],
              sectors = gaugeSectors(success = c(50000, 150000), 
                                     warning = c(10000, 49999),
                                     danger = c(0, 9000)))
    })
    
    # smr
    output$smr = renderGauge({
        gauge(bqt.sum$PENDAPATAN[4], 
              min = 0, 
              max = 150000,
              label = bqt.sum$JENIS[4],
              sectors = gaugeSectors(success = c(50000, 150000), 
                                     warning = c(10000, 49999),
                                     danger = c(0, 9000)))
    })
    
}

shinyApp(ui = ui, server = server)