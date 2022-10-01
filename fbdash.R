# fbdash.R

library(shiny)
library(tidyverse)
library(lubridate)
library(flexdashboard)
library(shinydashboard)
library(rdrop2)


# ui -----
ui <- dashboardPage(
    dashboardHeader(title = "F&B dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Petunjuk Prestasi Utama (KPI)", tabName = "kpi", icon = icon("dashboard")),
            menuItem("Pendapatan & Perbelanjaan", icon = icon("chart-bar"), tabName = "plchart"),
            menuItem("Kategori", icon = icon("chart-bar"), tabName = "ctchart")
        )
    ),
    
    # body ----
    dashboardBody(
        tabItems(
            tabItem(tabName = "kpi",
                    # Boxes need to be put in a row (or column)
                    fluidRow(
                        box(title = "Am", gaugeOutput("am")),
                        box(title = "Majlis Perkahwinan",gaugeOutput("wed")),
                        box(title = "Makan Malam Rejimental",gaugeOutput("mmr")),
                        box(title = "Seminar & Mesyuarat",gaugeOutput("smr"))
                    )
            ),
            tabItem(tabName = "plchart",
                    # Boxes need to be put in a row (or column)
                    fluidRow(
                        box(plotOutput("bqt"))
                    )
                    
            ),
            tabItem(tabName = "ctchart",
                    # Boxes need to be put in a row (or column)
                    fluidRow(
                        box(title = "Am",plotOutput("c.am")),
                        box(title = "Majlis Perkahwinan",plotOutput("c.wed")),
                        box(title = "Makan Malam Rejimental",plotOutput("c.mmr")),
                        box(title = "Seminar & Mesyuarat",plotOutput("c.smr"))
                    )
            )
        )
    )
)

# server ----
server <- function(input, output) {
    
    # get file from drop ----
    token <- readRDS("token.rds")
    drop_download(path = "/wp/fbtable.xlsx",overwrite = TRUE,dtoken = token)
    drop_download(path = "/wp/patt.xlsx",overwrite = TRUE,dtoken = token)
    
    att_tb <- readxl::read_excel("fbtable.xlsx", sheet = "att", range = "A2:Q62",
                                 col_types = c("date",replicate(16,"numeric")))
    exp_tb <-  readxl::read_excel("fbtable.xlsx", sheet = "exp",
                                  col_types = c("date","text","numeric","text","numeric"))
    bqt_tb <- readxl::read_excel("fbtable.xlsx", sheet = "bqt",
                                 col_types = c("date","text","text","text","text","text","numeric",
                                               "numeric"))
    df_tb <- readxl::read_excel("patt.xlsx", col_types = c("text","text"))
    
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
        # filter(month(DATE) == 9) %>% 
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
                  "PERBELANJAAN" = sum(25 * RATE * 8, na.rm=TRUE))
    
    bqt.type <- bqt.sum %>% 
        group_by(JENIS) %>% 
        summarise("JLH" = sum(PENDAPATAN))
    
    bqt.net <- sum(bqt.sum$PENDAPATAN) - sum(bqt.sum$PERBELANJAAN)

    output$am = renderGauge({
        gauge(bqt.sum$PENDAPATAN[1], 
              min = 0, 
              max = 15000,
              label = bqt.sum$JENIS[1],
              sectors = gaugeSectors(success = c(5000, 15000), 
                                     warning = c(1000, 4999),
                                     danger = c(0, 999)))
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
    
    # smr ----
    output$smr = renderGauge({
        gauge(bqt.sum$PENDAPATAN[4], 
              min = 0, 
              max = 150000,
              label = bqt.sum$JENIS[4],
              sectors = gaugeSectors(success = c(50000, 150000), 
                                     warning = c(10000, 49999),
                                     danger = c(0, 9000)))
    })
    
    # bqt ----
    output$bqt <- renderPlot({
        # Revenue and Exp by Week
        bqt.sum %>%
             summarise("PENDAPATAN" = round(sum(PENDAPATAN, na.rm=TRUE),0),
                      "PERBELANJAAN" = round(sum(PERBELANJAAN, na.rm=TRUE),0),
                      "NET" = PENDAPATAN - PERBELANJAAN) %>% 
            tidyr::gather(JENIS,VALUE,1:3) %>%
            ggplot(aes(x = factor(JENIS, level = c('PENDAPATAN', 'PERBELANJAAN', 'NET')),
                       fill = JENIS, na.rm = TRUE, y = VALUE)) +
            geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
            geom_text(aes(label=format(VALUE,digits = 3,big.mark=",")), vjust=1.6, color="black",
                      size=3.5) +
            xlab("Kategori") + ylab("Jumlah (RM)") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    })
    
    # c.am
    output$c.am <- renderPlot({
        bqt.sum %>%
            group_by(JENIS) %>%
            filter(JENIS == "Am") %>% 
            summarise("PENDAPATAN" = round(sum(PENDAPATAN, na.rm=TRUE),0),
                      "PERBELANJAAN" = round(sum(PERBELANJAAN, na.rm=TRUE),0),
                      "NET" = PENDAPATAN - PERBELANJAAN) %>% 
            tidyr::gather(JENIS,VALUE) %>%
            ggplot(aes(x = factor(JENIS, level = c('PENDAPATAN', 'PERBELANJAAN', 'NET')),
                       fill = JENIS, na.rm = TRUE, y = VALUE)) +
            geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
            geom_text(aes(label=format(VALUE,digits = 3,big.mark=",")), vjust=1.6, color="black",
                      size=3.5) +
            xlab("Am") + ylab("Jumlah (RM)") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    })
    
    # c.mmr
    output$c.mmr <- renderPlot({
        bqt.sum %>%
            group_by(JENIS) %>%
            filter(JENIS == "MMR") %>% 
            summarise("PENDAPATAN" = round(sum(PENDAPATAN, na.rm=TRUE),0),
                      "PERBELANJAAN" = round(sum(PERBELANJAAN, na.rm=TRUE),0),
                      "NET" = PENDAPATAN - PERBELANJAAN) %>% 
            tidyr::gather(JENIS,VALUE) %>%
            ggplot(aes(x = factor(JENIS, level = c('PENDAPATAN', 'PERBELANJAAN', 'NET')),
                       fill = JENIS, na.rm = TRUE, y = VALUE)) +
            geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
            geom_text(aes(label=format(VALUE,digits = 3,big.mark=",")), vjust=1.6, color="black",
                      size=3.5) +
            xlab("Makan Malam Rejimental (MMR)") + ylab("Jumlah (RM)") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    })
    
    # c.smr
    output$c.smr <- renderPlot({
        bqt.sum %>%
            group_by(JENIS) %>%
            filter(JENIS == "Seminar/Mesyuarat") %>% 
            summarise("PENDAPATAN" = round(sum(PENDAPATAN, na.rm=TRUE),0),
                      "PERBELANJAAN" = round(sum(PERBELANJAAN, na.rm=TRUE),0),
                      "NET" = PENDAPATAN - PERBELANJAAN) %>% 
            tidyr::gather(JENIS,VALUE) %>%
            ggplot(aes(x = factor(JENIS, level = c('PENDAPATAN', 'PERBELANJAAN', 'NET')),
                       fill = JENIS, na.rm = TRUE, y = VALUE)) +
            geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
            geom_text(aes(label=format(VALUE,digits = 3,big.mark=",")), vjust=1.6, color="black",
                      size=3.5) +
            xlab("Seminar/Mesyuarat") + ylab("Jumlah (RM)") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    })
    
    # c.wed
    output$c.wed <- renderPlot({
        bqt.sum %>%
            group_by(JENIS) %>%
            filter(JENIS == "Majlis Perkahwinan") %>% 
            summarise("PENDAPATAN" = round(sum(PENDAPATAN, na.rm=TRUE),0),
                      "PERBELANJAAN" = round(sum(PERBELANJAAN, na.rm=TRUE),0),
                      "NET" = PENDAPATAN - PERBELANJAAN) %>% 
            tidyr::gather(JENIS,VALUE) %>%
            ggplot(aes(x = factor(JENIS, level = c('PENDAPATAN', 'PERBELANJAAN', 'NET')),
                       fill = JENIS, na.rm = TRUE, y = VALUE)) +
            geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
            geom_text(aes(label=format(VALUE,digits = 3,big.mark=",")), vjust=1.6, color="black",
                      size=3.5) +
            xlab("Majlis Perkahwinan") + ylab("Jumlah (RM)") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    })
    
}

shinyApp(ui = ui, server = server)