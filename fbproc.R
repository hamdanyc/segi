# fbproc.R

# Init ----
library(tidyverse)
library(lubridate)
library(rdrop2)
library(cellranger)

options(scipen = 999)

# get file from drop ----
token <- readRDS("token.rds")
drop_download(path = "/wp/fbtable.xlsx",overwrite = TRUE,dtoken = token)
drop_download(path = "/wp/patt.xlsx",overwrite = TRUE,dtoken = token)

att_tb <- readxl::read_excel("fbtable.xlsx", sheet = "att",range = cell_cols("A:Q"),
                             col_types = c("date",replicate(16,"numeric")))
exp_tb <-  readxl::read_excel("fbtable.xlsx", sheet = "exp",
                              col_types = c("date","text","numeric","numeric","numeric"))
bqt_tb <- readxl::read_excel("fbtable.xlsx", sheet = "bqt",
                             col_types = c("date","text","text","text","text","text","numeric",
                                           "numeric"))
bqt_tb$REVENUE <- round(bqt_tb$REVENUE)
psm_tb <-  readxl::read_excel("fbtable.xlsx", sheet = "psm",
                              col_types = c("date","text","text","numeric","numeric"))
df_tb <- readxl::read_excel("patt.xlsx", col_types = c("text","text"))

mth <- month(today())

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

type <- function(txt,pattern){
  txt <- tolower(txt)
  term <- str_replace_all(txt, " ","|")
  lapply(term, function(x) str_detect(pattern, x)) %>% unlist()
}

# Banquet summary ----
bqt.sum <- psm_tb %>%
  right_join(bqt_tb, by = c("EO","DATE")) %>% 
  filter(month(DATE) == mth) %>% 
  mutate("MGU" = isoweek(DATE),
         "JENIS" = sapply(EVENT.y, function (t){
           case_when (any(type(t,smr)) ~ "Seminar/Mesyuarat",
                      any(type(t,wed)) ~ "Majlis Perkahwinan",
                      any(type(t,mmr)) ~ "MMR",
                      TRUE ~ "Am")
         })) %>%
  group_by(MGU,JENIS) %>%
  summarise("PERBELANJAAN" = round(sum(PSM * RATE * 8, na.rm=TRUE)), 
            "PENDAPATAN" = sum(REVENUE, na.rm = TRUE),
            "NOEVENT" = n())

bqt.type <- bqt.sum %>% 
  group_by(JENIS) %>% 
  summarise("PERBELANJAAN" = sum(PERBELANJAAN), 
            "PENDAPATAN" = sum(PENDAPATAN),
            "NOEVENT" = sum(NOEVENT))

bqt.net <- sum(bqt.sum$PENDAPATAN) - sum(bqt.sum$PERBELANJAAN)

# Staf summary ----
part.sum <- psm_tb %>%
  filter(month(DATE) == mth) %>%
  summarise( "ACARA" = n(),"PSM" = sum(PSM))

# Exp summary ----
exp.sum <- exp_tb %>%
  filter(month(DATE) == mth) %>% 
  mutate("MGU" = isoweek(DATE),
         "JUMLAH" = QTY * PRICE,
         "JENIS" = sapply(exp_tb$ITEM, function (t){
           case_when (any(type(t,sta)) ~ "ALAT TULIS",
                      any(type(t,bev)) ~ "MAKANAN & MINUMAN",
                      TRUE ~ "AM")
         })) %>% 
  group_by(MGU, JENIS) %>%
  summarise("JUMLAH" = sum(JUMLAH))

exp.type <- summarise(group_by(exp.sum,JENIS),"jlh" = sum(JUMLAH))

# Save Data ----
save.image(file = "fb.RData")
drop_upload(file = "fb.RData", path = "wp", dtoken = token)
