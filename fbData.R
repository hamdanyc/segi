# fbData.R

# Init ----

library(tidyverse)
library(lubridate)

# read excel ----
att_tb <- readxl::read_excel("fbatt.xlsx",
                             col_types = c("date",replicate(16,"numeric")))
exp_tb <- readxl::read_excel("fbexp.xlsx",
                             col_types = c("date","text","numeric","text","numeric"))
part_tb <- readxl::read_excel("fbpart.xlsx",
                              col_types = c("date","text","numeric","numeric"))
bqt_tb <- readxl::read_excel("fbbqt.xlsx",
                             col_types = c("date","text","text","text","text","text","numeric",
                                           "numeric","text"))
# Analysis ----

# Fun chart ----
tschart <- function(df, xvar = "MGU", yvar = "RD",
                    fillvar = "factor(MGU)") {
  ggplot(df,
         aes_string(x = xvar, y = yvar, fill = fillvar, na.rm = TRUE),
         environment = environment()) + 
    geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
    geom_text(aes_string(label=format(yvar,digits = 3,big.mark=",")), vjust=1.6, color="black",
              size=3.5) +
    xlab("Minggu") + ylab(yvar) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    guides(fill=guide_legend(title="Minggu"))
}
# Summary on attendance ----
ts <- att_tb %>%
  mutate("MGU" = isoweek(DATE)) %>% 
  group_by("BULAN"=months(DATE),MGU) %>% 
  summarise("DUTY"=sum(DUTY, na.rm=TRUE),"RD"=sum(RD, na.rm=TRUE),
            "RO"=sum(RO, na.rm=TRUE),
            "RO"=sum(RO, na.rm=TRUE),"AL"=sum(AL, na.rm=TRUE),
            "EL"=sum(EL, na.rm=TRUE),"MC"=sum(MC, na.rm=TRUE),
            "PH"=sum(PH, na.rm=TRUE),"RPH"=sum(RPH, na.rm=TRUE),
            "CH"=sum(CH, na.rm=TRUE),"UPL"=sum(UPL, na.rm=TRUE),
            "PL"=sum(PL, na.rm=TRUE),"HD"=sum(HD, na.rm=TRUE),
            "AB"=sum(AB, na.rm=TRUE),"ML"=sum(ML, na.rm=TRUE),
            "RN"=sum(RN, na.rm=TRUE),"TM"=sum(TM, na.rm=TRUE))

# gather col into rows
# by month
ts %>% 
  group_by(BULAN) %>% 
  summarise("DUTY"=sum(DUTY, na.rm=TRUE),"RD"=sum(RD, na.rm=TRUE),
            "RO"=sum(RO, na.rm=TRUE),
            "RO"=sum(RO, na.rm=TRUE),"AL"=sum(AL, na.rm=TRUE),
            "EL"=sum(EL, na.rm=TRUE),"MC"=sum(MC, na.rm=TRUE),
            "PH"=sum(PH, na.rm=TRUE),"RPH"=sum(RPH, na.rm=TRUE),
            "CH"=sum(CH, na.rm=TRUE),"UPL"=sum(UPL, na.rm=TRUE),
            "PL"=sum(PL, na.rm=TRUE),"HD"=sum(HD, na.rm=TRUE),
            "AB"=sum(AB, na.rm=TRUE),"ML"=sum(ML, na.rm=TRUE),
            "RN"=sum(RN, na.rm=TRUE),"TM"=sum(TM, na.rm=TRUE)) %>%
  filter(BULAN == "September") %>%
  select(-1) %>% 
  tidyr::gather() %>%
  ggplot(aes(x = key, fill = key, na.rm = TRUE, y = value)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=format(value,digits = 3,big.mark=",")), vjust=1.6, color="white",
            size=3.5) +
  xlab("Kategori") + ylab("Bil") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "RD")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "RO")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "AL")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "EL")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "MC")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "PH")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "RPH")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "CH")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "UPL")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "PL")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "HD")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "AB")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "ML")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "RN")

ts %>%
  filter(BULAN == "September") %>% 
  tschart(y = "TM")

# mapply(mult_one,vars1,vars2)
# mapply(tschart,ts,ts[,3:11])

# Summary on Banquet ----
# n() of Event by type
bqt_tb %>%
  mutate("MGU" = isoweek(DATE),
         "JENIS" = case_when(
           ACTIVITIES_REPORTS == "MESS NIGHT" ~ "MESS NIGHT",
           ACTIVITIES_REPORTS %in% c("WEDDING (BUFFET)","WEDDING (DOME)") ~ "WEDDING",
           TRUE ~ "MEETING/SEMINAR/HOSPITALITY")) %>%
  group_by(JENIS,MGU) %>%
  summarise("NOEVENT" = n()) %>% 
  ggplot(aes(x = MGU, fill = JENIS, na.rm = TRUE, y = NOEVENT)) +
  geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
  geom_text(aes(label=format(NOEVENT,digits = 3,big.mark=",")), vjust=1.6, color="black",
            size=3.5) +
  xlab("Minggu") + ylab("Bil") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Revenue of Event by type
bqt_tb %>%
  mutate("MGU" = isoweek(DATE),
         "JENIS" = case_when(
           ACTIVITIES_REPORTS == "MESS NIGHT" ~ "MESS NIGHT",
           ACTIVITIES_REPORTS %in% c("WEDDING (BUFFET)","WEDDING (DOME)") ~ "WEDDING",
           TRUE ~ "MEETING/SEMINAR/HOSPITALITY")) %>% 
  group_by("BULAN"=months(DATE),MGU,JENIS) %>% 
  summarise("REVENUE" = sum(REVENUE, na.rm=TRUE)) %>%
  tschart(y = "REVENUE", fillvar = "JENIS")

# Revenue and Exp by Week
bqt_tb %>%
  mutate("MGU" = isoweek(DATE)) %>% 
  group_by(MGU) %>% 
  summarise("REVENUE" = sum(REVENUE, na.rm=TRUE),
            "EXP" = sum(REVENUE * 0.1)) %>% 
  tidyr::gather("JENIS","VALUE",2:3) %>% 
  tschart(y = "VALUE", fillvar = "JENIS")
  
# bqt %>% # RD
#   filter(BULAN == "September") %>% 
#   tschart(y = "EXP")

# bqt %>%
#   filter(BULAN == "September") %>% 
#   tschart(y = "LP")

# Summary on Expenses ----
exp_tb <- exp_tb %>%
  mutate("MGU" = isoweek(TARIKH),
         "JUMLAH" = KUANTITI * HARGA,
         "JENIS" = case_when(
           `PERBELANJAAN/PEMBELIAAN` %in% c("PENSIL","EXAMINATION PAD",
                                            "EXAMINATION PAD","PAPER TRAY",
                                            "PLASTIK SAMPAH" ,"LAMINATING POUCHES (A4)")
                                            ~ "ALAT TULIS",
           TRUE ~ "MINUMAN/MAKANAN"))

exp_tb %>%
  group_by(JENIS,MGU) %>%
  summarise("BELANJA" = sum(JUMLAH)) %>% 
  ggplot(aes(x = factor(MGU), fill = JENIS, na.rm = TRUE, y = BELANJA)) +
  geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
  geom_text(aes(label=format(BELANJA,digits = 3,big.mark=",")),
            hjust=0.1, vjust=0.5, color="black",
            size=3.5) +
  xlab("Minggu") + ylab("JUMLAH(RM)") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Summary on Part-Time ----

prt.sum <- part_tb %>% 
  mutate("EXP" = QTY * PRICE * 8) 
  

# Table Part-time joint banquet ----

# bqt_tb %>% 
#   inner_join(prt.sum,"DATE") %>%
#   group_by(DATE) %>%
#   select(DATE,EVENT.x,QTY,EXP,EO_NO) %>% 
#   unique(DATE) %>% 
#   view()
# 
# bqt.ev %>%
#   inner_join(part_tb, by = "DATE") %>% 
#   group_by(DATE,JENIS) %>% 
#   mutate("EXP" = QTY * PRICE * 8) %>%
#   summarise("EXP" = sum(EXP),"REVENUE" = sum(REVENUE)) %>%
#   view()  

