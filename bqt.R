library(tidyverse)
library(lubridate)

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
sta <- c("pencil","pad","pen","marker","box","envelope","examination",
         "note","stapler","clip","rubber", "band","projektor","form",
         "pensil","board","white","klip","kertas","pemadam","borang",
         "buku","catatan","kotak","plastik","plastic","sampah","paper")

bev <- c("water","mineral","bottle","sweet","tea","coffee","jus","napkin",
         "air","minuman","botol","gula","susu","krim","teh","kopi","dinner",
         "buah","kacang","peanut","sugar","carton","karton","juice","lunch",
         "cordial","kordial","sirap","syrup","lemon","limau","maruku","kucing",
         "sack","pack","nescafe","fruit","milk","creamer","diary","sachet")

# Seminar/Bengkel/Mesyuarat/hospitaliti
smr <- c("seminar","meeting","product","presentation","persembahan","breakfast","iftar",
         "lunch","dinner","makan","bash","birthday","executive","talk","luncheon",
         "discussion","circle","preview","courtesy","kunjungan","minister","deputy",
         "perbincangan","eksesais","food","tasting","update","kempen","board","koordinasi",
         "chief","launch","pelancaran","demonstration","workshop","lab","bengkel",
         "makmal","session","review","tetamu","guest","foreign","army","navy","air",
         "conference","ulang","tahun","jadi","hari","group","company","syarikat")

# Majlis Perkahwinan
wed <- c("perkahwinan","wedding","reception","resepsi","pertunangan","akikah")

# MMR
mmr <- c("mess","night","regimental","rejimental","beradat","santapan","agong")

type <- function(txt,pattern){
  txt <- tolower(txt)
  term <- str_replace_all(txt, " ","|")
  lapply(term, function(x) str_detect(pattern, x)) %>% unlist()
}

bqt_tb <- readxl::read_excel("fbtable.xlsx", sheet = "bqt",
col_types = c("date","text","text","text","text","text","numeric",
"numeric","numeric","text"))

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
  group_by(MGU,JENIS) %>%
  summarise("PENDAPATAN" = sum(REVENUE, na.rm=TRUE),
            "PERBELANJAAN" = sum(PSM * RATE * 8, na.rm=TRUE),
            "NET" = PENDAPATAN - PERBELANJAAN,
            "NOEVENT" = n())

save.image("bqt.RData")

bqt.sum %>%
  group_by(MGU) %>%
  summarise("PENDAPATAN" = round(sum(PENDAPATAN, na.rm=TRUE),0),
            "PERBELANJAAN" = round(sum(PERBELANJAAN, na.rm=TRUE),0),
            "NET" = PENDAPATAN - PERBELANJAAN) %>% 
  tidyr::gather(JENIS,VALUE,2:4)


# Staf summary ----
part.sum <- bqt_tb %>%
  filter(month(DATE) == 9) %>%
  summarise( "ACARA" = n(),"PSM" = sum(PSM))

bqt.sum %>%
  group_by(MGU) %>%
  summarise("PENDAPATAN" = round(sum(PENDAPATAN, na.rm=TRUE),0),
            "PERBELANJAAN" = round(sum(PERBELANJAAN, na.rm=TRUE),0),
            "NET" = PENDAPATAN - PERBELANJAAN) %>% 
  tidyr::gather(JENIS,VALUE,2:4) %>%
  tschart(y = "VALUE", fillvar = "JENIS")
  

ggplot(aes(x = MGU, fill = JENIS, na.rm = TRUE, y = VALUE)) +
  geom_bar(stat="identity",position = position_dodge(width = 0.5)) +
  geom_text(aes(label=format(VALUE,digits = 3,big.mark=",")), vjust=1.6, color="black",
            size=3.5) +
  xlab("Minggu") + ylab("JUMLAH (RM)") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Revenue of Event by type
bqt.sum %>%
  group_by(MGU,JENIS) %>% 
  summarise("PENDAPATAN" = sum(PENDAPATAN, na.rm=TRUE)) %>%
  tschart(y = "PENDAPATAN", fillvar = "JENIS")
