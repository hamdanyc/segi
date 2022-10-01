# funType.R

# Init ----

library(stringr)
library(dplyr)
library(lubridate)

load("fb.RData")

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

# stat.lst <- tibble("word"=sta,type="stationery")
# bev.lst <- tibble("word"=bev,type="food & beverage")
# term.lst <- bind_rows(stat.lst,bev.lst)

# fun type ----
type <- function(txt,pattern){
  txt <- tolower(txt)
  term <- str_replace_all(txt, " ","|")
  lapply(term, function(x) str_detect(pattern, x)) %>% unlist()
}

# usage ----
txt <- "avian water"

case_when (any(type(txt,bev)) ~ "Makanan & Minuman",
           any(type(txt,sta)) ~ "Alat Tulis",
           TRUE ~ "Am")

case_when (any(type(t,sta)) ~ "Alat Tulis",
           any(type(t,wed)) ~ "Majlis Perkahwinan",
           any(type(t,mmr)) ~ "MMR",
           FALSE ~ "Am")

# exp
sapply(exp_tb$ITEM, function (t){
  case_when (any(type(t,sta)) ~ "Alat Tulis",
             any(type(t,bev)) ~ "Makanan & Minuman",
             TRUE ~ "Am")
})

# bqt ----
sapply(bqt_tb$EVENT, function (t){
  case_when (any(type(t,smr)) ~ "Seminar/Bengkel/Mesyuarat/hospitaliti",
             any(type(t,wed)) ~ "Majlis Perkahwinan",
             any(type(t,mmr)) ~ "MMR",
             TRUE ~ "Am")
})

bqt.sum <- bqt_tb %>%
  inner_join(part_tb, by = "EO") %>%
  filter(month(DATE.x) == 9) %>% 
  mutate("MGU" = isoweek(DATE.x),
         "JENIS" = sapply(EVENT, function (t){
           case_when (any(type(t,smr)) ~ "Seminar/Bengkel/Mesyuarat/hospitaliti",
                      any(type(t,wed)) ~ "Majlis Perkahwinan",
                      any(type(t,mmr)) ~ "MMR",
                      TRUE ~ "Am")
         })) %>%
  group_by(MGU,JENIS) %>%
  summarise("PENDAPATAN" = sum(REVENUE, na.rm=TRUE),
            "PERBELANJAAN" = sum(QTY * PRICE * 8))

bqt.type <- bqt.sum %>% 
  group_by(JENIS) %>% 
  summarise("JLH" = sum(PENDAPATAN))

# Exp summary ----
exp.sum <- exp_tb %>%
  filter(month(DATE) == 9) %>% 
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

nrow(exp.type)
exp.type$JENIS[1]
exp.type$JENIS[2]
exp.type$JENIS[3]
sum(exp.type$jlh)


exp.sum %>% 
  group_by(MGU, JENIS) %>%
  summarise("JUMLAH" = sum(JUMLAH)) %>%
  spread(JENIS,JUMLAH)
