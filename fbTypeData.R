# fbdfData.R

# sta <- c("pencil","pad","pen","marker","box","envelope","examination","folder",
#          "note","stapler","clip","rubber", "band","projektor","form","skrin","book",
#          "pensil","board","white","klip","kertas","pemadam","borang","projector",
#          "buku","catatan","kotak","plastik","plastic","sampah","paper","garbage")
# 
# bev <- c("water","mineral","bottle","sweet","tea","coffee","jus","napkin","sweetened",
#          "air","minuman","botol","gula-gula","susu","krim","teh","kopi","dinner","sarapan",
#          "buah","kacang","peanut","sugar","carton","karton","juice","lunch","krimer",
#          "cordial","kordial","sirap","syrup","lemon","limau","maruku","kucing","mint",
#          "grocery","flour","biskut","milo","kotak","ml","grams","liter","blend",
#          "sack","pack","nescafe","fruit","milk","creamer","diary","sachet","wangian")
# 
# # Seminar/Bengkel/Mesyuarat/hospitaliti
# smr <- c("seminar","meeting","product","presentation","persembahan","breakfast","iftar",
#          "lunch","dinner","makan","bash","birthday","executive","talk","luncheon","sambutan",
#          "discussion","circle","preview","courtesy","kunjungan","minister","deputy","hi-tea",
#          "perbincangan","eksesais","food","tasting","update","kempen","board","koordinasi",
#          "chief","launch","pelancaran","demonstration","workshop","lab","bengkel","jamuan",
#          "makmal","session","review","tetamu","guest","foreign","army","navy","air",
#          "perasmian","pembukaan","forum","wacana","simposium","ucaptama","penutup","anjuran",
#          "kursus","course","pameran","pelancaran","program",
#          "conference","ulang","tahun","harijadi","group","company","syarikat")
# 
# # Majlis Perkahwinan
# wed <- c("perkahwinan","wedding","reception","resepsi","pertunangan",
#          "akikah","ceremony","majlis")
# 
# # MMR
# mmr <- c("mess","night","regimental","rejimental","beradat","santapan","agong")
# 
# sta.tb <- tibble::tibble("item" = sta,"cat" = "sta")
# bev.tb <- tibble::tibble("item" = bev,"cat" = "bev")
# smr.tb <- tibble::tibble("item" = smr,"cat" = "smr")
# wed.tb <- tibble::tibble("item" = wed,"cat" = "wed")
# mmr.tb <- tibble::tibble("item" = mmr,"cat" = "mmr")
# df <- dplyr::bind_rows(sta.tb,bev.tb,smr.tb,wed.tb,mmr.tb)

# Init ----
library(dplyr)

df <- readr::read_rds("patt.RDs")

# write to excel
xlsx::write.xlsx(df,"patt.xlsx",sheetName = "data")

tb <- c("sta","smr","bev","wed","mmr")

wr.xlsx <- function (x) {
  df %>% 
    filter(cat == x) %>%
    select(item) %>% 
    xlsx::write.xlsx("patt.xlsx",sheetName = x,append = TRUE)
}

sapply(tb, function(x) wr.xlsx(x))
# wr.xlsx("sta")
# wr.xlsx("smr")

pattern <- function(x){
  df %>% 
  filter(cat == x) %>% 
    select(item)
}

# usage ----
sta <- pattern("sta")$item

