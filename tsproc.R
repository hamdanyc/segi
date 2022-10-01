# tsproc.R

# Init ----

library(tidyverse)
library(lubridate)
library(xlsx)
library(readr)

load("attend.RData")

ts_read <- function(xlsx,rge){
  tbs <- readxl::excel_sheets(xlsx) # read tabs
  col_names <- c("Id","Name","Day","Date","In","Out","In_2","Out_2")
  ts <- lapply(tbs, function(x) readxl::read_xlsx(path = xlsx,
                                                  range = rge,
                                                  col_types = c("text"),
                                                  col_names = col_names,
                                                  sheet = x))
  att_tb <- dplyr::bind_rows(lapply(ts, as.data.frame.list)) # bind all tabs
}

# Get excel data ----
xls <- c("data/7_19.xlsx","data/8_19.xlsx","data/9_19.xlsx","data/10_19.xlsx")
rg <- c("B5:I35","B6:I35","B6:I35","B6:I35")
ct <- c("01-01-2019","21-01-2019","1-02-2019","5-02-2019",
       "06-02-2019","01-05-2019","19-05-2019","19-05-2019","22-05-2019",
       "05-06-2019","06-06-2019","11-08-2019","12-08-2019",
       "31-08-2019","01-09-2019","2-09-2019","9-09-2019",
       "16-09-2019","27-10-2019","28-10-2019","9-11-2019",
       "25-12-2019") %>% 
  lubridate::dmy()
cd <- c("Thn Baru","Hari Thaipusam","Hari Wilayah Persekutuan",
       "Tahun Baru Cina","Tahun Baru Cina","Hari Pekerja",
       "Hari Wesak","Hari Wesak","Hari Nuzul Al-Quran",
       "Hari Raya Aidilfitri","Hari Raya Aidilfitri",
       "Hari Raya Haji","Hari Raya Haji","Hari Kebangsaan",
       "Awal Muharram","Awal Muharram","Hari Keputeraan YDP Agong",
       "Hari Malaysia","Hari Deepavali","Cuti Hari Deepavali",
       "Maulidur Rasul","Hari Krismas")
ph <- tibble("Dt" = ct,"Remark" = cd)

# skip this part
# mapply(mult_one,vars1,vars2)
# tb <- mapply(df, function(x,x1) ts(x,x1))

# att_tb <- dplyr::bind_rows(lapply(tb, as.data.frame.list))

# t7 <- ts(xls[1],rg[1])
t8 <- ts_read(xls[2],rg[2])
t9 <- ts_read(xls[3],rg[3])
t10 <- ts_read(xls[4],rg[4])

# new data
# add_tb <- ts_read(xls[4],rg[4])

att_tb <- dplyr::bind_rows(t8,t9,t10)
# att_tb <- dplyr::bind_rows(add_tb) %>% 
  

# House Keeping / change Name Value
# Name = "NA" , ""Checked by,"
# subsetting df based on variable values
# newdata <- mydata[ which(mydata$gender=='F'
#                          & mydata$age > 65), ]
# att_tb[which(!is.na(att_tb$In_2)),c(2,7,8)]

att_tb <- att_tb %>% 
  filter(!is.na(Day)) %>% 
  filter(!is.na(Name))

att_tb$Name[att_tb$Name == "ALIF AZAM" ] <- "ALIF AZAM BIN AZIZAN"
att_tb$Name[att_tb$Name == "MUHAMAD ALIFHAIQAL BIN AZMAN" ] <- "MUHAMAD ALIF HAIQAL BIN AZMAN" 
att_tb <- att_tb %>% 
  mutate(In=replace(In,substr(In,3,3)!= ":",NA),
         Out=replace(Out,substr(Out,3,3)!= ":",NA),
         Out_2=replace(In,substr(In,3,3)!= ":",NA))

# insert id / employee table
id_tb <- att_tb %>% 
  select(Id, Name) %>% 
  unique()

# Create att_ts ----
rules <- read_csv("rules.csv")
purpose <- "Do preparation for event [event/function name]"
wf <- c(0,1:23)
ot <- c(8:23,0,1:7)
rin <- c(paste0(12,"am"),paste0(1:11,"am"),"12pm",paste0(1:11,"pm"))
rout <- c(paste0(8:11,"am"),"12pm",paste0(1:11,"pm"),paste0(0:7,"am"))
wf_ot <- tibble(wf,ot,rin,rout)
tb <- tidyr::gather(rules,key="To",value="value",-From)

ts <- att_tb %>%
  mutate("Hr_In" = as.numeric(substr(In,1,2)),
         "Hr_Out" = as.numeric(substr(Out,1,2)),
         "In_mi" = as.numeric(substr(In,3,4)),
         "Hr_In" = case_when(In_mi < 30 ~ Hr_In,
                             TRUE ~ Hr_In + 1),
         "WIn" = case_when(Hr_In == 13 ~ "1pm",
                           Hr_In == 14 ~ "2pm",
                           Hr_In == 15 ~ "3pm",
                           Hr_In == 16 ~ "4pm",
                           Hr_In == 17 ~ "5pm",
                           Hr_In == 18 ~ "6pm",
                           Hr_In == 19 ~ "7pm",
                           Hr_In == 20 ~ "8pm",
                           Hr_In == 21 ~ "9pm",
                           Hr_In == 22 ~ "10pm",
                           Hr_In == 23 ~ "11pm",
                           Hr_In == 0 ~ "12am",
                           TRUE ~ paste0(Hr_In,"am")),
         "WOut" = case_when(Hr_Out == 13 ~ "1pm",
                            Hr_Out == 14 ~ "2pm",
                            Hr_Out == 15 ~ "3pm",
                            Hr_Out == 16 ~ "4pm",
                            Hr_Out == 17 ~ "5pm",
                            Hr_Out == 18 ~ "6pm",
                            Hr_Out == 19 ~ "7pm",
                            Hr_Out == 20 ~ "8pm",
                            Hr_Out == 21 ~ "9pm",
                            Hr_Out == 22 ~ "10pm",
                            Hr_Out == 23 ~ "11pm",
                            Hr_Out == 0 ~ "12am",
                            TRUE ~ paste0(Hr_Out,"am")),) %>% 
  inner_join(wf_ot,by=c("Hr_In"="wf")) %>%
  select(Id,Name,Day, Date, In, Out,RIn ="rin",ROut ="rout",WIn,WOut) %>% 
  inner_join(tb,by=c("RIn"="From","WOut"="To"))

att_ts <- ts %>%
  mutate("Dt" = as.Date(Day,"%d-%m-%Y"),
         "DATE" = format(Dt,"%d"),
         "DAY" = wday(Dt,label = TRUE),
         "REASON" = purpose,
         "Hr_In" = ROut,
         "Hr_Out" = WOut,
         "Hr" = value) %>% 
  select(Id, Name, Dt, DAY, DATE, REASON, RIn, ROut, WIn, WOut, Hr_In, Hr_Out, Hr)

# save objects att_ts
save(list = c("att_ts","ph","att_tb","ts","rules","tb","ts_read"), file = "attend.RData")
save(att_ts, file = "fbtime.RData")

# Upload file to drop ----
token <- readRDS("token.rds")
rdrop2::drop_upload("fbtime.RData",path ="wp",dtoken = token)

# Monthly Claim Data ----
td <- seq(as.Date('2019-10-16'), as.Date('2019-11-15'), by = "1 days") %>% tibble()
names(td) <- "Dt"

# joint td with ts_claimt
tb_claimt <- att_ts %>%
  full_join(td, by = "Dt") %>%
  filter(Dt >= "2019-10-16" & Dt <= "2019-11-15") %>% 
  full_join(ph) %>% 
  drop_na(Name)

# create workbook ----
# separate df for sheet
nm <- tb_claimt$Name  %>% unique()
ls <- case_when(stringr::word(nm,1) %in% c("MUHAMMAD","MUHAMAD","MOHAMAD","MOHD","ABD",
                                           "NUR","WAN","MEGAT","NOR","ABU") ~ stringr::word(nm,2),
                TRUE ~ stringr::word(nm,1)
                )

wb <- createWorkbook(type="xlsx")

hd <- lapply(nm, function(x){
  paste0("Name: ",x,"\n","Department: FOOD AND BEVERAGE")
})

df <- lapply(nm, function(x){
  tb_claimt %>% 
    filter(Name == x) %>% 
    select(Id,DAY,DATE,REASON,RIn,ROut,WIn,WOut,Hr_In,Hr_Out,Hr,Remark) # skip Name
})

# tb <- rbind(hd,df)

# lapply(ls, function(x) xlsx::removeSheet(wb, x))
new_sheet <- lapply(ls, function(x) new_sheet <- xlsx::createSheet(wb, sheetName = x))

# addDataFrame(x, sheet, col.names=TRUE, row.names=TRUE,
#              startRow=1, startColumn=1, colStyle=NULL, colnamesStyle=NULL,
#              rownamesStyle=NULL, showNA=FALSE, characterNA="", byrow=FALSE)
for (i in 1:length(nm)) {
  addDataFrame(paste0("Name: ",nm[i],"\n","Department: FOOD AND BEVERAGE"), new_sheet[[i]],
               col.names=FALSE, row.names=FALSE, startColumn=2)
  addDataFrame("OT Request for the Month of:\nHOD: ABU BAKAR BIN OTHMAN", new_sheet[[i]],
               col.names=FALSE, startColumn=12, row.names=FALSE)
  addDataFrame("ROSTER (Time)", new_sheet[[i]], startRow=3,
               col.names=FALSE, startColumn=6, row.names=FALSE)
  addDataFrame("WORKING (Time)", new_sheet[[i]],startRow=3,
               col.names=FALSE, startColumn=8, row.names=FALSE)
  addDataFrame("OVERTIME (Time)", new_sheet[[i]],startRow=3,
               col.names=FALSE, startColumn=10, row.names=FALSE)
  addDataFrame("Total Hours", new_sheet[[i]],startRow=3,
               col.names=FALSE, startColumn=12, row.names=FALSE)
  addDataFrame(df[[i]], new_sheet[[i]], startRow=5)
}

saveWorkbook(wb,"data/claimt.xlsx")

# Summary OT & CH by Mth, Name (Important)----
# tn <- att_ts %>% 
#   group_by("MTH" = month(Dt),Name) %>%
#   summarise( "OT" = case_when(sum(Hr,na.rm = TRUE) < 104 ~ sum(Hr,na.rm = TRUE),
#                               TRUE ~ 104),
#              "CH" = case_when(OT >= 104 ~ sum(Hr,na.rm = TRUE) - 104,
#                               TRUE ~ 0) %>% 
#                sum(na.rm = TRUE))

# PH by Name ----
# rph <- att_ts %>%
#   inner_join(ph) %>% 
#   group_by(Name,"Date" = Day,"Holiday" = Cuti) %>%
#   summarise("TH" = sum(TH, na.rm = TRUE))
# 
# writexl::write_xlsx(rph,path = "rph.xlsx")


