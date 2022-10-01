#al_init.R

# init ----
library(dplyr)
library(lubridate)
load("cuti/cuti.RData")

cuti <- read_csv("data/cuti_22.csv", 
                 col_types = cols(...1 = col_skip(),
                                  NO = col_skip(), staff_id = col_character(), al_fwd =col_integer(), al_entitle =col_integer()))
st_al <- read_csv("cuti/st_al.csv",
                  col_types = cols(...1 = col_skip(),
                                   NO = col_skip(), staff_id = col_character(), al_fwd =col_integer(), al_entitle =col_integer()))
rod <- read_csv("cuti/rod.csv")

# names(cuti) <- c("Id","Nama","Tkh Khidmat","Cuti Ke hadapan","Kelayakan")
# names(st_al) <- c("Nama","Tkh Cuti")
# cuti <- cuti[,1:5]

# update leave ----
# n1 <- c("","","","")
# n2 <- c("SYAWAL","FAZLEHIN","AIN","FIRDAUS")
# n3 <- c("","","","")
# n4 <- c(0,0,0,0)
# n5 <- c(10,10,10,10)
# 
# cuti <- add_row(cuti,Id=n1,Nama=n2,`Tkh Khidmat`=n3,`Cuti Ke hadapan`=n4,Kelayakan=n5)
# v_al <- cuti_22[6:11] %>% 
#   t 
# nama <- ""
# dt <- ""
# st_al <- data.frame(nama,dt)
# st_al <- st_al[-c(1),]
# tb <- function(x) tibble::add_row(st_al,nama=cuti_22$nama[x],dt=v_al[1:6,x])
# st_al <- lapply(1:11,tb) %>% bind_rows(st_al)
# st_al <- na.omit(st_al)
cg <- rod %>% select(Nama,Ganti) %>% 
  na.omit()
cg %>% 
  group_by(Nama) %>% 
  summarise("Cuti Gantian" = n())
rod %>% select(-4) %>% 
  na.omit() %>% 
  group_by(Nama) %>% 
  summarise("ROD" = n())

# save data frame ----
write.csv(st_al,"cuti/st-al.csv",row.names = FALSE)
write.csv(cuti,"cuti/cuti.csv",row.names = FALSE)
write.csv(cg,"cuti/cg.csv",row.names = FALSE)
write.csv(rod[,-4],"cuti/rod.csv",row.names = FALSE)
save.image("cuti/cuti.RData")

# data preview ----
st_al %>% inner_join(cuti) %>% 
  group_by(Nama) %>% tail()
  summarise("Jlh cuti"=n(),"Kelayakan"=first(Kelayakan), "Baki" = first(Kelayakan)-n())
  
tibble::add_row(st_al,Nama="nurul",`Tkh Cuti`="1/7/2022") %>% tail()

rod %>% 
  mutate(Jenis = case_when(Hari %in% c("SUNDAY","MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY") ~ "HM",
                           TRUE ~ "CU")) %>% 
  na.omit() %>% 
  group_by(Nama) %>% 
  summarise(total = n())

cuti %>% right_join(cg) %>% 
  View()

