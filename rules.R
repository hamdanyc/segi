# rules.R

# init ----
library(dplyr)

rules <- readr::read_csv("data/rules.csv")

tb <- tidyr::gather(rules,key="To",value="value",-From)

inner_join(ts,tb,by=c("RIn"="From","ROut"="To")) %>%
  select(Name,RIn,ROut,value.x) %>% tail(25)

tb %>% 
  filter(From=="12am" & To=="8pm")

# ts2 <- att_tb %>%
#   mutate("Hr_Out" = as.numeric(substr(Out,1,2))) %>% 
#   inner_join(wf_ot,by=c("Hr_Out"="wf")) %>% 
#   select(Day, Date, Wk_Out="rin", Hr_Out)
# 
# ts <- inner_join(ts1,ts2) %>%
#   mutate("Dt" = as.Date(Day,"%d-%m-%Y"),
#          "DATE" = format(Dt,"%d"),
#          "DAY" = wday(Dt,label = TRUE),
#          "REASON" = purpose,
#          "Wk_In"=RIn) %>% 
#   select(Id,Name,Dt,DAY,DATE,REASON,RIn,ROut,Wk_In,Wk_Out,Ot,Hr_Out)
# 
# inner_join(ts1,tb,by=c("RIn"="From","ROut"="To")) %>%
#   select(Name,RIn,ROut,Ot,value) %>% tail(25)
# 
# inner_join(ts,tb,by=c("RIn"="From","Wk_Out"="To")) %>% 
#   select(Id,Dt,RIn,Wk_Out,Ot,value)
# 
# att_ts <- att_tb %>%
#   mutate("Dt" = as.Date(Day,"%d-%m-%Y"),
#          "DATE" = format(Dt,"%d"),
#          "DAY" = wday(Dt,label = TRUE),
#          "REASON" = purpose,
#          "Hr_Out" = as.numeric(substr(Out,1,2)),
#          "In_mi" = as.numeric(substr(In,3,4)),
#          "Hr_In" = case_when(In_mi < 30 ~ Hr_In,
#                              TRUE ~ Hr_In + 1),
#          "RIn" = case_when(Hr_In == 0 ~ "12am",
#                            Hr_In >= 0 & Hr_In <= 11 ~ paste0(Hr_In,"am"),
#                            Hr_In >= 13 & Hr_In <= 23 ~ paste0(Hr_In,"pm")),
#          "ROut" = case_when(Hr_In == 0 ~ "8am",
#                             Hr_In >= 0 & Hr_In <= 3 ~ paste0(Hr_In+8,"am"),
#                             Hr_In == 4  ~ paste0(Hr_In+8,"pm"),
#                             Hr_In >= 5 & Hr_In <= 23 ~ paste0(Hr_In+8-12,"pm")),
#          #"Hr" = Hr_Out - wf_ot[wf_ot$wf== Hr_In,"ot"],
#          "WIn" = case_when(Hr_In == 13 ~ "1pm",
#                          Hr_In == 14 ~ "2pm",
#                          Hr_In == 15 ~ "3pm",
#                          Hr_In == 16 ~ "4pm",
#                          Hr_In == 17 ~ "5pm",
#                          Hr_In == 18 ~ "6pm",
#                          Hr_In == 19 ~ "7pm",
#                          Hr_In == 20 ~ "8pm",
#                          Hr_In == 21 ~ "9pm",
#                          Hr_In == 22 ~ "10pm",
#                          Hr_In == 23 ~ "11pm",
#                          Hr_In == 0 ~ "12am",
#                          TRUE ~ paste0(Hr_In,"am")),
#          "WOut" = case_when(Hr_Out == 13 ~ "1pm",
#                           Hr_Out == 14 ~ "2pm",
#                           Hr_Out == 15 ~ "3pm",
#                           Hr_Out == 16 ~ "4pm",
#                           Hr_Out == 17 ~ "5pm",
#                           Hr_Out == 18 ~ "6pm",
#                           Hr_Out == 19 ~ "7pm",
#                           Hr_Out == 20 ~ "8pm",
#                           Hr_Out == 21 ~ "9pm",
#                           Hr_Out == 22 ~ "10pm",
#                           Hr_Out == 23 ~ "11pm",
#                           Hr_Out == 0 ~ "12am",
#                           TRUE ~ paste0(Hr_Out,"am")),
#          Hr = if_else(Hr < 0,0,Hr),
#          Hr_In = ROut,
#          Hr_Out = WOut
#   ) %>%
#   select(Id, Name, Dt, DAY, DATE, REASON, RIn, ROut, WIn, WOut, Hr_In, Hr_Out, Hr)

# clean up att_ts table
# att_ts <- att_ts %>% 
#   mutate(WIn=replace(WIn,WIn == "NAam",NA),
#          WOut=replace(WOut,WOut == "NAam",NA),
#          Hr_Out=replace(Hr_Out,Hr_Out == "NAam",NA))
