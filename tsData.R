# tsData.R

# Init ----

library(tidyverse)

# ts <- readxl::read_excel("biweekly_ts.xlsx",range = "D38:G38",
#                         col_names = c("rh","ot","sl","vc"),
#                         col_types = "numeric")
# ns <- readxl::read_excel("biweekly_ts.xlsx",range = "C6:C6",
#                          col_names = "emp")

att_tb <- readxl::read_excel("mth_att.xlsx",
                             range = "A7:Q37",
                             col_types = c("date",replicate(16,"numeric"))
                             )

exp_tb <- readxl::read_excel("mth_rpt.xlsx",sheet = "PR EXPENSES",
                             range = "B7:G53",
                             col_types = c("date","text","numeric","text","numeric","numeric"))
part_tb <- readxl::read_excel("mth_rpt.xlsx",sheet = "PARTIMER")
bqt_tb <- readxl::read_excel("mth_rpt.xlsx",sheet = "BQT AVTIVITIES")

tbs <- readxl::excel_sheets("biweekly_ts.xlsx")

emp_ts <- lapply(tbs, function(x) readxl::read_xlsx(path = "biweekly_ts.xlsx",
                                                           range = "D38:G38",
                                                           col_names = c("rh","ot","sl","vc"),
                                                           col_types = "numeric",
                                                           sheet = x))

emp_tb <- dplyr::bind_rows(lapply(emp_ts, as.data.frame.list)) %>% 
  cbind("emp"=tbs)

# Analysis ----

# Total Reg hr by employee
# chart total (hour) by type ----
ts <- emp_tb %>%
  # group_by(emp) %>% 
  summarise("Regular" = sum(rh), "Overtime" = sum(ot),
            "Sick" = sum(sl), "Vacation" = sum(vc))

# gather col into rows
ts <- ts %>% tidyr::gather(key = "type")

ggplot(ts, aes(x = type, fill = type, na.rm = TRUE, y=value)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=format(value,digits = 3,big.mark=",")), vjust=1.6, color="white",
            size=3.5) +
  xlab("Type") + ylab("Hour") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# regular hr by employee ----
ggplot(emp_tb, aes(x = emp, fill = sl, na.rm = TRUE, y=sl)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=format(sl,digits = 3,big.mark=",")), vjust=1.6, color="white",
            size=3.5) +
  xlab("Employee") + ylab("Regular") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(emp_tb, aes(x = emp, fill = emp, na.rm = TRUE, y=rh)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=format(rh,digits = 3,big.mark=",")), vjust=1.6, color="white",
            size=3.5) +
  xlab("Employee") + ylab("Regular") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Overtime hr by employee ----
ggplot(emp_tb, aes(x = emp, fill = emp, na.rm = TRUE, y=ot)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=format(ot,digits = 3,big.mark=",")), vjust=1.6, color="white",
            size=3.5) +
  xlab("Employee") + ylab("Overtime") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# n sick leave hr by employee ----
ot <- table(emp_tb$ot) %>% data.frame()

ggplot(ot, aes(Var1,Freq,fill = Var1)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=format(Freq,digits = 3,big.mark=",")), vjust=1.6, color="black",
            size=3.5) +
  xlab("Over-time (Hour)") + ylab("Freq") +
  theme(axis.text.x=element_text(hjust=1,vjust=0.5))

# Sick leave hr by employee
ggplot(emp_tb, aes(x = emp, fill = emp, na.rm = TRUE, y=sl)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=format(sl,digits = 3,big.mark=",")), vjust=1.6, color="white",
            size=3.5) +
  xlab("Employee") + ylab("Sick") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# n sick leave hr by employee ----
sl <- table(emp_tb$sl) %>% data.frame()

ggplot(sl, aes(Var1,Freq,fill = Var1)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=format(Freq,digits = 3,big.mark=",")), vjust=1.6, color="black",
            size=3.5) +
  xlab("Sick leave (Hour)") + ylab("Freq") +
  theme(axis.text.x=element_text(hjust=1,vjust=0.5))

# Vacation leave by employee
ggplot(emp_tb, aes(x = emp, fill = emp, na.rm = TRUE, y=vc)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=format(vc,digits = 3,big.mark=",")), vjust=1.6, color="white",
            size=3.5) +
  xlab("Employee") + ylab("Vacation") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# n sick leave hr by employee ----
vc <- table(emp_tb$vc) %>% data.frame()

ggplot(vc, aes(Var1,Freq,fill = Var1)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=format(Freq,digits = 3,big.mark=",")), vjust=1.6, color="black",
            size=3.5) +
  xlab("Vacation (Hour)") + ylab("Freq") +
  theme(axis.text.x=element_text(hjust=1,vjust=0.5))

# # Min hr by dept
# emp_tb %>%
#   # group_by(emp) %>% 
#   summarise("Regular" = min(rh), "Overtime" = min(ot), "Sick" = min(sl), "Vacation" = min(vc))
# 
# # Total Hr*rate by dept ----
# wh <- emp_tb %>%
#   mutate(rh_rate = rh * hr_rate$rate[1], ot_rate = ot * hr_rate$rate[2],
#          sl_rate = sl * hr_rate$rate[3], vc_rate = vc * hr_rate$rate[4]) %>%
#   select(emp, rh_rate, ot_rate, sl_rate, vc_rate) %>% 
#   group_by(emp) %>%
#     summarise("Regular" = sum(rh_rate), "Overtime" = sum(ot_rate), 
#           "Sick" = sum(sl_rate), "Vacation" = sum(vc_rate))

# # summary (RM) by work hr ----
# summarise_all(wh[,2:5], sum) %>%
#   # gather col into rows
#   tidyr::gather(key = "type") %>%
#   ggplot(aes(x = type, fill = type, na.rm = TRUE, y=value)) +
#   geom_bar(stat="identity") +
#   geom_text(aes(label=format(value,digits = 3,big.mark=",")), vjust=1.6, color="white",
#             size=3.5) +
#   xlab("Type") + ylab("RM") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# 
# # regular hr*rate by employee ----
# ggplot(wh, aes(x = emp, fill = emp, na.rm = TRUE, y=Regular)) +
#   geom_bar(stat="identity") + 
#   geom_text(aes(label=format(Regular,digits = 3,big.mark=",")), vjust=1.6, color="white",
#             size=3.5) +
#   xlab("Employee") + ylab("Regular") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
# 
# # overtime hr*rate by employee ----
# ggplot(wh, aes(x = emp, fill = emp, na.rm = TRUE, y=Overtime)) +
#   geom_bar(stat="identity") + 
#   geom_text(aes(label=format(Overtime,digits = 3,big.mark=",")), vjust=1.6, color="white",
#             size=3.5) +
#   xlab("Employee") + ylab("Overtime") +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  
