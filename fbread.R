# fbread.R

# Init ----

library(tidyverse)
library(lubridate)

# read excel ----
att_tb <- readxl::read_excel("fbtable.xlsx", sheet = "att",
                             col_types = c("date",replicate(16,"numeric")))
exp_tb <- readxl::read_excel("fbtable.xlsx", sheet = "exp",
                             col_types = c("date","text","numeric","text","numeric"))
part_tb <- readxl::read_excel("fbtable.xlsx", sheet = "psm",
                              col_types = c("date","text","numeric","numeric"))
bqt_tb <- readxl::read_excel("fbtable.xlsx", sheet = "bqt",
                             col_types = c("date","text","text","text","text","text","numeric",
                                           "numeric","text"))
save.image("fb.RData")
