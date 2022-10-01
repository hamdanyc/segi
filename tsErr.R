
# Monthly Claim Data ----
ds <- tibble("Dt"=seq(as.Date('2019-10-16'), as.Date('2019-11-15'), by = "1 days"))
td <- apply(ds,1,function (x) data_frame(x,nm))
tt <- for(i in 1:5) {
  as.data.frame(td[i])}
tt <- dplyr::bind_rows(as.data.frame(td[1]),as.data.frame(td[2]))