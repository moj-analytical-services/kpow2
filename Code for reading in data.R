t1<-read.csv('data-raw/t1.csv' ,check.names = T)

devtools::use_data(t1, overwrite = T)

t2<-readr::read_csv('t1.csv')
t2$phase<-forcats::as_factor(t2$phase)

devtools::use_data(t2, overwrite = T)
