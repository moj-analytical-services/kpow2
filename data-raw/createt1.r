knife<-read.csv("~/knifecrime/data-raw/t1.csv", check.names=T)

devtools::use_data(knife)

rm(knife)


knife<-readr::read_csv("~/knifecrime/data-raw/t1.csv")

knife$phase<-forcats::as_factor(knife$phase)

devtools::use_data(knife, overwrite = T)

rm(knife)


?knife
