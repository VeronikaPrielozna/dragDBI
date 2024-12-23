## code to prepare `DATASET` dataset goes here
library(readxl)
Stormwaters<-read_excel("data-raw/Stormwaters.xlsx")
usethis::use_data(Stormwaters, overwrite = TRUE)
sinew::makeOxygen(Stormwaters, add_fields = "source")

DBI_CE<-read_excel("data-raw/DBI_CE.xlsx")
usethis::use_data(DBI_CE, overwrite = TRUE)
sinew::makeOxygen(DBI_CE, add_fields = "source")

DBI_SA<-read_excel("data-raw/DBI_SA.xlsx")
usethis::use_data(DBI_SA, overwrite = TRUE)
sinew::makeOxygen(DBI_SA, add_fields = "source")

DBI_CEC<-read_excel("data-raw/DBI_CEC.xlsx")
usethis::use_data(DBI_CEC, overwrite = TRUE)
sinew::makeOxygen(DBI_CEC, add_fields = "source")
