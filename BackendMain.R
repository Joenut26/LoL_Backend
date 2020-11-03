#Backend API#

#source functions
lapply(paste0("fun/",list.files("fun")),source)

library(curl)
library(jsonlite)
library(RJSONIO)
library(data.table)

source("authentication.R")

sumnames <- c("Zwieee","xMercy","risen dragon")

big_data <- lapply(sumnames,function(x){Backendfx(x,api_key)})

Backendfx("xMercy",api_key)


