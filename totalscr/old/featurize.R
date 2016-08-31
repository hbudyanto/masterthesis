library(utils)
library(futile.logger)

# run code
flog.info("Preprocessing item feature & interaction table")
source('~/PycharmProjects/dissertation/src/R/product.R')
source('~/PycharmProjects/dissertation/src/R/customer_v1.R')

flog.info("Create Zip File")
# sefile
setwd('~/PycharmProjects/dissertation/raw')
dir('~/PycharmProjects/dissertation/raw', full.names = TRUE)
zip(zipfile = 'dissertation', files = files2zip)

paste(path.expand("~/foo"),"/PycharmProjects/dissertation/raw)")


?zip
# Delete text
flog.info("Delete text files")
for (i in 1:length(files2zip))
  {
  #print(files2zip[i])
  file.remove(files2zip[i])
  }


