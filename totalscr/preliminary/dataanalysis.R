### R code for data analysis
### Author : Adi Budyanto
### Date : 25th August 2016

library(scales)
require(data.table)
require(plyr)
library(doParallel)

# set working path in Desktop
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data")  
  source('~/PycharmProjects/dissertation/src/v2/func.R')
} else {
  setwd("~/Dropbox/dissertation/raw_data")
  source('~/Dropbox/dissertation/src/func.R')
}

######################################################################
#### Load the original transaction data
trans <- read.csv('data/trans.csv')
# rm(list=setdiff(ls(), "trans"))

# format back selected variables to proper format
# date
trans$order_date <- as.POSIXct(trans$order_date)
trans$invoice_date <- as.POSIXct(trans$invoice_date)
trans$shipment_date <- as.POSIXct(trans$shipment_date)
trans$reminder_date <- as.POSIXct(trans$reminder_date)
# character
trans$customer_id <- as.character(trans$customer_id)
trans$product_id <- as.character(trans$product_id)
trans$order_no <- as.character(trans$order_no)
# numeric
trans$global_line_total_exc_vat <- as.numeric(as.character(trans$global_line_total_exc_vat))
trans$global_line_total_exc_vat[is.na(trans$global_line_total_exc_vat)] <- 0
trans$discount_percent <- as.numeric(as.character(trans$discount_percent))
trans$discount_percent[is.na(trans$discount_percent)] <- 0

# save after Rdata format of trans data
# save(trans, file = 'data/rdataTrans.Rda')
load('data/trans.Rda')

# filter customers from order year = 2014
cust14 <- trans[which(year(trans$order_date)==2014),]$customer_id
save(cust14, file = 'data/keys/cust14.rda')
load('data/keys/cust14.rda')

######################################################################
### part 1 customer analysis (Repeat Purchasers )
# create data from  selected columns at the transaction level
tmpTrans <- trans[which(trans$customer_id %in% cust14),c('customer_id','order_no','order_date')]
tmpTrans <- tmpTrans[!duplicated(tmpTrans),]
tmpTrans <- tmpTrans[with(tmpTrans, order(customer_id)), ]
tmpTrans$transyear <- year(tmpTrans$order_date)
tmpTrans$qty <- 1

groupingClm <- c("customer_id","transyear")
dataClm <- c("qty")

strt<-Sys.time() # start time
res <- ddply(tmpTrans, groupingClm, function(x) colSums(x[dataClm]))
print(Sys.time()-strt) # end time
names(res)[3]

# Repurchase frequency in 2015
freq2015 <- nrow(res[which(res$transyear == 2015),]) # 100361
freq2014 <- nrow(res[which(res$transyear == 2014),]) # 167109 or 60%

# Nb transactions placed by customers in total
res$totqty = 1
tmp <- res[which(res$transyear== 2014),]
frequency2014 <- setDT(tmp)[, lapply(.SD, sum), by=.(qty), .SDcols=c("totqty")]
frequency2014 <- frequency2014[with(frequency2014, order(qty)), ]

tmp <- res[which(res$transyear== 2015),]
frequency2015 <- setDT(tmp)[, lapply(.SD, sum), by=.(qty), .SDcols=c("totqty")]
frequency2015 <- frequency2015[with(frequency2015, order(qty)), ]

sum <- union(frequency2014$qty, frequency2015$qty)
sum <- data.frame('qty' = sum)
sum <- merge(sum, frequency2014, by=c('qty'), all.x = T)
colnames(sum)[2] <- 'frequency2014'
sum <- merge(sum, frequency2015, by=c('qty'), all.x = T)
colnames(sum)[3] <- 'frequency2015'
write.csv(sum, 'stat/customer_frequency_analysis.csv')

### note : build models where the customes were active both in 2014 and 2015 (repeat purchasers)
custchosen <- res[which(res$transyear == 2015),]$customer_id
save(custchosen, file = 'data/keys/custchosen.Rda')

######################################################################
# part 2  product analysis
tmpTrans <- trans[which(trans$customer_id %in% cust14),c('customer_id','order_date','product_id')]
tmpTrans <- tmpTrans[!duplicated(tmpTrans),]
tmpTrans <- tmpTrans[with(tmpTrans, order(customer_id)), ]
tmpTrans$transyear <- year(tmpTrans$order_date)
tmpTrans$qty <- 1

groupingClm <- c("customer_id","product_id")
dataClm <- c("qty")
resTmpProd <- ddply(tmpTrans, groupingClm, function(x) colSums(x[dataClm]))
resTmpProd$totqty <- 1

groupingClmProd <- c("product_id","qty")
dataClmProd <- c("totqty")
resProd <- ddply(resTmpProd, groupingClmProd, function(x) colSums(x[dataClmProd]),.parallel=T)
resProd <- resProd[with(resProd, order(product_id)), ]
stopCluster(makeCluster(4, type = "SOCK"))
write.csv(resProd, 'stat/product_frequency_analysis.csv')

x <- resProd[which(resProd$product_id == 'p62'),]
y <- res[which(res$product_id == 'p62' & res$qty == 17),]
z <- tmpTrans[which(tmpTrans$customer_id == 'u10543' & tmpTrans$product_id == 'p62'),]

load('data/keys/newprod15.rda')
prodnew2015
write.csv(prodnew2015, 'stat/prodnew2015.csv')

######################################################################
cust14.prodNew15 <- trans[which(trans$customer_id %in% cust14 & trans$product_id %in% prodnew2015),]$customer_id
save(cust14.prodNew15, file = 'data/keys/cust14.prodNew15.rda')
