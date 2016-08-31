### R code for Feature Construction on Items and User-Dependent Variables
### Author : Adi Budyanto
### Date : 20 August 2016

# set working path in Desktop
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data")  
  source('~/PycharmProjects/dissertation/src/v2/func.R')
} else {
  setwd("~/Dropbox/dissertation/raw_data")
  source('~/Dropbox/dissertation/src/func.R')
}

require(data.table)
require(plyr)
# library(lubridate)
cores <- 4
if(cores > 1) {
  library(doMC)
  registerDoMC(cores)
}

### All variables were engineered from 2014 data set 
# load selected variables from product table
feat.item.global <- read.csv('features/features.item.global.csv')
names(feat.item.global)
tmpCol <- c("product_id","manufacturer","category","brand","subcategory","subcat.manu","pack_qty")
selectedFeatItem <- feat.item.global[, tmpCol]

# load selected customers 2014 (who recorded transactions both in 2014 and 2015)
load('data/keys/cust14Filter.Rda')
load('data/trans.Rda')
tmpCol <- c("customer_id","product_id","order_no","order_date","price_type","global_line_total_exc_vat", "discount_percent","qty")
# only capture 2014 transactions 
tmpTrans <- trans[which(trans$customer_id %in% cust14Filter & year(trans$order_date)==2014 ),tmpCol]

# create global user - product features
tmpTrans <- merge(tmpTrans, selectedFeatItem, by=c("product_id"), all.x = T)
tmpTrans$apprx_discount <- ifelse(tmpTrans$discount_percent == 0,0,
                                       (tmpTrans$global_line_total_exc_vat*1.0066)/(tmpTrans$discount_percent*10)-tmpTrans$global_line_total_exc_vat)
tmpTrans$actual_qty <- tmpTrans$qty/tmpTrans$pack_qty 

## Sub-Category (parallel does make the computation runs faster)
# 1 total number products in the past
tmpCol <- c("customer_id","order_no","order_date","subcategory","product_id","actual_qty","global_line_total_exc_vat","apprx_discount")
tmpSub <- tmpTrans[,tmpCol]
tmpSub <- tmpSub[with(tmpSub, order(customer_id)), ]
#groupingClm <- c("customer_id","order_no","order_date","subcategory")
groupingClm <- c("customer_id","subcategory")

# 1.1 total purcashed on per-product basis
dataClm <- c("actual_qty")
strt<-Sys.time() # start time
tmpSubProd <- ddply(tmpSub, groupingClm, function(x) {colSums(x[dataClm])} ,.parallel = cores > 1 )
print(Sys.time()-strt) # end time

# create unique table per customer id 
tmp = data.frame(cbind(tmpSubProd, data.frame(model.matrix(~subcategory-1, tmpSubProd))))
for (i in 4:ncol(tmp)){
  tmp[,i] <- tmp[,i] * tmp$actual_qty
}
colnames(tmp)[4:ncol(tmp)] <- paste("nbProd.",sub("^...........", "", colnames(tmp)[4:ncol(tmp)]),sep="")
col <- colnames(tmp)[4:ncol(tmp)] 
tmp <- setDT(tmp)[, lapply(.SD, sum), by=.(customer_id), .SDcols=c(col)]
setDF(tmp)
featSubProd <- noZV(tmp)

# 1.2 Total amount of corresponding transactions spent per customer
dataClm <- c("global_line_total_exc_vat")
strt<-Sys.time() # start time
tmpSubTransSpent <- ddply(tmpSub, groupingClm, function(x) {colSums(x[dataClm])} ,.parallel = cores > 1)
print(Sys.time()-strt) # end time

# create unique table per customer id 
tmp = data.frame(cbind(tmpSubTransSpent, data.frame(model.matrix(~subcategory-1, tmpSubTransSpent))))
for (i in 4:ncol(tmp)){
  tmp[,i] <- tmp[,i] * tmp$global_line_total_exc_vat
}
colnames(tmp)[4:ncol(tmp)] <- paste("totTransSpent.",sub("^...........", "", colnames(tmp)[4:ncol(tmp)]),sep="")
col <- colnames(tmp)[4:ncol(tmp)] 
tmp <- setDT(tmp)[, lapply(.SD, sum), by=.(customer_id), .SDcols=c(col)]
setDF(tmp)
featSubTransSpent <- noZV(tmp)

#1.3 Total amount of corresponding discount received per customers
dataClm <- c("apprx_discount")
strt<-Sys.time() # start time
tmpSubDiscReceived<- ddply(tmpSub, groupingClm, function(x) {colSums(x[dataClm])} ,.parallel = cores >1)
print(Sys.time()-strt) # end time

# create unique table per customer id 
tmp = data.frame(cbind(tmpSubDiscReceived, data.frame(model.matrix(~subcategory-1, tmpSubDiscReceived))))
for (i in 4:ncol(tmp)){
  tmp[,i] <- tmp[,i] * tmp$apprx_discount
}
colnames(tmp)[4:ncol(tmp)] <- paste("SubDiscReceived.",sub("^...........", "", colnames(tmp)[4:ncol(tmp)]),sep="")
col <- colnames(tmp)[4:ncol(tmp)] 
tmp <- setDT(tmp)[, lapply(.SD, sum), by=.(customer_id), .SDcols=c(col)]
setDF(tmp)
featSubDiscReceived <- noZV(tmp)

# 1.4 total unique order transactions were made related to each subcategory
tmpCol <- c("customer_id","order_no","order_date","subcategory","product_id")
tmpSub <- tmpTrans[,tmpCol]
tmpSub <- tmpSub[!duplicated(tmpSub),]
tmpSub <- tmpSub[with(tmpSub, order(customer_id)), ]
tmpSub$qty <- 1

groupingClm <- c("customer_id","subcategory")
dataClm <- c("qty")
strt<-Sys.time() # start time
tmpSubUniqueOrder <- ddply(tmpSub, groupingClm, function(x) {colSums(x[dataClm])} ,.parallel = cores >1)
print(Sys.time()-strt) # end time

tmp = data.frame(cbind(tmpSubUniqueOrder, data.frame(model.matrix(~subcategory-1, tmpSubUniqueOrder))))
for (i in 4:ncol(tmp)){
  tmp[,i] <- tmp[,i] * tmp$qty
}
colnames(tmp)[4:ncol(tmp)] <- paste("totSubUniqueOrder.",sub("^...........", "", colnames(tmp)[4:ncol(tmp)]),sep="")
col <- colnames(tmp)[4:ncol(tmp)] 
tmp <- setDT(tmp)[, lapply(.SD, sum), by=.(customer_id), .SDcols=c(col)]
setDF(tmp)
featSubUniqueOrder <- noZV(tmp)

## Sub-Category Manufacturer
# 2 total number products in the past
tmpCol <- c("customer_id","order_no","order_date","subcat.manu","product_id","actual_qty","global_line_total_exc_vat","apprx_discount")
tmpSub <- tmpTrans[,tmpCol]
tmpSub <- tmpSub[with(tmpSub, order(customer_id)), ]
#groupingClm <- c("customer_id","order_no","order_date","subcategory")
groupingClm <- c("customer_id","subcat.manu")

# 2.1 total purcashed on per-product basis
dataClm <- c("actual_qty")
strt<-Sys.time() # start time
tmpSubProd <- ddply(tmpSub, groupingClm, function(x) {colSums(x[dataClm])} ,.parallel = cores > 1 )
print(Sys.time()-strt) # end time

# create unique table per customer id 
tmp = data.frame(cbind(tmpSubProd, data.frame(model.matrix(~subcat.manu-1, tmpSubProd))))
for (i in 4:ncol(tmp)){
  tmp[,i] <- tmp[,i] * tmp$actual_qty
}
colnames(tmp)[4:ncol(tmp)] <- paste("nbSubCatProd.",sub("^...........", "", colnames(tmp)[4:ncol(tmp)]),sep="")
col <- colnames(tmp)[4:ncol(tmp)]  
tmp <- setDT(tmp)[, lapply(.SD, sum), by=.(customer_id), .SDcols=c(col)]
setDF(tmp)
featSubCatProd <- noZV(tmp)

# 2.2 Total amount of corresponding transactions spent per customer
dataClm <- c("global_line_total_exc_vat")
strt<-Sys.time() # start time
tmpSubTransSpent <- ddply(tmpSub, groupingClm, function(x) {colSums(x[dataClm])} ,.parallel = cores > 1)
print(Sys.time()-strt) # end time

# create unique table per customer id 
tmp = data.frame(cbind(tmpSubTransSpent, data.frame(model.matrix(~subcat.manu-1, tmpSubTransSpent))))
for (i in 4:ncol(tmp)){
  tmp[,i] <- tmp[,i] * tmp$global_line_total_exc_vat
}
colnames(tmp)[4:ncol(tmp)] <- paste("totSubCatUniqueOrder.",sub("^...........", "", colnames(tmp)[4:ncol(tmp)]),sep="")
col <- colnames(tmp)[4:ncol(tmp)] 
tmp <- setDT(tmp)[, lapply(.SD, sum), by=.(customer_id), .SDcols=c(col)]
setDF(tmp)
featSubCatTransSpent <- noZV(tmp)

# 2.3 Total amount of corresponding discount received per customers
dataClm <- c("apprx_discount")
strt<-Sys.time() # start time
tmpSubDiscReceived<- ddply(tmpSub, groupingClm, function(x) {colSums(x[dataClm])} ,.parallel = cores >1)
print(Sys.time()-strt) # end time

# create unique table per customer id 
tmp = data.frame(cbind(tmpSubDiscReceived, data.frame(model.matrix(~subcat.manu-1, tmpSubDiscReceived))))
for (i in 4:ncol(tmp)){
  tmp[,i] <- tmp[,i] * tmp$apprx_discount
}
colnames(tmp)[4:ncol(tmp)] <- paste("SubCatDiscReceived.",sub("^...........", "", colnames(tmp)[4:ncol(tmp)]),sep="")
col <- colnames(tmp)[4:ncol(tmp)] 
tmp <- setDT(tmp)[, lapply(.SD, sum), by=.(customer_id), .SDcols=c(col)]
setDF(tmp)
featSubCatDiscReceived <- noZV(tmp)

# 2.4 total unique order transactions were made related to each subcategory
tmpCol <- c("customer_id","order_no","order_date","subcat.manu","product_id")
tmpSub <- tmpTrans[,tmpCol]
tmpSub <- tmpSub[!duplicated(tmpSub),]
tmpSub <- tmpSub[with(tmpSub, order(customer_id)), ]
tmpSub$qty <- 1

groupingClm <- c("customer_id","subcat.manu")
dataClm <- c("qty")
strt<-Sys.time() # start time
tmpSubUniqueOrder <- ddply(tmpSub, groupingClm, function(x) {colSums(x[dataClm])} ,.parallel = cores >1)
print(Sys.time()-strt) # end time

tmp = data.frame(cbind(tmpSubUniqueOrder, data.frame(model.matrix(~subcat.manu-1, tmpSubUniqueOrder))))
for (i in 4:ncol(tmp)){
  tmp[,i] <- tmp[,i] * tmp$qty
}
colnames(tmp)[4:ncol(tmp)] <- paste("totCatSubUniqueOrder.",sub("^...........", "", colnames(tmp)[4:ncol(tmp)]),sep="")
col <- colnames(tmp)[4:ncol(tmp)] 
tmp <- setDT(tmp)[, lapply(.SD, sum), by=.(customer_id), .SDcols=c(col)]
setDF(tmp)
featSubCatUniqueOrder <- noZV(tmp)

## merging all tables
summarize <- merge(featSubProd, featSubTransSpent, by=c("customer_id"))
summarize <- merge(summarize, featSubDiscReceived, by=c("customer_id"))
summarize <- merge(summarize, featSubUniqueOrder, by=c("customer_id"))
summarize <- merge(summarize, featSubCatProd, by=c("customer_id"))
summarize <- merge(summarize, featSubCatTransSpent, by=c("customer_id"))
summarize <- merge(summarize, featSubCatDiscReceived, by=c("customer_id"))
summarize <- merge(summarize, featSubCatUniqueOrder, by=c("customer_id"))
