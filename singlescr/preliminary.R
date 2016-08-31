### R code for customers sampling for the analysis
### Author : Adi Budyanto
### Date : 25th August 2016

require(plyr)
require(data.table)
# load transaction data
load('data/trans.Rda')

# rm(list=setdiff(ls(), "trans"))

## Feature Engineering User Depen

## 1 get all customers from transaction year = 2014
cust14 <- unique(trans[which(year(trans$order_date) == 2014), c("customer_id")])
## 2 get all customers from 2014 who were repeat purchasers in 2014
cust14Filter <- unique(trans[which(year(trans$order_date)== 2015 & trans$customer_id %in% cust14),c("customer_id")])
## 3 get also repeat purchasers who bought new product (for rebalancing class probabilities)
save(cust14, file ='data/keys/cust14.Rda')
save(cust14Filter, file ='data/keys/cust14Filter.Rda')

# get information about products
# unique product_id sold in the year 2015 and 2015
prodlist2014 = unique(trans[which((as.POSIXlt(trans$order_date)$year+1900)== 2014),c("product_id")])
prodlist2015 = unique(trans[which((as.POSIXlt(trans$order_date)$year+1900)== 2015),c("product_id")])

# to see if any products sold in 2014 also bought in 2015
# prod from 2014 which were bought in 2015
prod14.listedin15 <- intersect(prodlist2014,prodlist2015) # 166 products
# new product bought in 2015
prodnew2015 <- setdiff(prodlist2015,prodlist2014) # 16 products
# product from 2014 who did record any trans in 2015
prod14.notlistedin15 <- setdiff(unique(trans$product_id),union(prodnew2015,prod14.listedin15))

save(prodnew2015, file = 'data/keys/prodnew2015.Rdata')
save(prodlist2014, file = 'data/keys/prodexist2014.Rdata')
save(prod14.notlistedin15, file = 'data/keys/prod2014Didnoexist_in2015.Rdata')

# get cust14filter who purchase new prod 2015 in transaction year 2015
cust14BoughtnewProd <- unique(trans[which(trans$customer_id %in% cust14Filter & trans$product_id %in% prodnew2015 & year(trans$order_date) == 2015),
                                   "customer_id"])
save(cust14BoughtnewProd, file = 'data/keys/cust14WnewProd.Rda')
