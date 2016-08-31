  ## Preprocessing transaction data for input in Python (Interaction Matrix)
## desired output : user_id, product_id, purchase, timestamp
## required input : product.Rdata
## author : HAB, 11 July 2016

## ========================= Initialization 
# Load data and library
library(dplyr)
library(stringr)
library(reshape2)
library(futile.logger)

flog.info("Load Transaction Table")
# Set path for loading transaction data
load("~/Desktop/Dissertation/internship/data/transaction.Rdata")

flog.info("Import Product Table")
# import product table for reindexing product_id
prod <- read.csv("~/PycharmProjects/dissertation/src/R/prod.csv")
mod.prod = prod %>% select(modprod_id,product_id)
# left merge to append modprod_id into trans
trans = merge(x=trans,y=mod.prod,by='product_id',all.x= T)

flog.info("Cleaning Transactions Data")
## ========================= Data Manipulation
mod.trans = trans %>% select(customer_id, modprod_id , product_id, order_date, order_no) %>%
  filter (order_no != 90 &  order_no != 180 & order_no != 20 & order_no != 30 & order_no != 160 &
            order_no != 170 &  order_no != 10 & order_no != 105 & order_no != 70 & order_no != 175 &
            order_no != 60 &  order_no != 110 & order_no != 15 & order_no != 95 & order_no != 140 &
            order_no != 80 &  order_no != 120 & order_no != 150 & order_no != 75 & order_no != 40 &
            order_no != 165 &  order_no != 5 & order_no != 145 & order_no != 50 & order_no != 130 & 
            order_no != 35 &  order_no != 85 & order_no != 45 & order_no != 55 & order_no != '' &
            order_no != 155 &  order_no != 115 & order_no != 65 & order_no != 135 & order_no != 100
            & customer_id != 'NULL' & customer_id != '' & customer_id != -1 & !is.na(order_date)
            & product_id != 'NULL' &  product_id != 23.2758 & product_id != "")
# drop order_no and formatting order_date
mod.trans = mod.trans[,-which(names(mod.trans) %in% c("product_id","order_no"))]
mod.trans$order_date <- as.numeric(as.Date(strptime(mod.trans$order_date, "%Y-%m-%d")))
# remove duplicate
mod.trans = mod.trans[!duplicated(mod.trans),]
# clear up some space by deleting trans
flog.info("Drop Transaction Table for Clearing Some Space")
rm(trans)
## ========================= Reindexing Customer_id
# comment out coz reindexing should be done when holdout test
# mod.trans = mutate(mod.trans, newid = as.numeric(mod.trans$customer_id))
# cust = data.frame(newid = unique(mod.trans$newid))
# cust = mutate(cust, user_id = as.numeric(rownames(cust)))
# mod.trans = merge(x=mod.trans, y=cust, by="newid",all.x=T)
# mod.trans = mod.trans[c(5,2,3,4)]; mod.trans$newid <- NULL; mod.trans  = arrange(mod.trans,user_id)

## ========================= Random Sampling 
flog.info("Create Random Training Data")
# note: in train data, user_id should start from 1
# training data
set.seed(42)
n = 1000
# random sampling n users in training data
id = sample(unique(mod.trans$customer_id), n)
mod.train = subset(mod.trans, mod.trans$customer_id %in% id)
# testing condition of generated samples from train 
stopifnot(length(unique(mod.train$customer_id)) == length(unique(id)))
# assign 1 to purchase data
mod.train = mutate(mod.train, rating = 1)
# reindexing user_id to zero for training data
mod.train = mutate(mod.train, temp_id = as.numeric(mod.train$customer_id))
cust = data.frame(temp_id = unique(mod.train$temp_id))
cust = mutate(cust, user_id = as.numeric(rownames(cust)))
# merge back to origin data
mod.train = merge(x=mod.train, y=cust, by="temp_id",all.x=T); mod.train  = arrange(mod.train,user_id)
# arrange column for making easier data pre-processing 
mod.train = mod.train[c(6,3,5,4)];  
## note: in train data, user_id should start from train's max(user_id) + 1 

flog.info("Create Random Test Data")
# test data
n = 150; 
# take different customer_id to training data
idtest = sample(unique(filter(mod.trans, !(customer_id %in% id))$customer_id),n)
mod.test = subset(mod.trans, mod.trans$customer_id %in% idtest)
# assign 1 to purchase data
mod.test = mutate(mod.test, rating = 1)
# reindexing user_id to zero for training data
mod.test = mutate(mod.test, temp_id = as.numeric(mod.test$customer_id))
cust = data.frame(temp_id = unique(mod.test$temp_id))
# user_id must be added to max user_id from train data = max(mod.train$user_id)
cust = mutate(cust, user_id = max(mod.train$user_id)+as.numeric(rownames(cust)))
# merge back to origin data
mod.test = merge(x=mod.test, y=cust, by="temp_id",all.x=T); mod.test  = arrange(mod.test,user_id)
# arrange column for making easier data pre-processing 
mod.test = mod.test[c(6,3,5,4)];  


# subset <- function(n,cust, df) {
#   id = cust[sample(nrow(cust), n), ]
#   data = subset(df, df$customer_id %in% id)
#   data = mutate(data, rating=1)
#   return(data)  
# }
#test = subset(n=10, cust=customer, df=mod.trans)

## ========================= Export Table
flog.info("Export Training and Test Data")
write.table(mod.train, "~/PycharmProjects/dissertation/raw/train.txt", sep="\t",col.names = F,row.names = F)
write.table(mod.test, "~/PycharmProjects/dissertation/raw/test.txt", sep="\t",col.names = F, row.names = F)

## ========================================
# reindexing the customer_id
# x = mod.trans[mod.trans$customer_id %in% c('1000001','420608','1000032','1000089'), ]
# x = mutate(x, newid = as.numeric(x$customer_id ))
# 
# y = data.frame(newid = unique(x$newid))
# y = mutate(y, user_id = as.numeric(rownames(y)))
# x = merge(x=x, y=y, by="newid",all.x=T)

# exploratory search on NA customer_id
# x1 = filter(x, is.na(x$newid))
# filter(mod.trans, mod.trans$customer_id == 'NULL')
# filter(mod.trans, mod.trans$customer_id == '') 
# filter(mod.trans, mod.trans$product_id == 23.2758)
# filter(mod.trans, is.na(mod.trans$order_date))
# filter(mod.trans, is.na(mod.trans$product_id))
# filter(mod.trans, mod.trans$product_id == 'NULL')

## ========================================

