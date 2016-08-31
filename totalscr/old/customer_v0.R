  ## Preprocessing transaction data for input in Python (Interaction Matrix)
## desired output : user_id, product_id, purchase, timestamp
## required input : product.Rdata
## author : HAB, 11 July 2016

## ========================= Initialization 
# Set path
setwd('~/Desktop/Dissertation/internship/scr')
# Load data and library
library(dplyr)
library(stringr)
library(reshape2)
load("~/Desktop/Dissertation/internship/data/transaction.Rdata")

## ========================= Data Manipulation
mod.trans = trans %>% select(customer_id, product_id, order_date, order_no) %>%
  filter (order_no != 90 &  order_no != 180 & order_no != 20 & order_no != 30 & order_no != 160 &
            order_no != 170 &  order_no != 10 & order_no != 105 & order_no != 70 & order_no != 175 &
            order_no != 60 &  order_no != 110 & order_no != 15 & order_no != 95 & order_no != 140 &
            order_no != 80 &  order_no != 120 & order_no != 150 & order_no != 75 & order_no != 40 &
            order_no != 165 &  order_no != 5 & order_no != 145 & order_no != 50 & order_no != 130 & 
            order_no != 35 &  order_no != 85 & order_no != 45 & order_no != 55 & order_no != '' &
            order_no != 155 &  order_no != 115 & order_no != 65 & order_no != 135 & order_no != 100)
# drop order_no and formatting order_date
mod.trans$order_no <- NULL
mod.trans$order_date <- as.Date(strptime(mod.trans$order_date, "%Y-%m-%d"))
# remove duplicate
mod.trans = mod.trans[!duplicated(mod.trans),]

## ========================= Random Sampling 
customer = unique(mod.trans %>% select(customer_id))
# training data
n = 5000
id_train = customer[sample(nrow(customer), n), ]
train = subset(mod.trans, mod.trans$customer_id %in% id_train)
train = mutate(train, rating = 1)
# reorder the columns
train = train[c(1,2,4,3)]
# test data
n = 1000
id_test = customer[sample(nrow(customer), n), ]
test = subset(mod.trans, mod.trans$customer_id %in% id_test)
test = mutate(test, rating = 1)
# reorder the columns
test = test[c(1,2,4,3)]

## ========================= Export Table
write.table(train, "~/PycharmProjects/dissertation/raw/train.txt", sep="\t",col.names = F,row.names = F)
write.table(test, "~/PycharmProjects/dissertation/raw/test.txt", sep="\t",col.names = F, row.names = F)
