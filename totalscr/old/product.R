## Preprocessing Product data for input in Python (Item Features)
## desired output : product_id, product_name, category, product_type, product_lifecycle
## required input : product.Rdata
## author : HAB, 11 July 2016

## ========================= Initialization 
# Load data and library
library(dplyr)
library(stringr)
library(reshape)
library(futile.logger)

flog.info("Import CSV product table")
prod <- read.csv("~/PycharmProjects/dissertation/src/R/prod.csv")
## ========================= Little Mod and Statistics 
# select variables to be used and
temp.prod = prod %>% select(modprod_id, name,category, product_type, status,
                               product_lifecycle) %>% arrange(modprod_id)
colnames(temp.prod)[1] <- 'product_id'

flog.info("Stemming Special Character and Delete Space")
## ======================== Modi for Pre-Processing 
# delete space
temp.prod$category = tolower(gsub('([[:punct:]])|\\s+','',temp.prod$category))
# rename '' in product lifecycle to be 'active'
temp.prod = mutate(temp.prod,product_lifecycle = ifelse((product_lifecycle == ""),"active",product_lifecycle))
# create two new columns
temp.prod$status = paste('status_',temp.prod$status)
temp.prod$prod_lifecycle = paste('prod_lifecycle_',temp.prod$product_lifecycle)

flog.info("Begin Process to Create Item Features and Item Features Label")
# Reshaping (convert from data frame to contingency table)
# category
# put freq = 1 to allow for reshaping data
temp.table_cat = temp.prod %>% select(product_id,name,category)
# begin reshaping
temp.table_cat = mutate(temp.table_cat,freq=1)
temp.table_cat <- melt(temp.table_cat,id.vars=c('product_id','name','category'))
temp.table_cat <- cast(temp.table_cat,formula=product_id+name~category)
temp.table_cat <- replace(temp.table_cat,is.na(temp.table_cat),0)

# status
# put freq = 1 to allow for reshaping data
temp.table_stat = temp.prod %>% select(product_id,name,status)
# begin reshaping
temp.table_stat = mutate(temp.table_stat,freq=1)
temp.table_stat <- melt(temp.table_stat,id.vars=c('product_id','name','status'))
temp.table_stat <- cast(temp.table_stat,formula=product_id+name~status)
temp.table_stat <- replace(temp.table_stat,is.na(temp.table_stat),0)

# product_lifecycle
# put freq = 1 to allow for reshaping data
temp.table_prodlife = temp.prod %>% select(product_id,name,prod_lifecycle)
# begin reshaping
temp.table_prodlife = mutate(temp.table_prodlife,freq=1)
temp.table_prodlife <- melt(temp.table_prodlife,id.vars=c('product_id','name','prod_lifecycle'))
temp.table_prodlife <- cast(temp.table_prodlife,formula=product_id+name~prod_lifecycle)
temp.table_prodlife <- replace(temp.table_prodlife,is.na(temp.table_prodlife),0)

# Merging multiple columns
# put all dataframes to a list
list.temp_table = list(temp.table_cat,temp.table_stat,temp.table_prodlife)
temp.table = Reduce(function(...) merge(..., all=T), list.temp_table)
rm(temp.table_prodlife,temp.table_stat,temp.table_cat, prod, temp.prod)

# make list of features
item.feature = as.data.frame(colnames(temp.table))
item.feature = item.feature[-c(0:2), ]
item.feature = as.data.frame(item.feature)
colnames(item.feature)[1] <- "feature"

flog.info("Export txt file")
# export to text 
write.table(temp.table, "~/PycharmProjects/dissertation/raw/prod.txt", sep="|",col.names = F,row.names= F)
write.table(item.feature, "~/PycharmProjects/dissertation/raw/feat.txt", sep="|",col.names = F)

## ================================================================================ 
## ======================================== Learning R 
# convert data frame to table (useful for binarized variables (0,1))
# x = head(temp.prod,n=20)
# x = x %>% select(product_id, category)
# xtable = table(x$product_id, x$category, dnn=c('prod_id','category'))
# make it as table
# http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/
# countdf <- as.data.frame(table(x))
# x1 = xtabs( ~ x$product_id:x$name+x$category, data=countdf)
## ========================================

## ========================================
# serve same purpose as abovebut it uses reshape library (better)
# x = head(temp.prod,n=20)
# x = x %>% select(product_id, name, category)
# x = mutate(x,interactions=1)
# x <- melt(x,id.vars=c('product_id','name','category'))
# x <- cast(x,formula=product_id+name~category)
# x <- replace(x,is.na(x),0)
## ========================================

## ========================================
# replace multiple space with single space or underscore "-"
# temp.prod$category1 = tolower(str_replace(gsub("\\s+"," ", str_trim(temp.prod$category)), "B", "b"))
## ========================================

## ========================================
# Merging multiple columns
# http://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
# set.seed(1)
# list.of.data.frames = list(data.frame(x=1:10, a=1:10), data.frame(x=5:14, b=11:20))
# merged.data.frame = Reduce(function(...) merge(..., all=T), list.of.data.frames)
## ========================================

## Check statistics for grouping procut
# # unique prod category (21 variables) - use this!
# prod.category = temp.prod %>%
#   select (category) %>%
#   group_by(category) %>%
#   summarise(frequency = n()) %>%  
#   arrange(desc(frequency))
# # unique product type (5 variables)
# prod.prodtype = temp.prod %>%
#   select (product_type) %>%
#   group_by(product_type) %>%
#   summarise(frequency = n()) %>%  
#   arrange(desc(frequency))
# # unique status (1= Enabled 2= Disabled)
# prod.status = temp.prod %>%
#   select (status) %>%
#   group_by(status) %>%
#   summarise(frequency = n()) %>%  
#   arrange(desc(frequency))
# # unique product lifecycle
# prod.lifecycle = temp.prod %>%
#   select (product_lifecycle) %>%
#   group_by(product_lifecycle) %>%
#   summarise(frequency = n()) %>%  
#   arrange(desc(frequency))
