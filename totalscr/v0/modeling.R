# Enable Bellow Packages
require(data.table)

# Set Appropriate Path to the link
setwd("~/PycharmProjects/dissertation/raw_data")

# ###########################
# web <- read.csv('data/web.csv')
# dotmailer <- read.csv('data/subscribe.csv')
# ###########################

trans <- read.csv('data/trans.csv')
trans1 <- trans[!duplicated(trans),]
str(trans1)

#################################### FACTORIZING AND FORMATTING ##############################
#little bit cosmetic setting
trans1$order_no = as.character(trans1$order_no)
trans1$customer_id = as.character(trans1$customer_id)

trans1$order_date = as.POSIXct(trans1$order_date)
trans1$invoice_date = as.POSIXct(trans1$invoice_date)
trans1$shipment_date = as.POSIXct(trans1$shipment_date)
trans1$reminder_date = as.POSIXct(trans1$reminder_date)

# numeric - VAT
trans1$global_line_total_exc_vat = as.numeric(as.character(trans1$global_line_total_exc_vat))
trans1$global_line_total_exc_vat[which(is.na(trans1$global_line_total_exc_vat))] <- 0
# numeric - VAT
trans1$discount_percent = as.numeric(as.character(trans1$discount_percent))
trans1$discount_percent[which(is.na(trans1$discount_percent))] <- 0

#############################################################################################


# work with smaller dataset
# trans2 = trans1[sample(c(1:nrow(trans1)),10000),]

min(trans1$order_date)
max(trans1$order_date)

# base
start_list = seq(as.POSIXct(as.Date("2014-01-01 12:00:00")),length=24,by="months")
start_list = sort(start_list, decreasing = T)

end_list = seq(as.POSIXct(as.Date("2014-02-01  23:59:59")),length=24,by="months")-1
end_list = sort(end_list, decreasing = T)

# library(lubridate)
# start_list_week = floor_date(ymd("2014-01-01"), "week") + weeks(1:104)
# end_list_week = ceiling_date(ymd("2014-01-01"), "week") + weeks(0:104)

#################################### SETTING START DATE AND END DATE ##############################

start = start_list[5]; end = end_list[1]  

str(trans1); names(trans1)
feat = trans1[which(trans1$order_date >= start & trans1$order_date <= end),
              c(4,11,6,15,16,17,21)]
str(feat)

########################### MINI FEATURIZE USER WEEKLY DEPENDENT #######################

pairs = feat[,c(1,2)]
pairs = pairs[!duplicated(pairs),]

# payment_method
tmp = feat[,c(1,2,3)]
tmp = tmp[!duplicated(tmp),]
tmp = data.frame(cbind(tmp, data.frame(model.matrix(~payment_method-1, tmp))))
tmp = aggregate( cbind(payment_methodCard...New,payment_methodCard...Stored,payment_methodCheque,payment_methodFree,
                 payment_methodNULL,payment_methodPayPal,payment_methodstorecredit) ~ customer_id + product_id, data = tmp, max)
pairs = join(pairs, tmp, by =c('customer_id','product_id'))


# price type
tmp = feat[,c(1,2,6)]
tmp = tmp[!duplicated(tmp),]
tmp = data.frame(cbind(tmp, data.frame(model.matrix(~price_type-1, tmp))))
tmp = aggregate( cbind(price_type,price_typeDiscounted,price_typeNULL,price_typePLA,
                       price_typePLA...Discounted,price_typeRegular) ~ customer_id + product_id, data = tmp, max)
pairs = join(pairs, tmp, by =c('customer_id','product_id'))

# order type
tmp = feat[,c(1,2,7)]
tmp = tmp[!duplicated(tmp),]
tmp = data.frame(cbind(tmp, data.frame(model.matrix(~order_type-1, tmp))))
tmp = aggregate( cbind(order_type,order_typeAutomatic,order_typeTelesales,order_typeWeb) ~ customer_id + product_id, data = tmp, max)
pairs = join(pairs, tmp, by =c('customer_id','product_id'))

# global_VAT
tmp = feat[,c(1,2,4)]
melted <- melt(tmp,id.vars=c('customer_id','product_id'))
tmp_mean= dcast(melted, customer_id+product_id ~ variable, mean)
colnames(tmp_mean)[3] <- 'avg_mean_payment'

tmp_count= dcast(melted, customer_id+product_id ~ variable, length)
colnames(tmp_count)[3] <- 'count_product'

# discount_percent
tmp = feat[,c(1,2,5)]
melted <- melt(tmp,id.vars=c('customer_id','product_id'))
tmp_disc_mean= dcast(melted, customer_id+product_id ~ variable, mean)
colnames(tmp_disc_mean)[3] <- 'avg_discount'

pairs = join(pairs, tmp_mean, by=c('customer_id','product_id'))
pairs = join(pairs, tmp_count, by=c('customer_id','product_id'))
pairs = join(pairs, tmp_disc_mean, by=c('customer_id','product_id'))

rm(tmp, tmp_count, tmp_mean, tmp_disc_mean,melted)

################### JOIN WITH USER/ITEM GLOBAL ###################

# features item global
feat.item.global <- read.csv('features/features.item.global.csv')
# str(feat.item.global) # names(feat.item.global)
feat.item.global <- feat.item.global[,-c(2:7,31:40)]
colnames(feat.item.global)[1] <- 'product_id'

dailies = feat.item.global[which(feat.item.global$daysperlens30 == 1
                                 & feat.item.global$product_lifecyclediscountinued != 1 
                                 & feat.item.global$updated_at_year == 2016),c(1)]

dailies = unique(feat.item.global$product_id)

## -> run sampling data for target 0 from above dailies

# other = feat.item.global[which(feat.item.global$daysperlensother == 1),c(1)]
# testitems = c(as.character(dailies), as.character(other))

pairs = join(pairs, feat.item.global, by=c("product_id"))

# features cust global
feat.cust.global <- read.csv('features/features.cust.global.csv')
# str(feat.cust.global) # names(feat.cust.global)
feat.cust.global <- feat.cust.global[,-c(3:23)]

pairs = join(pairs, feat.cust.global, by=c("customer_id"))

# choose selected columns
names(pairs)
feat = pairs[,-c(23:34,91:108)]

########################### USER ITEM PAIRS #######################

# TARGET 1
data1 = data.table(pairs)
data1 <- data1[,c('customer_id', 'product_id'), with=F]
data1 <- data1[!duplicated(data1)]
data1$target <- rep(1,nrow(data1))

# TARGET 2
purchases = unique(data1$customer_id)
seeds <- sample(1:(10*length(purchases)), length(purchases))
items <- list()

# set k = 15
k = 3
for (i in 1:length(purchases)){ # k random coupons for each week purchaser
  set.seed(seeds[i])                    
  items[[i]] <- sample(dailies, k)
}

items <- lapply(1:length(items), function(i){ # remove viewed coupons from above list
  setdiff(items[[i]], data1$product_id[which(data1$customer_id == purchases[i])])
})

reps <- unlist(lapply(1:length(items), function(i){length(items[[i]])}))
data0 <- data.frame(cbind('customer_id' = rep(purchases, reps), 'product_id'=unlist(items)))
data0$target <- rep(0,nrow(data0))
data <- rbind(data0, data1)

df <- join(data, feat, by=c('customer_id', 'product_id'))
dir.create('features/features.cust.item.random')
write.table(df, paste0('features/features.cust.item.random/features.cust.item.k',k,'.seed',seeds[1],'.csv'),
            row.names = F, col.names = T, sep = ',')


########################### USER ITEM PAIRS #######################
xgb.model.make <- function(file='features.cust.item.k15.seed804078.csv',
                           eta=0.05, subsample=0.9, colsample=0.5, depth=7, metric='auc', rounds=1400,
                           cv = F, seed=29, feat.imp=F){
  
  # default rounds = 1400
  train = data.frame(fread(paste0('features/features.cust.item.random/', file)))
  Dtrain <- xgb.DMatrix(data=data.matrix(train[,-c(1:3)]), label=train$target, missing=NA)
  parameters <- list(booster='gbtree', 
                     objective = 'binary:logistic',
                     eta = eta, 
                     subsample = subsample,
                     colsample_bytree = colsample,# 0.5 = 0.7709, 1750
                     max_depth = depth, 
                     verbose = 1,
                     eval.metric=metric,
                     nthread=30)
  if (cv ==T){
    xgb.cv(params=parameters, data=Dtrain, 
           nfold=5,
           nrounds = rounds, seed=seed)
  }
  
  xgb.model <- xgb.train(params = parameters, seed =seed, nrounds=rounds, data=Dtrain, verbose=1)
  
  if (feat.imp ==T ){
    xgb.imp <- xgb.importance(feature_names = colnames(train)[-c(1:3)], model=xgb.model)
    return(list('feat.imp' = xgb.imp, 'model'=xgb.model))
  } else {
    return(xgb.model)
  }
  
}


## LOGISTIC REGRESSION : GRADIENT BOOSTING 
model1 <- xgb.model.make(file = 'features.cust.item.k3.seed1288575.csv', 
                         eta=0.1, subsample=0.5, colsample=0.5, depth=15, metric='auc', rounds=30,
                         cv = T, seed=29, feat.imp=F) 


#### SANITY CHECK
df0 = data.frame(data0)
df0 = join(df0,feat, by=c("product_id"))
write.table(df, paste0('features/features.cust.item.random/features.cust.item.k',k,'.seed',seeds[1],'.csv'),
            row.names = F, col.names = T, sep = ',')


################## NEW FEATURE ENGINEERING ################## 

# item features
temp.datax1 = join(data1,feat.item.global, by=c("product_id"))
temp.datax0 = join(data0,feat.item.global, by=c("product_id"))

# user features
temp.datax1 = join(temp.datax1,feat.cust.global, by=c("customer_id"))
temp.datax0 = join(temp.datax0,feat.cust.global, by=c("customer_id"))

# user week_dependent
temp.datax1= join(temp.datax1, pairs, by=c("customer_id", "product_id"))
temp.datax0= join(temp.datax0, pairs, by=c("customer_id", "product_id"))

temp.data = rbind(temp.datax1,temp.datax0); names(temp.data)
temp.df = data.frame(temp.data)[,-c(4:15,72:89)]

write.table(temp.df, paste0('features/features.cust.item.random/features.cust.item.k',k,'.seed',seeds[1],'.csv'),
            row.names = F, col.names = T, sep = ',')

x = temp.df[which(temp.df$customer_id== 'u186662'),]

train = data.frame(temp.df)
Dtrain <- xgb.DMatrix(data=data.matrix(train[,-c(1:3)]), label=train$target, missing=NA)

require(ggplot2)
xgb.plot.importance(model1$feat.imp)

model1 <- xgb.model.make(file = 'features.cust.item.k4.seed736410.csv', 
                         eta=0.005, subsample=0.9, colsample=0.1, depth=3, metric='auc', rounds=5,
                         cv = T, seed=29, feat.imp=T) 

#################### TUNING PARAMETERS FOR NOT 
## create function for xgboost modeling
xgb.model.Dtrain.make <- function(file=train,
                           eta=0.05, subsample=0.9, colsample=0.5, depth=7, metric='auc', rounds=1400,
                           cv = F, seed=29, feat.imp=F){
  
  print('get scale positive negative weight')
  sumwpos <- sum(1*(train$target==1.0))
  sumwneg <- sum(1*(train$target==0.0))
  
  print('get Dtrain formating for the data')
  Dtrain = xgb.DMatrix(data=data.matrix(file[,-c(1:3)]), label=train$target, missing=NA)
  
  print('set the parameters')
  parameters <- list(booster='gbtree', 
                     objective = 'binary:logistic',
                     scale_pos_weight = sumwneg/sumwpos,
                     eta = eta, 
                     subsample = subsample,
                     colsample_bytree = colsample,# 0.5 = 0.7709, 1750
                     max_depth = depth, 
                     verbose = 1,
                     eval.metric=metric,
                     nthread=30)
  if (cv ==T){
    xgb.cv(params=parameters, data=Dtrain, 
           nfold=5,
           nrounds = rounds, seed=seed)
  }
  print('finish')
  xgb.model <- xgb.train(params = parameters, seed =seed, nrounds=rounds, data=Dtrain, verbose=1)
  
  if (feat.imp ==T ){
    xgb.imp <- xgb.importance(feature_names = colnames(train)[-c(1:3)], model=xgb.model)
    return(list('feat.imp' = xgb.imp, 'model'=xgb.model))
  } else {
    return(xgb.model)
  }
  
}

## best setting
model1 <- xgb.model.Dtrain.make(file = train, 
                                eta=0.005, subsample=0.6, colsample=0.025, depth=1, metric='auc', rounds=10,
                                cv = T, seed=29, feat.imp=T)

xgb.plot.importance(model1$feat.imp)

## another shoot
model1 <- xgb.model.Dtrain.make(file = train, 
                                eta=0.005, subsample=0.6, colsample=0.025, depth=1, metric='auc', rounds=10,
                                cv = T, seed=29, feat.imp=T)



str(train)

############### ANOTHER WAY

