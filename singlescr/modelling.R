### R code for Modeling Process - 
### New Modification based on single purchase and take into account only customer who made purchase both in 2014 and 2015
### Author : Adi Budyanto
### Date : 20 August 2016

require(data.table)
require(plyr)
library(lubridate)
require(doParallel)
require(caret)

# set working path in desktop & load pre-defined functions
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data");source('~/PycharmProjects/dissertation/scr/singlescr/func.R')
} else {
  setwd("~/Dropbox/dissertation/raw_data"); source('~/Dropbox/dissertation/newscr/func.R')
}

# load selected customers 2014 (who recorded transactions both in 2014 and 2015)
load('data/keys/cust14Filter.Rda')
# load selected customers 2014 (who purchase new products listed in 2015)
load('data/keys/cust14WnewProd.Rda')
# load trans data that were already formatted (date, numeric)
load('data/trans.Rda')

#########################################################################################
#### Random select smaller set of 2014 customers for faster simulation (temp)
set.seed(123)
nsample <- 150; tmp.cust14 <- sample(cust14Filter,nsample)
nsample1 <- 50; tmp.cust14.new15 <- sample(cust14BoughtnewProd,nsample1)
# random select and update the number of samples choosen
tmp.cust14 <- union(tmp.cust14,tmp.cust14.new15); nsample <- nsample+nsample1

# elicit corresponding transaction nbs based on above customer nb
tmp.train <- trans[which(trans$customer_id %in% tmp.cust14),c('customer_id','order_no','order_date','product_id')]
# eliminate duplicate values while counting freq of same transaction
tmp.train <- count(tmp.train)
# tmp.train[which(tmp.train$customer_id == 'u19995'),] # check if a cust had both 2014 and 2015 transaction records

######################################################################
#### For each transaction order, create unique key of single identifier
tmp.train$key = paste(tmp.train$customer_id, tmp.train$order_no, sep="")

######################################################################
#### Splitting data for constructing positive and negative target/class
# positive samples / class = 1
data1 <- data.table(tmp.train)
data1 <- data1[,c('key', 'product_id'), with=F]
data1$target <- rep(1,nrow(data1))

# load a list of new product in 2015
load('data/keys/newprod15.rda')
amanNew <- sample(prodnew2015,10)

# negative samples / class = 0
# firstly, create target product_id for random sampling
aman <- ddply(data1, .(product_id), function(x) c(numOrder = nrow(x)))
aman <- aman[with(aman, order(-numOrder)), ]
amanTop <- aman[1:25,1]
amanSample <- c(amanNew,amanTop)

# create unique list of key identifier (cust_id + order_no)
purchases = unique(data1$key)
seeds <- sample(1:(10*length(purchases)), length(purchases))
items <- list()

# set k = 4 items to randomly sampled as negative target (6=20:80)
k = 4
for (i in 1:length(purchases)){ # k random items for each week purchaser
  set.seed(seeds[i])                    
  items[[i]] <- sample(amanSample, k)
}

# remove purchased items from aforementioned list
items <- lapply(1:length(items), function(i){ 
  setdiff(items[[i]], data1$product_id[which(data1$key == purchases[i])])
})
reps <- unlist(lapply(1:length(items), function(i){length(items[[i]])}))
data0 <- data.frame(cbind('key' = rep(purchases, reps), 'product_id'=unlist(items)))
data0$target <- rep(0,nrow(data0))

######################################################################
#### Binding data1 and data0 together and get back the unique identifiers
data <- rbind(data1,data0)
data <- data[with(data, order(key)),]
# colnames(tmp.train)[-4]

# temporary check : some error appears after incorporating sampling from new products
tmpUnData <- data[!(duplicated(data))]
tmpUnTrain <- unique(tmp.train[,colnames(tmp.train)[-4]])
purchaseData <- merge(tmpUnData,tmpUnTrain, by=c("key"))

# Link back to key data to get unique identifiers
# purchaseData <- merge(data,tmp.train[,-4], by=c("key"))
save(purchaseData, file = paste('features/features.cust.item.random/purchaseData',nsample,'.seed',123,'.rda',sep=''))

# release some memory
rm(tmpUnData, tmpUnTrain)
#####################################################################################
# Reformating purchaseData as dataframe and get the final form of targetData
# Load product table
feat.item.global <- read.csv('features/features.item.global.csv')
# names(feat.item.global)
item.features = colnames(feat.item.global)[c(1:7,143:ncol(feat.item.global))]

purchaseData <- data.frame(purchaseData)
targetData <- join(purchaseData[,c("order_date","order_no","target","customer_id","product_id")], feat.item.global[,item.features], by=c('product_id'))
# get unique customer_id
id <- unique(targetData$customer_id)

#####################################################################################
##### Get user-dependent features for each customer id
# load related Rdata files
# load('features/features.cust.dependent/feat.user.dependent.NbProd.Rda')
# load('features/features.cust.dependent/feat.user.dependent.NbProd.part2.Rda')
# load('features/features.cust.dependent/feat.user.dependent.TransSpent.Rda')
# load('features/features.cust.dependent/feat.user.dependent.TransSpent.part2.Rda')
# load('features/features.cust.dependent/feat.user.dependent.DiscReceived.Rda')
# load('features/features.cust.dependent/feat.user.dependent.TransSpent.part2.Rda')
# load('features/features.cust.dependent/feat.user.dependent.UniqueOrder.Rda')
# load('features/features.cust.dependent/feat.user.dependent.TransSpent.part2.Rda')

temp = list.files(path = "features/features.cust.dependent/", pattern="*.Rda")
for (i in 1:length(temp)) {load(paste("features/features.cust.dependent/",temp[i],sep=""))}

# 1 Tot number of past purchased products per selected features (categoy, subcat, brand, manu)
targetData$pastCatNbProd <- getAffinityScore(holder= featCatNbProd[which(rownames(featCatNbProd) %in% id),], test = targetData[,c("customer_id","category")])
targetData$pastSubNbProd <- getAffinityScore(holder= featSubNbProd[which(rownames(featSubNbProd) %in% id),], test = targetData[,c("customer_id","subcategory")])
targetData$pastManNbProd <- getAffinityScore(holder= featManNbProd[which(rownames(featManNbProd) %in% id),], test = targetData[,c("customer_id","manufacturer")])
targetData$pastBrandNbProd <- getAffinityScore(holder= featBrandNbProd[which(rownames(featBrandNbProd) %in% id),], test = targetData[,c("customer_id","brand")])

targetData$pastSubLensmanuNbProd <- getAffinityScore(holder= featSubLensmanuNbProd[which(rownames(featSubLensmanuNbProd) %in% id),], test = targetData[,c("customer_id","subcat.lens.manu")])
targetData$pastLensBrandManuNbProd <- getAffinityScore(holder= featLensBrandManuNbProd[which(rownames(featLensBrandManuNbProd) %in% id),], test = targetData[,c("customer_id","lens.brand.manu")])
targetData$pastSubBrandNbProd <- getAffinityScore(holder= featSubBrandNbProd[which(rownames(featSubBrandNbProd) %in% id),], test = targetData[,c("customer_id","subcat.brand")])
targetData$pastLensBrandNbProd <- getAffinityScore(holder= featLensBrandNbProd[which(rownames(featLensBrandNbProd) %in% id),], test = targetData[,c("customer_id","lens.brand")])

# 2 Tot amounts of past purchased products per selected features (categoy, subcat, brand, manu)
targetData$pastCatTransSpent <- getAffinityScore(holder= featCatTransSpent[which(rownames(featCatTransSpent) %in% id),], test = targetData[,c("customer_id","category")])
targetData$pastSubTransSpent <- getAffinityScore(holder= featSubTransSpent[which(rownames(featSubTransSpent) %in% id),], test = targetData[,c("customer_id","subcategory")])
targetData$pastManTransSpent <- getAffinityScore(holder= featManTransSpent[which(rownames(featManTransSpent) %in% id),], test = targetData[,c("customer_id","manufacturer")])
targetData$pastBrandTransSpent <- getAffinityScore(holder= featBrandTransSpent[which(rownames(featBrandTransSpent) %in% id),], test = targetData[,c("customer_id","brand")])

targetData$pastSubLensmanuTransSpent <- getAffinityScore(holder= featSubLensmanuTransSpent[which(rownames(featSubLensmanuTransSpent) %in% id),], test = targetData[,c("customer_id","subcat.lens.manu")])
targetData$pastLensBrandManuTransSpent <- getAffinityScore(holder= featLensBrandManuTransSpent[which(rownames(featLensBrandManuTransSpent) %in% id),], test = targetData[,c("customer_id","lens.brand.manu")])
targetData$pastSubBrandTransSpent <- getAffinityScore(holder= featSubBrandTransSpent[which(rownames(featSubBrandTransSpent) %in% id),], test = targetData[,c("customer_id","subcat.brand")])
targetData$pastLensBrandTransSpent <- getAffinityScore(holder= featLensBrandTransSpent[which(rownames(featLensBrandTransSpent) %in% id),], test = targetData[,c("customer_id","lens.brand")])

# 3 Tot amounts of discount received per selected features (categoy, subcat, brand, manu)
targetData$pastCatDiscReceived <- getAffinityScore(holder= featCatDiscReceived[which(rownames(featCatDiscReceived) %in% id),], test = targetData[,c("customer_id","category")])
targetData$pastSubDiscReceived <- getAffinityScore(holder= featSubDiscReceived[which(rownames(featSubDiscReceived) %in% id),], test = targetData[,c("customer_id","subcategory")])
targetData$pastManDiscReceived <- getAffinityScore(holder= featManDiscReceived[which(rownames(featManDiscReceived) %in% id),], test = targetData[,c("customer_id","manufacturer")])
targetData$pastBrandDiscReceived <- getAffinityScore(holder= featBrandDiscReceived[which(rownames(featBrandDiscReceived) %in% id),], test = targetData[,c("customer_id","brand")])

targetData$pastSubLensmanuDiscReceived<- getAffinityScore(holder= featSubLensmanuDiscReceived[which(rownames(featSubLensmanuDiscReceived) %in% id),], test = targetData[,c("customer_id","subcat.lens.manu")])
targetData$pastLensBrandManuDiscReceived <- getAffinityScore(holder= featLensBrandManuDiscReceived[which(rownames(featLensBrandManuDiscReceived) %in% id),], test = targetData[,c("customer_id","lens.brand.manu")])
targetData$pastSubBrandDiscReceived <- getAffinityScore(holder= featSubBrandDiscReceived[which(rownames(featSubBrandDiscReceived) %in% id),], test = targetData[,c("customer_id","subcat.brand")])
targetData$pastLensBrandDiscReceived<- getAffinityScore(holder= featLensBrandDiscReceived[which(rownames(featLensBrandDiscReceived) %in% id),], test = targetData[,c("customer_id","lens.brand")])

# 4 Tot numberof unique transactions recorded per selected features (categoy, subcat, brand, manu)
targetData$pastCatUniqueOrder <- getAffinityScore(holder= featCatUniqueOrder[which(rownames(featCatUniqueOrder) %in% id),], test = targetData[,c("customer_id","category")])
targetData$pastSubUniqueOrder <- getAffinityScore(holder= featSubUniqueOrder[which(rownames(featSubUniqueOrder) %in% id),], test = targetData[,c("customer_id","subcategory")])
targetData$pastManUniqueOrder <- getAffinityScore(holder= featManUniqueOrder[which(rownames(featManUniqueOrder) %in% id),], test = targetData[,c("customer_id","manufacturer")])
targetData$pastBrandUniqueOrder <- getAffinityScore(holder= featBrandUniqueOrder[which(rownames(featBrandUniqueOrder) %in% id),], test = targetData[,c("customer_id","brand")])

targetData$pastSubLensmanuUniqueOrder<- getAffinityScore(holder= featSubLensmanuUniqueOrder[which(rownames(featSubLensmanuUniqueOrder) %in% id),], test = targetData[,c("customer_id","subcat.lens.manu")])
targetData$pastLensBrandManuUniqueOrder <- getAffinityScore(holder= featLensBrandManuUniqueOrder[which(rownames(featLensBrandManuUniqueOrder) %in% id),], test = targetData[,c("customer_id","lens.brand.manu")])
targetData$pastSubBrandUniqueOrder <- getAffinityScore(holder= featSubBrandUniqueOrder[which(rownames(featSubBrandUniqueOrder) %in% id),], test = targetData[,c("customer_id","subcat.brand")])
targetData$pastLensBrandUniqueOrder <- getAffinityScore(holder= featLensBrandUniqueOrder[which(rownames(featLensBrandUniqueOrder) %in% id),], test = targetData[,c("customer_id","lens.brand")])

# release some memory
# rm(list = ls(pattern = 'feat'))

#####################################################################################
##### Load relevant data (purchase dummy data - matrix similarity) generated from similariy.R
temp = list.files(path = "features/features.itembasedCF/purchase.session", pattern="*.Rdata")
for (i in 1:length(temp)) {load(paste("features/features.itembasedCF/purchase.session/",temp[i],sep=""))}

temp = list.files(path = "features/features.matrix.cust.vars/purchase.session", pattern="*.Rdata")
for (i in 1:length(temp)) {load(paste("features/features.matrix.cust.vars/purchase.session/",temp[i],sep=""))}

#####################################################################################
### Constructing features from three sources : Item profiles, Customer, and ItembasedCF

# Get relevant itembasedCF per customer basis
strt<-Sys.time() # start time
# 1 Product ID (p1,p2,p3)
tmp.data <- useritemList[[14]]
tmp.matrix <- CFlist[[14]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreProd <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","product_id")])

# 2 Sub Category (dailies, two-weeklies, etc)
tmp.data <- useritemList[[16]]
tmp.matrix <- CFlist[[16]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreSubcategory <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","subcategory")])

# 3 Category (dailies, non-dailies, other)
tmp.data <- useritemList[[3]]
tmp.matrix <- CFlist[[3]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCategory <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","category")])

# 4 Lens type (spherical, toric,)
tmp.data <- useritemList[[12]]
tmp.matrix <- CFlist[[12]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLens<- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lenstype")])

# 5 Brand (acuvue,etc)
tmp.data <- useritemList[[1]]
tmp.matrix <- CFlist[[1]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreBrand<- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","brand")])

# 6 Manufacturer (jnj, etc)
tmp.data <- useritemList[[13]]
tmp.matrix <- CFlist[[13]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","manufacturer")])

# 7 Category Lens (dailies.toric)
tmp.data <- useritemList[[5]]
tmp.matrix <- CFlist[[5]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCatLens<- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","category.lens")])

# 8 Category Brand (dailies.acuvue)
tmp.data <- useritemList[[4]]
tmp.matrix <- CFlist[[4]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCatBrand <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","category.brand")])

# 9 Category Manufacturer (dailies_jnj)
tmp.data <- useritemList[[6]]
tmp.matrix <- CFlist[[6]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCatManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","category.manu")])

# 10 Lenstype Brand (toric_acuvue)
tmp.data <- useritemList[[8]]
tmp.matrix <- CFlist[[8]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLensBrand <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lens.brand")])

# 11 Lenstype Manufacturer (toric_jnj)
tmp.data <- useritemList[[10]]
tmp.matrix <- CFlist[[10]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLensManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lens.manu")])

# 12 Lenstype Sub category (toric_dailies)
tmp.data <- useritemList[[11]]
tmp.matrix <- CFlist[[11]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLensSubcat <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lens.subcat")])

# 13 Subcategory Brand (dailies_acuvue)
tmp.data <- useritemList[[15]]
tmp.matrix <- CFlist[[15]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreSubcatBrand <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","subcat.brand")])

# 14 Subcategory Manufacturer (dailies_jnj)
tmp.data <- useritemList[[18]]
tmp.matrix <- CFlist[[18]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreSubcatManu<- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","subcat.manu")])

# 15 Category Lenstype Manufacturer (nondailies_toric_jnj)
tmp.data <- useritemList[[7]]
tmp.matrix <- CFlist[[7]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCatLensManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","cat.lens.manu")])

# 16 Subcategory Lenstype Manufacturer (two_weeklies_toric_jnj)
tmp.data <- useritemList[[17]]
tmp.matrix <- CFlist[[17]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreSubcatLensManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","subcat.lens.manu")])

# 17 Lenstype Brand Manufacturer (toric_acuvue_jnj)
tmp.data <- useritemList[[9]]
tmp.matrix <- CFlist[[9]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLensBrandManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lens.brand.manu")])

# 18 Brand Manufacturer (toric_acuvue_jnj)
tmp.data <- useritemList[[2]]
tmp.matrix <- CFlist[[2]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreBrandManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","brand.manu")])

print(Sys.time()-strt) # end time

tmp <- names(targetData)[6:24]
targetFinalData <- targetData[, !(colnames(targetData) %in% tmp)]

# save original files before data splitting
save(purchaseData, targetFinalData, targetData, file = paste('features/features.cust.item.random/purchaseData',nsample,'.seed',123,'.rda',sep=''))
# nsample = 5000;
# load(paste('features/features.cust.item.random/purchaseData',nsample,'.seed',123,'.rda',sep=''))

#####################################################################################
## We'll split all of the 2014 data into the training set and a portion of the 2015 data too

# tmp.train$is2015 <- year(tmp.train$order_date) == 2015
# tmp <- targetData[which(targetData$customer_id %in% c('u100009','u100348')),c("order_date","order_no","target","customer_id","product_id")]

`%between%`<-function(x,rng) x>rng[1] & x<rng[2]

# new variable to determine the year of the transaction for the sake of constructing train & validation
# we use the first 4 month data in 2015 for tuning the algorithm and the rest for validation
tmp$is201504 <- (year(tmp$order_date)*100+month(tmp$order_date)) %between% c(201412,201504)
tmp$is201500 <- (year(tmp$order_date)) == 2015

# create training data set
training <- subset(tmp, !is201500)
# keep the record for original 2014 training data (lated used in trctrl)
pre2015 <- 1:nrow(training)
year2015 <- subset(tmp, is201500)

# append the first 4 months data to training dataset
training2 <- subset(tmp, is201504)
testing <- subset(tmp, !is201504 & is201500)

training <- rbind(training,training2)
training <- noZV(training)

testing <- testing[, names(training)]

# save original files before data splitting
save(training, pre2015, testing, file = paste('features/features.cust.item.random/preModelData',nsample,'.seed',123,'.rda',sep=''))

# preprocessing before feeding the data into machine learning algorithms
fullSet <- names(training)[!(names(training) %in% c("target","order_no","customer_id","product_id"))]

training[,2]= factor(training[,2], labels = c('N','Y'))
testing[,2]= factor(testing[,2], labels = c('N','Y'))

colnames(training)<- make.names(colnames(training))
colnames(testing)<- make.names(colnames(testing))

#####################################################################################
#### Begin Machine Learning Algorithm

# create new folders to store resutls and model
dir.create('results')
dir.create('images')
dir.create('models')

# Pack the training control parameters
xgb_trcontrol = trainControl(
  method = "LGOCV",
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",   # save losses across all models
  classProbs = TRUE,      # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  index = list(TestSet = pre2015),
  savePredictions = TRUE,
  allowParallel = TRUE
)

### Model 1 : Extreme Gradient Boosting
set.seed(123)
strt<-Sys.time() # start time
xgbTree <- train(x = training[,fullSet],
                 y = training$target,
                 method = "xgbTree",
                 metric = "ROC",
                 tuneGrid = expand.grid(nrounds = 10*(15:50), #10*(15:50)
                                        eta = c(0.1, 0.2, 0.4, 0.6, 0.8, 1), #c(0.1, 0.2, 0.4, 0.6, 0.8, 1)
                                        max_depth = c(2, 4, 6, 8, 10), #c(0.05,0.1,0.5)
                                        gamma = 0,               #default=0
                                        colsample_bytree = 0.8,    #default=1
                                        min_child_weight = 0.8),   #default=1
                 trControl = xgb_trcontrol,
                 preProc = c("center", "scale"),
                 maximize = TRUE,
                 subsample = 0.8,
                 verbose = 1,
                 base_score = 0.5,
                 nthread = 10
)
print(Sys.time()-strt) # end time
print(xgbTree)

# save Model
saveRDS(xgbTree, paste('models/xgbTree.rds',nsample,'.seed',123,'.rds',sep=''))

#### Store relevant graphics
# grid tune search plot
png(filename=paste('images/xgbTree_grid_tune.plot',nsample,'.seed',123,'.png',sep=''))
plot(xgbTree)
dev.off()

## scatter plot of the AUC against max_depth and eta
png(filename=paste('images/xgbTree_AUC_vs_maxdepth_eta.plot',nsample,'.seed',123,'.png',sep=''))
ggplot(xgbTree$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")
dev.off()

### Model 2: Neural Networks
nnetGrid <- expand.grid(size = 1:10, decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$size)

set.seed(123)
strt<-Sys.time() # start time
# model 1 : no transformation
nnetFit <- train(x = training[,fullSet], 
                 y = training$target,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = 1*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                 trControl = xgb_trcontrol)
print(Sys.time()-strt) # end time
print(nnetFit)
# Save Model
saveRDS(nnetFit, paste('models/nnetFit.rds',nsample,'.seed',123,'.rds',sep=''))

# model 2 : spatial sign transformation
set.seed(123)
strt<-Sys.time() # start time
nnetFit2 <- train(x =  training[,fullSet], 
                  y = training$target,
                  method = "nnet",
                  metric = "ROC",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid,
                  trace = FALSE,
                  maxit = 2000,
                  MaxNWts = 1*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                  trControl = xgb_trcontrol)
print(Sys.time()-strt) # end time
print(nnetFit2)
# Save Model
saveRDS(nnetFit2, paste('models/nnetFit2.rds',nsample,'.seed',123,'.rds',sep=''))

# model 3 : repeat the model 10 times, and take the average results
nnetGrid$bag <- FALSE

set.seed(123)
strt<-Sys.time() # start time
nnetFit3 <- train(x = training[,fullSet], 
                  y = training$target,
                  method = "avNNet",
                  metric = "ROC",
                  preProc = c("center", "scale"),
                  tuneGrid = nnetGrid,
                  repeats = 10,
                  trace = FALSE,
                  maxit = 2000,
                  MaxNWts = 10*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                  #allowParallel = FALSE, ## this will cause to many workers to be launched.
                  trControl = xgb_trcontrol)
print(Sys.time()-strt) # end time
print(nnetFit3)
# Save Models
saveRDS(nnetFit3, paste('models/nnetFit3.rds',nsample,'.seed',123,'.rds',sep=''))

# model 4 : repeat the model 10 times, and take the average results plus tranformation
set.seed(123)
strt<-Sys.time() # start time
nnetFit4 <- train(x = training[,fullSet], 
                  y = training$target,
                  method = "avNNet",
                  metric = "ROC",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid,
                  trace = FALSE,
                  maxit = 2000,
                  repeats = 10,
                  MaxNWts = 10*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                  #allowParallel = FALSE, 
                  trControl = xgb_trcontrol)
print(Sys.time()-strt) # end time
print(nnetFit4)
# Save Models
saveRDS(nnetFit4, paste('models/nnetFit4.rds',nsample,'.seed',123,'.rds',sep=''))

nnetFit4$pred <- merge(nnetFit4$pred,  nnetFit4$bestTune)
nnetCM <- confusionMatrix(nnetFit4, norm = "none")
nnetCM

nnet1 <- nnetFit$results
nnet1$Transform <- "No Transformation"
nnet1$Model <- "Single Model"

nnet2 <- nnetFit2$results
nnet2$Transform <- "Spatial Sign"
nnet2$Model <- "Single Model"

nnet3 <- nnetFit3$results
nnet3$Transform <- "No Transformation"
nnet3$Model <- "Model Averaging"
nnet3$bag <- NULL

nnet4 <- nnetFit4$results
nnet4$Transform <- "Spatial Sign"
nnet4$Model <- "Model Averaging"
nnet4$bag <- NULL

nnetResults <- rbind(nnet1, nnet2, nnet3, nnet4)
nnetResults$Model <- factor(as.character(nnetResults$Model),
                            levels = c("Single Model", "Model Averaging"))

#### Store relevant graphics
# grid tune search plot
png(filename=paste('images/xgbTree_grid_tune.plot',nsample,'.seed',123,'.png',sep=''))
plot(xgbTree)
dev.off()

library(latticeExtra)
png(filename=paste('images/nnet4_AUC.plot',nsample,'.seed',123,'.png',sep=''))
useOuterStrips(
  xyplot(ROC ~ size|Model*Transform,
         data = nnetResults,
         groups = decay,
         as.table = TRUE,
         type = c("p", "l", "g"),
         lty = 1,
         ylab = "ROC AUC (2008 Hold-Out Data)",
         xlab = "Number of Hidden Units",
         auto.key = list(columns = 4, 
                         title = "Weight Decay", 
                         cex.title = 1)))
dev.off()

#### Model 3 : Penalized Regression Logistics
# lasso, ridge and elastic net by configuring the alpha parameter to 1, 0 or in [0,1]
glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1), #c(0,  .1,  .2, .4, .6, .8, 1)
                        lambda = seq(.01, .2, length = 50))

set.seed(123)
strt<-Sys.time() # start time
glmnFit <- train(x = training[,fullSet], 
                 y = training$target,
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = xgb_trcontrol)

print(Sys.time()-strt) # end time

# Get the result from glmnet
print(glmnFit)
# Save Models
saveRDS(glmnFit, paste('models/glmnFit.rds',nsample,'.seed',123,'.rds',sep=''))

# get Plot of AUC against alpha and lambda
png(filename=paste('images/glmnFit_grid_tune.plot',nsample,'.seed',123,'.png',sep=''))
plot(glmnFit)
dev.off()

# get Plot of AUC against alpha and lambda (bubble chart)
png(filename=paste('images/glmnFit_AUC_vs_alpha_lambda.plot',nsample,'.seed',123,'.png',sep=''))
ggplot(glmnFit$results, aes(x = as.factor(alpha), y = lambda, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")
dev.off

glmnFit0 <- glmnFit
glmnFit0$results$lambda <- format(round(glmnFit0$results$lambda, 3))

glmnPlot <- plot(glmnFit0,
                 plotType = "level",
                 cuts = 15,
                 scales = list(x = list(rot = 90, cex = .65)))

# get Plot of AUC distribution alongside different alpha settings
png(filename=paste('images/glmnplot_AUC_Ridge_Lasso.plot',nsample,'.seed',123,'.png',sep=''))
update(glmnPlot,
       ylab = "Mixing Percentage\nRidge <---------> Lasso",
       sub = "",
       main = "Area Under the ROC Curve",
       xlab = "Amount of Regularization")
dev.off()

### Model : Support Vector Machines
library(kernlab)

# Running SVM Radial Kernel
set.seed(123)
sigmaRangeFull <- sigest(data.matrix(training[,fullSet]))
svmRGridFull <- expand.grid(sigma =  as.vector(sigmaRangeFull)[1],
                            C = 2^(-3:4))
set.seed(123)
strt<-Sys.time() # start time
svmRFitFull <- train(x = training[,fullSet], 
                     y = training$target,
                     method = "svmRadial",
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     tuneGrid = svmRGridFull,
                     trControl = xgb_trcontrol)

print(Sys.time()-strt) # end time
print(svmRFitFull)

# Save Models
saveRDS(svmRFitFull, paste('models/svmRFitFull.rds',nsample,'.seed',123,'.rds',sep=''))

## Store images - SVM radial kernel basis
# get plot of AUC against alpha and lambda
png(filename=paste('images/svmRFitFull_grid_tune.plot',nsample,'.seed',123,'.png',sep=''))
plot(svmRFitFull)
dev.off()

# get plot of AUC against sigma
png(filename=paste('images/svmRFitFull_AUC_vs_sigma.plot',nsample,'.seed',123,'.png',sep=''))
ggplot(svmRFitFull$results, aes(x = as.factor(sigma), y = C , size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")
dev.off()

#SVM Polynomial Kernel
svmPGrid <-  expand.grid(degree = 1:2,
                         scale = c(0.01, .005),
                         C = 2^(seq(-6, -2, length = 10)))
set.seed(123)
strt<-Sys.time() # start time
svmPFitFull <- train(x = training[,fullSet], 
                     y = training$target,
                     method = "svmPoly",
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     tuneGrid = svmPGrid,
                     trControl = xgb_trcontrol)

print(Sys.time()-strt) # end time
print(svmPFitFull)

# Save Models
saveRDS(svmPFitFull, paste('models/svmPFitFull.rds',nsample,'.seed',123,'.rds',sep=''))

## Store images - SVM radial kernel basis
# get plot of AUC against alpha and lambda
png(filename=paste('images/svmPFitFull_grid_tune.plot',nsample,'.seed',123,'.png',sep=''))
plot(svmPFitFull)
dev.off()

# get plot of AUC against sigma
png(filename=paste('images/svmPFitFull_AUC_vs_sigma.plot',nsample,'.seed',123,'.png',sep=''))
ggplot(svmPFitFull$results, aes(x = as.factor(degree), y = C , size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")
dev.off()

#####################################################################################
### Create Prediction for Testing Set
validation <- data.frame(order_no = testing$order_no , customer_id = testing$customer_id, product_id = testing$product_id, 
                         obs = testing$target)
validation$label <- ifelse(val.xgbTree$obs == 1,
                           "Actual_outcome:Purchase", 
                           "Actual_outcome:Not_Purchase")

# Model : Extreme Gradient Boosting
validation$prob.xgbTree <- predict(xgbTree, testing[,fullSet],type='prob')[,'Y']
validation$pred.xgbTree <- predict(xgbTree, testing[,fullSet])

# Model : Neural Network (model1)
validation$prob.nnetFit <- predict(nnetFit, testing[,fullSet],type='prob')[,'Y']
validation$pred.nnetFit <- predict(nnetFit, testing[,fullSet])

# Model : Neural Network (model2)
validation$prob.nnetFit2 <- predict(nnetFit2, testing[,fullSet],type='prob')[,'Y']
validation$pred.nnetFit2 <- predict(nnetFit2, testing[,fullSet])

# Model : Neural Network (model3)
validation$prob.nnetFit3 <- predict(nnetFit3, testing[,fullSet],type='prob')[,'Y']
validation$pred.nnetFit3 <- predict(nnetFit3, testing[,fullSet])

# Model : Neural Network (model4)
validation$prob.nnetFit4 <- predict(nnetFit4, testing[,fullSet],type='prob')[,'Y']
validation$pred.nnetFit4 <- predict(nnetFit4, testing[,fullSet])

# Model : Penalized Logistic Regression 
validation$prob.glmnFit<- predict(glmnFit, testing[,fullSet],type='prob')[,'Y']
validation$pred.glmnFit <- predict(glmnFit, testing[,fullSet])

# Model : Support Vector Machine - Radial Kernel Basis
validation$prob.svmRFitFull <- predict(svmRFitFull, testing[,fullSet],type='prob')[,'Y']
validation$pred.svmRFitFull <- predict(svmRFitFull, testing[,fullSet])

# Model : Support Vector Machine - Polynomial Kernel Basis
validation$prob.svmPFitFull <- predict(svmPFitFull, testing[,fullSet],type='prob')[,'Y']
validation$pred.svmPFitFull <- predict(svmPFitFull, testing[,fullSet])
