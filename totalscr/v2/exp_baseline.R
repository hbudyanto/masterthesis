## Experiment with Baseline Models
# date : 23rd August 2016

require(data.table)
require(plyr)
library(lubridate)
require(doParallel)
require(caret)

# set working path in Desktop
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data")  
  source('~/PycharmProjects/dissertation/src/v2/func.R')
} else {
  setwd("~/Dropbox/dissertation/raw_data")
  source('~/Dropbox/dissertation/src/func.R')
}

nsample = 5000;
load(paste('features/features.cust.item.random/preModelData',nsample,'.seed',123,'.rda',sep=''))

### Run baseline models for comparison
require(recommenderlab)
# create dataframe consisting customer and product relationship table
tmpTrain <- dummyProductID(data = training)
tmpTest <- dummyProductID(data = testing)

# manipulate Train table to have exact number of product as test table
prodMissingTrain <- setdiff(colnames(tmpTest),colnames(tmpTrain))
tmpMissing <- matrix(0, nrow=nrow(tmpTrain), ncol=length(prodMissingTrain))
colnames(tmpMissing) <- prodMissingTrain
tmpTrain <- cbind(tmpTrain, tmpMissing)

# manipulate test table to have exact number of product as train table
prodMissingTest <- setdiff(colnames(tmpTrain),colnames(tmpTest))
tmpMissing <- matrix(0, nrow=nrow(tmpTest), ncol=length(prodMissingTest))
colnames(tmpMissing) <- prodMissingTest
tmpTest <- cbind(tmpTest, tmpMissing)

# # keep customers who do not have any record transaction in Testset ONLY (see note in problems with createDatapartition in above)
# tmpUserMissing <- setdiff(unique(testing$customer_id), tmpTest$customer_id)
# tmpMissing <- matrix(0, nrow=length(tmpUserMissing), ncol=ncol(tmpTest)-1)
# tmp <- data.frame('customer_id' = tmpUserMissing, tmpMissing)
# colnames(tmp)[-1] <- colnames(tmpTest)[-1]
# tmpTest <- rbind(tmpTest, tmp)

# re-sorting table in the same format for both train and test
sortCol <-sort(intersect(colnames(tmpTest), colnames(tmpTrain)))
tmpTrain <- tmpTrain[,sortCol]
tmpTest <- tmpTest[,sortCol]

# formating the above dataframe into a matrix 
# training data
holderTrain  <- data.matrix(tmpTrain[2:ncol(tmpTrain)]) 
rownames(holderTrain) <- tmpTrain$customer_id
# testing data
holderTest  <- data.matrix(tmpTest[2:ncol(tmpTest)]) 
rownames(holderTest) <- tmpTest$customer_id

# converted to realRatingmatrix Object
realMatrixTrain <- binarize(as(holderTrain, "realRatingMatrix"), minRating = 1)
realMatrixTest <- binarize(as(holderTest, "realRatingMatrix"), minRating = 1)

pred.random <- Recommender(realMatrixTrain, method = "RANDOM"); #names(getModel(pred.popular))
names(getModel(pred.UBCF))
# make prediction
recom.random <- predict(pred.random, realMatrixTest, type="ratingMatrix", n= 4)
# store the results
recom.random <- as(recom.random, "matrix")

# 2. User-based filtering
pred.UBCF <- Recommender(realMatrixTrain, method = "UBCF"); #names(getModel(pred.popular))

# make prediction
recom.UBCF <- predict(pred.UBCF, realMatrixTest, type="ratingMatrix", n = 4)
# store the results
recom.UBCF <- as(recom.UBCF, "matrix")

recom.UBCF[which(rowSums(recom.UBCF)<=2),]
sum(as.numeric(recom.UBCF[which(rowSums(recom.UBCF) <=2 & rownames(recom.UBCF) == 'u102726'),]))


tmpVal <- testing[which(testing$customer_id =='u100124'), c('customer_id', 'product_id', 'target')]
tmpVal$pred.random <- getAffinityScore(holder = recom.random, test= tmpVal[, c('customer_id', 'product_id')])
tmpVal$pred.UBCF <- getAffinityScore(holder = recom.UBCF, test= tmpVal[, c('customer_id', 'product_id')])



