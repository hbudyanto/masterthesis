### Results Analysis
### Author : Adi Budyanto
### Date : 23 August 2016


# svmPFitFull <- readRDS(paste('models/svmPFitFull.rds',5000,'.seed',123,'.rds',sep=''))
nnetFit <- readRDS(paste('models/nnetFit.rds',5000,'.seed',123,'.rds',sep=''))
nnetFit2 <- readRDS(paste('models/nnetFit2.rds',5000,'.seed',123,'.rds',sep=''))
nnetFit3 <- readRDS(paste('models/nnetFit3.rds',5000,'.seed',123,'.rds',sep=''))
nnetFit4 <- readRDS(paste('models/nnetFit4.rds',5000,'.seed',123,'.rds',sep=''))

nnetFit$results[which(nnetFit$results$Spec== max(nnetFit$results$Spec)),]
nnetFit2$results[which(nnetFit2$results$Spec== max(nnetFit2$results$Spec)),]
nnetFit3$results[which(nnetFit3$results$Spec== max(nnetFit3$results$Spec)),]
nnetFit4$results[which(nnetFit4$results$Spec== max(nnetFit4$results$Spec)),]

# size 1:5
# decay : c(0, .1)

glmnFit <- readRDS(paste('models/glmnFit.rds',5000,'.seed',123,'.rds',sep=''))

glmnFit$results[which(glmnFit$results$Spec== max(glmnFit$results$Spec)),]

svmRFit <- readRDS(paste('models/svmRFitFull.rds',5000,'.seed',123,'.rds',sep=''))
svmPFit <- readRDS(paste('models/svmPFitFull.rds',5000,'.seed',123,'.rds',sep=''))

svmRFit$results[which(svmRFit$results$Spec== max(svmRFit$results$Spec)),]
svmPFit$results[which(svmPFit$results$Spec== max(svmPFit$results$Spec)),]

# store users who bought new 2015 product
tmp <- trans[which(trans$product_id %in% prodnew2015),]
custid.boughtnew15 <- as.character(unique(tmp$customer_id))
save(custid.boughtnew15,file = 'features/features.cust.item.random/custid.boughtnew15.rda')

# load data validation
load(paste('results/validation_allModels',3000,'.seed',123,'.rda',sep=''))
# load a list of product 15
load('features/features.cust.item.random/newprod15.rda')

valExistingItems <- validation[which(!(validation$product_id %in% prodnew2015)),]
valNewItems <- validation[which(validation$product_id %in% prodnew2015),]

#####################################################################################
##### ExistingItems
confusionMatrix(valExistingItems$pred.xgbTree,valExistingItems$obs) # 0.9075
confusionMatrix(valExistingItems$pred.nnetFit,valExistingItems$obs) # 0.7690
confusionMatrix(valExistingItems$pred.nnetFit2,valExistingItems$obs) # 0.7715
confusionMatrix(valExistingItems$pred.nnetFit3,valExistingItems$obs) # 0.8065
confusionMatrix(valExistingItems$pred.nnetFit4,valExistingItems$obs) # error - run again
confusionMatrix(valExistingItems$pred.glmnFit,valExistingItems$obs) # 0.7483
confusionMatrix(valExistingItems$pred.svmRFitFull,valExistingItems$obs) # 0.7493
confusionMatrix(valExistingItems$pred.svmPFitFull,valExistingItems$obs) # 0.7493

####
##### NewItems
confusionMatrix(valNewItems$pred.xgbTree,valNewItems$obs) # 0.9075
confusionMatrix(valNewItems$pred.nnetFit,valNewItems$obs) # 0.7690
confusionMatrix(valNewItems$pred.nnetFit2,valNewItems$obs) # 0.7715
confusionMatrix(valNewItems$pred.nnetFit3,valNewItems$obs) # 0.8065
confusionMatrix(valNewItems$pred.nnetFit4,valNewItems$obs) # error - run again
confusionMatrix(valNewItems$pred.glmnFit,valNewItems$obs) # 0.7483
confusionMatrix(valNewItems$pred.svmRFitFull,valNewItems$obs) # 0.7493
confusionMatrix(valNewItems$pred.svmPFitFull,valNewItems$obs) # 0.7493


library(pROC)
# svmRFitFull2014 <- merge(svmRFitFull$pred,  svmRFitFull$bestTune)
# svmRFitFull2014Roc <- roc(response = svmRFitFull2014$obs,
#                  predictor = svmRFitFull2014$Y,
#                  levels = rev(levels(svmRFitFull2014$obs)))
# plot(svmRFitFull2014Roc, type = "s", legacy.axes = TRUE)


names(validation)
# penalized logistic regresiion
glmnFitRoc <- roc(response = validation$obs,
                  predictor = validation$prob.glmnFit,
                  levels = rev(levels(validation$obs)))
# svm radial
svmRFitRoc <- roc(response = validation$obs,
                          predictor = validation$prob.svmRFitFull,
                          levels = rev(levels(validation$obs)))
# svm polynomial
svmPFitRoc <- roc(response = validation$obs,
                  predictor = validation$prob.svmPFitFull,
                  levels = rev(levels(validation$obs)))
# xgboost
xgboostRoc <- roc(response = validation$obs,
                  predictor = validation$prob.xgbTree,
                  levels = rev(levels(validation$obs)))
# nnetfit4
nnetFitRoc <- roc(response = validation$obs,
                  predictor = validation$prob.nnetFit3,
                  levels = rev(levels(validation$obs)))

plot(xgboostRoc, type = "s", col = 'red', legacy.axes = TRUE)
plot(nnetFitRoc, type = "s", add = TRUE, col = 'blue', legacy.axes = TRUE)
plot(glmnFitRoc, type = "s", add = TRUE,col = 'green', legacy.axes = TRUE)
plot(svmPFitRoc, type = "s", add = TRUE, col = 'brown', legacy.axes = TRUE)
plot(svmRFitRoc, type = "s", add = TRUE, col = 'black', legacy.axes = TRUE)
legend(0.7, .2, c('Extreme Gradient Boosting', 'Neural Nets', ' Ridge Regression', 'SVM Polynomial','SVM Radial') , 
       lty=1, col=c('red','blue','green','brown','black'), bty='n', cex=.95)



