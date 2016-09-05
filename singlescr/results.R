### Results Analysis
### Author : Adi Budyanto
### Date : 23 August 2016



nsample <- 5000

xgbTree <- readRDS(paste('models/xgbTree.rds',nsample,'.seed',123,'.rds',sep=''))
nnetFit <- readRDS(paste('models/nnetFit.rds',nsample,'.seed',123,'.rds',sep=''))
nnetFit2 <- readRDS(paste('models/nnetFit2.rds',nsample,'.seed',123,'.rds',sep=''))
nnetFit3 <- readRDS(paste('models/nnetFit3.rds',nsample,'.seed',123,'.rds',sep=''))
nnetFit4 <- readRDS(paste('models/nnetFit4.rds',nsample,'.seed',123,'.rds',sep=''))
glmnFit <- readRDS(paste('models/glmnFit.rds',nsample,'.seed',123,'.rds',sep=''))
svmRFit <- readRDS(paste('models/svmRFitFull.rds',nsample,'.seed',123,'.rds',sep=''))
svmPFit <- readRDS(paste('models/svmPFitFull.rds',nsample,'.seed',123,'.rds',sep=''))

xgbTree$results[which(xgbTree$results$Spec== max(xgbTree$results$Spec)),]
nnetFit$results[which(nnetFit$results$Spec== max(nnetFit$results$Spec)),]
nnetFit2$results[which(nnetFit2$results$Spec== max(nnetFit2$results$Spec)),]
nnetFit3$results[which(nnetFit3$results$Spec== max(nnetFit3$results$Spec)),]
nnetFit4$results[which(nnetFit4$results$Spec== max(nnetFit4$results$Spec)),]

svmRFit$results[which(svmRFit$results$Spec== max(svmRFit$results$Spec)),]
svmPFit$results[which(svmPFit$results$Spec== max(svmPFit$results$Spec)),]
glmnFit$results[which(glmnFit$results$Spec== max(glmnFit$results$Spec)),]

# load data validation
load(paste('results/validation_allsampl',nsample,'.seed',123,'.rda',sep=''))
# load a list of product 15
load('data/keys/newprod15.rda')

valExistingItems <- validation[which(!(validation$product_id %in% prodnew2015)),]
valNewItems <- validation[which(validation$product_id %in% prodnew2015),]

#####################################################################################
##### All items
confusionMatrix(validation$pred.xgbTree,validation$obs) 
confusionMatrix(validation$pred.nnetFit,validation$obs) 
confusionMatrix(validation$pred.nnetFit2,validation$obs) 
confusionMatrix(validation$pred.nnetFit3,validation$obs) 
# confusionMatrix(valExistingItems$pred.nnetFit4,valExistingItems$obs) # error - run again
confusionMatrix(validation$pred.glmnFit,validation$obs) 
confusionMatrix(validation$pred.svmRFitFull,validation$obs) 
confusionMatrix(validation$pred.svmPFitFull,validation$obs) 

##### ExistingItems
confusionMatrix(valExistingItems$pred.xgbTree,valExistingItems$obs) # 0.7280
confusionMatrix(valExistingItems$pred.nnetFit,valExistingItems$obs) # 0.6854
confusionMatrix(valExistingItems$pred.nnetFit2,valExistingItems$obs) # 0.6868
confusionMatrix(valExistingItems$pred.nnetFit3,valExistingItems$obs) # 0.6841
confusionMatrix(valExistingItems$pred.nnetFit4,valExistingItems$obs) # error - run again
confusionMatrix(valExistingItems$pred.glmnFit,valExistingItems$obs) # 0.6401
confusionMatrix(valExistingItems$pred.svmRFitFull,valExistingItems$obs) # 0.6978
confusionMatrix(valExistingItems$pred.svmPFitFull,valExistingItems$obs) # 0.6827

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


#####################################################################################
####### ROC for all

library(pROC)
names(valExistingItems)
# penalized logistic regresiion
glmnFitRoc <- roc(response = valExistingItems$obs,
                  predictor = valExistingItems$prob.glmnFit,
                  levels = rev(levels(valExistingItems$obs)))
# svm radial
svmRFitRoc <- roc(response = valExistingItems$obs,
                  predictor = valExistingItems$prob.svmRFitFull,
                  levels = rev(levels(valExistingItems$obs)))
# svm polynomial
svmPFitRoc <- roc(response = valExistingItems$obs,
                  predictor = valExistingItems$prob.svmPFitFull,
                  levels = rev(levels(valExistingItems$obs)))
# xgboost
xgboostRoc <- roc(response = valExistingItems$obs,
                  predictor = valExistingItems$prob.xgbTree,
                  levels = rev(levels(valExistingItems$obs)))
# nnetfit4
nnetFitRoc <- roc(response = valExistingItems$obs,
                  predictor = valExistingItems$prob.nnetFit,
                  levels = rev(levels(valExistingItems$obs)))

png(filename=paste('images/AUC_allmodels_plot',nsample,'.seed',123,'.png',sep=''))
plot(xgboostRoc, type = "s", col = 'red', legacy.axes = TRUE)
plot(nnetFitRoc, type = "s", add = TRUE, col = 'blue', legacy.axes = TRUE)
plot(svmRFitRoc, type = "s", add = TRUE, col = 'black', legacy.axes = TRUE)
plot(svmPFitRoc, type = "s", add = TRUE, col = 'brown', legacy.axes = TRUE)
plot(glmnFitRoc, type = "s", add = TRUE,col = 'green', legacy.axes = TRUE)
legend(0.7, .2, c('Extreme Gradient Boosting', 'Neural Nets', ' Ridge Regression', 'SVM Polynomial','SVM Radial') , 
       lty=1, col=c('red','blue','green','brown','black'), bty='n', cex=.95)
dev.off()



# List the predictors were used in the model (pg 325)
predictors(xgbTree)

# List importance predictors 
importance.xgbTree <- varImp(xgbTree)[[1]]
importance.xgbTree$predictors <- rownames(importance.xgbTree)
rownames(importance.xgbTree) <- NULL
# export to csv
write.csv(importance.xgbTree, file = paste('results/importance.xgbTree_caret',nsample,'.seed',123,'.csv',sep=''))

