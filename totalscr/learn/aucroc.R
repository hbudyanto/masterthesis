##################### EVALUATION METHOD ###################
 # logistics regression : glm model
 # compute prediction on the test set
 # compute tpr and fpr
 # plot ROC and AUCROC


mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")   

set.seed(2)
split <- sample.split(mydata$admit, SplitRatio = 0.9)

# get the data
dresstrain = subset(mydata, split == T)
dresstest = subset(mydata, split == F)

# apply logistics model
model <- glm (admit ~ . , data = dresstrain, family = 'binomial')
summary(model)

# set the logit prob for the training data
prob=predict(model,type=c("response"))  

library("ROCR")
# create prediction
pred <- prediction(prob, dresstrain$admit)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, colorize = TRUE, text.adj = c(-0.2,1.7))

# create prediction with test set
prob=predict(model, newdata=dresstest, type=c("response"))  
pred <- prediction(prob, dresstest$admit)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE, text.adj = c(-0.2,1.7))

data.frame(cbind('test' = dresstest$admit,'pred'=pred@labels[[1]]))

auc <- performance(pred, measure = "auc")
# AUC
auc@y.values[[1]]
