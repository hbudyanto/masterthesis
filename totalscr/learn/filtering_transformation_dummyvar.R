################### COMPUTING SECTIONS ###############

# learn about transformation (box cox, scale, center), apply PCA (feature extraction) 
# impute missing variables and feature selection (correlation, nearzero)
# create dummy variabels

library(AppliedPredictiveModeling)
# load the example
data(segmentationOriginal)
# see the variables
str(segmentationOriginal)
# subset the test and the trainig data
segData <- subset(segmentationOriginal, Case== "Train")
# 1009 cell <- double check : nrow(segData[which(segData$Case == 'Train'),])

cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
# now remove such columns
segData <- segData[, -(1:3)]

# remove original data's predictors whose values are binary versions of the predictors 
statusColNum = grep("Status", names(segData))
segData <- segData[, -statusColNum]
# now we have only numeric variables

####### TRANSFORMATION ########
# load library e1071 : provides sample skewness statistics
library(e1071)
# apply skewness to all predictos
skewValues <- apply(segData, 2, skewness)

# load library MASS : provides boxcox transformation - determine which types of variables to be used)
# load library caret : provides boxcoxtrans - find appropriate transformation and apply it to the new data

library(caret)
# Area1 : integer variable
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans

# original data
head(segData$AreaCh1)
hist(segData$AreaCh1) #right skewed
# after the transformation : looks like a normal
hist(predict(Ch1AreaTrans, segData$AreaCh1))

# note : !!! center and scale data prior PCA
# base R function : prcomp -> can be used for PCA
# lib caret : preProcess -> applies the transformation to a set of predictors

# below the data are centered and scaled prior to PCA
pcaObject <- prcomp(segData, center=TRUE,scale.=TRUE)
# Calculate the cumulative percentage of variance which each component accounts for
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3] # three first components account for nearly 47 % variance of the data
# the transformed values are stored in pcaObject 'x'
head(pcaObject$x[,1:5])
# another sub-object 'rotation' stores variable loadings (row -> predictor & column -> component)
head(pcaObject$rotation[, 1:5])

### note:
### pckg : spatialSign has function spatialSign(segData) to apply spatial sign transformation
### pckg : impute has function impute.knn to input missing values  (estimate missing data using KNN)
### pckg : caret has function preProcess to apply a series of transformation (transform, center, scale, impute)

# Boxcox, center, scale the data & executing PCA for signal extraction, the syntax would be :
trans <- preProcess(segData, method=c("BoxCox","center","scale","pca"))
trans

# Apply the transformation 
transformed = predict(trans,segData)
# These values are different than the previous PCA components[1] since at now, they were transformed prior to PCA
head(transformed[,1:5])
# 2,3,4,12,15,16 : same order with previous PCA
# the order of possible transformation : centering, scaling, imputation, feature extraction, spatial sign

####### FILTERING ########
# to filter near zero variance predictors, the pckg : 'caret' : 'nearZero' returns  the column numbers of any predictors 
# that fulfill the conditions (single unique value, zero variance)

nearZeroVar(segData) #no predictors should be removed

# for instance : show [2] should be removed
x = data.frame(cbind('x'=c(0,0,1,2,3,4,5,2,2,4,12,41,12,12,11),'y'= c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
nearZeroVar(x)

# filter based on correlated variables
correlations <- cor(segData)
dim(correlations)
# show the first 4 predictors
correlations[1:4, 1:4]

# to visually examine the correlation structure of the data, the corrplat pckg : corrplot
library(corrplot)
corrplot(correlations, order='hclust')
# to find predictors that are recommended to be removed
highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr) ## 33 variables
# highly correlated variables
colnames(segData)[highCorr]
# drop correleted variables off
filteredSegData = segData[, -highCorr]

## again : transformation, scaling, centering -> imputation (missing values, drop zero variance predictors) -> feature extraction (PCA)
## -> spatial sign

# note : some functions in subselect is also worth to look further

####### CREATING DUMMY VARIABLE ########
data(cars)
# write the levels of the car
type <- c("convertible", "coupe", "hatchback", "sedan", "wagon")
# get the category
cars$Type <- factor(apply(cars[, 14:18], 1, function(x) type[which(x == 1)]))
carSubset <- cars[sample(1:nrow(cars), 20), c(1, 2, 19)]
# sneak peek of the data
head(carSubset)
levels(carSubset$Type)

simpleMod <- dummyVars(~Mileage + Type,data = carSubset, 
                       ## Remove the variable name from the column name
                      levelsOnly = TRUE)
simpleMod

# to generate the dummy variables for the training set or any new samples
# used in conjuction 'predict' with 'dummyVars'
predict(simpleMod, head(carSubset))

# assume there is joint effect/interaction between mileage and type of car
withInteraction <- dummyVars(~Mileage + Type + Mileage:Type,
                             data = carSubset,
                             levelsOnly = TRUE)
withInteraction

# final data form
predict(withInteraction, head(carSubset))

# check all packages used in this 
sessionInfo()
