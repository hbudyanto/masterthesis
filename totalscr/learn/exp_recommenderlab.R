### Experiment with Recommender Lab

# set working path in Desktop
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data")  
  source('~/PycharmProjects/dissertation/src/v2/func.R')
} else {
  setwd("~/Dropbox/dissertation/raw_data")
  source('~/Dropbox/dissertation/src/func.R')
}

temp = list.files(path = "features/features.matrix.cust.vars", pattern="*.Rdata")
for (i in 1:length(temp)) {load(paste("features/features.matrix.cust.vars/",temp[i],sep=""))}

##### Load data : split into training and test set
tmp.data <- useritemList[[14]]
tmp <- noZV(tmp.data)
# Create empty matrix for item similarity  
holder  <- data.matrix(tmp[2:ncol(tmp)]) 
rownames(holder) <- tmp$customer_id
typeof(holder)

#### Converted to realRatingmatrix Object
r <- as(holder, "realRatingMatrix")
r
r.test <- as(holder[1001:1100,], "realRatingMatrix")
getRatingMatrix(r)
#### Few functions to seeing into data# Check whether r and m are identical
identical(as(r, "matrix"),holder[1:1000,])
# write a list on per use basis
as(r, "list")$u1
# putting back format as data frame
head(as(r, "data.frame"))
dataframe.holder <- as(r, "data.frame")
# nice visualiasation on density of user - item relationship table
image(r, main = "Raw Ratings")
# the average ratings for per user (shows that high fragmentation on item-p1)
hist(colMeans(r), breaks=15)


#### Binarization of both training and test data
r_b <- binarize(r, minRating=1)
r_b
r_t <- binarize(r.test, minRating=1)

x <- as(r_b, "matrix")
y <- as(r, "matrix")
as(r_t[1,], "matrix")

## Evaluation Scheme
# choose only users who purhased at least two items

r_b <- r_b[rowCounts(r_b)>1]

scheme_binary <- evaluationScheme(r_b, method = 'split', train = 0.9, k=1, given = 2)
scheme_binary

algorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "user-based CF" = list(name="UBCF", param=list(nn=50)),
  "item-based CF" = list(name="IBCF", param=list(k=50)))

results_binary <- evaluate(scheme_binary, algorithms,type = "topNList", n=c(1,3,5,10,15,20))
plot(results_binary, annotate=c(1,3), legend="topright")
                           
#### Creating recommender

## 1. Popular Items
pred.popular <- Recommender(r_b, method = "POPULAR")
names(getModel(pred.popular))

getModel(pred.popular)$topN 
# a top-N list to store the popularity order and further elements

recom.popular <- predict(pred.popular, r_t, n=3)
recom.popular

# look the results as list
as(recom.popular, "list")

recom.popular <- predict(pred.popular, r_t, type="topNList")
recom.popular
# store the results
results.popular <- as(recom.popular, "matrix")

## 2. Item-based collaborative filtering (IBCF)
pred.IBCF <- Recommender(r_b, method = "IBCF")
names(getModel(pred.IBCF))

getModel(pred.IBCF)$verbose
# a top-N list to store the popularity order and further elements
recom.IBCF <- predict(pred.IBCF, r_t)
recom.IBCF

# look the results as list
as(recom.IBCF, "matrix")

recom.IBCF <- predict(pred.IBCF, r_t, type="ratingMatrix")
recom.IBCF
# recom.IBCF1 <- predict(pred.IBCF, r_t, type="ratings")
# store the results
results.IBCF <- as(recom.IBCF, "matrix")
results.IBCF[is.na(results.IBCF)] <- 0
# results.IBCF1 <- as(recom.IBCF1, "matrix")
# results.IBCF1[is.na(results.IBCF1)] <- 0

#max(abs(results.IBCF1-results.IBCF)) # no different between ratings and rating matrix

## 3. User-based collaborative filtering (UBCF)
pred.UBCF <- Recommender(r_b, method = "UBCF")
names(getModel(pred.UBCF))

getModel(pred.UBCF)$sim 
# a top-N list to store the popularity order and further elements

recom.UBCF <- predict(pred.UBCF, r_t)
recom.UBCF

# look the results as list
as(recom.UBCF, "list")

recom.UBCF <- predict(pred.UBCF, r_t, type="ratingMatrix")
recom.UBCF
# store the results
results.UBCF <- as(recom.UBCF, "matrix")

## 3. User-based collaborative filtering (UBCF)
pred.UBCF <- Recommender(r_b, method = "UBCF")
names(getModel(pred.UBCF))

getModel(pred.UBCF)$sim 
# a top-N list to store the popularity order and further elements

recom.UBCF <- predict(pred.UBCF, r_t)
recom.UBCF

# look the results as list
as(recom.UBCF, "list")

recom.UBCF <- predict(pred.UBCF, r_t, type="ratingMatrix")
recom.UBCF
# store the results
results.UBCF <- as(recom.UBCF, "matrix")

## 4. Random Search
pred.random <- Recommender(r_b, method = "RANDOM")
names(getModel(pred.random))

getModel(pred.random)$labels
# a top-N list to store the popularity order and further elements

recom.random <- predict(pred.random, r_t)
recom.random

# look the results as list
as(recom.random, "list")

recom.random <- predict(pred.random, r_t, type="ratingMatrix")
recom.random
# store the results
results.random <- as(recom.random, "matrix")
