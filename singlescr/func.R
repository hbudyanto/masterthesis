
substrLeft <- function(x, n){substr(x, 1, nchar(x)-n)}
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
`%between%`<-function(x,rng) x>rng[1] & x<rng[2]

## A function to find and remove zero-variance ("ZV") predictors
noZV <- function(x) {
  keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
  x[,keepers,drop = FALSE]
}

dummyVariables <- function(data, feature) {
  # data = purchaseData
  # feature = "product_id"
  # # create dummy customer vs items relationship table
  tmp <- data[, c('key',feature)]
  tmp <- tmp[!duplicated(tmp),]
  tmp <- tmp[with(tmp, order(key)), ]
  # create product dummy variables
  colnames(tmp)[2] <- "temp"
  tmpMatrix <- data.frame(cbind(tmp, data.frame(model.matrix(~temp-1, tmp))))
  # rename the first words of "product_id" in the colnames and just mention the product
  colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^....", "",colnames(tmpMatrix)[3:ncol(tmpMatrix)])
  # aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
  col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)]
  tmp <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(key), .SDcols=c(col)]
  setDF(tmp) # convert back to dataframe
  tmp <- noZV(tmp)
  return(tmp)
}

## A function to compute item-based similarity matrix
## data -> row : users , colmn : variables - dummy (0,1)
getItemBasedSimMatrix <- function(data) {
  # Checker to see the data format
  stopifnot(colnames(data)[1]== 'customer_id')
  stopifnot(length(dim(data)) == 2)
  ## Create a function to exclude customer_id in column names
  "%w/o%" <- function(x, y) x[!x %in% y] 
  # Define Cosine Similarity function
  getCosine <- function(x,y)
    # Calculate the cosine similarity between two vectors
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(this.cosine)
  }
  # Drop any column named "customer_id"
  data.ibs <- data[,colnames(data) %w/o% c("customer_id","order_no")]
  # Create empty matrix for item similarity  
  ibs.similarity  <- matrix(NA, nrow=ncol(data.ibs), ncol=ncol(data.ibs),
                            # naming rows 
                            dimnames=list(colnames(data.ibs),
                                          # naming columns              
                                          colnames(data.ibs)))
  
  # Both loop parallelized:
  # Fill the above matrix with item-to-item similarity (cosine)
  # Use maximum number of available
  ibs.similarity <- foreach(i=1:ncol(data.ibs), .combine='rbind') %:%
    foreach(j=1:ncol(data.ibs), .combine='c') %dopar% {
      getCosine(as.matrix(data.ibs[i]),as.matrix(data.ibs[j]))
    }
  # Formatting back to data frame type of format
  ibs.similarity <- as.data.frame(ibs.similarity)
  # Formatting back the rownames and the columnnames
  colnames(ibs.similarity) <- colnames(data.ibs)
  rownames(ibs.similarity) <- colnames(data.ibs)
  return(ibs.similarity)
}

readjustColUserProdTable <- function(dummyData , purchaseData){
  # A function to retrieve back keys variable (column_id, order_no)
  dummyData <- head(tmp.data)
  data <- merge(dummyData, purchaseData[,c('customer_id',"order_no","key")], by=c("key"))
  data$key <- NULL
  
  # readjusting the columns into a proper order
  maxCol <- ncol(data); keyCol <- colnames(data)[(maxCol-1):maxCol]; resCol <- colnames(data)[1:(maxCol-2)]
  data <- data[, c(keyCol, resCol)]
  return(data)
}

## A function to produce user - based similarity matrix
## data -> row : users , colmn : variables - dummy (0,1)
## matrix similarity : matrix produced by getItemBasedSimMatrix
getItemCFPred <- function (data, matrix_similarity)
{
  # get unique customer id
  uniqueCust <- unique(data$customer_id)
  ## Parellization of Loops 
  results <- foreach(i=1:length(uniqueCust), .combine='rbind', .packages='foreach' ) %dopar% {
    # The value of the inner foreach loop is returned as
    # the value of the body of the outer foreach loop
    foreach(j=3:ncol(data), .combine='c') %do% {
      user <- uniqueCust[i]
      product <- colnames(data)[j]
      
      # select top-N = 5 items( + 1 which is the item itself)
      topN<-((head(n=6,(matrix_similarity[order(matrix_similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # We then get the user's purchase history for those 10 items
      topN.userPurchases <- as.matrix(data[which(data$customer_id == user),topN.names])
      
      # We then calculate the score for that product and that user
      sum(topN.userPurchases %*% topN.similarities)/ (nrow(topN.userPurchases) * sum(topN.similarities))
    }     
  }
  # naming product id as matrix's column
  colnames(results) <- colnames(data)[3:ncol(data)]
  # naming costumner_id as matrix's row
  rownames(results) <- uniqueCust
  return(results)
}

getPredScore <- function(holder, feature )
{
  # melting data tables from wide to long
  tempHolder <- melt(holder, measure.vars =colnames(holder), variable.name = feature, value.name = 'score')
  # rename column variables
  colnames(tempHolder)[1] <- 'customer_id'
  colnames(tempHolder)[2] <- feature
  # merge between target data and holder
  tmp <- merge(targetTemp, subset(tempHolder, score>0), by =c('customer_id', feature), all.x=T)
  tmp$score[is.na(tmp$score)] <- 0
  colnames(tmp)[ncol(tmp)] <- paste('score.',feature,sep='')
  return(tmp)
}

getPurchaseData <- function(feature, numeric, holder, customer )
{
  tempHolder <- holder[which(holder$customer %in% customer),]
  stopifnot(colnames(tempHolder)[2] == feature | colnames(tempHolder)[3] == numeric)

  tmp <- merge(purchaseTemp, tempHolder, by =c('customer_id', feature), all.x=T)
  colnames(tmp)[ncol(tmp)] <- 'score'
  tmp$score[is.na(tmp$score)] <- 0
  colnames(tmp)[ncol(tmp)] <- paste('pastRec.',feature,'.',numeric,sep='')
  return(tmp)
}

getPurchaseData90 <- function(feature, numeric, holder, customer )
{
  tempHolder <- holder[which(holder$customer %in% customer),]
  stopifnot(colnames(tempHolder)[2] == feature | colnames(tempHolder)[3] == numeric)
  
  tmp <- merge(purchaseTemp, tempHolder, by =c('customer_id', feature), all.x=T)
  colnames(tmp)[ncol(tmp)] <- 'score'
  tmp$score[is.na(tmp$score)] <- 0
  colnames(tmp)[ncol(tmp)] <- paste('pastRec90.',feature,'.',numeric,sep='')
  return(tmp)
}

# checker vector is empty or not
vector.is.empty <- function(x) return(length(x) ==0 )

getAffinityScore <- function(holder,test) 
{
  # get score from item-based CF on per user basis
  affinity_scores <- vector(mode="numeric", length=nrow(test))
  user <- rownames(holder)
  product <- as.character(colnames(holder))
  for (i in 1:nrow(test))
  { 
    row <- which(user == test[i,1])
    col <- which(product == test[i,2])
    if (vector.is.empty(row) | vector.is.empty(col)) {
      affinity_scores[i] <- 0
    } else {
      affinity_scores[i] <- holder[row,col]
    }
  }
  return(affinity_scores)
}

################################################################################################################
### R code for Feature Construction on User-Dependent Variables
user_dependent <- function(data, feature, numeric){  
  # input data frame consisisting of product_id, actual_qty , global_line_total_exc_vat , apprx_discount
  # allocate temporary variables
  tmpData <- data[,c("customer_id","product_id","order_date","order_no",feature,numeric)]
  # summing over multiple chosen columns for one input numeric variable
  groupingClm <- c('customer_id',feature)
  tmpGrouping <- ddply(tmpData, groupingClm, function(x) {colSums(x[numeric])} ,.parallel = cores > 1 )
  colnames(tmpGrouping)[2] <- feature
  return(tmpGrouping)
}

user_dependent_uniqueTrans <- function(data,feature)
{
  # input data frame consisisting of product_id, actual_qty , global_line_total_exc_vat , apprx_discount
  # allocate temporary variables
  tmpData <- data[,c("customer_id","order_date","order_no",feature)]
  tmpData <- tmpData[!(duplicated(tmpData)),]
  tmpData$qty <- 1
  # summing over multiple chosen columns for one input numeric variable
  groupingClm <- c('customer_id',feature)
  tmpGrouping <- ddply(tmpData, groupingClm, function(x) {colSums(x["qty"])} ,.parallel = cores > 1 )
  colnames(tmpGrouping)[2] <- feature
  return(tmpGrouping)
}

