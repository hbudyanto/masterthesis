setwd("~/PycharmProjects/dissertation/dropbox")

rand = 500
load(paste('features/features.cust.item.random/purchaseVector',rand,'.seed',568,'.rda',sep=''))
load(paste('features/features.cust.item.random/purchaseData',rand,'.seed',568,'.rda',sep=''))

library(doMC) # parallel running
library(data.table) # setDT function

dfTrain <- dfTrain[with(dfTrain, order(customer_id)), ]

# Select positive target only
purchase <- dfTrain[which(dfTrain$target== 1),c('customer_id','product_id')]
purchase <- purchase[with(purchase, order(customer_id)), ]

# Create Product Dummy Variables
data.purchase_matrix <- data.frame(cbind(purchase, data.frame(model.matrix(~product_id-1, purchase))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(data.purchase_matrix)[3:ncol(data.purchase_matrix)] <- sub("^..........", "", 
                                                                    colnames(data.purchase_matrix)[3:ncol(data.purchase_matrix)])

registerDoMC(12)
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(data.purchase_matrix)[3:ncol(data.purchase_matrix)] 
data.purchase <- setDT(data.purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(data.purchase) # convert back to dataframe;
rm(col)



###############################
### Item Collaborative Filtering

# Drop any column named "customer_id"
data.purchase.ibs <- (data.purchase[,!(names(data.purchase) %in% c("customer_id"))])

# Create a placeholder dataframe listing item vs. item
data.purchase.ibs.similarity  <- matrix(NA, nrow=ncol(data.purchase.ibs),ncol=ncol(data.purchase.ibs),dimnames=list(colnames(data.purchase.ibs),colnames(data.purchase.ibs)))

data.purchase.ibs.similarity[1:6,1:6]

# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(data.purchase.ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data.purchase.ibs)) {
    # Fill in placeholder with cosine similarities
    data.purchase.ibs.similarity[i,j] <- getCosine(as.matrix(data.purchase.ibs[i]),as.matrix(data.purchase.ibs[j]))
  }
}

data.purchase.ibs.similarity[1:6,1:6]

# Back to dataframe
data.purchase.ibs.similarity <- as.data.frame(data.purchase.ibs.similarity)

# Create placeholder matrix for item - item neighbourhood model
data.purchase.neighbours <- matrix(NA, nrow=ncol(data.purchase.ibs.similarity),ncol=11,dimnames=list(colnames(data.purchase.ibs.similarity)))
# Get the top 10 neighbours for each
for(i in 1:ncol(data.purchase.ibs)) 
{
  data.purchase.neighbours[i,] <- (t(head(n=11,rownames(data.purchase.ibs.similarity[order(data.purchase.ibs.similarity[,i],decreasing=TRUE),][i]))))
}

x <- data.purchase[which(data.purchase$p168==1 |data.purchase$p86==1),]
data.purchase.ibs.similarity[which(rownames(data.purchase.ibs.similarity) == 'p168'),]
data.purchase.ibs.similarity[which(rownames(data.purchase.ibs.similarity) == 'p165'),]

###############################
### User-Based Recommendation

# A placeholder matrix (use the original dataset)
holder <- matrix(NA, nrow=nrow(data.purchase),ncol=ncol(data.purchase)-1,dimnames=list((data.purchase$customer_id),colnames(data.purchase[-1])))

# Loop through the users (rows)
for(i in 1:nrow(holder)) 
{
  # Loops through the products (columns)
  for(j in 1:ncol(holder)) 
  {
    # Get the user's name and th product's name
    # We do this not to conform with vectors sorted differently 
    user <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store an empty string
    if(as.integer(data.purchase[data.purchase$customer_id==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN<-((head(n=11,(data.purchase.ibs.similarity[order(data.purchase.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # Drop the first one because it will always be the same song
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # We then get the user's purchase history for those 10 items
      topN.purchases<- data.purchase[,c("customer_id",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$customer_id==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("customer_id"))])
      
      # We then calculate the score for that product and that user
      
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } # close else statement
  } # end product for loop   
} # end user for loop

data.purchase.user.scores <- holder
data.purchase.user.scores <- apply(data.purchase.user.scores, 2, function(x) gsub("^$|^ $", 1, x))


# x <- as.numeric(apply(c, 2, function(x) gsub("NaN", 1, x)))

test <- data.frame(cbind('user' = rep(rownames(holder)[1:5],5), 'product_id' = sample(unique(dfTrain[,'product_id']),25)))
test <- test[with(test, order(user)), ]

test$affinity_user_product <- getAffinityScore(data.purchase.user.scores, test)

######## Function Needed

# Create a helper function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

getScore <- function(history, similarities)
{
  ## little modification to avoid NaN if no-similarities found in an item
  if (sum(similarities)==0) {
    result <- 0
  }
  if  (sum(similarities) > 0) {
    result <- sum(history*similarities)/sum(similarities)
  }
  return(result)
}

getAffinityScore <- function(holder,test) {
  affinity_scores <- vector(mode="numeric", length=nrow(test))
  userlist <- rownames(holder)
  songlist <- colnames(holder)
  for (i in 1:nrow(test))
  { 
    #print(i)
    row <- which(userlist == test[i,1])
    col <- which(songlist == test[i,2])
    affinity_scores[i] <- holder[row,col]
  }
  return(affinity_scores)
}

