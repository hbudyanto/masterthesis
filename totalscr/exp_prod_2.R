
getCosine <- function(x,y)
  # Write a function to :
  # Calculate the cosine similarity between two vectors
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

getScore <- function(history, similarities)
  # Write a function to :
  # Get similarity score for user-item based
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

getAffinityMatrix <- function(data.purchase){
  # Write a function to :
  # Calculate similarity score matrix between user and items
  
  # Checker to see the data format
  stopifnot(colnames(data.purchase)[1]== 'customer_id')
  stopifnot(length(dim(data.purchase)) == 2)

  # Drop any column named "customer_id"
  data.purchase.ibs <- (data.purchase[,!(names(data.purchase) %in% c("customer_id"))])
  
  # Create empty matrix for item similarity  
  data.purchase.ibs.similarity  <- matrix(NA, nrow=ncol(data.purchase.ibs), ncol=ncol(data.purchase.ibs),
                                          # naming rows 
                                          dimnames=list(colnames(data.purchase.ibs),
                                          # naming columns              
                                          colnames(data.purchase.ibs)))

  # Fill the above matrix with item-to-item similarity (cosine)
  # Loop through the columns
  for(i in 1:ncol(data.purchase.ibs)) {
    # Loop through the columns for each column
    for(j in 1:ncol(data.purchase.ibs)) {
      # Fill in placeholder with cosine similarities
      data.purchase.ibs.similarity[i,j] <- getCosine(as.matrix(data.purchase.ibs[i]),as.matrix(data.purchase.ibs[j]))
    }
  }

  # Formatting back to data frame type of format
  data.purchase.ibs.similarity <- as.data.frame(data.purchase.ibs.similarity)
  
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
        holder[i,j] <-""
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
  return(holder)
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

# Get affinity user-item matrix
data.purchase.user.scores <- getAffinityMatrix(data.purchase = data.purchase)
# Replace empty cell (past purchased items) with 1
data.purchase.user.scores <- apply(data.purchase.user.scores, 2, function(x) gsub("^$|^ $", 1, x))


test <- data.frame(cbind('user' = rep(x[1:5],5), 
                         'product_id' = sample(unique(dfTrain[,'product_id']),25)))
test <- test[with(test, order(user)), ]
# Get affinity user-item score
test$affinityScore <- getAffinityScore(data.purchase.user.scores, test)

