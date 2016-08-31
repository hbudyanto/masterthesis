
###############################
### Item Collaborative Filtering

data.purchase <- x1

# Drop any column named "customer_id"
data.purchase.ibs <- (data.purchase[,!(names(data.purchase) %in% c("customer_id"))])

# Create a placeholder dataframe listing item vs. item
data.purchase.ibs.similarity  <- matrix(NA, nrow=ncol(data.purchase.ibs),ncol=ncol(data.purchase.ibs),dimnames=list(colnames(data.purchase.ibs),colnames(data.purchase.ibs)))

# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(data.purchase.ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data.purchase.ibs)) {
    # Fill in placeholder with cosine similarities
    data.purchase.ibs.similarity[i,j] <- getCosine(as.matrix(data.purchase.ibs[i]),as.matrix(data.purchase.ibs[j]))
  }
}

# Back to dataframe
data.purchase.ibs.similarity <- as.data.frame(data.purchase.ibs.similarity)

holder <- matrix(NA, nrow=nrow(data.purchase),ncol=ncol(data.purchase)-1,dimnames=list((data.purchase$customer_id),colnames(data.purchase[-1])))

# case when user did not purchase the item before
user <- rownames(holder)[1]
product <- colnames(holder)[29]

# case when user didpurchase the item before
user <- rownames(holder)[1]
product <- colnames(holder)[73]

as.integer(data.purchase[data.purchase$customer_id==user,product]) == 1

# We first have to get a product's top 10 neighbours sorted by similarity
# Get top 10 most similar items based on cosine similarity
topN<-((head(n=11,(data.purchase.ibs.similarity[order(data.purchase.ibs.similarity[,product],decreasing=TRUE),][product]))))
topN.names <- as.character(rownames(topN))
topN.similarities <- as.numeric(topN[,1])

# drop first entry -> the product itself
# topN.similarities<-topN.similarities[-1]
# topN.names<-topN.names[-1]

# We then get the user's purchase history for those 10 items
topN.purchases<- data.purchase[,c("customer_id",topN.names)]
# Get specific row which coresponds to the customer_id
topN.userPurchases<-topN.purchases[topN.purchases$customer_id==user,]
# Drop the customer_id and Get only the numerical varialbels
topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("customer_id"))])

sum(topN.userPurchases*topN.similarities)/sum(topN.similarities)
