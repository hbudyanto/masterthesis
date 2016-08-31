
## Experiment : User to User Similarity

new_cust <- cust[]


old_cust <- cust[which(year(cust$created_at) < 2015),c('customer_id', 'gender','days_worn')]
new_cust <- cust[which(year(cust$created_at) == 2015),c('customer_id', 'gender','days_worn')]

old_cust$gender_daysworn <- paste(old_cust$gender, old_cust$days_worn, sep='')
data_oldcust <- old_cust[,c('customer_id','gender_daysworn')]

length("gender_daysworn")

# Create Product Dummy Variables
data.oldcust_matrix <- data.frame(cbind(data_oldcust, data.frame(model.matrix(~gender_daysworn-1, data_oldcust))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(data.oldcust_matrix)[3:ncol(data.oldcust_matrix)] <- sub("^...............", "", 
                                                                    colnames(data.oldcust_matrix)[3:ncol(data.oldcust_matrix)])

registerDoMC(12)
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(data.oldcust_matrix)[3:ncol(data.oldcust_matrix)] 
data_oldcust <- setDT(data.oldcust_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(data_oldcust) # convert back to dataframe;
rm(col)


# Drop any column named "customer_id"
data_oldcust.ibs <- (data_oldcust[,!(names(data_oldcust) %in% c("customer_id"))])

# Create a placeholder dataframe listing item vs. item
data_oldcust.ibs.similarity  <- matrix(NA, nrow=ncol(data_oldcust.ibs),ncol=ncol(data_oldcust.ibs),dimnames=list(colnames(data_oldcust.ibs),colnames(data_oldcust.ibs)))

# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(data_oldcust.ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data_oldcust.ibs)) {
    # Fill in placeholder with cosine similarities
    data_oldcust.ibs.similarity[i,j] <- getCosine(as.matrix(data_oldcust.ibs[i]),as.matrix(data_oldcust.ibs[j]))
  }
}

# Back to dataframe
data.purchase.ibs.similarity <- as.data.frame(data.purchase.ibs.similarity)
