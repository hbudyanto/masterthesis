### R code for calculating Jaccard Similarity
### Author : Adi Budyanto
### Date : 20 August 2016

# Create product - user relationship table
tempData <- testing[which(testing$target==1), c('customer_id','product_id')]
tempData <- tempData[with(tempData, order(customer_id)), ]
# column x product
tmpMatrix <- data.frame(cbind(tempData, data.frame(model.matrix(~product_id-1, tempData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^..........", "",colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
tempData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(tempData) # convert back to dataframe;

data.purchase.ibs <- noZV(tempData[,!(names(tempData) %in% c("customer_id"))])

# Function to calculate Jaccard Similarity
library(Matrix)
jaccard <- function(m) {
  ## common values:
  A = tcrossprod(m)
  ## indexes for non-zero common values
  im = which(A > 0, arr.ind=TRUE)
  ## counts for each row
  b = rowSums(m)
  
  ## only non-zero values of common
  Aim = A[im]
  
  ## Jacard formula: #common / (#i + #j - #common)
  J = sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
  return( J )
}


# Create Sparse Matrix of the data
sparseMatrix.prod <- Matrix(as.matrix(data.purchase.ibs),sparse= T)
simJaccard.prod <- jaccard(tmpSparse)

# Formatting back the data
simJaccard.prod <- as.matrix(simJaccard.prod)
colnames(simJaccard.prod) = rownames(simJaccard.prod) <- rownames(data.purchase.ibs)

