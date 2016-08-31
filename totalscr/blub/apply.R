

## SAPPLY, LAPPY, APPLY

m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)
head(m)


# APPLY

# get the mean values of all rows
apply(m,1,mean)
# get the mean values of columns
apply(m,2,mean)

# count elements of x which are lesser than 0
apply(m,2,function(x) length(x[x<0]))
# note : the function takes an arbitrary var called x, which is in 
# the form of a vector
apply(m,2, is.vector)

# mean of positive elements in each column
apply(m,2, function(x) mean(x[x>0]))

# SAPPLY
# it will return as a vector
sapply(1:5, function(x) x^3)
# it will return as a list
lapply(1:5, function(x) x^3)

# from sapply to lappy
sapply(1:5, function(x) x^3, simplify=F)

# from lapply to sapply
unlist(lapply(1:5, function(x) x^3))


sapply(1:ncol(m), function(x) mean(m[,x]))
sapply(1:3, function(x, y) mean(y[,x]), y=m)


# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family#gs.ecwv7_0
# dimension 1 -> view from top to bottom /collection of line vectors (rows)
# dimension 2 -> view from left to right / collection of columns vector
X<-matrix(rnorm(30), nrow=5, ncol=6)
# row
apply(X, 1 ,sum) 
# col
apply(X, 2 ,sum) 

####### STACK OVERFLOW
# http://stackoverflow.com/questions/3505701/r-grouping-functions-sapply-vs-lapply-vs-apply-vs-tapply-vs-by-vs-aggrega

# apply - When you want to apply a function to the rows or columns of a matrix 
# (and higher-dimensional analogues); not generally advisable for data frames 
# as it will coerce to a matrix first.

# Two dimensional matrix
M <- matrix(seq(1,16), 4, 4)
# Apply min to rows
apply(M, 1, min)
# Apply max to columns
apply(M, 2, max)

# 3 dimensional array
M <- array( seq(32), dim = c(4,4,2))

# Apply min across each M[*, , ] - i.e Sum across 2nd and 3rd dimension
apply(M, 1, min)
apply(M, 2, min)

# Apply min across each M[*, , ] - i.e Sum across 2nd and 3rd dimension
apply(M, 1, sum) # sum rows over two dimension
apply(M, 2, sum) # sum columns over two dim

# operation across each M[*,*,] - i.e sum accross 3rd dimension
apply(M, c(1,2), sum)
apply(M, c(1,2), sum)

# If you want row/column means or sums for a 2D matrix, be sure to investigate the highly optimized, 
# lightning-quick colMeans, rowMeans, colSums, rowSums.

# lapply - When you want to apply a function to each element of a list 
# in turn and get a list back

x <- list(a = 1, b = 1:3, c = 10:100) 
lapply(x, FUN = length) 

# can be used to know how many products per user bought
lapply(x, FUN = sum) 


