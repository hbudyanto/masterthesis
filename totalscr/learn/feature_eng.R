setwd("~/PycharmProjects/dissertation/raw_data")
prod <- read.csv('data/prod.csv')
trans <- read.csv('data/trans.csv')

library(plyr)
library(caret)
library(lubridate)

cores <- 3
if(cores > 1) {
  library(doMC)
  registerDoMC(cores)
}

# construct sample analysis
set.seed(123)
id = sample(trans$customer_id,10)
temp.data = trans[which(trans$customer_id %in% id),
                  c("customer_id","order_no","product_id",
                    "global_line_total_exc_vat")]
t.product = prod[,c("product_id","name","modality","product_type","category")]
temp.data = join(temp.data,t.product, by=c('product_id'))
temp.data = temp.data[with(temp.data, order(customer_id)), ]
names(temp.data)

# replace missing ('') and rename it -> facrorize it
temp.data$modality <- gsub(" ", "", tolower(as.character(temp.data$modality)))
temp.data$modality [temp.data$modality == ""] <- 'nonLens'
temp.data$mod.modality  <- factor(paste("modal_", temp.data$modality , sep = ""))

# count unique order made by each customer
x = temp.data[,c(1,2)]; x = x[!duplicated(x),]
ddply(x, .(customer_id), function(x) c(numOrder = nrow(x)))

# count total product purchased per modality
countOrder <- ddply(temp.data, .(customer_id),
      function(x) as.data.frame(t(as.matrix(table(x$mod.modality)))),
      .parallel = cores > 1)
countOrder <- noZV(countOrder)

# featurize modality
temp.data$mod.category = temp.data$category
levels(temp.data$mod.category) = c('dailies','other','solution','colours','dailies',
                                   'eye_care','eye_care','eye_care','solution','solution',
                                   'eye_care','eye_care','other_','solution','dailies',
                                   'eye_care','other','other_','other_','solution',
                                   'other_')

temp.data = data.frame(cbind(temp.data, data.frame(model.matrix(~mod.category-1, temp.data))))

# knowing all similar columns begin with mod.
colnames(temp.data)[grep("mod.", names(temp.data), fixed = TRUE)]

# count variety of different products purchased by each customer
countMod <- ddply(temp.data, .(customer_id),
                 function(x) {
                   data.frame(
                     mod.categorydailies = sum(x$mod.categorydailies, na.rm = TRUE),
                     mod.categoryother = sum(x$mod.categoryother, na.rm = TRUE),
                     mod.categorysolution = sum(x$mod.categorysolution, na.rm = TRUE),
                     mod.categorycolours = sum(x$mod.categorycolours, na.rm = TRUE),
                     mod.categoryeye_care = sum(x$mod.categoryeye_care, na.rm = TRUE),
                     mod.categoryother_ = sum(x$mod.categoryother_, na.rm = TRUE))
                 },
                 .parallel = cores > 1)
countMod <- noZV(countMod)


## For each role, calculate the frequency of people in each age group
unique(do.call("c", temp.data[,grep("mod.category", names(temp.data), fixed = TRUE)]))

temp.data$price.exc.vat = as.integer(as.character(temp.data$global_line_total_exc_vat))
bPrice <- unique(temp.data[,grep("price", names(temp.data), fixed = TRUE)])
bPrice <- bPrice[!is.na(bPrice)]
temp.data$price.exc.vat <- factor(paste(temp.data$price.exc.vat), levels = paste(sort(bPrice)))

## count the number of different products each customer buys in different price ranges
countModPrice <- ddply(temp.data, .(customer_id),
      function(x) {
        tabDF <- as.data.frame(table(x$mod.modality, x$price.exc.vat))
        out <- data.frame(t(tabDF$Freq))
        names(out) <- paste(tabDF$Var1, tabDF$Var2, sep = ".")
        out
      },
      .parallel = cores > 1)
countModPrice <- noZV(countModPrice)
