### R code for Item to Item Collaborative Filtering
### Author : Adi Budyanto
### Date : 15 August 2016

require(data.table)
require(plyr)
require(doMC)
registerDoMC(12)

# Load the function to run item-to-item similarity distance
source('~/PycharmProjects/dissertation/src/v1/func_v2.R')

# Load the original transaction data
trans <- read.csv('data/trans.csv')

key = trans[,c('customer_id','order_no','order_date','product_id','is_lens')]
key = key[!duplicated(key),]
key = key[with(key, order(customer_id)), ]
key$order_date <- as.POSIXct(key$order_date)
key$is2015 <- year(key$order_date) == 2015

# Select only pre2015 or 2014 year of transaction
train <- subset(key, !is2015)

## Load Item Features and select relevant features
feat.item.global <- read.csv('features/features.item.global.csv')
item.features = feat.item.global[,colnames(feat.item.global)[c(1,131:138)]]

purchase <- join(train, item.features, by=c('product_id'))
### Separate data sets into 6 features

## 1. Product_id
purchase_prod = purchase[,c('customer_id', 'product_id')]
purchase_prod = purchase_prod[!duplicated(purchase_prod),]
purchase_prod <- purchase_prod[with(purchase_prod, order(customer_id)), ]

# Create Product Dummy Variables
purchase_prod_matrix <- data.frame(cbind(purchase_prod, data.frame(model.matrix(~product_id-1, purchase_prod))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_prod_matrix)[3:ncol(purchase_prod_matrix)] <- sub("^..........", "", 
                                                                    colnames(purchase_prod_matrix)[3:ncol(purchase_prod_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_prod_matrix)[3:ncol(purchase_prod_matrix)] 
purchase_prod <- setDT(purchase_prod_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_prod) # convert back to dataframe;
rm(col)

save(purchase_prod, file = 'features/matrix_2014_item_product.Rdata')

## 2. Category Type
purchase_cat = purchase[,c('customer_id', 'category_type')]
purchase_cat = purchase_cat[!duplicated(purchase_cat),]
purchase_cat <- purchase_cat[with(purchase_cat, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_cat, data.frame(model.matrix(~category_type-1, purchase_cat))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^.............", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_cat <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_cat) # convert back to dataframe;
rm(col)

save(purchase_cat, file = 'features/matrix_2014_item_category_type.Rdata')

## 3. Category Brand

purchase_brand = purchase[,c('customer_id', 'category_brand')]
purchase_brand = purchase_brand[!duplicated(purchase_brand),]
purchase_brand <- purchase_brand[with(purchase_brand, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_brand, data.frame(model.matrix(~category_brand-1, purchase_brand))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^..............", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_brand <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_brand) # convert back to dataframe;
rm(col)

save(purchase_brand, file = 'features/matrix_2014_item_category_brand.Rdata')

## 4. Category Manufacturer
purchase_manu = purchase[,c('customer_id', 'category_manufacturer')]
purchase_manu = purchase_manu[!duplicated(purchase_manu),]
purchase_manu <- purchase_manu[with(purchase_manu, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_manu, data.frame(model.matrix(~category_manufacturer-1, purchase_manu))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^.....................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_manu <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_manu) # convert back to dataframe;
rm(col)

save(purchase_manu, file = 'features/matrix_2014_item_category_manufacturer.Rdata')

## 5. Category Type Manufacturer
purchase_cat_manu = purchase[,c('customer_id', 'category_type_manufaturer')]
purchase_cat_manu = purchase_cat_manu[!duplicated(purchase_manu),]
purchase_cat_manu <- purchase_cat_manu[with(purchase_cat_manu, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_cat_manu, data.frame(model.matrix(~category_type_manufaturer-1, purchase_cat_manu))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^.........................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_cat_manu <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_cat_manu) # convert back to dataframe;
rm(col)

save(purchase_cat_manu, file = 'features/matrix_2014_item_category_type_manufacturer.Rdata')

## 6. Sub Category Type
purchase_sub_cat = purchase[,c('customer_id', 'subcategory_type')]
purchase_sub_cat = purchase_sub_cat[!duplicated(purchase_sub_cat),]
purchase_sub_cat <- purchase_sub_cat[with(purchase_sub_cat, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_sub_cat, data.frame(model.matrix(~subcategory_type-1, purchase_sub_cat))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_sub_cat <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_sub_cat) # convert back to dataframe;
rm(col)

save(purchase_sub_cat, file = 'features/matrix_2014_item_subcategory_type.Rdata')

## 7. Sub Category Brand

purchase_sub_brand = purchase[,c('customer_id', 'subcategory_brand')]
purchase_sub_brand = purchase_sub_brand[!duplicated(purchase_sub_brand),]
purchase_sub_brand <- purchase_sub_brand[with(purchase_sub_brand, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_sub_brand, data.frame(model.matrix(~subcategory_brand-1, purchase_sub_brand))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^.................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_sub_brand <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_sub_brand) # convert back to dataframe;
rm(col)

save(purchase_sub_brand, file = 'features/matrix_2014_item_subcategory_brand.Rdata')

## 8. Sub Category Manufacturer
purchase_sub_manu = purchase[,c('customer_id', 'subcategory_manufacturer')]
purchase_sub_manu = purchase_sub_manu[!duplicated(purchase_sub_manu),]
purchase_sub_manu <- purchase_sub_manu[with(purchase_sub_manu, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_sub_manu, data.frame(model.matrix(~subcategory_manufacturer-1, purchase_sub_manu))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^........................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_sub_manu <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_sub_manu) # convert back to dataframe;
rm(col)

save(purchase_sub_manu, file = 'features/matrix_2014_item_subcategory_manufacturer.Rdata')

## 9. Category Type Manufacturer
purchase_sub_cat_manu = purchase[,c('customer_id', 'subcategory_type_manufaturer')]
purchase_sub_cat_manu = purchase_sub_cat_manu[!duplicated(purchase_sub_cat_manu),]
purchase_sub_cat_manu <- purchase_sub_cat_manu[with(purchase_sub_cat_manu, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_sub_cat_manu, data.frame(model.matrix(~subcategory_type_manufaturer-1, purchase_sub_cat_manu))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^............................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_sub_cat_manu <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_sub_cat_manu) # convert back to dataframe;
rm(col)

save(purchase_sub_cat_manu, file = 'features/matrix_2014_item_subcategory_type_manufacturer.Rdata')



