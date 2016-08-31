require(plyr)
require(data.table)
require(doParallel)

# set working path in Desktop
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data");source('~/PycharmProjects/dissertation/newscr/func.R')
} else {
  setwd("~/Dropbox/dissertation/raw_data"); source('~/Dropbox/dissertation/newscr/func.R')
}

### All variables were engineered from 2014 data set 
# load selected variables from product table
feat.item.global <- read.csv('features/features.item.global.csv')
tmpCol <- c("product_id","manufacturer","category","brand","subcategory","subcat.manu","pack_qty")
selectedFeatItem <- feat.item.global[, tmpCol]

# load selected customers 2014 (who recorded transactions both in 2014 and 2015)
load('data/keys/cust14Filter.Rda')
# load trans data that were already formatted (date, numeric)
load('data/trans.Rda')
tmpCol <- c("customer_id","product_id","order_no","order_date")
# load selected features from item global table (category, brand, manufacturer, etc)
feat.item.global <- read.csv('features/features.item.global.csv')
item.features = colnames(feat.item.global)[c(1:7,143:ncol(feat.item.global))]

# only capture 2014 transactions 
tmpTrans <- trans[which(trans$customer_id %in% cust14Filter & year(trans$order_date)== 2014 ),tmpCol]
tmpTrans$key <- paste(tmpTrans$customer_id, tmpTrans$order_no, sep="")
purchaseData <- join(tmpTrans, feat.item.global[,item.features], by=c('product_id'))

########################################################################################
#### begin constructing dummy transactions and item-based CF on pre-selected features
# Use maximum number of available
cl <- makeCluster(detectCores()); registerDoParallel(cl) 

strt<-Sys.time() # start time

# 1 Product ID (p1,p2,p3)
custProdtable <- dummyVariables(data = purchaseData, feature = "product_id")
custProdtable.prod <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.prod <- getItemBasedSimMatrix(data=custProdtable.prod)

# 2 Sub Category (dailies, two-weeklies, etc)
custProdtable <- dummyVariables(data = purchaseData, feature = "subcategory")
custProdtable.subcategory <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.subcategory  <- getItemBasedSimMatrix(data=custProdtable.subcategory)

# 3 Category (dailies, non-dailies, other)
custProdtable <- dummyVariables(data = purchaseData, feature = "category")
custProdtable.category <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.category  <- getItemBasedSimMatrix(data=custProdtable.category)

# 4 Lens type (spherical, toric,)
custProdtable <- dummyVariables(data = purchaseData, feature = "lenstype")
custProdtable.lenstype <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.lenstype  <- getItemBasedSimMatrix(data=custProdtable.lenstype)

# 5 Brand (acuvue,etc)
custProdtable <- dummyVariables(data = purchaseData, feature = "brand")
custProdtable.brand <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.brand  <- getItemBasedSimMatrix(data=custProdtable.brand)

# 6 Manufacturer (jnj, etc)
custProdtable <- dummyVariables(data = purchaseData, feature = "manufacturer")
custProdtable.manufacturer <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.manufacturer  <- getItemBasedSimMatrix(data=custProdtable.manufacturer)

# 7 Category Lens (dailies.toric)
custProdtable <- dummyVariables(data = purchaseData, feature = "category.lens")
custProdtable.categoryLens <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.categoryLens   <- getItemBasedSimMatrix(data=custProdtable.categoryLens)

# 8 Category Brand (dailies.acuvue)
custProdtable <- dummyVariables(data = purchaseData, feature = "category.brand")
custProdtable.categoryBrand <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.categoryBrand   <- getItemBasedSimMatrix(data=custProdtable.categoryBrand)

# 9 Category Manufacturer (dailies_jnj)
custProdtable <- dummyVariables(data = purchaseData, feature = "category.manu")
custProdtable.categoryManu <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.categoryManu  <- getItemBasedSimMatrix(data=custProdtable.categoryManu)

# 10 Lenstype Brand (toric_acuvue)
custProdtable <- dummyVariables(data = purchaseData, feature = "lens.brand")
custProdtable.lensBrand <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.lensBrand  <- getItemBasedSimMatrix(data=custProdtable.lensBrand)

# 11 Lenstype Manufacturer (toric_jnj)
custProdtable <- dummyVariables(data = purchaseData, feature = "lens.manu")
custProdtable.lensManu <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.lensManu <- getItemBasedSimMatrix(data=custProdtable.lensManu)

# 12 Lenstype Sub category (toric_dailies)
custProdtable <- dummyVariables(data = purchaseData, feature = "lens.subcat")
custProdtable.lensSub <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.lensSub <- getItemBasedSimMatrix(data=custProdtable.lensSub)

# 13 Subcategory Brand (dailies_acuvue)
custProdtable <- dummyVariables(data = purchaseData, feature = "subcat.brand")
custProdtable.subBrand <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.subBrand  <- getItemBasedSimMatrix(data=custProdtable.subBrand)

# 14 Subcategory Manufacturer (dailies_jnj)
custProdtable <- dummyVariables(data = purchaseData, feature = "subcat.manu")
custProdtable.subManu <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.subManu <- getItemBasedSimMatrix(data=custProdtable.subManu)

# 15 Category Lenstype Manufacturer (nondailies_toric_jnj)
custProdtable <- dummyVariables(data = purchaseData, feature = "cat.lens.manu")
custProdtable.catLensManu <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.catLensManu <- getItemBasedSimMatrix(data=custProdtable.catLensManu)

# 16 Subcategory Lenstype Manufacturer (two_weeklies_toric_jnj)
custProdtable <- dummyVariables(data = purchaseData, feature = "subcat.lens.manu")
custProdtable.subLensManu <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.subLensManu <- getItemBasedSimMatrix(data=custProdtable.subLensManu)

# 17 Lenstype Brand Manufacturer (toric_acuvue_jnj)
custProdtable <- dummyVariables(data = purchaseData, feature = "lens.brand.manu")
custProdtable.lensBrandManu <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.lensBrandManu <- getItemBasedSimMatrix(data=custProdtable.lensBrandManu)

# 18 Brand Manufacturer (toric_acuvue_jnj)
custProdtable <- dummyVariables(data = purchaseData, feature = "brand.manu")
custProdtable.brandManu <- readjustColUserProdTable(dummyData=custProdtable, purchaseData=purchaseData)
itembasedCF.brandManu <- getItemBasedSimMatrix(data=custProdtable.brandManu)

print(Sys.time()-strt) # end time 

stopCluster(cl) # stop parallezing

########################################################################################
### Storing data.final dataframes
dir.create('features/features.matrix.cust.vars')

# storing customers product relationship matrix
useritemList = lapply(ls(pattern = "custProdtable."), get)
save(useritemList ,file= 'features/features.matrix.cust.vars/useritems_list.Rdata')

# storing data frame names
useritemColnames = ls(pattern = "custProdtable.")
save(useritemColnames, file = 'features/features.matrix.cust.vars/useritems_columnNames.Rdata' )

########################################################################################
### Storing data.final dataframes
dir.create('features/features.itembasedCF')

# storing customers product relationship matrix
CFlist = lapply(ls(pattern = "itembasedCF."), get)
save(CFlist ,file= 'features/features.itembasedCF/itembasedCFs_list.Rdata')

# storing data frame names
CFcolnames= ls(pattern = "itembasedCF.")
save(CFcolnames, file = 'features/features.itembasedCF/itembasedCFs_colNames.Rdata' )



