### R code for Feature Construction on Items and User-Dependent Variables
### Author : Adi Budyanto
### Date : 20 August 2016

require(data.table)
require(plyr)
require(doMC)
cores <- detectCores()
if(cores > 1) {
  registerDoMC(cores)
}

# set working path in Desktop
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data");source('~/PycharmProjects/dissertation/newscr/func.R')
} else {
  setwd("~/Dropbox/dissertation/raw_data"); source('~/Dropbox/dissertation/src/newscr/func.R')
}

#####################################################################################
### All variables were engineered from 2014 data set 
# load selected variables from product table
globalItem <- read.csv('features/features.item.global.csv')
names(globalItem)
tmpCol <- c("product_id","manufacturer","category","brand","subcategory", #part 1
            "subcat.lens.manu", "lens.brand.manu", "subcat.brand", "lens.brand", #part 2
            "pack_qty")
selectedFeatItem <- globalItem[, tmpCol]


# load selected customers 2014 (who recorded transactions both in 2014 and 2015)
load('data/keys/cust14Filter.Rda')
load('data/trans.Rda')
tmpCol <- c("customer_id","product_id","order_no","order_date","price_type","global_line_total_exc_vat", "discount_percent","qty")
# only capture 2014 transactions 
tmpTrans <- trans[which(trans$customer_id %in% cust14Filter & year(trans$order_date)==2014 ),tmpCol]

# create global user - product features
tmpTrans <- merge(tmpTrans, selectedFeatItem, by=c("product_id"), all.x = T)
tmpTrans$apprx_discount <- ifelse(tmpTrans$discount_percent == 0,0,
                                  (tmpTrans$global_line_total_exc_vat*1.0066)/(tmpTrans$discount_percent*10)-tmpTrans$global_line_total_exc_vat)
tmpTrans$actual_qty <- tmpTrans$qty/tmpTrans$pack_qty 

#####################################################################################
# 1 Category
featCatNbProd <- user_dependent(data = tmpTrans, feature = "category", numeric = "actual_qty")
featCatTransSpent <- user_dependent(data = tmpTrans, feature = "category", numeric = "global_line_total_exc_vat")
featCatDiscReceived <- user_dependent(data = tmpTrans, feature = "category", numeric = "apprx_discount")
featCatUniqueOrder <- user_dependent_uniqueTrans(data = tmpTrans, feature ="category")

# 2 Sub Category
featSubNbProd <- user_dependent(data = tmpTrans, feature = "subcategory", numeric = "actual_qty")
featSubTransSpent <- user_dependent(data = tmpTrans, feature = "subcategory", numeric = "global_line_total_exc_vat")
featSubDiscReceived <- user_dependent(data = tmpTrans, feature = "subcategory", numeric = "apprx_discount")
featSubUniqueOrder <- user_dependent_uniqueTrans(data = tmpTrans, feature ="subcategory")

# 3 Manufacturer
featManNbProd <- user_dependent(data = tmpTrans, feature = "manufacturer", numeric = "actual_qty")
featManTransSpent <- user_dependent(data = tmpTrans, feature = "manufacturer", numeric = "global_line_total_exc_vat")
featManDiscReceived <- user_dependent(data = tmpTrans, feature = "manufacturer", numeric = "apprx_discount")
featManUniqueOrder <- user_dependent_uniqueTrans(data = tmpTrans, feature ="manufacturer")

# 4 Brand
featBrandNbProd <- user_dependent(data = tmpTrans, feature = "brand", numeric = "actual_qty")
featBrandTransSpent <- user_dependent(data = tmpTrans, feature = "brand", numeric = "global_line_total_exc_vat")
featBrandDiscReceived <- user_dependent(data = tmpTrans, feature = "brand", numeric = "apprx_discount")
featBrandUniqueOrder <- user_dependent_uniqueTrans(data = tmpTrans, feature ="brand")

# 5 Subcategory lenstype brand
featSubLensmanuNbProd <- user_dependent(data = tmpTrans, feature = "subcat.lens.manu", numeric = "actual_qty")
featSubLensmanuTransSpent <- user_dependent(data = tmpTrans, feature = "subcat.lens.manu", numeric = "global_line_total_exc_vat")
featSubLensmanuDiscReceived <- user_dependent(data = tmpTrans, feature = "subcat.lens.manu", numeric = "apprx_discount")
featSubLensmanuUniqueOrder <- user_dependent_uniqueTrans(data = tmpTrans, feature ="subcat.lens.manu")

# 6 Lenstype brand Manufacturer
featLensBrandManuNbProd <- user_dependent(data = tmpTrans, feature = "lens.brand.manu", numeric = "actual_qty")
featLensBrandManuTransSpent <- user_dependent(data = tmpTrans, feature = "lens.brand.manu", numeric = "global_line_total_exc_vat")
featLensBrandManuDiscReceived <- user_dependent(data = tmpTrans, feature = "lens.brand.manu", numeric = "apprx_discount")
featLensBrandManuUniqueOrder <- user_dependent_uniqueTrans(data = tmpTrans, feature ="lens.brand.manu")

# 7 Subcategory Brand
featSubBrandNbProd <- user_dependent(data = tmpTrans, feature = "subcat.brand", numeric = "actual_qty")
featSubBrandTransSpent <- user_dependent(data = tmpTrans, feature = "subcat.brand", numeric = "global_line_total_exc_vat")
featSubBrandDiscReceived <- user_dependent(data = tmpTrans, feature = "subcat.brand", numeric = "apprx_discount")
featSubBrandUniqueOrder <- user_dependent_uniqueTrans(data = tmpTrans, feature ="subcat.brand")

# 8 Lenstype Brand
featLensBrandNbProd <- user_dependent(data = tmpTrans, feature = "lens.brand", numeric = "actual_qty")
featLensBrandTransSpent <- user_dependent(data = tmpTrans, feature = "lens.brand", numeric = "global_line_total_exc_vat")
featLensBrandDiscReceived <- user_dependent(data = tmpTrans, feature = "lens.brand", numeric = "apprx_discount")
featLensBrandUniqueOrder <- user_dependent_uniqueTrans(data = tmpTrans, feature ="lens.brand")

#####################################################################################

dir.create('features/features.cust.dependent')

# storing files according to each figures (part 1)
save(featCatNbProd,featSubNbProd,featManNbProd,featBrandNbProd,
     file = 'features/features.cust.dependent/feat.user.dependent.NbProd.Rda')
save(featCatTransSpent,featSubTransSpent, featManTransSpent,featBrandTransSpent,
     file = 'features/features.cust.dependent/feat.user.dependent.TransSpent.Rda')
save(featCatDiscReceived,featSubDiscReceived, featManDiscReceived,featBrandDiscReceived,
     file = 'features/features.cust.dependent/feat.user.dependent.DiscReceived.Rda')
save(featCatUniqueOrder,featSubUniqueOrder, featManUniqueOrder,featBrandUniqueOrder,
     file = 'features/features.cust.dependent/feat.user.dependent.UniqueOrder.Rda')

# storing files according to each figures (part 2)
save(featSubLensmanuNbProd,featLensBrandManuNbProd,featSubBrandNbProd,featLensBrandNbProd,
     file = 'features/features.cust.dependent/feat.user.dependent.NbProd.part2.Rda')
save(featSubLensmanuTransSpent,featLensBrandManuTransSpent,featSubBrandTransSpent,featLensBrandTransSpent,
     file = 'features/features.cust.dependent/feat.user.dependent.TransSpent.part2.Rda')
save(featSubLensmanuDiscReceived,featLensBrandManuDiscReceived,featSubBrandDiscReceived,featLensBrandDiscReceived,
     file = 'features/features.cust.dependent/feat.user.dependent.DiscReceived.part2.Rda')
save(featSubLensmanuUniqueOrder,featLensBrandManuUniqueOrder,featSubBrandUniqueOrder,featLensBrandUniqueOrder,
     file = 'features/features.cust.dependent/feat.user.dependent.UniqueOrder.part2.Rda')

# merging all tables
# bug found in renaming same variables
# list.temp_table = lapply(ls(pattern = "feat"), get)
# feat.user.dependent = Reduce(function(...) merge(..., by=c("customer_id"), all=T), list.temp_table)

# summarized <- merge(featCatNbProd, featCatTransSpent)
# summarized <- merge(summarized,featCatDiscReceived)
# summarized <- merge(summarized,featCatUniqueOrder)
# summarized <- merge(summarized,featSubNbProd)
# summarized <- merge(summarized,featSubTransSpent)
# summarized <- merge(summarized,featSubDiscReceived)
# summarized <- merge(summarized,featSubUniqueOrder)
# summarized <- merge(summarized,featManNbProd)
# summarized <- merge(summarized,featManTransSpent)
# summarized <- merge(summarized,featManDiscReceived)
# summarized <- merge(summarized,featManUniqueOrder)
# summarized <- merge(summarized,featBrandNbProd)
# summarized <- merge(summarized,featBrandTransSpent)
# summarized <- merge(summarized,featBrandDiscReceived)
# summarized <- merge(summarized,featBrandUniqueOrder)

# store file in both format (rda and csv)
# save(summarized, file = 'features/feat.user.dependent.Rda')
# write.table(summarized, 'features/feat.user.dependent.csv', row.names = F, col.names = T, sep=',')
