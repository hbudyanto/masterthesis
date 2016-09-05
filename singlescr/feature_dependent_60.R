### R code for Feature Construction on Items and User-Dependent Variables within the last 60 days
### Author : Adi Budyanto
### Date : 20 August 2016

require(data.table)
require(plyr)
require(doMC)
cores <- detectCores()
if(cores > 1) {
  registerDoMC(cores)
}


# set working path in desktop & load pre-defined functions
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data");source('~/PycharmProjects/dissertation/scr/singlescr/func.R')
} else {
  setwd("~/Dropbox/dissertation/raw_data"); source('~/Dropbox/dissertation/scr/singlescr/func.R')
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
tmpTrans <- trans[which(trans$customer_id %in% cust14Filter & year(trans$order_date)== 2014 & month(trans$order_date) %between% c(10,12)),tmpCol]

# create global user - product features
tmpTrans <- merge(tmpTrans, selectedFeatItem, by=c("product_id"), all.x = T)
tmpTrans$apprx_discount <- ifelse(tmpTrans$discount_percent == 0,0,
                                  (tmpTrans$global_line_total_exc_vat*1.0066)/(tmpTrans$discount_percent*10)-tmpTrans$global_line_total_exc_vat)
tmpTrans$actual_qty <- tmpTrans$qty/tmpTrans$pack_qty 

#####################################################################################
# 1 Category
featCatNbProdWith60 <- user_dependent(data = tmpTrans, feature = "category", numeric = "actual_qty")
featCatTransSpentWith60 <- user_dependent(data = tmpTrans, feature = "category", numeric = "global_line_total_exc_vat")
featCatDiscReceivedWith60 <- user_dependent(data = tmpTrans, feature = "category", numeric = "apprx_discount")
featCatUniqueOrderWith60 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="category")

# 2 Sub Category
featSubNbProdWith60With60 <- user_dependent(data = tmpTrans, feature = "subcategory", numeric = "actual_qty")
featSubTransSpentWith60 <- user_dependent(data = tmpTrans, feature = "subcategory", numeric = "global_line_total_exc_vat")
featSubDiscReceivedWith60 <- user_dependent(data = tmpTrans, feature = "subcategory", numeric = "apprx_discount")
featSubUniqueOrderWith60 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="subcategory")

# 3 Manufacturer
featManNbProdWith60 <- user_dependent(data = tmpTrans, feature = "manufacturer", numeric = "actual_qty")
featManTransSpentWith60 <- user_dependent(data = tmpTrans, feature = "manufacturer", numeric = "global_line_total_exc_vat")
featManDiscReceivedWith60 <- user_dependent(data = tmpTrans, feature = "manufacturer", numeric = "apprx_discount")
featManUniqueOrderWith60 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="manufacturer")

# 4 Brand
featBrandNbProdWith60 <- user_dependent(data = tmpTrans, feature = "brand", numeric = "actual_qty")
featBrandTransSpentWith60 <- user_dependent(data = tmpTrans, feature = "brand", numeric = "global_line_total_exc_vat")
featBrandDiscReceivedWith60 <- user_dependent(data = tmpTrans, feature = "brand", numeric = "apprx_discount")
featBrandUniqueOrderWith60 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="brand")

# 5 Subcategory lenstype brand
featSubLensmanuNbProdWith60 <- user_dependent(data = tmpTrans, feature = "subcat.lens.manu", numeric = "actual_qty")
featSubLensmanuTransSpentWith60 <- user_dependent(data = tmpTrans, feature = "subcat.lens.manu", numeric = "global_line_total_exc_vat")
featSubLensmanuDiscReceivedWith60With60 <- user_dependent(data = tmpTrans, feature = "subcat.lens.manu", numeric = "apprx_discount")
featSubLensmanuUniqueOrderWith60 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="subcat.lens.manu")

# 6 Lenstype brand Manufacturer
featLensBrandManuNbProdWith60 <- user_dependent(data = tmpTrans, feature = "lens.brand.manu", numeric = "actual_qty")
featLensBrandManuTransSpentWith60 <- user_dependent(data = tmpTrans, feature = "lens.brand.manu", numeric = "global_line_total_exc_vat")
featLensBrandManuDiscReceivedWith60With60 <- user_dependent(data = tmpTrans, feature = "lens.brand.manu", numeric = "apprx_discount")
featLensBrandManuUniqueOrderWith60 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="lens.brand.manu")

# 7 Subcategory Brand
featSubBrandNbProdWith60 <- user_dependent(data = tmpTrans, feature = "subcat.brand", numeric = "actual_qty")
featSubBrandTransSpentWith60 <- user_dependent(data = tmpTrans, feature = "subcat.brand", numeric = "global_line_total_exc_vat")
featSubBrandDiscReceivedWith60With60 <- user_dependent(data = tmpTrans, feature = "subcat.brand", numeric = "apprx_discount")
featSubBrandUniqueOrderWith60 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="subcat.brand")

# 8 Lenstype Brand
featLensBrandNbProdWith60 <- user_dependent(data = tmpTrans, feature = "lens.brand", numeric = "actual_qty")
featLensBrandTransSpentWith60 <- user_dependent(data = tmpTrans, feature = "lens.brand", numeric = "global_line_total_exc_vat")
featLensBrandDiscReceivedWith60With60 <- user_dependent(data = tmpTrans, feature = "lens.brand", numeric = "apprx_discount")
featLensBrandUniqueOrderWith60 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="lens.brand")

#####################################################################################

dir.create('features/features.cust.dependent')

# storing files according to each figures 
save(featCatNbProd,featSubNbProdWith60,featManNbProdWith60,featBrandNbProdWith60,featSubLensmanuNbProdWith60, featLensBrandManuNbProdWith60, featSubBrandNbProdWith60, featLensBrandNbProdWith60,
     file = 'features/features.cust.dependent/feat.user.dependent.NbProdWith60.Rda')
save(featCatTransSpentWith60,featSubTransSpentWith60, featManTransSpentWith60,featBrandTransSpentWith60, featSubLensmanuTransSpentWith60, featLensBrandManuTransSpentWith60, featSubBrandTransSpentWith60, featLensBrandTransSpentWith60,
     file = 'features/features.cust.dependent/feat.user.dependent.TransSpentWith60.Rda')
save(featCatDiscReceivedWith60,featSubDiscReceivedWith60, featManDiscReceivedWith60,featBrandDiscReceivedWith60,featSubLensmanuDiscReceivedWith60, featLensBrandManuDiscReceivedWith60, featSubBrandDiscReceivedWith60, featLensBrandDiscReceivedWith60,
     file = 'features/features.cust.dependent/feat.user.dependent.DiscReceivedWith60.Rda')
save(featCatUniqueOrder,featSubUniqueOrder, featManUniqueOrder,featBrandUniqueOrderWith60, featSubLensmanuUniqueOrderWith60, featLensBrandManuUniqueOrderWith60, featSubBrandUniqueOrderWith60, featLensBrandUniqueOrderWith60,
     file = 'features/features.cust.dependent/feat.user.dependent.UniqueOrderWith60.Rda')

load('features/features.cust.dependent/feat.user.dependent.NbProdWith60.Rda')
load('features/features.cust.dependent/feat.user.dependent.TransSpentWith60.Rda')
load('features/features.cust.dependent/feat.user.dependent.DiscReceivedWith60.Rda')
load('features/features.cust.dependent/feat.user.dependent.UniqueOrderWith60.Rda')

