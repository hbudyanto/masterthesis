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
featCatNbProdWith90 <- user_dependent(data = tmpTrans, feature = "category", numeric = "actual_qty")
featCatTransSpentWith90 <- user_dependent(data = tmpTrans, feature = "category", numeric = "global_line_total_exc_vat")
featCatDiscReceivedWith90 <- user_dependent(data = tmpTrans, feature = "category", numeric = "apprx_discount")
featCatUniqueOrderWith90 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="category")

# 2 Sub Category
featSubNbProdWith90 <- user_dependent(data = tmpTrans, feature = "subcategory", numeric = "actual_qty")
featSubTransSpentWith90 <- user_dependent(data = tmpTrans, feature = "subcategory", numeric = "global_line_total_exc_vat")
featSubDiscReceivedWith90 <- user_dependent(data = tmpTrans, feature = "subcategory", numeric = "apprx_discount")
featSubUniqueOrderWith90 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="subcategory")

# 3 Manufacturer
featManNbProdWith90 <- user_dependent(data = tmpTrans, feature = "manufacturer", numeric = "actual_qty")
featManTransSpentWith90 <- user_dependent(data = tmpTrans, feature = "manufacturer", numeric = "global_line_total_exc_vat")
featManDiscReceivedWith90 <- user_dependent(data = tmpTrans, feature = "manufacturer", numeric = "apprx_discount")
featManUniqueOrderWith90 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="manufacturer")

# 4 Brand
featBrandNbProdWith90 <- user_dependent(data = tmpTrans, feature = "brand", numeric = "actual_qty")
featBrandTransSpentWith90 <- user_dependent(data = tmpTrans, feature = "brand", numeric = "global_line_total_exc_vat")
featBrandDiscReceivedWith90 <- user_dependent(data = tmpTrans, feature = "brand", numeric = "apprx_discount")
featBrandUniqueOrderWith90 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="brand")

# 5 Subcategory lenstype brand
featSubLensmanuNbProdWith90 <- user_dependent(data = tmpTrans, feature = "subcat.lens.manu", numeric = "actual_qty")
featSubLensmanuTransSpentWith90 <- user_dependent(data = tmpTrans, feature = "subcat.lens.manu", numeric = "global_line_total_exc_vat")
featSubLensmanuDiscReceivedWith90 <- user_dependent(data = tmpTrans, feature = "subcat.lens.manu", numeric = "apprx_discount")
featSubLensmanuUniqueOrderWith90 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="subcat.lens.manu")

# 6 Lenstype brand Manufacturer
featLensBrandManuNbProdWith90 <- user_dependent(data = tmpTrans, feature = "lens.brand.manu", numeric = "actual_qty")
featLensBrandManuTransSpentWith90 <- user_dependent(data = tmpTrans, feature = "lens.brand.manu", numeric = "global_line_total_exc_vat")
featLensBrandManuDiscReceivedWith90 <- user_dependent(data = tmpTrans, feature = "lens.brand.manu", numeric = "apprx_discount")
featLensBrandManuUniqueOrderWith90 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="lens.brand.manu")

# 7 Subcategory Brand
featSubBrandNbProdWith90 <- user_dependent(data = tmpTrans, feature = "subcat.brand", numeric = "actual_qty")
featSubBrandTransSpentWith90 <- user_dependent(data = tmpTrans, feature = "subcat.brand", numeric = "global_line_total_exc_vat")
featSubBrandDiscReceivedWith90 <- user_dependent(data = tmpTrans, feature = "subcat.brand", numeric = "apprx_discount")
featSubBrandUniqueOrderWith90 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="subcat.brand")

# 8 Lenstype Brand
featLensBrandNbProdWith90 <- user_dependent(data = tmpTrans, feature = "lens.brand", numeric = "actual_qty")
featLensBrandTransSpentWith90 <- user_dependent(data = tmpTrans, feature = "lens.brand", numeric = "global_line_total_exc_vat")
featLensBrandDiscReceivedWith90 <- user_dependent(data = tmpTrans, feature = "lens.brand", numeric = "apprx_discount")
featLensBrandUniqueOrderWith90 <- user_dependent_uniqueTrans(data = tmpTrans, feature ="lens.brand")

#####################################################################################

dir.create('features/features.cust.dependent/within90/')

# storing files according to each figures 
save(featCatNbProdWith90,featSubNbProdWith90,featManNbProdWith90,featBrandNbProdWith90,featSubLensmanuNbProdWith90, featLensBrandManuNbProdWith90, featSubBrandNbProdWith90, featLensBrandNbProdWith90,
     file = 'features/features.cust.dependent/within90/feat.user.dependent.NbProdWith90.Rda')
save(featCatTransSpentWith90,featSubTransSpentWith90, featManTransSpentWith90,featBrandTransSpentWith90, featSubLensmanuTransSpentWith90, featLensBrandManuTransSpentWith90, featSubBrandTransSpentWith90, featLensBrandTransSpentWith90,
     file = 'features/features.cust.dependent/within90/feat.user.dependent.TransSpentWith90.Rda')
save(featCatDiscReceivedWith90,featSubDiscReceivedWith90, featManDiscReceivedWith90,featBrandDiscReceivedWith90,featSubLensmanuDiscReceivedWith90, featLensBrandManuDiscReceivedWith90, featSubBrandDiscReceivedWith90, featLensBrandDiscReceivedWith90,
     file = 'features/features.cust.dependent/within90/feat.user.dependent.DiscReceivedWith90.Rda')
save(featCatUniqueOrderWith90,featSubUniqueOrderWith90, featManUniqueOrderWith90,featBrandUniqueOrderWith90, featSubLensmanuUniqueOrderWith90, featLensBrandManuUniqueOrderWith90, featSubBrandUniqueOrderWith90, featLensBrandUniqueOrderWith90,
     file = 'features/features.cust.dependent/within90/feat.user.dependent.UniqueOrderWith90.Rda')

load('features/features.cust.dependent/within90/feat.user.dependent.NbProdWith90.Rda')
load('features/features.cust.dependent/within90/feat.user.dependent.TransSpentWith90.Rda')
load('features/features.cust.dependent/within90/feat.user.dependent.DiscReceivedWith90.Rda')
load('features/features.cust.dependent/within90/feat.user.dependent.UniqueOrderWith90.Rda')



